structure IGraph = FuncGraph (struct
                               type ord_key = int
                               val compare = Int.compare
                               end)

structure Liveness : LIVENESS =
struct
                             
type live = IntSet.set
type liveInfo = {livein: live, liveout: live}
type graph = unit IGraph.graph
type node = unit IGraph.node
(* in interference graph: node id === temp *)
datatype igraph =
         IGRAPH of {graph: graph,
                    tnode: Temp.temp -> node,
                    gtemp: node -> Temp.temp,
                    moves: (Temp.temp * Temp.temp) list}                       

fun topologicalSort fg start =
    let
        val nodes = Flow.Graph.nodes fg
        val N = ref (List.length nodes)
        val marked : IntSet.set ref = ref IntSet.empty

        fun dfs (nodeID, sorted) =
            case IntSet.member (!marked, nodeID) of
                true => sorted
              | false =>
                (marked := IntSet.add (!marked, nodeID);
                 nodeID::(foldl dfs sorted (Flow.Graph.preds (Flow.Graph.getNode (fg, nodeID)))))
    in
        dfs (start, [])
    end

fun constructLiveMap (fg, nodes) =
    let
        (* sorted list of flow graph node ID *)
        val sorted = topologicalSort fg (Flow.Graph.getNodeID (List.nth (nodes, (List.length nodes) - 1)))
        (* val _ = app (fn n => print (Int.toString n ^ " ")) sorted *)
        (* val _ = print "\n" *)

        (* add all temps in liveout & a0~a3 & caller saves regs to the graph *)
        fun initLiveMap [] = IntMap.empty
          | initLiveMap (node::nodes) =
            let
                val nodeID = Flow.Graph.getNodeID node
                val m = initLiveMap nodes
            in
                IntMap.insert (m, nodeID, {livein = IntSet.empty,
                                           liveout = IntSet.empty})
            end
                
        fun iterate ([], liveMap, done) = (liveMap, done)
          | iterate ((nodeID::nodeIDs), liveMap, done) =
            let
                val node = Flow.Graph.getNode (fg, nodeID)
                (* original livein and liveout *)
                val {livein = liveIn', liveout = liveOut'} =
                    IntMap.lookup (liveMap, nodeID)
                (* calculate live out of this node *)
                (* LO[n] = union LI[succ] for all succs of n *)
                fun helper (nodeID, s) =
                    let
                        val {livein, liveout} = IntMap.lookup (liveMap, nodeID)
                    in
                        IntSet.union (s, livein)
                    end
                
                val liveOut = foldl helper IntSet.empty (Flow.Graph.succs node)

                (* calculate live in of this node *)
                (* LI[n] = use[n] union (LO[n] - def[n]) *)
                val (Flow.INFO {def, use, ismove}) = Flow.Graph.nodeInfo node
                val liveIn = IntSet.union (IntSet.fromList use,
                                           IntSet.subtractList (liveOut, def))

                (* check if is done *)
                val done = if IntSet.equal (liveIn, liveIn') andalso
                              IntSet.equal (liveOut, liveOut')
                           then true andalso done
                           else false andalso done

                (* update live map *)
                val (liveMap, _) = IntMap.remove (liveMap, nodeID)
                val liveMap = IntMap.insert (liveMap, nodeID,
                                             {livein = liveIn,
                                              liveout = liveOut})
            in
                iterate (nodeIDs, liveMap, done)
            end

        fun construct liveMap =
            case iterate (sorted, liveMap, true) of
                (liveMap, true) => liveMap
              | (liveMap, false) => construct liveMap
    in
        construct (initLiveMap nodes)
    end

fun displayLiveMap saytemp (liveMap, []) = ()
  | displayLiveMap saytemp (liveMap, node::nodes) =
    let
        val nodeID = Flow.Graph.getNodeID node
        val {livein, liveout} = IntMap.lookup (liveMap, nodeID)
    in
        print ("Node: " ^ (Int.toString nodeID) ^ "\n");
        print "Liveout: ";
        app (fn t => print ((saytemp t) ^ " ")) (IntSet.listItems liveout);
        print "\n";
        displayLiveMap saytemp (liveMap, nodes)
    end
        
fun showGraph ig =
    let
        fun helper (nodeID, _) =
            "Node" ^ (Int.toString nodeID) ^ ": " ^
            (Frame.saytemp nodeID)
    in
        IGraph.printGraph helper ig
    end
        
fun interferenceGraph (fg, nodes) =
    let
        val liveMap = constructLiveMap (fg, nodes)
        (* val _ = displayLiveMap Frame.saytemp (liveMap, nodes) *)
        val existed = ref IntSet.empty

        fun helper (node, (ig, moves)) = 
            let
                fun addNode (ig, nodeID) =
                    if IntSet.member (!existed, nodeID)
                    then ig
                    else (existed := IntSet.add (!existed, nodeID);
                          IGraph.addNode (ig, nodeID, ()))
                
                val nodeID = Flow.Graph.getNodeID node
                (* val _ = print ("Processing flow node" ^ (Int.toString nodeID)
                               ^ "...\n") *)
                val (Flow.INFO {def, use, ismove}) = Flow.Graph.nodeInfo node
                val {livein, liveout} = IntMap.lookup (liveMap, nodeID)
            in
                case ismove of
                    true =>
                    let
                        (* a move only has one def and one use *)
                        val def = List.nth (def, 0)
                        val use = List.nth (use, 0)
                        val ig = addNode (ig, def)
                        val ig = addNode (ig, use)
                        val ig = foldl (fn (lo, ig) =>
                                           if use = lo
                                           then ig
                                           else
                                               let
                                                   val ig = addNode (ig, lo)
                                               in
                                                   IGraph.doubleEdge (ig, def, lo)
                                               end)
                                       ig (IntSet.listItems liveout)
                    in
                        (ig, (def, use)::moves)
                    end
                  | false =>
                    (foldl (fn (d, ig) =>
                               let
                                   val ig = addNode (ig, d)
                               in
                                   foldl (fn (lo, ig) =>
                                             let
                                                 val ig = addNode (ig, lo)
                                             in
                                                 IGraph.doubleEdge (ig, d, lo)
                                             end
                                         ) ig (IntSet.listItems liveout)
                               end) ig def,
                     moves)
            end

        fun tnode ig nodeID = IGraph.getNode (ig, nodeID)

        fun flowNode2LiveOut node =
            let
                val nodeID = Flow.Graph.getNodeID node
                val {livein, liveout} = IntMap.lookup (liveMap, nodeID)
            in
                IntSet.listItems liveout
            end

        val (ig, moves) = foldl helper (IGraph.empty, []) nodes
        (* postprocessing the moves, convert them from ID to igraph node *)
        (* val moves = map (fn (id1, id2) => (IGraph.getNode (ig, id1), *)
        (*                                    IGraph.getNode (ig, id2))) moves *)
    in
        (IGRAPH {graph = ig,
                 tnode = tnode ig,
                 gtemp = IGraph.getNodeID,
                 moves = moves},
         flowNode2LiveOut)
    end
        
fun show (IGRAPH {graph = ig, tnode, gtemp, moves}) =
    let
        fun helper (nodeID, _) =
            "Node" ^ (Int.toString nodeID) ^ ": " ^
            (Frame.saytemp nodeID)

        fun filter nodeID = true
    in
        IGraph.printGraph' filter helper (TextIO.stdOut, ig)
    end

fun show' (out, IGRAPH {graph = ig, tnode, gtemp, moves}) =
    let
        val predefRegs = IntSet.fromList (Frame.calleesaves @ Frame.callersaves @
                                          Frame.specialregs @ Frame.argregs)
        fun filter nodeID = if IntSet.member (predefRegs, nodeID)
                            then false
                            else true
        
        fun stringify (nodeID, _) =
            "Node" ^ (Int.toString nodeID) ^ ": " ^
            (Frame.saytemp nodeID)
    in
        IGraph.printGraph' filter stringify (out, ig)
    end
        
end
