structure Liveness : LIVENESS =
struct

structure IGraph = FuncGraph (struct
                               type ord_key = int
                               val compare = Int.compare
                               end)
                             
type live = IntSet.set
type liveInfo = {livein: live, liveout: live}
type graph = unit IGraph.graph
type node = unit IGraph.node
(* in interference graph: node id === temp *)
datatype igraph =
         IGRAPH of {graph: graph,
                    tnode: Temp.temp -> node,
                    gtemp: node -> Temp.temp,
                    moves: (node * node) list}                       

                       
(* structure Temp2Node = IntMapTable (type key = Temp.temp *)
(*                                    fun getInt k = k) *)
(* structure Node2Temp = IntMapTable (type key = node *)
(*                                    fun getInt ) *)

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
        print "\n\n";
        displayLiveMap saytemp (liveMap, nodes)
    end
        
fun interferenceGraph (fg, nodes) =
    let
        val liveMap = constructLiveMap (fg, nodes)

        fun getInitIGraph () =
            foldl (fn (node, ig) =>
                      let
                          val nodeID = Flow.Graph.getNodeID node
                          val {livein, liveout} = IntMap.lookup (liveMap, nodeID)
                      in
                          foldl (fn (lo, ig) =>
                                    IGraph.addNode (ig, lo, ()))
                                ig (IntSet.listItems liveout)
                      end) IGraph.empty nodes

        fun helper (node, (ig, moves)) = 
            let
                val nodeID = Flow.Graph.getNodeID node
                val (Flow.INFO {def, use, ismove}) = Flow.Graph.nodeInfo node
                val {livein, liveout} = IntMap.lookup (liveMap, nodeID)
            in
                case ismove of
                    true =>
                    let
                        (* a move only has one def and one use *)
                        val def = List.nth (def, 0)
                        val use = List.nth (use, 0)
                    in
                        (foldl (fn (lo, ig) =>
                                  if use = lo
                                  then ig
                                  else IGraph.doubleEdge (ig, def, lo)) ig (IntSet.listItems liveout),
                         (def, use)::moves)
                    end
                  | false =>
                    (foldl (fn (d, ig) =>
                              foldl (fn (lo, ig) =>
                                        IGraph.doubleEdge (ig, d, lo)
                                    ) ig (IntSet.listItems liveout)) ig def,
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

        val (ig, moves) = foldl helper (getInitIGraph (), []) nodes
        (* postprocessing the moves, convert them from ID to igraph node *)
        val moves = map (fn (id1, id2) => (IGraph.getNode (ig, id1),
                                           IGraph.getNode (ig, id2))) moves
    in
        (IGRAPH {graph = ig,
                 tnode = tnode ig,
                 gtemp = IGraph.getNodeID,
                 moves = moves},
         flowNode2LiveOut)
    end
        
end
