structure MakeGraph :
sig
    val instrs2graph : Assem.instr list -> Flow.flowInfo Flow.Graph.graph * Flow.flowInfo Flow.Graph.node list
    val displayGraph : Flow.flowInfo Flow.Graph.graph -> (Assem.temp -> string) -> unit
    val reset : unit -> unit
end = 
struct

structure A = Assem
exception NodeIDNotFound

structure labelNodeMap = BinaryMapFn (struct type ord_key = string
                                             val compare = String.compare
                                      end)
structure idInstrMap = BinaryMapFn (struct type ord_key = int
                                           val compare = Int.compare
                                    end)
val lnMap : Flow.flowInfo Flow.Graph.node labelNodeMap.map ref = ref (labelNodeMap.empty)
val iiMap : Assem.instr idInstrMap.map ref = ref (idInstrMap.empty)
val nodeID = ref 0

fun nextID () =
    let
        val id = !nodeID
    in
        nodeID := id + 1;
        id
    end

fun reset () = (lnMap := labelNodeMap.empty;
                iiMap := idInstrMap.empty;
                nodeID := 0)
                                                       
fun instrs2graph [] = (Flow.Graph.empty, [])
  | instrs2graph ((A.OPER {assem, dst, src, jump})::instrs) =
    let
        val (fg, nodes) = instrs2graph instrs
        val id = nextID ()
        val (fg, node) = Flow.Graph.addNode' (fg, id,
                                              Flow.INFO {def = dst,
                                                         use = src,
                                                         ismove = false})
        val (fg, nextNodes) =
            case jump of
                SOME labs =>
                foldl (fn (lab, (fg, ns)) =>
                          case labelNodeMap.find (!lnMap, Symbol.name lab) of
                            SOME n => (fg, n::ns)
                          | NONE =>
                            let
                                val (fg, n) =
                                    Flow.Graph.addNode' (fg, nextID (),
                                                         Flow.INFO {def = [],
                                                                    use = [],
                                                                    ismove = false})
                            in
                                lnMap := labelNodeMap.insert (!lnMap, Symbol.name lab, n);
                                (fg, n::ns)
                            end) (fg, []) labs
              | NONE => case nodes of
                            [] => (fg, [])
                          | (n::_) => (fg, [n])
        val fg = foldl (fn (n, fg) =>
                           Flow.Graph.addEdge (fg,
                                               {from = Flow.Graph.getNodeID node,
                                                to = Flow.Graph.getNodeID n}))
                       fg nextNodes
    in
        iiMap := idInstrMap.insert (!iiMap, id, A.OPER {assem = assem, dst = dst, src = src, jump = jump});
        (fg, node::nodes)
    end
  | instrs2graph ((A.LABEL {assem, lab})::instrs) =
    let
        val (fg, nodes) = instrs2graph instrs
        val (fg, node, id) =
            case labelNodeMap.find (!lnMap, Symbol.name lab) of
                SOME n => (fg, n, Flow.Graph.getNodeID n)
              | NONE =>
                let
                    val id = nextID ()
                    val (fg, n) =
                        Flow.Graph.addNode' (fg, id,
                                             Flow.INFO {def = [],
                                                        use = [],
                                                        ismove = false})
                in
                    lnMap := labelNodeMap.insert (!lnMap, Symbol.name lab, n);
                    (fg, n, id)
                end
        val fg = case nodes of
                     (n::_) => Flow.Graph.addEdge
                                   (fg, {from = Flow.Graph.getNodeID node,
                                         to = Flow.Graph.getNodeID n})
                   | [] => fg
    in
        iiMap := idInstrMap.insert (!iiMap, id, A.LABEL {assem = assem, lab = lab});
        (fg, node::nodes)
    end
  | instrs2graph ((A.MOVE {assem, dst, src})::instrs) =
    let
        val (fg, nodes) = instrs2graph instrs
        val id = nextID ()
        val (fg, node) = Flow.Graph.addNode' (fg, id,
                                              Flow.INFO {def = [dst],
                                                         use = [src],
                                                         ismove = true})
        val fg = case nodes of
                     (n::_) => Flow.Graph.addEdge
                                   (fg, {from = Flow.Graph.getNodeID node,
                                         to = Flow.Graph.getNodeID n})
                   | [] => fg
    in
        iiMap := idInstrMap.insert (!iiMap, id, A.MOVE {assem = assem, dst = dst, src = src});
        (fg, node::nodes)
    end

fun displayGraph fg saytemp =
    let
        fun printInfo (nodeID, Flow.INFO {def, use, ismove}) =
            let
                val assem = (case idInstrMap.find (!iiMap, nodeID) of
                                 SOME a => A.format saytemp a
                               | NONE => raise NodeIDNotFound)
                val assem = if (String.isSuffix "\n" assem) = true
                            then String.substring (assem, 0, (String.size assem) - 1)
                            else assem
            in
                assem ^ " (n" ^ (Int.toString nodeID) ^
                "): def -> " ^ foldl (fn (t, s) => s ^ " " ^ (saytemp t)) "" def ^
                " use -> " ^ foldl (fn (t, s) => s ^ " " ^ (saytemp t)) "" use ^
                " ismove -> " ^ (case ismove of
                                     true => "true"
                                   | false => "false")
            end
    in
        Flow.Graph.printGraph printInfo fg
    end
    
end
