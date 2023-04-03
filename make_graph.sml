structure MakeGraph :
sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end = 
struct

structure A = Assem

structure labelNodeMap = BinaryMapFn (struct type ord_key = string
                                                val compare = String.compare
                                         end)
val lnMap : Flow.Graph.node labelNodeMap.map ref = ref (labelNodeMap.empty)
                      
fun instrs2graph [] = (Flow.FGRAPH {control = Graph.newGraph (),
                                    def = Graph.Table.empty,
                                    use = Graph.Table.empty,
                                    ismove = Graph.Table.empty},
                       [])
  | instrs2graph ((A.OPER {assem, dst, src, jump})::instrs) =
    let
        val (Flow.FGRAPH {control, def, use, ismove}, nodes) = instrs2graph instrs
        val node = Graph.newNode control
        val def' = Graph.Table.enter (def, node, dst)
        val use' = Graph.Table.enter (use, node, src)
        val ismove' = Graph.Table.enter (ismove, node, false)
        val nextNodes =
            case jump of
                SOME labs =>
                map (fn lab =>
                        case labelNodeMap.find (!lnMap, Symbol.name lab) of
                            SOME n => n
                          | NONE =>
                            let
                                val n = Graph.newNode control
                            in
                                lnMap := labelNodeMap.insert (!lnMap, Symbol.name lab, n);
                                n
                            end) labs
              | NONE => case nodes of
                            [] => []
                          | (n::_) => [n]
    in                
        app (fn n => Graph.mk_edge ({from = node, to = n})) nextNodes;
        (Flow.FGRAPH {control = control, def = def', use = use', ismove = ismove'},
         node::nodes)
    end
  | instrs2graph ((A.LABEL {assem, lab})::instrs) =
    let
        val (Flow.FGRAPH {control, def, use, ismove}, nodes) = instrs2graph instrs
        val node = case labelNodeMap.find (!lnMap, Symbol.name lab) of
                       SOME n => n
                     | NONE =>
                       let
                           val n = Graph.newNode control
                       in
                           lnMap := labelNodeMap.insert (!lnMap, Symbol.name lab, n);
                           n
                       end
        val def' = Graph.Table.enter (def, node, [])
        val use' = Graph.Table.enter (use, node, [])
        val ismove' = Graph.Table.enter (ismove, node, false)
    in
        case nodes of
            (n::_) => Graph.mk_edge ({from = node, to = n})
          | [] => ();
        (Flow.FGRAPH {control = control, def = def', use = use', ismove = ismove'},
         node::nodes)
    end
  | instrs2graph ((A.MOVE {assem, dst, src})::instrs) =
    let
        val (Flow.FGRAPH {control, def, use, ismove}, nodes) = instrs2graph instrs
        val node = Graph.newNode control
        val def' = Graph.Table.enter (def, node, [dst])
        val use' = Graph.Table.enter (use, node, [src])
        val ismove' = Graph.Table.enter (ismove, node, true)
    in
        case nodes of
            (n::_) => Graph.mk_edge ({from = node, to = n})
          | [] => ();
        (Flow.FGRAPH {control = control, def = def', use = use', ismove = ismove'},
         node::nodes)
    end
        
end
