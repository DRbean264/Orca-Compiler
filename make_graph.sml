structure MakeGraph :
sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
    val displayGraph : TextIO.outstream -> (Assem.temp -> string) -> Flow.flowgraph * Flow.Graph.node list * Assem.instr list -> unit
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

fun displayGraph out saytemp (_, [], _) = ()
  | displayGraph out saytemp (_, _, []) = () 
  | displayGraph out saytemp (Flow.FGRAPH {control, def, use, ismove}, node::nodes, instr::instrs) =
    let
        fun displayMove node =
            case Graph.Table.look (ismove, node) of
                SOME true => TextIO.output (out, "Is a move node\n")
              | SOME false => TextIO.output (out, "Is not a move node\n")
              | NONE => TextIO.output (out, "Cannot find ismove info\n")

        fun displayList (name, node) =
            case name of
                "Def" =>
                (TextIO.output (out, "Def: ");
                 case Graph.Table.look (def, node) of
                     SOME temps => app (fn t => TextIO.output (out, (saytemp t) ^ " ")) temps
                   | NONE => TextIO.output (out, "Cannot find def info");
                 TextIO.output (out, "\n"))
              | "Use" =>
                (TextIO.output (out, "Use: ");
                 case Graph.Table.look (use, node) of
                     SOME temps => app (fn t => TextIO.output (out, (saytemp t) ^ " ")) temps
                   | NONE => TextIO.output (out, "Cannot find use info\n");
                 TextIO.output (out, "\n"))
              | _ => () 
    in
        TextIO.output (out, A.format saytemp instr);
        TextIO.output (out, "Node name: " ^ (Graph.nodename node) ^ "\n");
        displayList ("Def", node);
        displayList ("Use", node);
        displayMove node;
        TextIO.output (out, "\n");
        displayGraph out saytemp (Flow.FGRAPH {control = control, def = def, use = use, ismove = ismove}, nodes, instrs)
    end
    
end
