structure Reg_Alloc : REG_ALLOC =
struct

structure F = Frame

exception UnknownTemp
exception SpillDetected

type allocation = F.register Temp.Table.table

fun rewriteProgram (instrs, spills) = instrs
                             
fun alloc (instrs, frame) =
    let
        val registers = map (fn t =>
                                case Temp.Table.look (F.tempMap, t) of
                                    SOME reg => reg
                                  | NONE => raise UnknownTemp)
                            (F.specialregs @ F.argregs @
                             F.calleesaves @ F.callersaves)
        
        (* control flow graph *)
        val (fg, nodes) = MakeGraph.instrs2graph instrs
        (* build *)
        val (igraph, node2Liveout) =
            Liveness.interferenceGraph (fg, nodes)
        (* coloring *)
        val (allocation, spills) = Color.color ({interference = igraph,
                                                 initial = F.tempMap,
                                                 spillCost = IGraph.degree,
                                                 registers = registers})
    in
        if (List.length spills) = 0
        then (instrs, allocation)
        (* TODO: if there're spills, rewrite the program,
           then call alloc again *)
        else raise SpillDetected
    end

end
