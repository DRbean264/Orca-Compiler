structure Reg_Alloc : REG_ALLOC =
struct

structure F = Frame

exception UnknownTemp
exception SpillDetected

type allocation = F.register Temp.Table.table

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

structure Color : COLOR =
struct

type allocation = F.register Temp.Table.table

fun color {{graph = ig, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
        val K = List.length registers

        (* (nodeID * nodeID) -> map of set *)
        fun transMove moves = 
                            
        fun main (ig, stack, moveMap, spills) =
            let
                (* get move related nodes from moveMap *)
                val moveRelated
                
                (* if degree is < K & not a move related node *)
                (* interference graph * stack -> interference graph * stack *)
                fun simplify (ig, stack) = (ig, stack)

                fun coalesce (ig, moveMap, changed) = coalesce (ig, moveMap, changed)

                fun pickSpill (ig, stack) = (ig, stack, 0)

                val (ig, stack) = simplify (ig, stack)
                val (ig, moveMap, changed) = coalesce (ig, moveMap, false)
            in
                if changed = true
                then main (ig, stack, moveMap, spills)
                else if IntMap.empty moveMap
                then
                    let
                        val (ig, stack, nodeID) = pickSpill (ig, stack)
                    in
                        main (ig, stack, moveMap, nodeID::spills)
                    end
            end
    in
    end
                             
end
