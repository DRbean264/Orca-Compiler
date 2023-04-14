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


        fun assignColor stack = 
            
        (* ig: interference graph, IGraph
           stack: select stack, list
           moveMap: move edges, intmap of intset 
           alias: alias of coalesced nodes, intmap
           spills: spill nodes, list*)
        fun main (ig, stack, moveMap, alias, spills) =
            let
                (* get move related nodes from moveMap *)
                (* it's an int set *)
                val moveRelated
                
                (* interference graph * stack -> interference graph * stack *)
                fun simplify (ig, stack) =
                    let
                        (* check if there're only precolored nodes left *)
                        fun checkDone [] = true
                          | checkDone (node::nodes) =
                            if Temp.Table.inDomain (initial, IGraph.getNodeID node)
                            then checkDone nodes
                            else false
                        
                        (* find a trivial and not move-related and 
                           non-precolored node,
                           return nodeID *)
                        fun findCand [] = NONE
                          | findCand (node::nodes) =
                            let
                                val nodeID = IGraph.getNodeID node
                            in
                                if ((IGraph.degree node) < K
                                    andalso
                                    not (IntSet.member (moveRelated, nodeID))
                                    andalso
                                    (Temp.Table.inDomain (initial, nodeID)))
                                then SOME nodeID
                                else findCand nodes
                            end
                                
                        val nodes = IGraph.nodes ig
                    in
                        if checkDone nodes
                        then (ig, stack, true)
                        else
                            case findCand nodes of
                                SOME nID =>
                                (* remove it from the graph &
                                   push it onto stack *)
                                simplify (IGraph.removeNode (ig, nID), nID::stack, false)
                              | NONE => (ig, stack, false)
                    end

                fun coalesce (ig, moveMap, alias, changed) = (ig, moveMap, alias, changed)

                fun pickSpill (ig, stack) = (ig, stack, 0)

                fun freeze moveMap = moveMap

                (* simplify until nothing can be removed *)
                val (ig, stack, done) = simplify (ig, stack)
            in
                if done
                then assignColor (stack, alias)
                else
                    let
                        (* coalesce until no moves can be merged *)
                        val (ig, moveMap, alias, changed) = coalesce (ig, moveMap, alias, false)
                    in
                        (* if some moves are coalesced, then simplify again *)
                        if changed = true
                        then main (ig, stack, moveMap, alias, spills)
                        (* check if there's move we can freeze *)
                        else if IntMap.isEmpty moveMap
                        then
                            let
                                val (ig, stack, nodeID) = pickSpill (ig, stack)
                            in
                                main (ig, stack, moveMap, alias, nodeID::spills)
                            end
                         (* freeze one move, and simplify again *)
                        else main (ig, stack, freeze moveMap, alias, spills)
                    end
            end
    in
    end
                             
end
