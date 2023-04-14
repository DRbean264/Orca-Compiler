structure Color : COLOR =
struct

structure F = Frame

exception ErrorPickingSpill

type allocation = F.register Temp.Table.table

fun color {interference = Liveness.IGRAPH {graph = ig, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
        val K = List.length registers

        (* (defID * useID) -> map of set *)
        fun transMove [] = IntMap.empty
          | transMove ((defID, useID)::moves) =
            let
                val moveMap = transMove moves
            in
                case IntMap.find (moveMap, defID) of
                    SOME s => IntMap.insert (moveMap, defID,
                                             IntSet.add (s, useID))
                  | NONE => IntMap.insert (moveMap, defID,
                                           IntSet.add (IntSet.empty, useID))
            end

        fun assignColor (stack, alias, spills) = (initial, spills)
            
        (* ig: interference graph, IGraph
           stack: select stack, list
           moveMap: move edges, intmap of intset 
           alias: alias of coalesced nodes, intmap
           spills: spill nodes, list*)
        fun main (ig, stack, moveMap, alias, spills) =
            let
                (* get move related nodes from moveMap *)
                (* it's an int set *)
                val moveRelated = IntMap.foldl IntSet.union IntSet.empty moveMap
                val moveRelated = IntSet.union ((IntSet.fromList (IntMap.listKeys moveMap)), moveRelated)
                
                (* interference graph * stack -> 
                   interference graph * stack *)
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
                                simplify (IGraph.removeNode (ig, nID), nID::stack)
                              | NONE => (ig, stack, false)
                    end

                fun coalesce (ig, moveMap, alias, changed) = (ig, moveMap, alias, changed)

                (* pick a non-precolored & high degree node *)
                fun pickSpill (ig, stack) =
                    let
                        fun helper ([], (cand, cost)) = cand
                          | helper (node::nodes, (cand, cost)) =
                            let
                                val id = IGraph.getNodeID node
                                val cost' = spillCost node
                            in
                                if not (IntSet.member (moveRelated, id)) andalso
                                   cost' > cost
                                then helper (nodes, (id, cost'))
                                else helper (nodes, (cand, cost))
                            end
                                              
                        val spill = helper (IGraph.nodes ig, (~1, ~1))
                        (* remove it from the graph & push it onto stack *)
                        val ig = IGraph.removeNode (ig, spill)
                    in
                        (ig, spill::stack, spill)
                    end

                (* freeze one move *)
                fun freeze moveMap =
                    let
                        val ((k, vset)::_) = IntMap.listItemsi moveMap
                        val (v::vs) = IntSet.listItems vset
                    in
                        (* update moveMap *)
                        if (List.length vs) = 0
                        then moveMap
                        else IntMap.insert (moveMap, k, IntSet.fromList vs)
                    end

                (* simplify until nothing can be removed *)
                val (ig, stack, done) = simplify (ig, stack)
            in
                if done
                then assignColor (stack, alias, spills)
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

        val moveMap = transMove moves
    in
        main (ig, [], moveMap, IntMap.empty, [])
    end
                             
end
