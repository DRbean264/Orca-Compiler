structure Color : COLOR =
struct

structure F = Frame

exception ErrorPickingSpill
exception UnknownAllocation
exception MoveNotFound
exception ColorNotFound
exception DEBUGGING

type allocation = F.register Temp.Table.table

fun color {interference = Liveness.IGRAPH {graph = ig, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
        val K = List.length registers
        val regSet = StringSet.fromList registers

        fun isPrecolored id = Temp.Table.inDomain (initial, id)
        (* (defID * useID) -> map of set *)
        fun transMove [] = IntMap.empty
          | transMove ((defID, useID)::moves) =
            let
                val moveMap = transMove moves
            in
                if defID = useID
                   orelse (isPrecolored defID andalso isPrecolored useID)
                then moveMap
                else
                     case IntMap.find (moveMap, defID) of
                         SOME s => IntMap.insert (moveMap, defID,
                                                  IntSet.add (s, useID))
                       | NONE => IntMap.insert (moveMap, defID,
                                                IntSet.add (IntSet.empty, useID))
            end

        (* IntMap * int -> IntMap * int *)
        fun getAlias (alias, id) =
            case IntMap.find (alias, id) of
                SOME id' =>
                let
                    val (alias, id'') = getAlias (alias, id')
                in
                    (IntMap.insert (alias, id, id''), id'')
                end
              | NONE => (alias, id)

        fun assignColor (stack, alias, spills) = 
            let
                (* spills here is an int set *)
                fun assignStack ([], allocation, spills) = (allocation, spills)
                  | assignStack ((nID, adjs)::stack, allocation, spills) =
                    let
                        (* get all available colors *)
                        val colors =
                            foldl (fn (id, ss) =>
                                      case Temp.Table.look (allocation, id) of
                                          SOME reg => StringSet.subtract (ss, reg)
                                        | NONE => ss)
                                  regSet
                                  (map (fn i => #2 (getAlias (alias, i))) adjs)
                    in
                        if StringSet.isEmpty colors
                        then
                            assignStack (stack, allocation, spills)
                        else
                            let
                                (* pick one color *)
                                val color = case StringSet.listItems colors of
                                              [] => raise ColorNotFound
                                            | c::_ => c
                                val spills = IntSet.subtract (spills, nID)
                            in
                                assignStack (stack, Temp.Table.enter (allocation,
                                                                      nID,
                                                                      color),
                                             spills)
                            end
                    end

                (* spills here is an int set *)
                fun assignAlias (allocation) =
                     foldl (fn (id, allocation) =>
                               let
                                   val (_, id') = getAlias (alias, id)
                               in                                  
                                   case Temp.Table.look (allocation, id') of
                                       SOME reg =>
                                        Temp.Table.enter (allocation, id, reg)
                                     | NONE => allocation
                               end)
                           allocation (IntMap.listKeys alias)

                val spills = IntSet.fromList spills
                (* assign colors for nodes in the stack *)
                val (allocation, spills) = assignStack (stack, initial, spills)
                (* assign colors for nodes in alias *)
                val allocation = assignAlias allocation
            in
                (allocation, IntSet.listItems spills)
            end
            
        (* ig: interference graph, IGraph
           stack: select stack, list of (id * list of adjs))
           moveMap: move edges, intmap of intset 
           alias: alias of coalesced nodes, intmap *)
        fun main (ig, stack, moveMap, alias, spills) =
            let
                (* get move related nodes from moveMap *)
                (* it's an int set *)
                val moveRelated = IntMap.foldl IntSet.union IntSet.empty moveMap
                val moveRelated = IntSet.union ((IntSet.fromList (IntMap.listKeys moveMap)), moveRelated)
                val moveRelated = IntSet.map (fn id =>
                                                 let
                                                     val (_, id') = getAlias (alias, id)
                                                 in
                                                     id'
                                                 end) moveRelated

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
                                if ((IGraph.outDegree node) < K
                                    andalso
                                    not (IntSet.member (moveRelated, nodeID))
                                    andalso
                                    not (Temp.Table.inDomain (initial, nodeID)))
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
                                let
                                    val adjs = IGraph.adj (IGraph.getNode (ig, nID))
                                in
                                    (* remove it from the graph &
                                       push it onto stack *)
                                    simplify (IGraph.removeNode (ig, nID),
                                              (nID, adjs)::stack)
                                end
                              | NONE => (ig, stack, false)
                    end

                fun coalesce (ig, moveMap, alias, changed) =
                    let
                        fun briggs (n1, n2) =
                            let
                                val newAdjIDs = IntSet.fromList (List.concat [(IGraph.adj n1), (IGraph.adj n2)])
                            in
                                IntSet.numItems newAdjIDs < K
                            end
                        fun george (n1, n2) =
                            let
                                val n1nbs = IntSet.fromList (IGraph.adj n1)
                                val n2nbs = IntSet.fromList (IGraph.adj n2)
                                val notShared = IntSet.difference (n1nbs, n2nbs)
                                val degrees = map (fn(nID) => (IGraph.outDegree (IGraph.getNode (ig, nID)))) (IntSet.listItems notShared)
                            in
                                foldl (fn(d, b) => b andalso d < K) true degrees
                            end
                        fun removeMove (moveMap, n1ID, n2ID) =
                            let
                                val (newMap, moveSet) = IntMap.remove (moveMap, n1ID)
                                val moveSet = IntSet.subtract (moveSet, n2ID)
                            in
                                if (IntSet.numItems moveSet) > 0
                                then
                                    IntMap.insert (newMap, n1ID, moveSet)
                                else
                                    newMap
                            end
                        (* Unlike the others, premerge takes the unaliased ids since it needs to remove the entry from moveMap*)
                        fun preMerge(moveMap, alias, n1ID, n2ID) =
                            let
                                val (alias, realN1ID) = getAlias(alias, n1ID)
                                val (alias, realN2ID) = getAlias(alias, n2ID)
                                val moveMap = if IntMap.inDomain (moveMap, n1ID)
                                    then removeMove (moveMap, n1ID, n2ID)
                                    else moveMap
                                val moveMap = if IntMap.inDomain (moveMap, n2ID)
                                    then removeMove (moveMap, n2ID, n1ID)
                                    else moveMap
                            in
                                (moveMap, alias, realN1ID, realN2ID)
                            end
                        fun merge (ig, moveMap, alias, realN1ID, realN2ID) =
                            let
                                val deg1 = IGraph.outDegree (IGraph.getNode (ig, realN1ID))
                                val deg2 = IGraph.outDegree (IGraph.getNode (ig, realN2ID))
                                val (keepID, removeID) = if isPrecolored realN2ID orelse deg2>deg1 then (realN2ID, realN1ID)
                                              else (realN1ID, realN2ID)
                                val keep = IGraph.getNode (ig, keepID)
                                val knbs = IntSet.fromList (IGraph.adj keep)
                                val rnbs = IntSet.fromList (IGraph.adj (IGraph.getNode (ig, removeID)))
                                val newEdges = IntSet.listItems (IntSet.difference (rnbs, knbs))

                                val ig = IGraph.removeNode (ig, removeID)
                                val ig = foldl (fn (nb, ig) => IGraph.doubleEdge (ig, keepID, nb)) ig newEdges
                                (* We alias the real ids, not the original because those can be part of a long chain *)
                                (* and we need to make sure then entire alias chain points at realN1ID at the end. *)
                                val alias = IntMap.insert (alias, removeID, keepID)
                            in
                                (ig, moveMap, alias)
                            end

                        fun tryCoalesce (n1ID, nbSet, (alias, done, id1, id2)) =
                            let
                                val (alias, realN1ID) = getAlias(alias, n1ID)
                                val n1 = IGraph.getNode (ig, realN1ID)
                                fun helper (n2ID, (alias, done, id1, id2)) =
                                    let val (alias, realN2ID) = getAlias(alias, n2ID)
                                        val n2 = IGraph.getNode (ig, realN2ID)
                                        val constrained = IGraph.isAdjacent (n1, n2)
                                    in
                                        if done orelse constrained then (alias, done, id1, id2)
                                        else
                                            if briggs (n1, n2) orelse george (n1, n2) orelse george (n2, n1)
                                            then
                                                (alias, true, n1ID, n2ID)
                                            else
                                                (alias, done, id1, id2)
                                    end
                            in
                                IntSet.foldl helper (alias, done, id1, id2) nbSet
                            end
                        val (alias, found, n1ID, n2ID) = IntMap.foldli tryCoalesce (alias, false, 0, 0) moveMap
                        val (moveMap, alias, realN1ID, realN2ID) = preMerge (moveMap, alias, n1ID, n2ID)
                        val (ig, moveMap, alias) = if found andalso realN1ID <> realN2ID
                            then merge (ig, moveMap, alias, realN1ID, realN2ID)
                            else (ig, moveMap, alias)
                    in
                        if found then coalesce(ig, moveMap, alias, true) else (ig, moveMap, alias, changed)
                    end

                (* pick a non-precolored & high degree node *)
                fun pickSpill (ig, stack, spills) =
                    let
                        fun helper ([], (cand, cost)) = cand
                          | helper (node::nodes, (cand, cost)) =
                            let
                                val id = IGraph.getNodeID node
                                val cost' = IGraph.outDegree node
                            in
                                if not (Temp.Table.inDomain (initial, id)) andalso
                                   cost' > cost
                                then helper (nodes, (id, cost'))
                                else helper (nodes, (cand, cost))
                            end
                                
                        val spill = helper (IGraph.nodes ig, (~1, ~1))

                        val adjs = IGraph.adj (IGraph.getNode (ig, spill))
                        (* remove it from the graph & push it onto stack *)
                        val ig = IGraph.removeNode (ig, spill)
                    in
                        (ig, (spill, adjs)::stack, spill::spills)
                    end

                (* freeze one move *)
                fun freeze moveMap =
                    let
                        val (k, vset) = case IntMap.firsti moveMap of
                                          SOME(k, vset) => (k, vset)
                                        | NONE => raise MoveNotFound
                        val v = case IntSet.listItems vset of
                                  [] => raise MoveNotFound
                                | v::_ => v
                    in
                        (* update moveMap *)
                        if (IntSet.numItems vset) = 1
                        then #1 (IntMap.remove (moveMap, k))
                        else IntMap.insert (moveMap, k, IntSet.delete (vset, v))
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
                                val (ig, stack, spills) =
                                    pickSpill (ig, stack, spills)
                            in
                                main (ig, stack, moveMap, alias, spills)
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
