structure Color : COLOR =
struct

structure F = Frame

exception ErrorPickingSpill
exception UnknownAllocation
exception MoveNotFound
exception DEBUGGING

type allocation = F.register Temp.Table.table

fun color {interference = Liveness.IGRAPH {graph = ig, tnode, gtemp, moves}, initial, spillCost, registers} =
    let
        val iteration = ref 1
        val K = List.length registers
        val regSet = StringSet.fromList registers

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

        fun assignColor (stack, alias) = 
            let
                (* the spills here is a set *)
                fun assignStack ([], allocation, spills) = (allocation, spills)
                  | assignStack (nID::stack, allocation, spills) =
                    let
                        (* get all available colors *)
                        val colors =
                            foldl (fn (id, ss) =>
                                      case Temp.Table.look (allocation, id) of
                                          SOME reg => StringSet.subtract (ss, reg)
                                        | NONE => ss)
                                  regSet
                                  (IGraph.adj (IGraph.getNode (ig, nID)))
                    in
                        if StringSet.isEmpty colors
                        then (* actual spill *)
                            (print ("During coloring: Actual spill -> Node " ^ (Int.toString nID) ^ "\n");
                             assignStack (stack, allocation, nID::spills))
                        else
                            let
                                (* pick one color *)
                                val (color::_) = StringSet.listItems colors
                            in
                                print ("During coloring: Node " ^ (Int.toString nID) ^ " -> " ^ color ^ "\n");
                                assignStack (stack, Temp.Table.enter (allocation,
                                                                      nID,
                                                                      color),
                                             spills)
                            end
                    end

                fun assignAlias allocation =
                    foldl (fn (id, allocation) =>
                              let
                                  val (_, id') = getAlias (alias, id)
                              in
                                  case Temp.Table.look (allocation, id') of
                                      SOME reg =>
                                      (print ("During coloring: Node " ^ (Int.toString id) ^ " -> " ^ reg ^ "\n");
                                       Temp.Table.enter (allocation, id, reg))
                                    | NONE => raise UnknownAllocation
                              end)
                          allocation (IntMap.listKeys alias)
                        
                (* assign colors for nodes in the stack *)
                val (allocation, spills) = assignStack (stack, initial, [])
                (* assign colors for nodes in alias *)
                val allocation = assignAlias allocation
            in
                (allocation, spills)
            end
            
        (* ig: interference graph, IGraph
           stack: select stack, list
           moveMap: move edges, intmap of intset 
           alias: alias of coalesced nodes, intmap *)
        fun main (ig, stack, moveMap, alias) =
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

                val _ = print ("\nIteration " ^ (Int.toString (!iteration)) ^ ":\n")
                val _ = print "Move related nodes :"
                val _ = IntSet.app (fn i => print ("Node " ^ (Int.toString i) ^ " ")) moveRelated
                val _ = print "\n"
                val _ = iteration := !iteration + 1
                              
                (* val _ = if !iteration = 3 *)
                (*         then raise DEBUGGING *)
                (*         else () *)
                                             
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
                                (print ("In simplify: pick node " ^ (Int.toString nID) ^ "\n");
                                 (* remove it from the graph &
                                    push it onto stack *)
                                 simplify (IGraph.removeNode (ig, nID), nID::stack))
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
                                val degrees = map (fn(nID) => (IGraph.degree (IGraph.getNode (ig, nID)))) (IntSet.toList notShared)
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

                        (* Unlike the others, merge takes the unaliased ids since it needs to remove the entry from moveMap*)
                        fun merge (ig, moveMap, alias, n1ID, n2ID) =
                            let 
                                val (alias, realN1ID) = getAlias(alias, n1ID)
                                val (alias, realN2ID) = getAlias(alias, n2ID)
                                val moveMap = removeMove (moveMap, n1ID, n2ID) 
                                val n1 = IGraph.getNode (ig, realN1ID)
                                val n1nbs = IntSet.fromList (IGraph.adj n1)
                                val n2nbs = IntSet.fromList (IGraph.adj (IGraph.getNode (ig, realN2ID)))
                                val newEdges = IntSet.toList (IntSet.difference (n2nbs, n1nbs))

                                val ig = foldl (fn (nb, ig) => IGraph.doubleEdge (ig, realN1ID, nb)) ig newEdges
                                (*We alias the real ids, not the original because those can be part of a long chain
                                  and we need to make sure then entire alias chain points at realN1ID at the end.*)
                                val alias = IntMap.insert (alias, realN2ID, realN1ID)
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
                                    in
                                        if done then (alias, done, id1, id2)
                                        else
                                            if briggs (n1, n2) orelse george(n1, n2) orelse george(n2, n1)
                                            then
                                                (alias, true, n1ID, n2ID)
                                            else
                                                (alias, done, id1, id2)
                                    end
                            in
                                IntSet.foldl helper (alias, done, id1, id2) nbSet
                            end
                        val (alias, found, n1ID, n2ID) = IntMap.foldli tryCoalesce (alias, false, 0, 0) moveMap
                        val (ig, moveMap, alias) = if found then merge (ig, moveMap, alias, n1ID, n2ID) else (ig, moveMap, alias)
                    in
                        if found then coalesce(ig, moveMap, alias, true) else (ig, moveMap, alias, changed)
                    end

                (* pick a non-precolored & high degree node *)
                fun pickSpill (ig, stack) =
                    let
                        fun helper ([], (cand, cost)) = cand
                          | helper (node::nodes, (cand, cost)) =
                            let
                                val id = IGraph.getNodeID node
                                val cost' = spillCost node
                            in
                                if not (Temp.Table.inDomain (initial, id)) andalso
                                   cost' > cost
                                then helper (nodes, (id, cost'))
                                else helper (nodes, (cand, cost))
                            end
                                
                        val spill = helper (IGraph.nodes ig, (~1, ~1))
                        (* remove it from the graph & push it onto stack *)
                        val ig = IGraph.removeNode (ig, spill)
                    in
                        print ("In spilling: pick node " ^ (Int.toString spill) ^ "\n");
                        (ig, spill::stack)
                    end

                (* freeze one move *)
                fun freeze moveMap =
                    let
                        val ((k, vset)::_) = IntMap.listItemsi moveMap
                        val (v::vs) = IntSet.listItems vset
                    in
                        print ("In freeze: pick move edge Node " ^ (Int.toString k) ^
                               " <-> Node " ^ (Int.toString v) ^ "\n");
                        (* update moveMap *)
                        if (List.length vs) = 0
                        then #1 (IntMap.remove (moveMap, k))
                        else IntMap.insert (moveMap, k, IntSet.fromList vs)
                    end

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
                        then main (ig, stack, moveMap, alias)
                        (* check if there's move we can freeze *)
                        else if IntMap.isEmpty moveMap
                        then
                            let
                                val (ig, stack) = pickSpill (ig, stack)
                            in
                                main (ig, stack, moveMap, alias)
                            end
                         (* freeze one move, and simplify again *)
                        else main (ig, stack, freeze moveMap, alias)
                    end
            end

        val moveMap = transMove moves
    in
        main (ig, [], moveMap, IntMap.empty)
    end
                             
end
