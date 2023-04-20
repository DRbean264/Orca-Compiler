structure Reg_Alloc : REG_ALLOC =
struct

    structure F = Frame
    structure A = Assem

    exception UnknownTemp
    exception SpillDetected
    exception DEBUGGING

    type allocation = F.register Temp.Table.table

    fun int2string i =
        if i >= 0
        then Int.toString i
        else "-" ^ (Int.toString (~i))
                                 
    fun alloc (instrs, frame) =
            let
                fun genFetch ([], spills) = ([], [])
                  | genFetch (src::srcs, spills) =
                    let
                        val (srcs', instrs) = genFetch (srcs, spills)
                    in
                        if IntMap.inDomain (spills, src)
                        then
                            let
                                val newT = Temp.newtemp ()
                                val offset = IntMap.lookup (spills, src)
                            in
                                (newT::srcs',
                                 (A.OPER {assem = "lw 'd0, " ^ (int2string offset) ^ "('s0)\n",
                                          dst = [newT],
                                          src = [F.FP],
                                          jump = NONE})::instrs)
                            end
                        else (src::srcs', instrs)
                    end

                fun genStore ([], spills) = ([], [])
                  | genStore (dst::dsts, spills) =
                    let
                        val (dsts', instrs) = genStore (dsts, spills)
                    in
                        if IntMap.inDomain (spills, dst)
                        then
                            let
                                val newT = Temp.newtemp ()
                                val offset = IntMap.lookup (spills, dst)
                            in
                                (newT::dsts',
                                 (A.OPER {assem = "sw 's0, " ^ (int2string offset) ^ "('s1)\n",
                                          dst = [],
                                          src = [newT, F.FP],
                                          jump = NONE})::instrs)
                            end
                        else (dst::dsts', instrs)
                    end

                (* spills is an int map, value is offset from frame *)
                fun rewriteProgram ([], spills) = []
                  | rewriteProgram ((instr as A.LABEL _)::instrs, spills) =
                    instr::(rewriteProgram (instrs, spills))
                  | rewriteProgram ((instr as A.OPER {assem, dst, src, jump})::instrs, spills) =
                    let
                        val (src', fetches) = genFetch (src, spills)
                        val (dst', stores) = genStore (dst, spills)
                    in
                        fetches @
                        [A.OPER {assem = assem,
                                 dst = dst',
                                 src = src',
                                 jump = jump}] @
                        stores @
                        (rewriteProgram (instrs, spills))
                    end
                  | rewriteProgram ((instr as A.MOVE {assem, dst, src})::instrs, spills) =
                    let
                        val (src'::_, fetches) = genFetch ([src], spills)
                        val (dst'::_, stores) = genStore ([dst], spills)
                    in
                        fetches @
                        [A.MOVE {assem = assem,
                                 dst = dst',
                                 src = src'}] @
                        stores @
                        (rewriteProgram (instrs, spills))
                    end
                        
                (* control flow graph *)
                val (fg, nodes) = (MakeGraph.reset (); MakeGraph.instrs2graph instrs)
                (* build *)
                val (igraph, node2Liveout) =
                    Liveness.interferenceGraph (fg, nodes)

                (* debugging *)
                (* val _ = print "\n"
                val _ = Liveness.show' (TextIO.stdOut, igraph) *)
                (* debugging *)
                                       
                (* coloring *)
                val (allocation, spills) = Color.color ({interference = igraph,
                            initial = F.tempMap,
                            spillCost = IGraph.outDegree,
                            registers = F.registers})
            in
                (* (instrs, allocation) *)
                if (List.length spills) = 0
                then (instrs, allocation)
                (* if there're spills, rewrite the program, 
                   then call alloc again *) 
                else
                    let
                        (* allocate space for spillings *)
                        val spill2off = foldl (fn (spill, m) =>
                                                  IntMap.insert (m, spill, F.getOffset (F.allocLocal frame true))) IntMap.empty spills
                        val instrs = rewriteProgram (instrs, spill2off)
                    in
                        alloc (instrs, frame)
                    end
            end

end
