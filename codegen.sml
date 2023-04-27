signature CODEGEN =
sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
structure Frame : FRAME = MipsFrame
structure A = Assem
structure T = Tree

exception UnexpectedStm
exception UnexpectedExp

val calldefs = [Frame.RV, Frame.RA] @ Frame.callersaves @ Frame.argregs

fun int2string i =
    if i >= 0
    then Int.toString i
    else "-" ^ (Int.toString (~i))
                                                              
fun codegen (frame as {outSpace, ...} : Frame.frame) stm =
    let
        val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x :: !ilist

        (* there's no SEQ *)
        fun munchStm (T.LABEL lab) =
            emit (A.LABEL {assem = (Symbol.name lab) ^ ":\n",
                           lab = lab})
          | munchStm (T.JUMP (T.NAME lab, labs)) =
            emit (A.OPER {assem = "j 'j0\n",
                          dst = [],
                          src = [],
                          jump = SOME labs})
          | munchStm (T.JUMP (exp, labs)) =
            emit (A.OPER {assem = "jr 's0\n",
                          dst = [],
                          src = [munchExp exp],
                          jump = SOME labs})
          | munchStm (T.CJUMP (opr, e1, e2, t, f)) =
            let
                val oprStr = case opr of
                                 T.EQ => "beq"
                               | T.NE => "bne"
                               | T.LT => "blt"
                               | T.GT => "bgt"
                               | T.LE => "ble"
                               | T.GE => "bge"
                               | T.ULT => "bltu"
                               | T.ULE => "bleu"
                               | T.UGT => "bgtu"
                               | T.UGE => "bgeu"
            in
                emit (A.OPER {assem = oprStr ^ " 's0, 's1, 'j0\n",
                              dst = [],
                              src = [munchExp e1, munchExp e2],
                              jump = SOME [t, f]})
            end
          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e2, T.CONST i)), e1)) =
            emit (A.OPER {assem = "sw 's0, " ^ (int2string i) ^ "('s1)\n",
                          dst = [],
                          src = [munchExp e1, munchExp e2],
                          jump = NONE})
          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e2)), e1)) =
            munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e2, T.CONST i)), e1))
          | munchStm (T.MOVE (T.TEMP t, T.MEM (T.BINOP (T.PLUS, exp, T.CONST i)))) =
            emit (A.OPER {assem = "lw 'd0, " ^ (int2string i) ^ "('s0)\n",
                          dst = [t],
                          src = [munchExp exp],
                          jump = NONE})
          | munchStm (T.MOVE (T.TEMP t, T.MEM (T.BINOP (T.PLUS, T.CONST i, exp)))) =
            munchStm (T.MOVE (T.TEMP t, T.MEM (T.BINOP (T.PLUS, exp, T.CONST i))))
          | munchStm (T.MOVE (T.TEMP t, T.CALL (e, args))) =
            (munchCall (e, args);
             munchStm (T.MOVE (T.TEMP t, T.TEMP (Frame.RV))))
          | munchStm (T.MOVE (T.MEM (T.CONST i), exp)) =
            emit (A.OPER {assem = "sw 's0, " ^ (int2string i) ^ "(r0)\n",
                          dst = [],
                          src = [munchExp exp],
                          jump = NONE})
          | munchStm (T.MOVE (T.MEM e2, T.CALL (e1, args))) =
            (munchCall (e1, args);
             munchStm (T.MOVE (T.MEM e2, T.TEMP (Frame.RV))))
          | munchStm (T.MOVE (T.MEM e2, e1)) =
            emit (A.OPER {assem = "sw 's0, 0('s1)\n",
                          dst = [],
                          src = [munchExp e1, munchExp e2],
                          jump = NONE})
          | munchStm (T.MOVE (T.TEMP t1, T.TEMP t2)) =
            emit (A.MOVE {assem = "move 'd0, 's0\n",
                          dst = t1,
                          src = t2})
          | munchStm (T.MOVE (T.TEMP t, exp)) =
            emit (A.MOVE {assem = "move 'd0, 's0\n",
                           dst = t,
                           src = munchExp exp})
          | munchStm (T.MOVE (e1, e2)) =
            emit (A.MOVE {assem = "move 'd0, 's0\n",
                          dst = munchExp e1,
                          src = munchExp e2})
          | munchStm (T.EXP (T.CALL (e, args))) =
            munchCall (e, args)
          | munchStm (T.EXP exp) = (munchExp exp; ())
          | munchStm stm = (print "\nUnmatched statement!!!:\n";
                            Printtree.printtree (TextIO.stdOut, stm);
                            raise UnexpectedStm)
        and munchCall (f, args) =
            let
                fun max (a, b) = if a > b then a else b
                val space = (List.length args) - 4 
            in
                (* update outgoing parameters space *)
                if space > (!outSpace)
                then outSpace := space
                else ();
                case f of
                    T.NAME lab =>
                    emit (A.OPER {assem = "jal " ^ (Symbol.name lab) ^ "\n",
                                  dst = calldefs,
                                  src = munchArgs (0, args),
                                  jump = NONE})
                  | _ => ()
            end
        (* Caller Prologue:
           move all the arguments to their correct positions *)
        and munchArgs (i, []) = []
          | munchArgs (i, arg::args) = 
            let
                val t = munchExp arg
                val loc = if i < 4
                          then T.TEMP (List.nth (Frame.argregs, i))
                          else T.MEM (T.BINOP (
                                           T.PLUS,
                                           T.TEMP (Frame.SP),
                                           T.BINOP (T.MUL, T.CONST (Frame.wordSize), T.CONST (i - 4))))
            in
                munchStm (T.MOVE (loc, T.TEMP t));
                if i < 4
                then
                    (List.nth (Frame.argregs, i))::(munchArgs (i + 1, args))
                else
                    munchArgs (i + 1, args)
            end
        (* We don't need to deal with CALL, NAME, ESEQ *)
        and munchExp (T.CONST i) =
            result (fn t => emit (A.OPER {assem = "li 'd0, " ^ (int2string i) ^ "\n",
                                          dst = [t],
                                          src = [],
                                          jump = NONE}))
          | munchExp (T.TEMP t) = t
          | munchExp (T.BINOP (T.PLUS, exp, T.CONST i)) =
            munchBinopImm ("addi", exp, i)
          | munchExp (T.BINOP (T.MINUS, exp, T.CONST i)) =
            munchBinopImm ("addi", exp, ~i)
          | munchExp (T.BINOP (T.AND, exp, T.CONST i)) =
            munchBinopImm ("andi", exp, i)
          | munchExp (T.BINOP (T.OR, exp, T.CONST i)) =
            munchBinopImm ("ori", exp, i)
          | munchExp (T.BINOP (T.XOR, exp, T.CONST i)) =
            munchBinopImm ("xori", exp, i)
          | munchExp (T.BINOP (T.PLUS, T.CONST i, exp)) =
            munchExp (T.BINOP (T.PLUS, exp, T.CONST i))
          | munchExp (T.BINOP (T.AND, T.CONST i, exp)) =
            munchExp (T.BINOP (T.AND, exp, T.CONST i))
          | munchExp (T.BINOP (T.OR, T.CONST i, exp)) =
            munchExp (T.BINOP (T.OR, exp, T.CONST i))
          | munchExp (T.BINOP (T.XOR, T.CONST i, exp)) =
            munchExp (T.BINOP (T.XOR, exp, T.CONST i))
          | munchExp (T.BINOP (opr, e1, e2)) =
            let
                val oprName = case opr of
                                  T.PLUS => "add"
                                | T.MINUS => "sub"
                                | T.MUL => "mul"
                                | T.DIV => "div"
                                | T.AND => "and"
                                | T.OR => "or"
                                | T.LSHIFT => "sll"
                                | T.RSHIFT => "srl"
                                | T.ARSHIFT => "sra"
                                | T.XOR => "xor"
            in
                result (fn t => emit (A.OPER {assem = oprName ^ " 'd0, 's0, 's1\n",
                                              dst = [t],
                                              src = [munchExp e1, munchExp e2],
                                              jump = NONE}))
            end
          | munchExp (T.MEM (T.BINOP (T.PLUS, exp, T.CONST i))) =
            result (fn t => emit (A.OPER {assem = "lw 'd0, " ^ (int2string i) ^ "('s0)\n",
                                          dst = [t],
                                          src = [munchExp exp],
                                          jump = NONE}))
          | munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST i, exp))) =
            munchExp (T.MEM (T.BINOP (T.PLUS, exp, T.CONST i)))
          | munchExp (T.MEM (T.CONST i)) =
            result (fn t => emit (A.OPER {assem = "lw 'd0, 0(" ^ (int2string i) ^ ")\n",
                                          dst = [t],
                                          src = [],
                                          jump = NONE}))
          | munchExp (T.MEM exp) =
            result (fn t => emit (A.OPER {assem = "lw 'd0, 0('s0)\n",
                                          dst = [t],
                                          src = [munchExp exp],
                                          jump = NONE}))
          | munchExp (T.NAME lab) =
            result (fn t => emit (A.OPER {assem = "la 'd0, " ^ (Symbol.name lab) ^ "\n",
                                          dst = [t],
                                          src = [],
                                          jump = NONE}))
          | munchExp exp = (print "\nUnmatched expression!!!:\n";
                            Printtree.printtree (TextIO.stdOut, T.EXP exp);
                            raise UnexpectedExp)
        and munchBinopImm (oprName, T.TEMP r, i) =
            result (fn t => emit (A.OPER {assem = oprName ^ " 'd0, 's0, " ^ (int2string i) ^ "\n",
                                          dst = [t],
                                          src = [r],
                                          jump = NONE}))
          | munchBinopImm (oprName, exp, i) =
            result (fn t => emit (A.OPER {assem = oprName ^ " 'd0, 's0, " ^ (int2string i) ^ "\n",
                                          dst = [t],
                                          src = [munchExp exp],
                                          jump = NONE}))
        and result gen = let val t = Temp.newtemp() in gen t; t end
    in
        munchStm stm;
        rev (!ilist)
    end
end
