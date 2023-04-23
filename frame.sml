signature FRAME =
sig
    type frame
    type access
    type register

    datatype frag = PROC of {body : Tree.stm, frame : frame}
                  | STRING of Temp.label * string
    val FP : Temp.temp
    val RV : Temp.temp
    val RA : Temp.temp
    val ZERO : Temp.temp
    val SP : Temp.temp
    val registers: register list
    val tempMap: register Temp.Table.table
    val tempReset : Temp.temp
    val saytemp : Temp.temp -> string

    val specialregs : Temp.temp list
    val argregs : Temp.temp list
    val calleesaves : Temp.temp list
    val callersaves : Temp.temp list
    val wordSize : int
    val exp : access -> Tree.exp -> Tree.exp
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list ->
                         {prolog: string, body : Assem.instr list, epilog: string}
    val name : frame -> string
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val getOffset : access -> int
    val externalCall : string * Tree.exp list -> Tree.exp
    val string : Temp.label * string -> string

    (* debugging only *)
    val printFormalInfo : access list -> unit
    val printAccInfo : access -> unit
    val printFrameInfo : frame -> unit
    val printRegInfo : unit -> unit
end

structure MipsFrame : FRAME =
struct
structure A = Assem
structure T = Tree

exception NOOFFSET
exception DEBUGGING
                  
type register = string
(* in Byte *)
val wordSize = 4
(* 
  ZERO: register 0 which is always 0
  FP: frame pointer reg
  SP: stack pointer reg
  (there should be two, but we don't need another one)
  RV: return value reg 
  RA: return address reg
  *)
val ZERO = Temp.newtemp ()
val FP = Temp.newtemp ()
val SP = Temp.newtemp ()
val RV = Temp.newtemp ()
val RA = Temp.newtemp ()

fun tempList 0 = []
  | tempList n = Temp.newtemp()::tempList(n-1)

val specialregs = [RV, FP, SP, RA, ZERO]
val argregs = tempList 4
val calleesaves = tempList 8
val callersaves = tempList 10
val tempReset = Temp.newtemp ()
                             
fun insertLists (m, t::tlist, s::slist) = (insertLists ((Temp.Table.enter (m, t, s), tlist, slist)))
  | insertLists (m, [], s) = m
  | insertLists (m, t, []) = m

fun makeregs (s, n) =
  let fun helper (s, 0) = [s ^ (Int.toString 0)]
        | helper (s, n) = (s ^ (Int.toString n)) :: helper (s, n - 1)
  in
    List.rev (helper (s, n))
  end

val tempMap = insertLists (Temp.Table.empty, specialregs, ["$v0", "$fp", "$sp", "$ra", "$zero"])
val tempMap = insertLists (tempMap, argregs, makeregs ("$a", 3))
val tempMap = insertLists (tempMap, calleesaves, makeregs ("$s", 7))
val tempMap = insertLists (tempMap, callersaves, makeregs ("$t", 9))

val registers = Temp.Table.listItems tempMap
                          
datatype access = InFrame of int
                | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list,
              localNum: int ref, escapeNum: int, outSpace: int ref}
datatype frag = PROC of {body : Tree.stm, frame : frame}
              | STRING of Temp.label * string

fun saytemp t =
    case Temp.Table.look (tempMap, t) of
        SOME s => s
      | NONE => Temp.makestring t
                                           
(*
My current understanding of frame layout is like
|       a2       |
|       a1       |
|  static link   |
------------------   fp
|   local var 1  |
|   local var 2  |
|   local var 3  |
*)                                       
fun newFrame ({name, formals}) =
    let
        val escapeNum = ref 0

        fun helper true =
            (escapeNum := !escapeNum + 1;
             InFrame ((!escapeNum - 1) * wordSize))
          | helper false = InReg (Temp.newtemp ())
    in
        (* NOTE: the reason localnum is 1 is because 
           we always reserve a space for old FP *)
        {name = name, formals = map helper formals,
         localNum = ref 1, escapeNum = (!escapeNum),
         outSpace = ref 0}
    end

fun name ({name, ...} : frame) = Symbol.name name
    
fun formals ({formals, ...} : frame) = formals
        
(* For each incoming register parameter, 
   move it to the place from which it is seen from within the function *)
fun procEntryExit1 (frame, stm) =
    let
        fun move ([], _, stm) = stm
          | move (formal::formals, i, stm) =
            let
                val dst =
                    case formal of
                        InFrame offset =>
                        T.MEM (T.BINOP (T.PLUS,
                                        T.TEMP FP,
                                        T.BINOP (T.MUL, T.CONST wordSize,
                                                 T.CONST offset)))
                      | InReg t => 
                        T.TEMP t
                val src =
                    if i < 4
                    then T.TEMP (List.nth (argregs, i))
                    else T.MEM (T.BINOP (T.PLUS,
                                         T.TEMP FP,
                                         T.BINOP (T.MUL, T.CONST wordSize,
                                                  T.CONST ((#escapeNum frame) + i - 4))))
            in
                T.SEQ (T.MOVE (dst, src),
                       move (formals, i + 1, stm))
            end
    in
        move (formals frame, 0, stm)
    end

(* sink instruction *)
fun procEntryExit2 (frame as {localNum, outSpace, ...} : frame, body) =
    let
        fun genSpill ([], (l1, l2)) = (l1, l2)
          | genSpill (t::ts, (l1, l2)) =
            let
                val newT = Temp.newtemp ()
                val (l1', l2') = genSpill (ts, (l1, l2))
            in
                ((A.MOVE {assem = "move 'd0, 's0\n",
                          dst = newT,
                          src = t})::l1',
                 (A.MOVE {assem = "move 'd0, 's0\n",
                          dst = t,
                          src = newT})::l2')
            end

        val (l1, l2) = genSpill (RA::calleesaves, ([], []))
    in
        l1 @ body @ l2 @
        (* append sink instruction *)
        [A.OPER {assem = "",
                 src = specialregs @ calleesaves,
                 dst = [], jump = SOME []}]
    end

fun procEntryExit3 (frame as {name, localNum, escapeNum, outSpace, ...} : frame,
                    body) =
    let
        val offset1 = wordSize * (!localNum + escapeNum + !outSpace)
        val offset2 = wordSize * (!localNum + !outSpace)
        val prolog = (Symbol.name name) ^ ":\n" ^
                     "subu $sp, $sp, " ^ (Int.toString offset1) ^ "\n" ^
                     "move $t0, $fp\n" ^
                     "addi $fp, $sp, " ^ (Int.toString offset2) ^ "\n" ^
                     "sw $t0, -4($fp)\n"
        val epilog = "lw $fp, -4($fp)\n" ^
                     "addi $sp, $sp, " ^ (Int.toString offset1) ^ "\n" ^
                     "jr $ra\n"
    in        
        {prolog = prolog,
         body = body,
         epilog = epilog}
    end
        
fun allocLocal ({localNum, ...} : frame) true =
    (localNum := !localNum + 1;
     InFrame (~ (!localNum * wordSize)))
  | allocLocal _ false = InReg (Temp.newtemp ())

fun getOffset (InFrame i) = i
  | getOffset (InReg t) = raise NOOFFSET
                               
fun printAccInfo (InFrame i) = print ("InFrame(" ^ (Int.toString i) ^ ")")
  | printAccInfo (InReg t) = print ("InReg(" ^ (Int.toString t) ^ ")")
                               
fun printFormalInfo [] = ()
  | printFormalInfo [acc] =
    printAccInfo acc
  | printFormalInfo (acc::accs) =
    (printAccInfo acc;
     print ", ";
     printFormalInfo accs)

fun printFrameInfo {name, formals, localNum, escapeNum, outSpace} =
    (print ("Name: " ^ (Symbol.name name) ^ "\n" ^
            "Formals: [");
     printFormalInfo formals;
     print ("]\nNumber of variables on stack: " ^
            (Int.toString (!localNum)) ^ "\n"))

fun printRegInfo () =
    print ("$fp: InReg(" ^ (Int.toString FP) ^ ")\n")

        
fun exp (InFrame offset) fp =
    T.MEM (T.BINOP (T.PLUS, fp, T.CONST offset))
  | exp (InReg t) _ = T.TEMP t

fun externalCall (s, args) = T.CALL (T.NAME (Temp.namedlabel s), args)

fun string (lab, s) =
    (Symbol.name lab) ^ ":\n" ^
    ".word 0x" ^ (Int.fmt StringCvt.HEX (String.size s)) ^ "\n" ^
    ".asciiz \"" ^ (String.toString s) ^ "\"\n"
                              
end
    
structure Frame : FRAME = MipsFrame
