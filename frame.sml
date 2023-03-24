signature FRAME =
sig
    type frame
    type access
    datatype frag = PROC of {body : Tree.stm, frame : frame}
                  | STRING of Temp.label * string
    val FP : Temp.temp
    val RV : Temp.temp
    val wordSize : int
    val exp : access -> Tree.exp -> Tree.exp
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val externalCall: string * Tree.exp list -> Tree.exp
    (* debugging only *)
    val printFormalInfo : access list -> unit
    val printAccInfo : access -> unit
    val printFrameInfo : frame -> unit
    val printRegInfo : unit -> unit
end

structure MipsFrame : FRAME =
struct
structure T = Tree

(* in Byte *)
val wordSize = 4
(* 
  FP: frame pointer reg
  RV: return value reg
 *)
val FP = Temp.newtemp ()
val RV = Temp.newtemp ()
                   
datatype access = InFrame of int
                | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list, localNum: int ref}
datatype frag = PROC of {body : Tree.stm, frame : frame}
              | STRING of Temp.label * string
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
        val formalNum = ref 0

        fun helper true =
            (formalNum := !formalNum + 1;
             InFrame ((!formalNum - 1) * wordSize))
          | helper false = InReg (Temp.newtemp ())
    in
        {name = name, formals = map helper formals, localNum = ref 0}
    end

(* TODO: implement in future stage, part of view shift *)
(* NOTE: the stm could be T.EXP converted from unNx
   So treat procedure and non-procedure functions differently
 *)
fun procEntryExit1 (frame, stm) = stm
        
fun name ({name, ...} : frame) = name
    
fun formals ({formals, ...} : frame) = formals
    
fun allocLocal ({localNum, ...} : frame) true =
    (localNum := !localNum + 1;
     InFrame (~ (!localNum * wordSize)))
  | allocLocal _ false = InReg (Temp.newtemp ())

fun printAccInfo (InFrame i) = print ("InFrame(" ^ (Int.toString i) ^ ")")
  | printAccInfo (InReg t) = print ("InReg(" ^ (Int.toString t) ^ ")")
                               
fun printFormalInfo [] = ()
  | printFormalInfo [acc] =
    printAccInfo acc
  | printFormalInfo (acc::accs) =
    (printAccInfo acc;
     print ", ";
     printFormalInfo accs)

fun printFrameInfo {name, formals, localNum} =
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
end

structure Frame : FRAME = MipsFrame
