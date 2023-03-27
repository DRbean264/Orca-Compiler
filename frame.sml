signature FRAME =
sig
    type frame
    type access
    datatype frag = PROC of {body : Tree.stm, frame : frame}
                  | STRING of Temp.label * string
    val FP : Temp.temp
    val RV : Temp.temp
    val RA : Temp.temp
    val ZERO : Temp.temp
    val SP : Temp.temp
    val A0 : Temp.temp
    val A1 : Temp.temp
    val A2 : Temp.temp
    val A3 : Temp.temp
    val T0 : Temp.temp
    val T1 : Temp.temp
    val T2 : Temp.temp
    val T3 : Temp.temp
    val T4 : Temp.temp
    val T5 : Temp.temp
    val T6 : Temp.temp
    val T7 : Temp.temp
    val T8 : Temp.temp
    val T9 : Temp.temp

    val wordSize : int
    val exp : access -> Tree.exp -> Tree.exp
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val name : frame -> string
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
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
structure T = Tree

(* in Byte *)
val wordSize = 4
(* 
  FP: frame pointer reg
  RV: return value reg
  RA: return address reg
  
  *)
val ZERO = Temp.newtemp ()
val FP = Temp.newtemp ()
val SP = Temp.newtemp ()
val RV = Temp.newtemp ()
val A0 = Temp.newtemp ()
val A1 = Temp.newtemp ()
val A2 = Temp.newtemp ()
val A3 = Temp.newtemp ()
val T0 = Temp.newtemp ()
val T1 = Temp.newtemp ()
val T2 = Temp.newtemp ()
val T3 = Temp.newtemp ()
val T4 = Temp.newtemp ()
val T5 = Temp.newtemp ()
val T6 = Temp.newtemp ()
val T7 = Temp.newtemp ()
val T8 = Temp.newtemp ()
val T9 = Temp.newtemp ()
val RA = Temp.newtemp ()
                      
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
        
fun name ({name, ...} : frame) = Symbol.name name
    
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

fun string (lab, s) = (Symbol.name lab) ^ ": " ^ s ^ "\n"

end
    
structure Frame : FRAME = MipsFrame
