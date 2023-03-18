signature FRAME =
sig
    type frame
    type access
    datatype frag = PROC of {body : Tree.stm, frame : frame}
                  | STRING of Temp.label * string
    val FP : Temp.temp
    val wordSize : int
    val exp : access -> Tree.exp -> Tree.exp
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val externalCall: string * Tree.exp list -> Tree.exp
    (* debugging only *)
    val printAccInfo : frame -> unit list
end

structure MipsFrame : FRAME =
struct
structure T = Tree

(* in Byte *)
val wordSize = 4
val FP = Temp.newtemp ()
                   
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
        (* TODO: add view shift stuff:
           (1) ??? *)
        fun helper true =
            (formalNum := !formalNum + 1;
             InFrame ((!formalNum - 1) * wordSize))
          | helper false = InReg (Temp.newtemp ())
    in
        {name = name, formals = map helper formals, localNum = ref 0}
    end
    
fun name ({name, ...} : frame) = name
    
fun formals ({formals, ...} : frame) = formals
    
fun allocLocal ({localNum, ...} : frame) true =
    (localNum := !localNum + 1;
     InFrame (~ (!localNum * wordSize)))
  | allocLocal _ false = InReg (Temp.newtemp ())

fun printAccInfo frame = 
    let
        val accs = formals frame
        fun helper acc =
            case acc of
                InFrame i => print ("InFrame(" ^ (Int.toString i) ^ ") ")
              | InReg t => print ("InReg(" ^ (Int.toString t) ^ ") ")
    in
        map helper accs
    end

fun exp (InFrame offset) fp =
    T.MEM (T.BINOP (T.PLUS, fp, T.CONST offset))
  | exp (InReg t) _ = T.TEMP t

fun externalCall (s, args) = T.CALL (T.NAME (Temp.namedlabel s), args)
end

structure Frame : FRAME = MipsFrame
