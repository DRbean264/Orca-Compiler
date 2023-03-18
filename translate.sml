signature TRANSLATE =
sig
    type level
    type access
    type exp
    val dummyExp : exp
    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access

    (* interface for IR translation *)
    val simpleVar : access * level -> exp
    val fieldVar : exp * int -> exp
    val subscriptVar : exp * exp -> exp

    val intExp : int -> exp
    val nilExp : unit -> exp
    val stringExp : string -> exp
    val arrayExp : exp * exp -> exp
    val recordExp : int * exp list -> exp

    val opExp : Absyn.oper * exp * exp -> exp
    val strCompExp : Absyn.oper * exp * exp -> exp
    val ifExp : exp * exp * exp -> exp
    val whileExp : exp * exp * Temp.label -> exp
    val forExp : access * exp * exp * exp * Temp.label -> exp
    val breakExp : Temp.label -> exp
    val callExp : Temp.label * (exp list) * level * level -> exp
    val seqExp : exp list -> exp
    val assignExp : exp * exp -> exp
    val varDecExp : access * exp -> exp
    val letExp : exp list * exp -> exp
                                     
    val reset : unit -> unit
    (* TODO: remove debugging stuff *)
    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val printAccInfo : level -> unit list
    val getLogicalLevel : level -> int
end

structure Translate : TRANSLATE =
struct
exception LevelIdNotFound
exception SeqEmpty
exception StmAsCondError
exception RelopOnly
exception EmptySequence
              
structure T = Tree
structure F = Frame
structure A = Absyn
                  
(* the level here is a level id, the depth is stored in the lfmap *)
type level = int
type access = level * Frame.access
datatype exp = Ex of T.exp
             | Nx of T.stm
             | Cx of Temp.label * Temp.label -> T.stm
                          
structure LevelFrameMap = IntMapTable (type key = level
		                       fun getInt l = l)

val dummyExp = Nx (T.LABEL (Temp.namedlabel "dummyLabel"))
val fragments : F.frag list ref = ref []
                                      
(* NOTE: the outermost level has static link, but should never be used *)
val outermost = 0
val lfmap : (int * Frame.frame) LevelFrameMap.table ref =
    ref (LevelFrameMap.enter (LevelFrameMap.empty, outermost, (0, Frame.newFrame ({name = Temp.namedlabel "tig_main", formals = [true]}))))
                    
val nextId = ref (outermost + 1)
fun getNextId () =
    let
        val res = !nextId
    in
        nextId := res + 1;
        res
    end

fun newLevel {parent, name, formals} =
    let
        (* add static link as the first parameter *)
        val frame = Frame.newFrame ({name = name,
                                     formals = true::formals})
    in
        case LevelFrameMap.look (!lfmap, parent) of
            SOME (lev, _) =>
            let
                val id = getNextId ()
            in
                (* store this frame and its logical level in the map*)
                lfmap := LevelFrameMap.enter (!lfmap, id, (lev + 1, frame));
                id
            end
          | NONE => raise LevelIdNotFound
    end

fun formals level =
    case LevelFrameMap.look (!lfmap, level) of
        SOME (_, frame) =>
        let
            fun helper acc = (level, acc)
        in
            map helper (Frame.formals frame)
        end
      | NONE => raise LevelIdNotFound

fun allocLocal level escape =
    let
        val (_, frame) = case LevelFrameMap.look (!lfmap, level) of
                               SOME v => v
                             | NONE => raise LevelIdNotFound
        val acc = Frame.allocLocal frame escape
    in
        (level, acc)
    end

fun reset () =
    (lfmap :=
     LevelFrameMap.enter (LevelFrameMap.empty, outermost, (0, Frame.newFrame ({name = Temp.namedlabel "tig_main", formals = []})));
     nextId := outermost + 1)
                    
fun printAccInfo level =
    let
        val (_, frame) = case LevelFrameMap.look (!lfmap, level) of
                             SOME v => v
                           | NONE => raise LevelIdNotFound
    in
        Frame.printAccInfo frame
    end

fun getLogicalLevel level =
    let
        val (lev, _) = case LevelFrameMap.look (!lfmap, level) of
                           SOME v => v
                         | NONE => raise LevelIdNotFound
    in
        lev
    end

fun toSeq [] = raise SeqEmpty
  | toSeq [s] = s
  | toSeq (s::seq) =
    T.SEQ(s, toSeq seq)
        
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp ()
        val t = Temp.newlabel () and f = Temp.newlabel ()
    in T.ESEQ (toSeq [T.MOVE (T.TEMP r, T.CONST 1),
                      genstm (t, f),
                      T.LABEL f,
                      T.MOVE (T.TEMP r, T.CONST 0),
                      T.LABEL t],
               T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ (s, T.CONST 0)

fun unNx (Ex e) = T.EXP e
  | unNx (Nx s) = s
  | unNx (Cx genstm) =
    let
        val l = Temp.newlabel ()
    in
        toSeq [genstm (l, l), T.LABEL l]
    end

fun unCx (Cx c) = c
  | unCx (Nx s) = raise StmAsCondError
  | unCx (Ex e) = fn (t, f) =>
                     T.CJUMP (T.NE, e, T.CONST 0, t, f)

fun simpleVar ((level1, acc), level2) =
    let
        val (l1, fr1) = case LevelFrameMap.look (!lfmap, level1) of
                            SOME v => v
                          | NONE => raise LevelIdNotFound
        val (l2, fr2) = case LevelFrameMap.look (!lfmap, level2) of
                            SOME v => v
                          | NONE => raise LevelIdNotFound

        fun helper (fp, 0) = fp
          | helper (fp, n) = helper (T.MEM fp, n - 1)
                             
        val realFP = helper (T.TEMP (F.FP), l2 - l1)
    in
        Ex (F.exp acc realFP)
    end

(* id stands for the index of the symbol in the record *)
fun fieldVar (base, id) =
    Ex (T.MEM (T.BINOP (T.PLUS, unEx base, T.BINOP (T.MUL, T.CONST F.wordSize, T.CONST id))))

fun subscriptVar (base, id) =
    Ex (T.MEM (T.BINOP (T.PLUS, unEx base, T.BINOP (T.MUL, T.CONST F.wordSize, unEx id))))

fun intExp i = Ex (T.CONST i)
                  
fun nilExp () = Ex (T.CONST 0)

fun stringExp s =
    let
        val lab = Temp.newlabel ()
    in
        fragments := (F.STRING (lab, s))::(!fragments);
        Ex (T.NAME lab)
    end

(* function initArray : [address, size, initial] *)
fun arrayExp (size, init) =
    let
        val n = unEx size
        val s = T.BINOP (T.MUL,
                         T.CONST (F.wordSize),
                         T.BINOP (T.PLUS, T.CONST 1, n))
        val base = F.externalCall ("malloc", [s])
        val arr = Temp.newtemp ()
    in
        Ex (T.ESEQ (toSeq [
                         T.MOVE (T.TEMP arr, base),
                         T.MOVE (T.MEM (T.TEMP arr), n),
                         T.MOVE (T.TEMP arr, T.BINOP (T.PLUS,
                                                      T.CONST (F.wordSize),
                                                      T.TEMP arr)),
                         (* initialize array *)
                         T.EXP (F.externalCall ("initArray", [T.TEMP arr,
                                                              n,
                                                              unEx init]))
                     ],
                    T.TEMP arr))
    end

fun recordExp (size, exps) =
    let
        val reco = Temp.newtemp ()
        val s = T.BINOP (T.MUL,
                         T.CONST (F.wordSize),
                         T.CONST size)
        val base = F.externalCall ("malloc",[s])

        fun initRecord (base, [], id) = T.EXP (T.CONST 0)
          | initRecord (base, exp::exps, id) =
            T.SEQ (T.MOVE (T.MEM (T.BINOP (T.PLUS, base,
                                           T.BINOP (T.MUL, T.CONST (F.wordSize), T.CONST id))), unEx exp),
                   initRecord (base, exps, id + 1))
    in
        Ex (T.ESEQ (toSeq [
                         T.MOVE (T.TEMP reco, base),
                         (* intialize record *)
                         initRecord (T.TEMP reco, exps, 0)
                     ],
                    T.TEMP reco))
    end
        
fun opExp (A.PlusOp, e1, e2) = Ex (T.BINOP (T.PLUS, unEx e1, unEx e2))
  | opExp (A.MinusOp, e1, e2) = Ex (T.BINOP (T.MINUS, unEx e1, unEx e2))
  | opExp (A.TimesOp, e1, e2) = Ex (T.BINOP (T.MUL, unEx e1, unEx e2))
  | opExp (A.DivideOp, e1, e2) = Ex (T.BINOP (T.DIV, unEx e1, unEx e2))
  | opExp (A.EqOp, e1, e2) = Cx (fn (t, f) => T.CJUMP (T.EQ, unEx e1, unEx e2, t, f))
  | opExp (A.NeqOp, e1, e2) = Cx (fn (t, f) => T.CJUMP (T.NE, unEx e1, unEx e2, t, f))
  | opExp (A.LtOp, e1, e2) = Cx (fn (t, f) => T.CJUMP (T.LT, unEx e1, unEx e2, t, f))
  | opExp (A.LeOp, e1, e2) = Cx (fn (t, f) => T.CJUMP (T.LE, unEx e1, unEx e2, t, f))
  | opExp (A.GtOp, e1, e2) = Cx (fn (t, f) => T.CJUMP (T.GT, unEx e1, unEx e2, t, f))
  | opExp (A.GeOp, e1, e2) = Cx (fn (t, f) => T.CJUMP (T.GE, unEx e1, unEx e2, t, f))

(* 
function stringEqual: [string addr1, length1, string addr2, length2]
function stringLt/Le/Gt/Ge: [string addr1, length1, string addr2, length2]
 *)
fun strCompExp (A.EqOp, e1, e2) =
    let
        val base1 = unEx e1
        val base2 = unEx e2
        val str1 = T.BINOP (T.PLUS, base1, T.CONST (F.wordSize))
        val str2 = T.BINOP (T.PLUS, base2, T.CONST (F.wordSize))
    in
        Ex (F.externalCall ("stringEqual",
                            [str1, T.MEM (base1), str2, T.MEM (base2)]))
    end
  | strCompExp (A.NeqOp, e1, e2) =
    let
        val res = unEx (strCompExp (A.EqOp, e1, e2))
    in
        Ex (T.BINOP (T.MINUS, T.CONST 1, res))
    end
  | strCompExp (oper, e1, e2) =
    let
        val base1 = unEx e1
        val base2 = unEx e2
        val str1 = T.BINOP (T.PLUS, base1, T.CONST (F.wordSize))
        val str2 = T.BINOP (T.PLUS, base2, T.CONST (F.wordSize))
        val func = case oper of 
                       A.LtOp => "stringLt"
                     | A.LeOp => "stringLe"
                     | A.GtOp => "stringGt"
                     | A.GeOp => "stringGe" 
                     | _ => raise RelopOnly
    in
        Ex (F.externalCall (func,
                            [str1, T.MEM (base1), str2, T.MEM (base2)]))
    end

fun ifExp (test, th', el') =
    let
        val res = Temp.newtemp ()
        val genstm = unCx test
        val t = Temp.newlabel () and f = Temp.newlabel ()
        val y = Temp.newlabel () and z = Temp.newlabel ()
        val join = Temp.newlabel ()
    in
        case (th', el') of
            (Nx then', Nx else') =>
            Nx (toSeq [
                     genstm (t, f),
                     T.LABEL t,
                     then',
                     T.JUMP (T.NAME join, [join]),
                     T.LABEL f,
                     else',
                     T.LABEL join
               ])
          | (Cx then', Cx else') =>
            Ex (T.ESEQ (toSeq [
                             genstm (y, z),
                             T.LABEL y,
                             then' (t, f),
                             T.LABEL z,
                             else' (t, f),
                             T.LABEL t,
                             T.MOVE (T.TEMP res, T.CONST 1),
                             T.JUMP (T.NAME join, [join]),
                             T.LABEL f,
                             T.MOVE (T.TEMP res, T.CONST 0),
                             T.LABEL join
                         ],
                        T.TEMP res))
          | (then', Cx else') =>
            Ex (T.ESEQ (toSeq [
                             genstm (t, z),
                             T.LABEL z,
                             T.MOVE (T.TEMP res, T.CONST 1),
                             else' (join, f),
                             T.LABEL f,
                             T.MOVE (T.TEMP res, T.CONST 0),
                             T.JUMP (T.NAME join, [join]),
                             T.LABEL t,
                             T.MOVE (T.TEMP res, unEx then'),
                             T.LABEL join
                         ],
                        T.TEMP res))
          | (Cx then', else') =>
            Ex (T.ESEQ (toSeq [
                             genstm (z, f),
                             T.LABEL z,
                             T.MOVE (T.TEMP res, T.CONST 0),
                             then'(t, join),
                             T.LABEL t,
                             T.MOVE (T.TEMP res, T.CONST 1),
                             T.JUMP (T.NAME join, [join]),
                             T.LABEL f,
                             T.MOVE (T.TEMP res, unEx else'),
                             T.LABEL join
                         ],
                        T.TEMP res))
          | (then', else') =>
            Ex (T.ESEQ (toSeq [
                             genstm (t, f),
                             T.LABEL t,
                             T.MOVE (T.TEMP res, unEx then'),
                             T.JUMP (T.NAME join, [join]),
                             T.LABEL f,
                             T.MOVE (T.TEMP res, unEx else'),
                             T.LABEL join
                         ],
                        T.TEMP res))
    end

fun whileExp (test, body, break) =
    let
        val start = Temp.newlabel ()
        val check = Temp.newlabel ()
        val genstm = unCx test
    in
        Nx (toSeq [
                 T.JUMP (T.NAME check, [check]),
                 T.LABEL start,
                 unNx body,
                 T.LABEL check,
                 genstm (start, break),
                 T.LABEL break
           ])
    end

fun forExp ((level, acc), lo, hi, body, break) =
    let
        val label1 = Temp.newlabel ()
        val label2 = Temp.newlabel ()
        val h = Temp.newtemp ()
        val i = F.exp acc (T.TEMP F.FP)
    in
        Nx (toSeq [
                 T.MOVE (i, unEx lo),
                 T.MOVE (T.TEMP h, unEx hi),
                 T.CJUMP (T.LE, i, T.TEMP h, label2, break),
                 T.LABEL label1,
                 T.MOVE (i, T.BINOP (T.PLUS, i, T.CONST 1)),
                 T.LABEL label2,
                 unNx body,
                 T.CJUMP (T.LT, i, T.TEMP h, label1, break),
                 T.LABEL break
           ])
    end

fun breakExp label = Nx (T.JUMP (T.NAME label, [label]))

fun callExp (label, exps, callLev, defLev) =
    let
        fun getStaticLink (callLev, defLev) =
            let
                val base = T.TEMP (F.FP)
                fun helper (fp, 0) = fp
                  | helper (fp, n) = helper (T.MEM fp, n - 1)
            in
                helper (base, callLev - defLev + 1)
            end
        
        val sl = getStaticLink (callLev, defLev)
        (* the tiger library function don't need a static link *)
        val exps' =
            if defLev = outermost
            then map (fn exp => unEx exp) exps
            else sl::(map (fn exp => unEx exp) exps)
    in
        Ex (T.CALL (T.NAME label, exps'))
    end

fun seqExp [] = raise EmptySequence
  | seqExp [exp] = exp
  | seqExp (exp::exps) =
    let
        fun toEseq (stm, []) = raise EmptySequence
          | toEseq (stm, [exp]) = T.ESEQ (stm, unEx exp) 
          | toEseq (stm, exp::exps) = toEseq (T.SEQ (stm, unNx exp), exps)
    in
        Ex (toEseq (unNx exp, exps))
    end

fun assignExp (var, value) = Nx (T.MOVE (unEx var, unEx value))

fun varDecExp ((level, acc), exp) =
    Nx (T.MOVE (F.exp acc (T.TEMP (F.FP)), unEx exp))

fun letExp ([], exp) = exp
  | letExp (exps, exp) = 
    Ex (T.ESEQ (toSeq (map (fn exp => unNx exp) exps), unEx exp))

end
