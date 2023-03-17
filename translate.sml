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

    val opExp : Absyn.oper * exp * exp -> exp

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
exception NotArithOp
exception NotEqOp
exception NotCompOp
              
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

val dummyExp = Ex (T.CONST 0)
                                      
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
                             
        val realFP = helper (T.TEMP (Frame.FP), l2 - l1)
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
end
