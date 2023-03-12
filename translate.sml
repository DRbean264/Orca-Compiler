signature TRANSLATE =
sig
    type level
    type access
    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access
    val reset : unit -> unit
    (* TODO: remove debugging stuff *)
    val printAccInfo : level -> unit list
    val getLogicalLevel : level -> int
end

structure Translate : TRANSLATE =
struct
exception LevelIdNotFound

(* the level here is a level id, the logical level is stored in the lfmap *)
type level = int
type access = level * Frame.access

structure LevelFrameMap = IntMapTable (type key = level
		                       fun getInt l = l)

(* TODO: check if this outermost level has static link or not later *)
val outermost = 0
val lfmap : (int * Frame.frame) LevelFrameMap.table ref =
    ref (LevelFrameMap.enter (LevelFrameMap.empty, outermost, (0, Frame.newFrame ({name = Temp.namedlabel "tig_main", formals = []}))))
                    
val nextId = ref (outermost + 1)
fun getNextId () =
    let
        val res = !nextId
    in
        nextId := res + 1;
        res
    end

(* TODO: take care of the corner case outermost *)
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
                    
val nextId = ref (outermost + 1)
        
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
end
