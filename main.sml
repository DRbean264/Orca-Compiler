structure Main =
struct
structure F = Frame
structure T = Tree
structure C = Canon
structure R = Reg_Alloc

exception UnknownAllocation
                  
fun emitproc out (F.PROC {body, frame}) =
    let
        (* val saytemp = F.saytemp *)
        fun saytemp allocation t =
            case Temp.Table.look (allocation, t) of
                SOME reg => reg
              | NONE => raise UnknownAllocation 
        
        val stms = Canon.linearize body
        val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
        val instrs = List.concat (map (MipsGen.codegen frame) stms')
        
        (* register allocation *)
        val (instrs, allocation) = R.alloc (instrs, frame)

        val {prolog, body = instrs', epilog} = F.procEntryExit3 (frame, instrs)

        (* use the result of allocation to format the assembly code *)
        val format0 = Assem.format (saytemp allocation)
    in
        app (fn i => TextIO.output (out, format0 i)) instrs'
    end
  | emitproc out (F.STRING (lab, s)) = TextIO.output(out, F.string (lab, s))

fun withOpenFile fname f = 
    let
        val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
       handle e => (TextIO.closeOut out; raise e)
    end 

fun compile filename = 
    let
        val _ = (Temp.reset (Frame.tempReset); MakeGraph.reset ())
        val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in
        if !ErrorMsg.anyErrors
        then ()
        else withOpenFile (filename ^ ".s")
                          (fn out => (app (emitproc out) frags))
    end

end
