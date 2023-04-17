structure Main =
struct
structure F = Frame
structure T = Tree
structure C = Canon
structure R = Reg_Alloc
structure A = Assem

exception UnknownAllocation

(* remove self move instruction *)
fun cleanUp saytemp [] = []
  | cleanUp saytemp ((instr as A.MOVE {assem, dst, src})::instrs) =
    if saytemp dst = saytemp src
    then cleanUp saytemp instrs
    else instr::(cleanUp saytemp instrs)
  | cleanUp saytemp (instr::instrs) = instr::(cleanUp saytemp instrs)            
              
fun emitproc (out1, out2) (F.PROC {body, frame}) =
    let
        (* val saytemp = F.saytemp *)
        fun saytemp allocation t =
            case Temp.Table.look (allocation, t) of
                SOME reg => reg
              | NONE => raise UnknownAllocation 
        
        val stms = Canon.linearize body
        val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
        val instrs = List.concat (map (MipsGen.codegen frame) stms')
        
        val {prolog, body = instrs', epilog} = F.procEntryExit3 (frame, instrs)

        (* register allocation *)
        val (instrs'', allocation) = R.alloc (instrs', frame)
        val instrs''' = cleanUp (saytemp allocation) instrs''
                                                                
        (* use the result of allocation to format the assembly code *)
        val format0 = Assem.format F.saytemp
        val format0' = Assem.format (saytemp allocation)
    in
        app (fn i => TextIO.output (out2, format0 i)) instrs'';
        app (fn i => TextIO.output (out1, format0' i)) instrs'''
    end
  | emitproc (out1, out2) (F.STRING (lab, s)) =
    (TextIO.output (out1, F.string (lab, s));
     TextIO.output (out2, F.string (lab, s)))

fun withOpenFile fname1 fname2 f = 
    let
        val out1 = TextIO.openOut fname1
        val out2 = TextIO.openOut fname2
    in (f (out1, out2) before (TextIO.closeOut out1; TextIO.closeOut out2))
       handle e => (TextIO.closeOut out1; TextIO.closeOut out2; raise e)
    end 

fun compile filename = 
    let
        val _ = Temp.reset (Frame.tempReset)
        val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in
        if !ErrorMsg.anyErrors
        then ()
        else withOpenFile (filename ^ ".s") (filename ^ ".org.s")
                          (fn (out1, out2) => (app (emitproc (out1, out2)) frags))
    end

end
