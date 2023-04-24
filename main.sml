structure Main =
struct
structure F = Frame
structure T = Tree
structure C = Canon
structure R = Reg_Alloc
structure A = Assem

exception UnknownAllocation

fun read_file_contents (filename: string): string =
    let
        val in_stream = TextIO.openIn filename
        val contents = TextIO.inputAll in_stream
    in
        TextIO.closeIn in_stream;
        contents
    end

(* remove self move instruction *)
fun cleanUp saytemp [] = []
  | cleanUp saytemp ((instr as A.MOVE {assem, dst, src})::instrs) =
    if saytemp dst = saytemp src
    then cleanUp saytemp instrs
    else instr::(cleanUp saytemp instrs)
  | cleanUp saytemp (instr::instrs) = instr::(cleanUp saytemp instrs)            
              
fun emitproc out (F.PROC {body, frame}) =
    let
        val _ = print ("Emitting: " ^ (F.name frame) ^ "\n")
        
        fun saytemp allocation t =
            case Temp.Table.look (allocation, t) of
                SOME reg => reg
              | NONE => raise UnknownAllocation 
        
        val stms = Canon.linearize body
        val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
                                        
        val instrs = List.concat (map (MipsGen.codegen frame) stms')        
        val instrs' = F.procEntryExit2 (frame, instrs)

        (* register allocation *)
        val (instrs', allocation) = R.alloc (instrs', frame)
        val instrs' = cleanUp (saytemp allocation) instrs'

        val {prolog, body = instrs', epilog} = F.procEntryExit3 (frame, instrs')
                                                                
        (* use the result of allocation to format the assembly code *)
        val format = Assem.format (saytemp allocation)
    in        
        TextIO.output (out, "\n.text\n");
        TextIO.output (out, prolog);
        app (fn i => TextIO.output (out, format i)) instrs';
        TextIO.output (out, epilog)
    end
  | emitproc out (F.STRING (lab, s)) =
    (TextIO.output (out, "\n.data\n");
     TextIO.output (out, F.string (lab, s)))

fun withOpenFile fname f = 
    let
        val out = TextIO.openOut fname
    in
        (* put the contents of runtime-le.s & sysspim.s into the file *)
        TextIO.output (out, read_file_contents "sysspim.s");
        TextIO.output (out, read_file_contents "runtime-le.s");
        (f out before (TextIO.closeOut out))
        handle e => (TextIO.closeOut out; raise e)
    end 
        
fun compile filename = 
    let
        val _ = Temp.reset (Frame.tempReset)
        val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in
        if !ErrorMsg.anyErrors
        then ()
        else withOpenFile (filename ^ ".s")
                          (fn out => (app (emitproc out) frags))
    end

end
