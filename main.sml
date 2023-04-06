structure Main =
struct
structure F = Frame
structure T = Tree
structure C = Canon

fun getnames m t =
    case Temp.Table.look (m, t) of
        SOME(s) => s
      | NONE => Temp.makestring t
                  
fun emitproc out (F.PROC {body, frame}) =
    let (* val _ = print ("emit " ^ F.name frame ^ "\n") *)
        val stms = Canon.linearize body
        (*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
        (* debugging only *)
        (* val _ = print "Canonicalized statements:\n" *)
        (* val _ = app (fn stm => Printtree.printtree (TextIO.stdOut, stm)) stms' *)
        (* val _ = print "\n" *)

        (* fun helper stm = *)
        (*     (print "Processing statement:\n"; *)
        (*      Printtree.printtree (TextIO.stdOut, stm); *)
        (*      MipsGen.codegen frame stm) *)
                
        (* val instrs = List.concat (map helper stms') *)
        (* debugging only *)
                                                                         
        val instrs = List.concat (map (MipsGen.codegen frame) stms')
        val {prolog, body = instrs', epilog} = F.procEntryExit3 (frame, instrs)

        val (fg, nodes) = MakeGraph.instrs2graph instrs'
                                                                
        val format0 = Assem.format (getnames F.tempMap)
    in app (fn i => TextIO.output (out, format0 i)) instrs';
       MakeGraph.displayGraph fg (getnames F.tempMap)
    end
  | emitproc out (F.STRING (lab, s)) = TextIO.output(out, F.string (lab, s))

fun withOpenFile fname f = 
    let val out = TextIO.openOut fname
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
