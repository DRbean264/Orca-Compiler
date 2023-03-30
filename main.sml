structure Main =
struct
structure F = Frame
structure T = Tree
structure C = Canon

fun getsome (SOME x) = x

fun emitproc out (F.PROC {body, frame}) =
    let val _ = print ("emit " ^ F.name frame ^ "\n")
        val stms = Canon.linearize body
        (*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
        (* debugging only *)
        (* val _ = print "Canonicalized statements:\n" *)
        (* val _ = app (fn stm => Printtree.printtree (TextIO.stdOut, stm)) stms' *)
        (* val _ = print "\n" *)

        fun helper stm =
            (print "Processing statement:\n";
             Printtree.printtree (TextIO.stdOut, stm);
             MipsGen.codegen frame stm)
                
        val instrs = List.concat (map helper stms')
        (* debugging only *)
                                 
        fun getnames m t =
            case Temp.Table.look (m, t) of
                SOME(s) => s
              | NONE => Temp.makestring t
                                        
        (* val instrs = List.concat (map (MipsGen.codegen frame) stms') *)
        val {prolog, body = instrs', epilog} = F.procEntryExit3 (frame, instrs)
        val format0 = Assem.format (getnames F.tempMap)
    in app (fn i => TextIO.output (out, format0 i)) instrs'
    end
  | emitproc out (F.STRING (lab, s)) = TextIO.output(out, F.string (lab, s))

fun withOpenFile fname f = 
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out) 
       handle e => (TextIO.closeOut out; raise e)
    end 

fun compile filename = 
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in
        if !ErrorMsg.anyErrors
        then ()
        else withOpenFile (filename ^ ".s")
                          (fn out => (app (emitproc out) frags))
    end
        
(* fun run filename = 
            let
                val absyn = Parse.parse filename
                val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)

                fun displayFrags [] = ()
                    | displayFrags ((F.STRING (label, str))::frags) =
                        (print ("String: (Label: " ^ (Symbol.name label) ^
                                ", string literal: \"" ^ str ^ "\")\n");
                            displayFrags frags)
                    | displayFrags ((F.PROC {body, frame})::frags) =
                        (F.printFrameInfo frame;
                            print "Body of the frame:\n";
                            Printtree.printtree (TextIO.stdOut, body);
                            print "\n";
                            displayFrags frags)

                fun displayFrags' [] = ()
                    | displayFrags' ({body, frame}::frags) =
                        (F.printFrameInfo frame;
                            print "Body of the frame:\n";
                            foldl (fn (stm, _) =>
                                    (print "New stm:\n";
                                        Printtree.printtree (TextIO.stdOut, stm);
                                        print "\n")) () body;
                            print "\n";
                            displayFrags' frags)

                fun canonicalize frags =
                        let
                            fun helper (F.STRING s, res) = res
                                | helper (F.PROC {body, frame}, res) =
                                    {body = C.traceSchedule (C.basicBlocks (C.linearize body)),
                                        frame = frame}::res
                        in
                            foldl helper [] frags
                        end

                (* frags' : {body : T.stm list, frame : F.frame} list *)
                val frags' = canonicalize frags
                                  
            in
                if !ErrorMsg.anyErrors
                then ()
                else
                    (F.printRegInfo ();
                        displayFrags' frags')
            end *)
end
