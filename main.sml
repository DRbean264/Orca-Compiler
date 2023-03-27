structure Main =
struct
structure F = Frame
structure T = Tree
structure C = Canon

fun run filename = 
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
    end
end
