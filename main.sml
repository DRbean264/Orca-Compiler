structure Main =
struct
structure F = Frame

fun run filename = 
    let
        val absyn = Parse.parse filename
        val _ = FindEscape.findEscape absyn
        val frags = Semant.transProg absyn

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
    in
        if !ErrorMsg.anyErrors
        then ()
        else
            (F.printRegInfo ();
             displayFrags frags)
    end
end
