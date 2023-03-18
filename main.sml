structure Main =
struct
fun run filename = 
    let
        val absyn = Parse.parse filename
        val exp = Semant.transProg absyn
        (* val _ = PrintAbsyn.print (TextIO.stdOut, absyn) *)
    in
        if !ErrorMsg.anyErrors
        then ()
        else Printtree.printtree (TextIO.stdOut, Translate.unNx exp)
    end
end
