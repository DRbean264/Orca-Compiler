structure Main =
struct
fun run filename =
    let
        val absyn = Parse.parse filename
        val _ = PrintAbsyn.print (TextIO.stdOut, absyn)
    in
        Semant.transProg absyn
    end
end
