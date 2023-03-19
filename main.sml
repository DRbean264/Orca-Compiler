structure Main =
struct
fun run filename = 
    let
        val absyn = Parse.parse filename
        val _ = FindEscape.findEscape absyn
        val exp = Semant.transProg absyn
    in
        if !ErrorMsg.anyErrors
        then ()
        else Printtree.printtree (TextIO.stdOut, Translate.unNx exp)
    end
end
