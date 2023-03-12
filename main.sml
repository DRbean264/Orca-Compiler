structure Main =
struct
fun run filename = 
    let
        val absyn = Parse.parse filename
        (* val _ = PrintAbsyn.print (TextIO.stdOut, absyn) *)
    in
        Semant.transProg absyn;
        (* TODO: maybe not doing this if type checking fails *)
        FindEscape.findEscape absyn
    end
end
