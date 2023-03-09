signature FRAME =
sig
    type frame
    type access
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
end

structure MipsFrame : FRAME =
struct
type frame = {name: Temp.label, formals: access list, numLocal: int, isns : string}
type access = InFrame of int
            | InReg of Temp.temp

(* TODO: now assume all formals are true, escaping *)
fun newFrame ({name, formals}) =
    
fun name fr = Symbol.symbol "0"
fun formals fr = []
fun allocLocal fr escape = 0
end

structure Frame : FRAME = MipsFrame
