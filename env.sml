signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result: ty}
    val base_tenv : ty Symbol.table (* predefined types*)
    val base_venv : enventry Symbol.table (* predefined functions*)
end

structure Env =
struct
type access = unit
type ty = Types.ty
datatype enventry = VarEntry of {ty: ty}
                  | FunEntry of {formals: ty list, result: ty}

fun genBaseTenv () =
    let
        val tenv : ty Symbol.table = Symbol.empty
        val tenv = Symbol.enter (tenv, Symbol.symbol "int", Types.INT)
        val tenv = Symbol.enter (tenv, Symbol.symbol "string", Types.STRING)
    in tenv end

fun genBaseVenv () =
    let
        val venv : enventry Symbol.table = Symbol.empty

        (* procedures *)
        val venv = Symbol.enter (venv, Symbol.symbol "print",
                                 FunEntry {formals = [Types.STRING], result = Types.UNIT})
        val venv = Symbol.enter (venv, Symbol.symbol "flush",
                                 FunEntry {formals = [], result = Types.UNIT})
        val venv = Symbol.enter (venv, Symbol.symbol "exit",
                                 FunEntry {formals = [Types.INT], result = Types.IMPOSSIBLE})
                                
        (* functions which have return value *)
        val venv = Symbol.enter (venv, Symbol.symbol "getchar",
                                 FunEntry {formals = [], result = Types.STRING})
        val venv = Symbol.enter (venv, Symbol.symbol "ord",
                                 FunEntry {formals = [Types.STRING], result = Types.INT})
        val venv = Symbol.enter (venv, Symbol.symbol "chr",
                                 FunEntry {formals = [Types.INT], result = Types.STRING})
        val venv = Symbol.enter (venv, Symbol.symbol "size",
                                 FunEntry {formals = [Types.STRING], result = Types.INT})
        val venv = Symbol.enter (venv, Symbol.symbol "substring",
                                 FunEntry {formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING})
        val venv = Symbol.enter (venv, Symbol.symbol "concat",
                                 FunEntry {formals = [Types.STRING, Types.STRING], result = Types.STRING})
        val venv = Symbol.enter (venv, Symbol.symbol "not",
                                 FunEntry {formals = [Types.INT], result = Types.INT})
    in venv end
        
val base_tenv = genBaseTenv ()
val base_venv = genBaseVenv ()
end
