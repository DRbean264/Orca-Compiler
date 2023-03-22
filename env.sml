signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty: ty, access: access}
                      | FunEntry of {formals: ty list, result: ty,
                                     level: Translate.level, label: Temp.label}
    val base_tenv : ty Symbol.table (* predefined types*)
    val base_venv : enventry Symbol.table (* predefined functions*)

    val genBaseTenv : unit -> ty Symbol.table
    val genBaseVenv : unit -> enventry Symbol.table
end

structure Env : ENV = 
struct
type access = Translate.access
type ty = Types.ty
datatype enventry = VarEntry of {ty: ty, access: access}
                  | FunEntry of {formals: ty list, result: ty,
                                 level: Translate.level, label: Temp.label}

structure T = Translate

fun genBaseTenv () =
    let
        val tenv : ty Symbol.table = Symbol.empty
        val tenv = Symbol.enter (tenv, Symbol.symbol "int", Types.INT)
        val tenv = Symbol.enter (tenv, Symbol.symbol "string", Types.STRING)
    in tenv end

fun genBaseVenv () =
    let
        val libFuncs = [
            {name = "print", formals = [Types.STRING], result = Types.UNIT},
            {name = "flush", formals = [], result = Types.UNIT},
            {name = "exit", formals = [Types.INT], result = Types.UNIT},
            {name = "getchar", formals = [], result = Types.STRING},
            {name = "ord", formals = [Types.STRING], result = Types.INT},
            {name = "chr", formals = [Types.INT], result = Types.STRING},
            {name = "size", formals = [Types.STRING], result = Types.INT},
            {name = "substring", formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING},
            {name = "concat", formals = [Types.STRING, Types.STRING], result = Types.STRING},
            {name = "not", formals = [Types.INT], result = Types.INT}
        ]
        
        fun addFunc ({name, formals, result}, venv) =
            let
                val label = Temp.namedlabel name
                val level = T.newLevel ({parent = T.outermost,
                                         name = label,
                                         (* dummy formals *)
                                         formals = []})
            in
                Symbol.enter (venv, Symbol.symbol name,
                              FunEntry {formals = formals,
                                        result = result,
                                        level = level,
                                        label = label})
            end
        
        val venv : enventry Symbol.table =
            foldl addFunc Symbol.empty libFuncs
    in
        venv
    end
        
val base_tenv = genBaseTenv ()
val base_venv = genBaseVenv ()
end
