structure FindEscape : sig val findEscape: Absyn.exp -> unit end =
struct
(* Abbreviation *)
structure A = Absyn
exception VarDecNotFound
exception InnerVarUsed
              
type depth = int
type escEnv = (depth * bool ref) Symbol.table

fun traverseVar (env : escEnv, d : depth, A.SimpleVar (sym, pos)) : unit = 
    (case Symbol.look (env, sym) of
         SOME (d', esc) =>
         if d' < d
         then (esc := true;
               ErrorMsg.error pos ("(Not an error message) Variable: " ^ (Symbol.name sym) ^ " is found escaping\n"))
         else if d' > d
         then raise InnerVarUsed
         else ()
       | NONE => (ErrorMsg.error pos ("(Real error message) Variable: " ^ (Symbol.name sym) ^ " is not found in environment\n");
                  raise VarDecNotFound))
  | traverseVar (env, d, A.FieldVar (var, sym, pos)) =
    traverseVar (env, d, var)
  | traverseVar (env, d, A.SubscriptVar (var, exp, pos)) =
    traverseVar (env, d, var)
and traverseExp (env : escEnv, d : depth, A.VarExp var) : unit =
    traverseVar (env, d, var)
  | traverseExp (env, d, A.NilExp) = ()
  | traverseExp (env, d, A.IntExp _) = ()
  | traverseExp (env, d, A.StringExp _) = ()
  | traverseExp (env, d, A.OpExp {left, oper, right, pos}) =
    (traverseExp (env, d, left);
     traverseExp (env, d, right))
  | traverseExp (env, d, A.RecordExp {fields, typ, pos}) =
    foldl (fn ((_, exp, _), _) => traverseExp (env, d, exp)) () fields
  | traverseExp (env, d, A.SeqExp seq) =
    foldl (fn ((exp, _), _) => traverseExp (env, d, exp)) () seq
  | traverseExp (env, d, A.AssignExp {var, exp, pos}) =
    (traverseVar (env, d, var);
     traverseExp (env, d, exp))
  | traverseExp (env, d, A.IfExp {test, then', else', pos}) =
    (traverseExp (env, d, test);
     traverseExp (env, d, then');
     case else' of
         SOME exp => traverseExp (env, d, exp)
       | NONE => () 
    )
  | traverseExp (env, d, A.WhileExp {test, body, pos}) =
    (traverseExp (env, d, test);
     traverseExp (env, d, body))
  | traverseExp (env, d, A.BreakExp pos) = ()
  | traverseExp (env, d, A.LetExp {decs, body, pos}) =
     traverseExp (traverseDecs (env, d, decs), d, body)
  | traverseExp (env, d, A.ArrayExp {typ, size, init, pos}) =
    (traverseExp (env, d, size);
     traverseExp (env, d, init))
  | traverseExp (env, d, A.ForExp {var, escape, lo, hi, body, pos}) =
    let
        val env' = Symbol.enter (env, var, (d, escape))
    in
        traverseExp (env, d, lo);
        traverseExp (env, d, hi);
        traverseExp (env', d, body)
    end
  | traverseExp (env, d, A.CallExp {func, args, pos}) =
    foldl (fn (arg, _) => traverseExp (env, d, arg)) () args
and traverseDecs (env, d, s : Absyn.dec list) : escEnv =
    foldl (fn (dec, env) => traverseDec (env, d, dec)) env s
and traverseDec (env, d, A.TypeDec _) = env
  | traverseDec (env, d, A.VarDec {name, escape, typ, init, pos}) =
    (traverseExp (env, d, init);
     Symbol.enter (env, name, (d, escape)))
  | traverseDec (env, d, A.FunctionDec []) = env
  | traverseDec (env, d, A.FunctionDec ({name, params, result, body, pos}::funcList)) =
    let
        val env' = foldl (fn ({name, escape, typ, pos}, env) =>
                             Symbol.enter (env, name, (d + 1, escape))) env params
    in
        traverseExp (env', d + 1, body);
        traverseDec (env, d, A.FunctionDec funcList)
    end

fun findEscape (prog : Absyn.exp) : unit =
    traverseExp (Symbol.empty : escEnv, 0, prog)
end
