(* Notes:
1. Type compatibility: one type should be the child of the second type *)
signature SEMANT =
sig
    val transProg : Absyn.exp -> unit
end

structure Semant : SEMANT =
struct

(* abbreviation *)
structure A = Absyn
structure E = Env
structure T = Types
val error = ErrorMsg.error

(* used to check if break is in the loop *)
val inLoop : unit option ref = ref NONE
val level = ref 0

fun enterLoop () = (inLoop := SOME (); level := !level + 1)
fun exitLoop () = (level := !level - 1; if !level = 0 then inLoop := NONE else ())
fun reset () = (inLoop := NONE; level := 0)
                      
fun checkCompatType (typ1, typ2, pos) =
    let
        fun errMsg () = error pos "Type doesn't match the declared one"
    in
        (* The joined type should be equal to the type1 *)
        case (T.join (typ1, typ2), typ1) of
            (T.INT, T.INT) => ()
          | (T.STRING, T.STRING) => ()
          | (T.UNIT, T.UNIT) => ()
          | (T.NIL, T.NIL) => ()
          | (T.IMPOSSIBLE, T.IMPOSSIBLE) => ()
          | (T.ARRAY (ty1, uniq1), T.ARRAY (ty2, uniq2)) => if uniq1 = uniq2 then () else errMsg ()
          | (T.RECORD (fn1, uniq1), T.RECORD (fn2, uniq2)) => if uniq1 = uniq2 then () else errMsg ()
          | _ => errMsg ()
    end
                                    
fun checkUnit ({exp, ty = T.UNIT}, pos) = ()
  | checkUnit (_, pos) = error pos "Unit required"

fun checkNil ({exp, ty = T.NIL}, pos) = ()
  | checkNil (_, pos) = error pos "Nil required"
                               
fun checkInt ({exp, ty = T.INT}, pos) = ()
  | checkInt (_, pos) = error pos "Integer required"

fun checkString ({exp, ty = T.STRING}, pos) = ()
  | checkString (_, pos) = error pos "String required"

fun checkArray (uniq, {exp, ty = (T.ARRAY (_, uniq'))}, pos) =
    if uniq = uniq' then () else error pos "Same array type required"
  | checkArray (uniq, {exp, ty = _}, pos) = error pos "Array required"

fun checkRecord (uniq, {exp, ty = (T.RECORD (_, uniq'))}, pos) =
    if uniq = uniq' then () else error pos "Same record type required"
  | checkRecord (uniq, {exp, ty = T.NIL}, pos) = ()
  | checkRecord (uniq, {exp, ty = _}, pos) = error pos "Record required"

fun checkRecordReverse ({exp, ty = (T.RECORD _)}, pos) = ()
  | checkRecordReverse ({exp, ty = _}, pos) = error pos "Record required"

(* origFields : (symbol * ty) list
   rawFields: (symbol * ty * pos) list *)
fun checkRecordFields ([], [], pos) = ()
  | checkRecordFields ([], rawFields, pos) = error pos "More fields than expected" 
  | checkRecordFields (origFields, [], pos) = error pos "Fewer fields than expected"
  | checkRecordFields ((sym1, ty1)::origFields, (sym2, ty2, pos')::rawFields, pos) =
    ( (* check if their names are the same *)
      if (Symbol.name sym1) = (Symbol.name sym2) then () else error pos' "Fields should be of the same name and same order of the definition";
      (* check if their types are the compatible *)
      checkCompatType (ty1, ty2, pos');
      checkRecordFields (origFields, rawFields, pos))

(* args : ty list
   args': ty list *)
fun checkFunFields ([], [], pos) = ()
  | checkFunFields ([], args', pos) = error pos "More arguments than expected"
  | checkFunFields (args, [], pos) = error pos "Fewer arguments than expected"
  | checkFunFields (ty::args, ty'::args', pos) =
    (checkCompatType (ty, ty', pos);
     checkFunFields (args, args', pos))
        
fun transExp (venv,tenv) =
    let
        fun trexp (A.NilExp) = {exp = (), ty = T.NIL}
          | trexp (A.IntExp i) = {exp = (), ty = T.INT}
          | trexp (A.StringExp (s, pos)) = {exp = (), ty = T.STRING}
          | trexp (A.ArrayExp {typ, size, init, pos}) =
            let
                (* size should be an int *)
                val _ = checkInt (trexp size, pos)
                val actualTy = Symbol.look (tenv, typ)
                val trInit = trexp init
            in
                (* the type should be in the type environment & should be an array type *)
                case actualTy of
                    SOME (T.ARRAY (ty', uniq')) =>
                    (* check type compatibility *)
                    (checkCompatType (ty', #ty trInit, pos);
                     {exp = (), ty = T.ARRAY (ty', uniq')})
                  | SOME _ =>
                    (error pos ("The type: " ^ (Symbol.name typ) ^ " is not an array type");
                     {exp = (), ty = T.IMPOSSIBLE})
                  | NONE =>
                    (error pos ("Unknown type: " ^ (Symbol.name typ));
                     {exp = (), ty = T.IMPOSSIBLE})
            end              
          (* support recusive definition of record *)
          (* check all fields names and types are in the same order as declaration *)
          | trexp (A.RecordExp {fields, typ, pos}) =
            let
                (* check field names *)
                val actualTy = Symbol.look (tenv, typ)
                (* (symbol * exp * pos) list -> (symbol * ty * pos) list *)
                val fields'' = map (fn (sym, exp, pos) => (sym, #ty (trexp exp), pos)) fields                                           
            in
                case actualTy of
                    SOME (T.RECORD (fieldsGen, uniq)) =>
                    let
                        (* fetch the original fields *)
                        val fields' = fieldsGen ()
                        val _ = checkRecordFields (fields', fields'', pos)
                    in
                        {exp = (), ty = T.RECORD (fieldsGen, uniq)}
                    end
                  | SOME _ =>
                    (error pos ("The type: " ^ (Symbol.name typ) ^ " is not a record type");
                     {exp = (), ty = T.IMPOSSIBLE})
                  | NONE =>
                    (error pos ("Unknown type: " ^ (Symbol.name typ));
                     {exp = (), ty = T.IMPOSSIBLE})
            end
          | trexp (A.OpExp {left, oper = A.PlusOp, right, pos}) =
            trarithmetic (left, right, pos)  
          | trexp (A.OpExp {left, oper = A.MinusOp, right, pos}) =
            trarithmetic (left, right, pos)
          | trexp (A.OpExp {left, oper = A.TimesOp, right, pos}) =
            trarithmetic (left, right, pos)
          | trexp (A.OpExp {left, oper = A.DivideOp, right, pos}) =
            trarithmetic (left, right, pos)
          | trexp (A.OpExp {left, oper = A.EqOp, right, pos}) =
            treq (left, right, pos)
          | trexp (A.OpExp {left, oper = A.NeqOp, right, pos}) =
            treq (left, right, pos)
          | trexp (A.OpExp {left, oper = A.LtOp, right, pos}) =
            trcompare (left, right, pos)
          | trexp (A.OpExp {left, oper = A.LeOp, right, pos}) =
            trcompare (left, right, pos)
          | trexp (A.OpExp {left, oper = A.GtOp, right, pos}) =
            trcompare (left, right, pos)
          | trexp (A.OpExp {left, oper = A.GeOp, right, pos}) =
            trcompare (left, right, pos)
          | trexp (A.CallExp {func, args, pos}) =
            let
                (* exp list -> ty list *)
                val args' = map (fn exp => #ty (trexp exp)) args
            in
                case Symbol.look (venv, func) of
                    SOME (E.FunEntry {formals, result}) =>
                    let
                        (* check each argument type is compatible with the defined one *)
                        val _ = checkFunFields (formals, args', pos)
                    in
                        {exp = (), ty = result}
                    end
                  | SOME (E.VarEntry _) =>
                    (error pos ("The name: " ^ (Symbol.name func) ^ " is not a function but a variable");
                     {exp = (), ty = T.IMPOSSIBLE})
                  | NONE =>
                    (error pos ("Unknown function: " ^ (Symbol.name func));
                     {exp = (), ty = T.IMPOSSIBLE})
            end
          | trexp (A.SeqExp expList) = trSeqExp expList
          | trexp (A.AssignExp {var, exp, pos}) =
            (checkCompatType (#ty (trvar var), #ty (trexp exp) , pos);
             {exp = (), ty = T.UNIT})
          | trexp (A.BreakExp pos) =
            (case !inLoop of
                 SOME () => ()
               | NONE => error pos "BREAK should be inside a FOR or WHILE loop";
             {exp = (), ty = T.UNIT})
          | trexp (A.WhileExp {test, body, pos}) =
            (
              enterLoop ();
              (* check if type of test is int *)
              checkInt (trexp test, pos);
              (* check if type of body is unit *)
              checkUnit (trexp body, pos);
              exitLoop ();
              {exp = (), ty = T.UNIT})
          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
            (enterLoop ();
             exitLoop ();
             {exp = (), ty = T.UNIT})
        and trvar (A.SimpleVar (sym, pos)) =
            (case Symbol.look (venv, sym) of
                 SOME (E.VarEntry {ty}) => {exp = (), ty = ty}
               | SOME (E.FunEntry _) =>
                 (error pos ("The name: " ^ Symbol.name sym ^ " is a function");
                  {exp = (), ty = T.IMPOSSIBLE})
               | NONE =>
                 (error pos ("Unknown variable: " ^ Symbol.name sym);
                  {exp = (), ty = T.IMPOSSIBLE}))
          | trvar (A.FieldVar (var, sym, pos)) =
            (* the type of var should be a record *)
            (case #ty (trvar var) of
                 T.RECORD (gen, _) =>
                 let
                     (* find the type of the field *)
                     fun findType ([], sym, pos) =
                         (error pos ("Invalid field: " ^ (Symbol.name sym) ^ " in the record");
                         T.IMPOSSIBLE)
                       | findType ((field, ty)::fields, sym, pos) =
                         if (Symbol.name field) = (Symbol.name sym)
                         then ty else findType (fields, sym, pos)
                     
                     val fields = gen ()                                      
                 in
                     {exp = (), ty = findType (fields, sym, pos)}
                 end
               | _ =>
                 (error pos "Field selection is only for record type";
                  {exp = (), ty = T.IMPOSSIBLE}))
          | trvar (A.SubscriptVar (var, exp, pos)) =
            (* the type of var should be an array *)
            (case #ty (trvar var) of
                 T.ARRAY (ty, _) =>
                 (checkInt (trexp exp, pos);
                  {exp = (), ty = ty})
               | _ =>
                 (error pos "Indexing is only for array type";
                  {exp = (), ty = T.IMPOSSIBLE}))
        and trarithmetic (left, right, pos) =
            (checkInt (trexp left, pos);
             checkInt (trexp right, pos);
             {exp = (), ty = T.INT})
        and treq (left, right, pos) =
            let
                val {exp = _, ty = leftTy} = trexp left
                val trRight = trexp right
                val _ = case leftTy of
                            T.INT => checkInt (trRight, pos)
                          | T.STRING => checkString (trRight, pos)
                          | T.ARRAY (_, uniq) => checkArray (uniq, trRight, pos)
                          | T.RECORD (_, uniq) => checkRecord (uniq, trRight, pos)
                          | T.NIL => checkRecordReverse (trRight, pos)
                          | _ => error pos "Uncomparable types for equality"
            in
                {exp = (), ty = T.INT}
            end
        and trcompare (left, right, pos) =
            let
                val {exp = _, ty = leftTy} = trexp left
                val {exp = _, ty = rightTy} = trexp right
                val _ = case (leftTy, rightTy) of
                            (T.INT, T.INT) => ()
                          | (T.STRING, T.STRING) => ()
                          | _ => error pos "Uncomparable types"
            in
                {exp = (), ty = T.INT}
            end
        and trSeqExp [] = {exp = (), ty = T.UNIT}
          | trSeqExp [(exp, pos)] = {exp = (), ty = #ty (trexp exp) }
          | trSeqExp ((exp, pos)::expList) =
            (trexp exp;
             trSeqExp expList)
    in
        trexp
    end

(* TODO: change the return stuff in the next phase *)
fun transProg prog = (reset (); transExp (E.base_venv, E.base_tenv) prog; ())
       
end
