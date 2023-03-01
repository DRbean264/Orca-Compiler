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

datatype temp = NAMETEMP of (A.symbol * T.ty)
              | ARRAYTEMP of (A.symbol * T.ty)
              | RECORDTEMP of T.ty
                
fun ty2str (T.UNIT) = "unit"
  | ty2str (T.NIL) = "nil"
  | ty2str (T.INT) = "int"
  | ty2str (T.STRING) = "string"
  | ty2str (T.ARRAY _) = "array"
  | ty2str (T.RECORD _) = "record"
  | ty2str (T.IMPOSSIBLE) = "impossible"
  | ty2str (T.ROOT) = "root"
                
fun checkCompatTypeBase (typ1, typ2, pos) =
    (* The joined type should be equal to the type1 *)
    case (T.join (typ1, typ2), typ1) of
        (T.INT, T.INT) => true
      | (T.STRING, T.STRING) => true
      | (T.UNIT, T.UNIT) => true
      | (T.NIL, T.NIL) => true
      | (T.IMPOSSIBLE, T.IMPOSSIBLE) => true
      | (T.ARRAY (ty1, uniq1), T.ARRAY (ty2, uniq2)) => if uniq1 = uniq2 then true else false
      | (T.RECORD (fn1, uniq1), T.RECORD (fn2, uniq2)) => if uniq1 = uniq2 then true else false
      | _ => false

fun checkCompatType (typ1, typ2, pos) =
    if checkCompatTypeBase (typ1, typ2, pos) then ()
    else error pos ("Type: " ^ (ty2str typ2) ^ " doesn't match the declared one: " ^ (ty2str typ1))
                 
fun checkParentType (typ1, typ2, pos) = checkCompatTypeBase (typ1, typ2, pos) orelse checkCompatTypeBase (typ2, typ1, pos)
        
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
        
fun transExp (venv, tenv) =
    let
        fun trexp (A.NilExp, inLoop) = {exp = (), ty = T.NIL}
          | trexp (A.IntExp i, inLoop) = {exp = (), ty = T.INT}
          | trexp (A.StringExp (s, pos), inLoop) = {exp = (), ty = T.STRING}
          | trexp (A.ArrayExp {typ, size, init, pos}, inLoop) =
            let
                (* size should be an int *)
                val _ = checkInt (trexp (size, NONE), pos)
                val actualTy = Symbol.look (tenv, typ)
                val trInit = trexp (init, NONE)
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
          (* check all fields names and types are in the same order as declaration *)
          | trexp (A.RecordExp {fields, typ, pos}, inLoop) =
            let
                (* check field names *)
                val actualTy = Symbol.look (tenv, typ)
                (* (symbol * exp * pos) list -> (symbol * ty * pos) list *)
                val fields'' = map (fn (sym, exp, pos) => (sym, #ty (trexp (exp, NONE)), pos)) fields                                           
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
          | trexp (A.OpExp {left, oper = A.PlusOp, right, pos}, inLoop) =
            trarithmetic (left, right, pos)  
          | trexp (A.OpExp {left, oper = A.MinusOp, right, pos}, inLoop) =
            trarithmetic (left, right, pos)
          | trexp (A.OpExp {left, oper = A.TimesOp, right, pos}, inLoop) =
            trarithmetic (left, right, pos)
          | trexp (A.OpExp {left, oper = A.DivideOp, right, pos}, inLoop) =
            trarithmetic (left, right, pos)
          | trexp (A.OpExp {left, oper = A.EqOp, right, pos}, inLoop) =
            treq (left, right, pos)
          | trexp (A.OpExp {left, oper = A.NeqOp, right, pos}, inLoop) =
            treq (left, right, pos)
          | trexp (A.OpExp {left, oper = A.LtOp, right, pos}, inLoop) =
            trcompare (left, right, pos)
          | trexp (A.OpExp {left, oper = A.LeOp, right, pos}, inLoop) =
            trcompare (left, right, pos)
          | trexp (A.OpExp {left, oper = A.GtOp, right, pos}, inLoop) =
            trcompare (left, right, pos)
          | trexp (A.OpExp {left, oper = A.GeOp, right, pos}, inLoop) =
            trcompare (left, right, pos)
          | trexp (A.CallExp {func, args, pos}, inLoop) =
            let
                (* exp list -> ty list *)
                val args' = map (fn exp => #ty (trexp (exp, NONE))) args
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
          | trexp (A.VarExp var, inLoop) = trvar var
          | trexp (A.SeqExp expList, inLoop) =
            let
                fun trSeqExp [] = {exp = (), ty = T.UNIT}
                  | trSeqExp [(exp, pos)] = {exp = (), ty = #ty (trexp (exp, inLoop)) }
                  | trSeqExp ((exp, pos)::expList) =
                    (trexp (exp, inLoop); trSeqExp expList) 
            in
                trSeqExp expList
            end
          | trexp (A.AssignExp {var, exp, pos}, inLoop) =
            (checkCompatType (#ty (trvar var), #ty (trexp (exp, NONE)) , pos);
             {exp = (), ty = T.UNIT})
          | trexp (A.BreakExp pos, inLoop) =
            (case inLoop of
                 SOME () => {exp = (), ty = T.IMPOSSIBLE}
               | NONE => (error pos "BREAK should be only in for/while loop";
                          {exp = (), ty = T.IMPOSSIBLE}))
          | trexp (A.WhileExp {test, body, pos}, inLoop) =
            ( (* check if type of test is int *)
              checkInt (trexp (test, NONE), pos);
              (* check if type of body is compatible with unit *)
              checkCompatType (T.UNIT, #ty (trexp (body, SOME ())), pos);
              {exp = (), ty = T.UNIT})
          | trexp (A.ForExp {var, escape, lo, hi, body, pos}, inLoop) =
            let
                val venv' = Symbol.enter (venv, var, E.VarEntry {ty = T.INT})
            in
                ( (* check if types of lo & hi are int *)
                  checkInt (trexp (lo, NONE), pos);
                  checkInt (trexp (hi, NONE), pos);
                  (* check if type of body is compatible with unit *)
                  checkCompatType (T.UNIT, #ty (transExp (venv', tenv) (body, SOME ())), pos);
                  {exp = (), ty = T.UNIT})
            end
          | trexp (A.IfExp {test, then', else' = NONE, pos}, inLoop) =
            ( (* check if type of test is int *)
              checkInt (trexp (test, NONE), pos);
              (* check if type of body is compatible with unit *)
              checkCompatType (T.UNIT, #ty (trexp (then', inLoop)), pos);
              {exp = (), ty = T.UNIT})
          | trexp (A.IfExp {test, then', else' = SOME elseExp, pos}, inLoop) =
            let
                val {exp = _, ty = ty1} = trexp (then', inLoop)
                val {exp = _, ty = ty2} = trexp (elseExp, inLoop)
            in
                ( (* check if type of test is int *)
                  checkInt (trexp (test, NONE), pos);
                  (* one type has to be another's parent type *)
                  print (ty2str ty1);
                  print (ty2str ty2);
                  if checkParentType (ty1, ty2, pos)
                  then {exp = (), ty = T.join (ty1, ty2)}
                  else
                      (error pos ("Incompatible types: " ^ (ty2str ty1) ^ " and " ^ (ty2str ty2) ^ "\n"); 
                       {exp = (), ty = T.IMPOSSIBLE}))
            end
          | trexp (A.LetExp {decs, body, pos}, inLoop) =
            let
                val {venv = venv', tenv = tenv'} = transDecs (venv, tenv, decs)
            in
                transExp (venv', tenv') (body, inLoop)
            end
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
                 (checkInt (trexp (exp, NONE), pos);
                  {exp = (), ty = ty})
               | _ =>
                 (error pos "Indexing is only for array type";
                  {exp = (), ty = T.IMPOSSIBLE}))
        and trarithmetic (left, right, pos) =
            (checkInt (trexp (left, NONE), pos);
             checkInt (trexp (right, NONE), pos);
             {exp = (), ty = T.INT})
        and treq (left, right, pos) =
            let
                val {exp = _, ty = leftTy} = trexp (left, NONE)
                val trRight = trexp (right, NONE)
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
                val {exp = _, ty = leftTy} = trexp (left, NONE)
                val {exp = _, ty = rightTy} = trexp (right, NONE)
                val _ = case (leftTy, rightTy) of
                            (T.INT, T.INT) => ()
                          | (T.STRING, T.STRING) => ()
                          | _ => error pos "Uncomparable types"
            in
                {exp = (), ty = T.INT}
            end
    in
        trexp
    end
and transDec (venv, tenv, A.VarDec {name, escape, typ = NONE, init, pos}) =
    let
        val {exp, ty} = transExp (venv, tenv) (init, NONE)
    in
        case ty of
            T.NIL =>
            (error pos "initializing nil expressions to unspecified type variable";
             {venv = Symbol.enter (venv, name, E.VarEntry {ty = T.IMPOSSIBLE}),
              tenv = tenv})
          | t => 
            {venv = Symbol.enter (venv, name, E.VarEntry {ty = t}),
             tenv = tenv}
    end
  | transDec (venv, tenv, A.VarDec {name, escape, typ = SOME (sym, pos'), init, pos}) =
    let
        val expectedTy = Symbol.look (tenv, sym)
        val actualTy = #ty (transExp (venv, tenv) (init, NONE))
    in
        case expectedTy of
            SOME expectedTy' =>
            (* check if the types are compatible *)
            (checkCompatType (expectedTy', actualTy, pos);
             {venv = Symbol.enter (venv, name, E.VarEntry {ty = expectedTy'}),
              tenv = tenv})
          | NONE =>
            (error pos' ("Unknown type: " ^ (Symbol.name sym));
             {venv = Symbol.enter (venv, name, E.VarEntry {ty = T.IMPOSSIBLE}),
              tenv = tenv})
    end
  | transDec (venv, tenv, A.TypeDec tyList) =
    let
        (* TODO: make sure all new names don't exist before *)
        (* process each of them with transTy *)
        (* {name: symbol, ty: ty, pos: pos} *)
        fun processTy {name, ty, pos} =
            case ty of
                A.NameTy (sym, pos) => (name, NAMETEMP (sym, transTy (tenv, ty)))
              | A.ArrayTy (sym, pos) => (name, ARRAYTEMP (sym, transTy (tenv, ty)))
              | A.RecordTy fieldList => (name, RECORDTEMP (transTy (tenv, ty)))
        (* (symbol * temp) list *)
        val headers = map processTy tyList

        fun displayHeaders [] = ()
          | displayHeaders ((name, NAMETEMP (sym, ty))::headers) =
            (print ((Symbol.name name) ^ " is a nametemp with type name :" ^ (Symbol.name sym) ^ " with current type: " ^ (ty2str ty) ^ "\n");
             displayHeaders headers)
          | displayHeaders ((name, RECORDTEMP ty)::headers) =
            (print ((Symbol.name name) ^ " is a recordtemp\n");
             displayHeaders headers)
          | displayHeaders ((name, ARRAYTEMP (sym, ty))::headers) =
            (print ((Symbol.name name) ^ " is a arraytemp of type: " ^ (Symbol.name sym) ^ " with current type: " ^ (ty2str ty) ^ "\n");
             displayHeaders headers)

        (* store all record fields' types in a list *)
        fun getRecordInfo ([]) = []
          | getRecordInfo ({name, ty, pos}::tyList) =
            let
                fun helper {name, escape, typ, pos} = (typ, pos)
            in
                case ty of
                    A.RecordTy fieldList => (name, map helper fieldList)::(getRecordInfo tyList)
                  | _ => getRecordInfo tyList
            end
        (* (symbol, (symbol, pos) list) list *)
        val recordInfo = getRecordInfo tyList

        fun fetchTypeInfo (name, []) = NONE
          | fetchTypeInfo (name, (name', typeInfo)::recordInfo) =
            if Symbol.name name = Symbol.name name'
            then SOME typeInfo else fetchTypeInfo (name, recordInfo)
                                       
        (* mutually recursive function *)
        (* lookup headers for type definition *)
        fun tempTenv name =
            let
                (* TODO: check if there's type def loop *)
                fun findTy (name, []) = NONE
                  | findTy (name, (name', t)::headers) =
                    if Symbol.name name = Symbol.name name'
                    then temp2Ty t
                    else findTy (name, headers)
                and temp2Ty (ARRAYTEMP (name, ty)) =
                    (case ty of
                         T.ARRAY (T.IMPOSSIBLE, uniq) =>
                         (case findTy (name, headers) of
                              SOME t => SOME (T.ARRAY (t, uniq))
                            | NONE => NONE)
                       | t => SOME t)
                  | temp2Ty (RECORDTEMP ty) = SOME ty
                  | temp2Ty (NAMETEMP (name, ty)) =
                    case ty of
                        T.IMPOSSIBLE => findTy (name, headers)
                      | t => SOME t 
            in
                case findTy (name, headers) of
                    SOME (T.RECORD (gen, uniq)) => SOME (T.RECORD (gen' (name, gen), uniq))
                  | SOME ty => SOME ty
                  | NONE => NONE
            end
        and gen' (name, gen) =
            let
                fun finalGen () =
                    let
                        val fields = gen ()
                        fun processFields ([], []) = []
                          | processFields ([], _) = []
                          | processFields (_, []) = []
                          | processFields ((name, ty)::fields, (typ, pos)::typeInfo) =
                            let
                                (* first search temp Tenv *)
                                val actualTy = case tempTenv typ of
                                                   SOME t => t
                                                 | NONE =>
                                                   (* then use the result from gen *)
                                                   case ty of
                                                       T.IMPOSSIBLE => (error pos ("Unknown type: " ^ (Symbol.name typ));
                                                                        T.IMPOSSIBLE)
                                                     | t => t
                            in
                                (name, actualTy)::(processFields (fields, typeInfo))
                            end
                    in
                        (* it's guaranteed that we'll never get NONE *)
                        processFields (fields, Option.valOf (fetchTypeInfo (name, recordInfo)))
                    end
            in
                finalGen
            end

        (* go through the headers, add all new types in tenv *)
        (* fun update ((name, _), tenv) = Symbol.enter (tenv, name, Option.valOf (tempTenv name)) *)
        fun update ((name, temp), tenv) =
            (case temp of
                 RECORDTEMP (T.RECORD _) =>
                 let
                     val actualTy = Option.valOf (tempTenv name)
                 in
                     case actualTy of
                         T.RECORD (gen, _) => gen ()
                       | _ => [] 
                 end
              | _ => [];
             Symbol.enter (tenv, name, Option.valOf (tempTenv name)))
        val tenv' = foldl update tenv headers
    in
        {venv = venv, tenv = tenv'}
    end
  | transDec (venv, tenv, A.FunctionDec funList) =
    let
        fun transFun {name, params, result, body, pos} =
            let
                fun transParam {name, escape, typ, pos} =
                    case Symbol.look (tenv, typ)
                     of SOME t => {name = name, ty = t}
                      | NONE => (error pos ("Unknown type: " ^ (Symbol.name typ));
                                 {name = name, ty = T.IMPOSSIBLE})
                (* {name, ty} *)
                val params' = map transParam params
                val returnTy = case result of
                                   SOME (rt, pos) => 
                                   (case Symbol.look (tenv, rt) of
                                        SOME t => t
                                      | NONE => (error pos ("Unknown return type: " ^ (Symbol.name rt)); T.IMPOSSIBLE))
                                 | NONE => T.UNIT
            in
                {name = name, params = params', returnTy = returnTy, body = body, pos = pos}
            end
        val funcs = map transFun funList
        (* add all function definitions into venv *)
        fun addHeaders ({name, params, returnTy, body, pos}, venv) =
            let
                fun fetchTy ({name, ty}) = ty
            in
                Symbol.enter (venv, name, E.FunEntry {formals = map fetchTy params,
                                                      result = returnTy})
            end       
        val venv' = foldl addHeaders venv funcs
        (* add each function's formal vars into venv' *)
        fun addFormals ({name, params, returnTy, body, pos}) =
            let
                fun enterparam ({name, ty}, venv) =
                    Symbol.enter (venv, name, E.VarEntry {ty = ty})
            in
                foldl enterparam venv' params
            end
        val venvs'' = map addFormals funcs
        fun processBody ({name, params, returnTy, body, pos}::funcs, venv''::venvs'') =
            let
                val {exp = _, ty = actualTy} = transExp (venv'', tenv) (body, NONE)
            in
                (checkCompatType (returnTy, actualTy, pos);
                 processBody (funcs, venvs''))
            end
          | processBody (_, _) = ()
    in
        (processBody (funcs, venvs'');
         {venv = venv', tenv = tenv})
    end
and transTy (tenv, A.NameTy (sym, pos)) =
    (case Symbol.look (tenv, sym) of
         SOME ty => ty
       | NONE => T.IMPOSSIBLE)
  | transTy (tenv, A.ArrayTy (sym, pos)) =
    (case Symbol.look (tenv, sym) of
         SOME ty => T.ARRAY (ty, ref ())
       | NONE => T.ARRAY (T.IMPOSSIBLE, ref ()))
  | transTy (tenv, A.RecordTy fields) =
    let
        fun fieldGen fields =
            let
                fun gen () = map (fn {name, escape, typ, pos} =>
                                     case Symbol.look (tenv, typ) of
                                         SOME ty => (name, ty) 
                                       | NONE => (name, T.IMPOSSIBLE)) fields
            in
                gen
            end
    in
       T.RECORD (fieldGen fields, ref ())
    end
and transDecs (venv, tenv, []) = {venv = venv, tenv = tenv}
  | transDecs (venv, tenv, dec::decs) =
    let
        val {venv = venv', tenv = tenv'} = transDec (venv, tenv, dec)
    in
        transDecs (venv', tenv', decs)
    end     
        
(* TODO: change the return stuff in the next phase *)
fun transProg prog = (transExp (E.base_venv, E.base_tenv) (prog, NONE); ())
                         
end
