(* Notes:
1. Type compatibility: one type should be the child of the second type *)
signature SEMANT =
sig
    val transProg : Absyn.exp -> Translate.exp
end

structure Semant : SEMANT =
struct

(* abbreviation *)
structure A = Absyn
structure E = Env
structure T = Types
structure Trans = Translate
(* string set *)
structure StringBinarySet = BinarySetFn (struct type ord_key = string
                                                val compare = String.compare
                                         end)
val error = ErrorMsg.error

datatype temp = NAMETEMP of (A.symbol * T.ty * A.pos)
              | ARRAYTEMP of (A.symbol * T.ty * A.pos)
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
        
fun transExp (venv, tenv, level) =
    let
        (* TODO: check if nil is correct *)
        fun trexp (A.NilExp, inLoop) = {exp = Trans.nilExp (), ty = T.NIL}
          | trexp (A.IntExp i, inLoop) = {exp = Trans.intExp i, ty = T.INT}
          | trexp (A.StringExp (s, pos), inLoop) = {exp = Trans.stringExp s, ty = T.STRING}
          | trexp (A.ArrayExp {typ, size, init, pos}, inLoop) =
            let
                val {exp = e1, ty = ty1} = trexp (size, NONE)
                val {exp = e2, ty = ty2} = trexp (init, NONE)
                (* size should be an int *)
                val actualTy = Symbol.look (tenv, typ)
            in
                (checkInt ({exp = e1, ty = ty1}, pos);
                 (* the type should be in the type environment & should be an array type *)
                 case actualTy of
                     SOME (T.ARRAY (ty', uniq')) =>
                     (* check type compatibility *)
                     (checkCompatType (ty', ty2, pos);
                      {exp = Trans.arrayExp (e1, e2), ty = T.ARRAY (ty', uniq')})
                   | SOME _ =>
                     (error pos ("The type: " ^ (Symbol.name typ) ^ " is not an array type");
                      {exp = Trans.dummyExp, ty = T.IMPOSSIBLE})
                   | NONE =>
                     (error pos ("Unknown type: " ^ (Symbol.name typ));
                      {exp = Trans.dummyExp, ty = T.IMPOSSIBLE}))
            end              
          (* check all fields names and types are in the same order as declaration *)
          | trexp (A.RecordExp {fields, typ, pos}, inLoop) =
            let
                (* check field names *)
                val actualTy = Symbol.look (tenv, typ)
                (* fields: (symbol * exp * pos) list -> (symbol * ty * pos) list *)
                fun procFields ([], size) = {fields = [], size = size, exps = []}
                  | procFields ((sym, exp, pos)::fields, size) =
                    let
                        val {exp = e, ty = t} = trexp (exp, NONE)
                        val {fields = fs, size = s, exps = es} = procFields (fields, size + 1)
                    in
                        {fields = ((sym, t, pos)::fs), size = s, exps = (e::es)}
                    end

                val {fields = fields', size, exps} = procFields (fields, 0)
            in
                case actualTy of
                    SOME (T.RECORD (fieldsGen, uniq)) =>
                    let
                        (* fetch the original fields *)
                        val fields'' = fieldsGen ()
                        val _ = checkRecordFields (fields'', fields', pos)
                    in
                        {exp = Trans.recordExp (size, exps), ty = T.RECORD (fieldsGen, uniq)}
                    end
                  | SOME _ =>
                    (error pos ("The type: " ^ (Symbol.name typ) ^ " is not a record type");
                     {exp = Trans.dummyExp, ty = T.IMPOSSIBLE})
                  | NONE =>
                    (error pos ("Unknown type: " ^ (Symbol.name typ));
                     {exp = Trans.dummyExp, ty = T.IMPOSSIBLE})
            end
          | trexp (A.OpExp {left, oper = A.PlusOp, right, pos}, inLoop) =
            trarithmetic (A.PlusOp, left, right, pos)  
          | trexp (A.OpExp {left, oper = A.MinusOp, right, pos}, inLoop) =
            trarithmetic (A.MinusOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.TimesOp, right, pos}, inLoop) =
            trarithmetic (A.TimesOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.DivideOp, right, pos}, inLoop) =
            trarithmetic (A.DivideOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.EqOp, right, pos}, inLoop) =
            treq (A.EqOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.NeqOp, right, pos}, inLoop) =
            treq (A.NeqOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.LtOp, right, pos}, inLoop) =
            trcompare (A.LtOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.LeOp, right, pos}, inLoop) =
            trcompare (A.LeOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.GtOp, right, pos}, inLoop) =
            trcompare (A.GtOp, left, right, pos)
          | trexp (A.OpExp {left, oper = A.GeOp, right, pos}, inLoop) =
            trcompare (A.GeOp, left, right, pos)
          | trexp (A.CallExp {func, args, pos}, inLoop) =
            let
                fun procArgs [] = {args = [], exps = []}
                  | procArgs (arg::args) =
                    let
                        val {exp, ty} = trexp (arg, NONE)
                        val {args = args', exps = exps'} = procArgs args
                    in
                        {args = ty::args', exps = exp::exps'}
                    end
                (* args : exp list -> ty list *)
                val {args = args', exps} = procArgs args
            in
                case Symbol.look (venv, func) of
                    SOME (E.FunEntry {formals, result, level = level', label}) =>
                    let
                        (* check each argument type is compatible with the defined one *)
                        val _ = checkFunFields (formals, args', pos)
                    in
                         {exp = Trans.callExp (label, exps, level, level'), ty = result}
                    end
                  | SOME (E.VarEntry _) =>
                    (error pos ("The name: " ^ (Symbol.name func) ^ " is not a function but a variable");
                     {exp = Trans.dummyExp, ty = T.IMPOSSIBLE})
                  | NONE =>
                    (error pos ("Unknown function: " ^ (Symbol.name func));
                     {exp = Trans.dummyExp, ty = T.IMPOSSIBLE})
            end
          | trexp (A.VarExp var, inLoop) = trvar var
          | trexp (A.SeqExp expList, inLoop) =
            let
                fun trSeqExp [] = {exps = [Trans.dummyExp], ty = T.UNIT}
                  | trSeqExp [(exp, pos)] =
                    let
                        val {exp = e, ty} = trexp (exp, inLoop)
                    in
                        {exps = [e], ty = ty}
                    end
                  | trSeqExp ((exp, pos)::expList) =
                    let
                        val {exp = e, ty = _} = trexp (exp, inLoop)
                        val {exps = es, ty = t} = trSeqExp expList
                    in
                        {exps = e::es, ty = t}
                    end

                val {exps, ty} = trSeqExp expList
            in
                {exp = Trans.seqExp exps, ty = ty}
            end
          | trexp (A.AssignExp {var, exp, pos}, inLoop) =
            let
                val {exp = e1, ty = ty1} = trvar var
                val {exp = e2, ty = ty2} = trexp (exp, NONE)
            in
                (checkCompatType (ty1, ty2, pos);
                 {exp = Trans.assignExp (e1, e2), ty = T.UNIT})
            end
          | trexp (A.BreakExp pos, inLoop) =
            (case inLoop of
                 SOME label => {exp = Trans.breakExp label, ty = T.IMPOSSIBLE}
               | NONE => (error pos "BREAK should be only in for/while loop";
                          {exp = Trans.dummyExp, ty = T.IMPOSSIBLE}))
          | trexp (A.WhileExp {test, body, pos}, inLoop) =
            let
                val break = Temp.newlabel ()
                val {exp = e1, ty = ty1} = trexp (test, NONE)
                val {exp = e2, ty = ty2} = trexp (body, SOME break)
            in
            ( (* check if type of test is int *)
              checkInt ({exp = e1, ty = ty1}, pos);
              (* check if type of body is compatible with unit *)
              checkCompatType (T.UNIT, ty2, pos);
              {exp = Trans.whileExp (e1, e2, break), ty = T.UNIT})
            end
          | trexp (A.ForExp {var, escape, lo, hi, body, pos}, inLoop) =
            let
                val acc = Translate.allocLocal level (!escape)
                val venv' = Symbol.enter (venv, var, E.VarEntry {ty = T.INT,
                                                                 access = acc})
                val break = Temp.newlabel ()
                val {exp = e1, ty = ty1} = trexp (lo, NONE)
                val {exp = e2, ty = ty2} = trexp (hi, NONE)
                val {exp = e3, ty = ty3} = transExp (venv', tenv, level) (body, SOME break)
            in
                ( (* check if types of lo & hi are int *)
                  checkInt ({exp = e1, ty = ty1}, pos);
                  checkInt ({exp = e2, ty = ty2}, pos);
                  (* check if type of body is compatible with unit *)
                  checkCompatType (T.UNIT, ty3, pos);
                  {exp = Trans.forExp (acc, e1, e2, e3, break), ty = T.UNIT})
            end
          | trexp (A.IfExp {test, then', else' = NONE, pos}, inLoop) =
            let
                val {exp = e1, ty = ty1} = trexp (test, NONE)
                val {exp = e2, ty = ty2} = trexp (then', inLoop)
            in
                ( (* check if type of test is int *)
                  checkInt ({exp = e1, ty = ty1}, pos);
                  (* check if type of body is compatible with unit *)
                  checkCompatType (T.UNIT, ty2, pos);
                  {exp = Trans.ifExp (e1, e2, Trans.dummyExp), ty = T.UNIT})
            end
          | trexp (A.IfExp {test, then', else' = SOME elseExp, pos}, inLoop) =
            let
                val {exp = e1, ty = ty1} = trexp (test, NONE)
                val {exp = e2, ty = ty2} = trexp (then', inLoop)
                val {exp = e3, ty = ty3} = trexp (elseExp, inLoop)
            in
                ( (* check if type of test is int *)
                  checkInt ({exp = e1, ty = ty1}, pos);
                  (* one type has to be another's parent type *)
                  if checkParentType (ty2, ty3, pos)
                  then {exp = Trans.ifExp (e1, e2, e3), ty = T.join (ty2, ty3)}
                  else
                      (error pos ("Incompatible types: " ^ (ty2str ty2) ^ " and " ^ (ty2str ty3) ^ "\n"); 
                       {exp = Trans.dummyExp, ty = T.IMPOSSIBLE}))
            end
          | trexp (A.LetExp {decs, body, pos}, inLoop) =
            let
                val {venv = venv', tenv = tenv', exps} = transDecs (venv, tenv, decs, level)
                val {exp, ty} = transExp (venv', tenv', level) (body, inLoop)
            in
                {exp = Trans.letExp (exps, exp), ty = ty}
            end
        and trvar (A.SimpleVar (sym, pos)) =
            (case Symbol.look (venv, sym) of
                 SOME (E.VarEntry {ty, access}) => {exp = Trans.simpleVar (access, level), ty = ty}
               | SOME (E.FunEntry _) =>
                 (error pos ("The name: " ^ Symbol.name sym ^ " is a function");
                  {exp = Trans.dummyExp, ty = T.IMPOSSIBLE})
               | NONE =>
                 (error pos ("Unknown variable: " ^ Symbol.name sym);
                  {exp = Trans.dummyExp, ty = T.IMPOSSIBLE}))
          | trvar (A.FieldVar (var, sym, pos)) =
            let
                val {exp, ty} = trvar var
            in
                (* the type of var should be a record *)
                (case ty of
                     T.RECORD (gen, _) =>
                     let
                         (* find the type and index of the field *)
                         fun findTypeId ([], sym, id, pos) =
                             (error pos ("Invalid field: " ^ (Symbol.name sym) ^ " in the record");
                              (T.IMPOSSIBLE, id))
                           | findTypeId ((field, ty)::fields, sym, id, pos) =
                             if (Symbol.name field) = (Symbol.name sym)
                             then (ty, id) else findTypeId (fields, sym, id + 1, pos)
                         val fields = gen ()
                         val (typ, id) = findTypeId (fields, sym, 0, pos)
                     in
                         {exp = Trans.fieldVar (exp, id), ty = typ}
                     end
                   | _ =>
                     (error pos "Field selection is only for record type";
                      {exp = Trans.dummyExp, ty = T.IMPOSSIBLE}))
            end
          | trvar (A.SubscriptVar (var, exp, pos)) =
            let
                val {exp = e1, ty = typ1} = trvar var
                val {exp = e2, ty = typ2} = trexp (exp, NONE)
            in
            (* the type of var should be an array *)
            (case typ1 of
                 T.ARRAY (ty, _) =>
                 (checkInt ({exp = e2, ty = typ2}, pos);
                  {exp = Trans.subscriptVar (e1, e2), ty = ty})
               | _ =>
                 (error pos "Indexing is only for array type";
                  {exp = Trans.dummyExp, ty = T.IMPOSSIBLE}))
            end
        and trarithmetic (oper, left, right, pos) =
            let
                val {exp = e1, ty = t1} = trexp (left, NONE)
                val {exp = e2, ty = t2} = trexp (right, NONE)
            in
                (checkInt ({exp = e1, ty = t1}, pos);
                 checkInt ({exp = e2, ty = t2}, pos);
                 {exp = Trans.opExp (oper, e1, e2), ty = T.INT})
            end
        and treq (oper, left, right, pos) =
            let
                val {exp = e, ty = leftTy} = trexp (left, NONE)
                val trRight = trexp (right, NONE)
                val _ = case leftTy of
                            T.INT => checkInt (trRight, pos)
                          | T.STRING => checkString (trRight, pos)
                          | T.ARRAY (_, uniq) => checkArray (uniq, trRight, pos)
                          | T.RECORD (_, uniq) => checkRecord (uniq, trRight, pos)
                          | T.NIL => checkRecordReverse (trRight, pos)
                          | _ => error pos "Uncomparable types for equality"
            in
                case leftTy of
                    T.STRING => 
                    {exp = Trans.strCompExp (oper, e, #exp trRight), ty = T.INT}
                  | _ =>
                    {exp = Trans.opExp (oper, e, #exp trRight), ty = T.INT}
            end
        and trcompare (oper, left, right, pos) =
            let
                val {exp = e1, ty = leftTy} = trexp (left, NONE)
                val {exp = e2, ty = rightTy} = trexp (right, NONE)
            in
                case (leftTy, rightTy) of
                    (T.INT, T.INT) =>
                    {exp = Trans.opExp (oper, e1, e2), ty = T.INT}
                  | (T.STRING, T.STRING) =>
                    {exp = Trans.strCompExp (oper, e1, e2), ty = T.INT}
                  | _ => (error pos "Uncomparable types";
                          {exp = Trans.opExp (oper, e1, e2), ty = T.INT})
                               
            end
    in
        trexp
    end
and transDec (venv, tenv, A.VarDec {name, escape, typ = NONE, init, pos}, level) =
    let
        val acc = Translate.allocLocal level (!escape)

        (* for debugging only *)
        val _ = print ("New local variable: " ^ (Symbol.name name) ^ "\n" ^
                       "Level ID: " ^ (Int.toString level) ^ "\n" ^
                       "Access info: " ^ (Trans.printAccInfo acc) ^ "\n\n")
        (* for debugging only *)
                                       
        val {exp, ty} = transExp (venv, tenv, level) (init, NONE)
    in
        case ty of
            T.NIL =>
            (error pos "initializing nil expressions to unspecified type variable";
             {venv = Symbol.enter (venv, name, E.VarEntry {ty = T.IMPOSSIBLE,
                                                           access = acc}),
              tenv = tenv,
              exp = SOME (Trans.varDecExp (acc, exp))})
          | t => 
            {venv = Symbol.enter (venv, name, E.VarEntry {ty = t, access = acc}),
             tenv = tenv,
             exp = SOME (Trans.varDecExp (acc, exp))}
    end
  | transDec (venv, tenv, A.VarDec {name, escape, typ = SOME (sym, pos'), init, pos}, level) =
    let
        val acc = Translate.allocLocal level (!escape)
        val expectedTy = Symbol.look (tenv, sym)
        val {exp, ty = actualTy} = transExp (venv, tenv, level) (init, NONE)
    in
        case expectedTy of
            SOME expectedTy' =>
            (* check if the types are compatible *)
            (checkCompatType (expectedTy', actualTy, pos);
             {venv = Symbol.enter (venv, name, E.VarEntry {ty = expectedTy',
                                                           access = acc}),
              tenv = tenv,
              exp = SOME (Trans.varDecExp (acc, exp))})
          | NONE =>
            (error pos' ("Unknown type: " ^ (Symbol.name sym));
             {venv = Symbol.enter (venv, name, E.VarEntry {ty = T.IMPOSSIBLE,
                                                           access = acc}),
              tenv = tenv,
              exp = SOME (Trans.varDecExp (acc, exp))})
    end
  | transDec (venv, tenv, A.TypeDec tyList, level) =
    let
        (* process each of them with transTy *)
        (* {name: symbol, ty: ty, pos: pos} *)
        fun processTy {name, ty, pos} =
            case ty of
                A.NameTy (sym, pos) => (name, NAMETEMP (sym, transTy (tenv, ty), pos))
              | A.ArrayTy (sym, pos) => (name, ARRAYTEMP (sym, transTy (tenv, ty), pos))
              | A.RecordTy fieldList => (name, RECORDTEMP (transTy (tenv, ty)))
        (* (symbol * temp) list *)
        val headers = map processTy tyList

        (* Check for multiple types with the same name *)
        fun hasDup ({name, ty, pos}, (set, dup)) =
            if AtomSet.member (set, (Atom.atom (Symbol.name name)))
                         then (error pos ("Multiple declarations of type " ^ (Symbol.name name)); (set, true))
                         else (AtomSet.add (set, (Atom.atom (Symbol.name name))), dup)

        val (_, dups) =  foldl hasDup (AtomSet.empty, false) tyList

        fun displayHeaders [] = ()
          | displayHeaders ((name, NAMETEMP (sym, ty, pos))::headers) =
            (print ((Symbol.name name) ^ " is a nametemp with type name :" ^ (Symbol.name sym) ^ " with current type: " ^ (ty2str ty) ^ "\n");
             displayHeaders headers)
          | displayHeaders ((name, RECORDTEMP ty)::headers) =
            (print ((Symbol.name name) ^ " is a recordtemp\n");
             displayHeaders headers)
          | displayHeaders ((name, ARRAYTEMP (sym, ty, pos))::headers) =
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
                fun findTy (name, [], seen) = NONE
                  | findTy (name, (name', t)::headers, seen) =
                    if Symbol.name name = Symbol.name name'
                    then temp2Ty (t, seen)
                    else findTy (name, headers, seen)
                and temp2Ty (ARRAYTEMP (name, ty, pos), seen) =
                    if StringBinarySet.exists (fn i => (Symbol.name name) = i) seen
                    then (error pos ("Loop detected in type declaration of " ^ (Symbol.name name));
                          SOME T.IMPOSSIBLE)
                    else
                        let
                            val seen = StringBinarySet.add (seen, Symbol.name name)
                        in
                            case ty of
                                T.ARRAY (t, uniq) =>
                                (case findTy (name, headers, seen) of
                                     SOME t' => SOME (T.ARRAY (t', uniq))
                                   | NONE =>
                                     case t of
                                         T.IMPOSSIBLE => NONE
                                       | t' => SOME (T.ARRAY (t', uniq)))
                              | _ => NONE 
                        end
                  | temp2Ty (RECORDTEMP ty, seen) = SOME ty
                  | temp2Ty (NAMETEMP (name, ty, pos), seen) =
                        (* check if the name is in the seen list *)
                        if StringBinarySet.exists (fn i => (Symbol.name name) = i) seen
                        then (error pos ("Loop detected in type declaration of " ^ (Symbol.name name));
                              SOME T.IMPOSSIBLE)
                        else
                            let
                                (* add current nametemp into seen list *)
                                val seen = StringBinarySet.add (seen, Symbol.name name)
                            in
                                case findTy (name, headers, seen) of
                                    NONE =>
                                    (case ty of
                                         T.IMPOSSIBLE => NONE
                                       | t => SOME t)
                                  | SOME t => SOME t
                            end
            in
                case findTy (name, headers, StringBinarySet.empty) of
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
        {venv = venv, tenv = tenv', exp = NONE}
    end
  | transDec (venv, tenv, A.FunctionDec funList, level) =
    let
        (* Check for multiple types with the same name *)
        fun hasDup ({name, params, result, body, pos}, (set, dup)) =
            if AtomSet.member (set, (Atom.atom (Symbol.name name)))
                         then (error pos ("Multiple declarations of function " ^ (Symbol.name name)); (set, true))
                         else (AtomSet.add (set, (Atom.atom (Symbol.name name))), dup)

        val (_, dups) =  foldl hasDup (AtomSet.empty, false) funList

        fun transFun {name, params, result, body, pos} =
            let
                (* create a new level for each function definition *)
                fun fetchEsc ({name, escape, typ, pos}) = !escape
                val label = Temp.newlabel ()
                val formals = map fetchEsc params
                val newLevel = Translate.newLevel ({parent = level,
                                                    name = label,
                                                    formals = formals})

                (* for debugging only *)
                fun printBoolLst [] = ""
                  | printBoolLst (b::lst) =
                    case b of
                        true => "true " ^ (printBoolLst lst)
                      | false => "false " ^ (printBoolLst lst)
                                                  
                val _ = print ("Function definition: " ^ (Symbol.name name) ^ "\n" ^
                               "Level ID: " ^ (Int.toString newLevel) ^ "\n" ^
                               "Logical level: " ^ (Int.toString (Translate.getLogicalLevel newLevel)) ^ "\n" ^
                               "label: " ^ (Symbol.name label) ^ "\n" ^
                               "original formals: [ " ^ (printBoolLst formals) ^ " ]\n" ^
                               "formals access info: [ ")
                val _ = Translate.printFormalInfo newLevel
                val _ = print " ]\n\n"
                (* for debugging only *)
                                                  
                fun transParam {name, escape, typ, pos} =
                    case Symbol.look (tenv, typ)
                     of SOME t => {name = name, ty = t, escape = escape}
                      | NONE => (error pos ("Unknown type: " ^ (Symbol.name typ));
                                 {name = name, ty = T.IMPOSSIBLE, escape = escape})
                (* {name, ty, escape} *)
                val params' = map transParam params
                val returnTy = case result of
                                   SOME (rt, pos) => 
                                   (case Symbol.look (tenv, rt) of
                                        SOME t => t
                                      | NONE => (error pos ("Unknown return type: " ^ (Symbol.name rt)); T.IMPOSSIBLE))
                                 | NONE => T.UNIT
            in
                {name = name, params = params', returnTy = returnTy, body = body, pos = pos, level = newLevel, label = label}
            end
        val funcs = map transFun funList
        (* add all function definitions into venv *)
        fun addHeaders ({name, params, returnTy, body, pos, level, label}, venv) =
            let
                fun fetchTy ({name, ty, escape}) = ty
            in
                Symbol.enter (venv, name, E.FunEntry {formals = map fetchTy params,
                                                      result = returnTy,
                                                      level = level,
                                                      label = label})
            end       
        val venv' = foldl addHeaders venv funcs
        (* add each function's formal vars into venv' *)
        fun addFormals ({name, params, returnTy, body, pos, level, label}) =
            let
                val accessList = Translate.formals level
                fun enterParam ({name, ty, escape}::params, acc::accessList, venv) =
                    enterParam (params, accessList, Symbol.enter (venv, name, E.VarEntry {ty = ty, access = acc}))
                  | enterParam (_, _, venv) = venv
            in
                enterParam (params, accessList, venv')
            end
        val venvs'' = map addFormals funcs
        fun processBody ({name, params, returnTy, body, pos, level, label}::funcs, venv''::venvs'') =
            let
                val {exp = _, ty = actualTy} = transExp (venv'', tenv, level) (body, NONE)
            in
                (checkCompatType (returnTy, actualTy, pos);
                 processBody (funcs, venvs''))
            end
          | processBody (_, _) = ()
    in
        (processBody (funcs, venvs'');
         {venv = venv', tenv = tenv, exp = NONE})
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
and transDecs (venv, tenv, [], level) = {venv = venv, tenv = tenv, exps = []}
  | transDecs (venv, tenv, dec::decs, level) =
    let
        val {venv = venv', tenv = tenv', exp} = transDec (venv, tenv, dec, level)
        val {venv = venv'', tenv = tenv'', exps} = transDecs (venv', tenv', decs, level)
    in
        case exp of
            SOME e =>
            {venv = venv'', tenv = tenv'', exps = e::exps}
          | NONE => 
            {venv = venv'', tenv = tenv'', exps = exps}
    end     
        
(* TODO: change the return stuff in the next phase *)
fun transProg prog =
    let
        val _ = Translate.reset ()
        val {exp, ty} = transExp (E.base_venv, E.base_tenv, Translate.outermost) (prog, NONE)
    in
        exp
    end
                         
end
