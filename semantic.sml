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
                                                   
fun transExp (venv,tenv) =
    let
        fun trexp (A.NilExp) = {exp = (), ty = T.NIL}
          | trexp (A.IntExp i) = {exp = (), ty = T.INT}
          | trexp (A.StringExp (s, pos)) = {exp = (), ty = T.STRING}
          (* size should be an int; initial value can't be nil *)
          | trexp (A.ArrayExp {typ, size, init, pos}) =
            (checkInt (trexp size, pos);
             if init = A.NilExp then error pos "nil can't be the initial value of array" else ();
             {exp = (), ty = T.ARRAY (#ty (trexp init), ref ())})
          (* support recusive definition of record *)
          (* | trexp (A.RecordExp {fields, typ, pos}) = *)
          (*   let *)
          (*       fun gen () =  *)
          (*   in *)
          (*       {exp = (), ty = T.RECORD (gen, ref ())} *)
          (*   end *)
          | trexp (A.OpExp {left, oper = A.PlusOp, right, pos}) =
            (checkInt (trexp left, pos);
             checkInt (trexp right, pos);
             {exp = (), ty = T.INT})
          | trexp (A.OpExp {left, oper = A.MinusOp, right, pos}) =
            (checkInt (trexp left, pos);
             checkInt (trexp right, pos);
             {exp = (), ty = T.INT})
          | trexp (A.OpExp {left, oper = A.TimesOp, right, pos}) =
            (checkInt (trexp left, pos);
             checkInt (trexp right, pos);
             {exp = (), ty = T.INT})
          | trexp (A.OpExp {left, oper = A.DivideOp, right, pos}) =
            (checkInt (trexp left, pos);
             checkInt (trexp right, pos);
             {exp = (), ty = T.INT})
          | trexp (A.OpExp {left, oper = A.EqOp, right, pos}) =
            let
                val {exp = _, ty = leftTy} = trexp left
                val _ = case leftTy of
                            T.INT => checkInt (trexp right, pos)
                          | T.STRING => checkString (trexp right, pos)
                          | T.ARRAY (_, uniq) => checkArray (uniq, trexp right, pos)
                          | T.RECORD (_, uniq) => checkRecord (uniq, trexp right, pos)
                          | T.NIL => checkRecordReverse (trexp right, pos)
                          | _ => error pos "Uncomparable type"
            in
                {exp = (), ty = T.INT}
            end
          (* | trexp (A.OpExp {left, oper = A.PlusOp, right, pos}) = *)
          (*   (checkInt(trexp left, pos); *)
          (*    checkInt(trexp right, pos); *)
          (*    {exp = (), ty = T.INT}) *)
          (* | trexp (A.OpExp {left, oper = A.PlusOp, right, pos}) = *)
          (*   (checkInt(trexp left, pos); *)
          (*    checkInt(trexp right, pos); *)
          (*    {exp = (), ty = T.INT}) *)
                
    in
        trexp
    end

(* TODO: change the return stuff in the next phase *)
fun transProg prog = (transExp (E.base_venv, E.base_tenv) prog; ())
       
end
