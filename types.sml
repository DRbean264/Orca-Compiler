structure Types =
struct

type unique = unit ref

(* RECORD of (Symbol.symbol * ty) list * unique *)
(* | NAME of Symbol.symbol * ty option ref *)
                   
datatype ty = RECORD of (unit -> (Symbol.symbol * ty) list) * unique
            | UNIT
            | NIL
            | INT
            | STRING
            | ARRAY of ty * unique
            | IMPOSSIBLE
            | ROOT

(* Subtype polymorphism:
   find the lowest common ancestor type of two types *)
fun join (IMPOSSIBLE, t) = t
  | join (t, IMPOSSIBLE) = t
  | join (ROOT, t) = ROOT
  | join (t, ROOT) = ROOT
  | join (NIL, RECORD r) = RECORD r
  | join (RECORD r, NIL) = RECORD r
  | join (NIL, NIL) = NIL
  | join (INT, INT) = INT
  | join (UNIT, UNIT) = UNIT
  | join (STRING, STRING) = STRING
  | join (ARRAY (t1, uniq1), ARRAY (t2, uniq2)) = if uniq1 = uniq2 then ARRAY (t1, uniq1) else ROOT
  | join (RECORD (f1, uniq1), RECORD (f2, uniq2)) = if uniq1 = uniq2 then RECORD (f1, uniq1) else ROOT
  | join _ = ROOT
end
