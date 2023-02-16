structure Absyn = 
struct

        type pos = int   and   symbol = Symbol.symbol

        datatype var = SimpleVar of symbol * pos
        | FieldVar of var * symbol * pos
        | SubscriptVar of var * exp * pos

        and exp = VarExp of var
        | NilExp
        | IntExp of int
        | StringExp of string * pos
        | CallExp of {func: symbol, args: exp list, pos: pos}
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
        | RecordExp of {fields: (symbol * exp * pos) list,
                typ: symbol, pos: pos}
        | SeqExp of (exp * pos) list
        | AssignExp of {var: var, exp: exp, pos: pos}
        | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
        | WhileExp of {test: exp, body: exp, pos: pos}
        | ForExp of {var: symbol, escape: bool ref,
                lo: exp, hi: exp, body: exp, pos: pos}
        | BreakExp of pos
        | LetExp of {decs: dec list, body: exp, pos: pos}
        | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}

        and dec = FunctionDec of fundec list
        | VarDec of {name: symbol,
                escape: bool ref,
                typ: (symbol * pos) option,
                init: exp,
                pos: pos}
        | TypeDec of tydec list

        and ty = NameTy of symbol * pos
        | RecordTy of field list
        | ArrayTy of symbol * pos

        and oper = PlusOp | MinusOp | TimesOp | DivideOp
        | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

        withtype field = {name: symbol, escape: bool ref, 
                        typ: symbol, pos: pos}
        and   fundec = {name: symbol,
                        params: field list,
                        result: (symbol * pos) option,
                        body: exp,
                        pos: pos}
        and   tydec = {name: symbol, ty: ty, pos: pos}
     
  fun equalsSymbolOption (SOME(a: symbol, _: pos), SOME(b: symbol, _: pos)) = a = b
    | equalsSymbolOption (NONE, NONE) = true
    | equalsSymbolOption (a, b) = false

  fun equalsFields (a::a_list, b::b_list) = 
    let fun equalsField (a:field, b:field) = 
        #name a = #name b andalso
        !(#escape a) = !(#escape b) andalso
        #typ a = #typ b
    in
      equalsField (a,b) andalso equalsFields(a_list,b_list)
    end
    | equalsFields ([], []) = true
    | equalsFields (a, []) = false
    | equalsFields ([], b) = false

  fun equalsVar (SimpleVar (a_symbol, _), SimpleVar (b_symbol, _)) = a_symbol = b_symbol
    | equalsVar (FieldVar  (a_var, a_symbol, _), FieldVar (b_var, b_symbol, _)) = 
        equalsVar (a_var, b_var) andalso a_symbol=b_symbol
    | equalsVar (SubscriptVar (a_var, a_exp, _), SubscriptVar (b_var, b_exp, _)) = 
        equalsVar (a_var, b_var) andalso equalsExp (a_exp, b_exp) 
    | equalsVar (a, b) = false

  and equalsExp (VarExp a, VarExp b) = equalsVar(a,b)
    | equalsExp (NilExp, NilExp) = true
    | equalsExp (IntExp a, IntExp b) = a = b
    | equalsExp (StringExp (a, _), StringExp (b, _)) = a = b
    | equalsExp (
        CallExp {func=a_func, args=a_args, pos=_}, 
        CallExp {func=b_func, args=b_args, pos=_}) = 
        a_func = b_func andalso equalsExpList(a_args,b_args) 
    | equalsExp (
        OpExp {left=a_left, oper=a_oper, right=a_right, pos=_}, 
        OpExp {left=b_left, oper=b_oper, right=b_right, pos=_}) =  
        equalsExp (a_left, b_left) andalso
        equalsOper (a_oper, b_oper) andalso
        equalsExp (a_right, b_right)
    | equalsExp (
        RecordExp {fields=a_fields, typ=a_typ, pos=_},
        RecordExp {fields=b_fields, typ=b_typ, pos=_}) =
          let fun equalsRecordFields
            ((a_symbol, a_exp:exp, _)::a_list,
            (b_symbol, b_exp:exp, _)::b_list) = 
                a_symbol = b_symbol andalso
                equalsExp(a_exp, b_exp) andalso
                equalsRecordFields(a_list, b_list)
            | equalsRecordFields ([],[]) = true
            | equalsRecordFields (a,[]) = false
            | equalsRecordFields ([],b) = false
          in
            equalsRecordFields(a_fields, b_fields) andalso a_typ = b_typ
          end
    | equalsExp (
        SeqExp ((a_exp:exp, _)::a_list),
        SeqExp ((b_exp:exp, _)::b_list)) =
        equalsExp(a_exp, b_exp) andalso
        equalsExp(SeqExp(a_list), SeqExp(b_list))
    | equalsExp (
        SeqExp ([]), SeqExp ([])) = true
    | equalsExp (
        AssignExp {var = a_var, exp = a_exp, pos = _},
        AssignExp {var = b_var, exp = b_exp, pos = _}) =
        equalsVar(a_var, b_var) andalso
        equalsExp(a_exp, b_exp)
    | equalsExp (
        IfExp {test = a_test, then' = a_then, else' = a_else, pos = _},
        IfExp {test = b_test, then' = b_then, else' = b_else, pos = _}) =
        let fun equalsExpOption (SOME(a: exp), SOME(b: exp)) = equalsExp(a,b)
          | equalsExpOption (NONE, NONE) = true
          | equalsExpOption (a, b) = false
        in
          equalsExp(a_test, b_test) andalso
          equalsExp(a_then, b_then) andalso
          equalsExpOption (a_else, b_else)
        end
    | equalsExp (
        WhileExp {test = a_test, body = a_body, pos = _},
        WhileExp {test = b_test, body = b_body, pos = _}) =
        equalsExp(a_test, b_test) andalso
        equalsExp(a_body, b_body)
    | equalsExp (
        ForExp {var = a_var, escape = a_escape, lo = a_lo,
            hi = a_hi, body = a_body, pos = _},
        ForExp {var = b_var, escape = b_escape, lo = b_lo,
            hi = b_hi, body = b_body, pos = _}) =
        a_var = b_var andalso
        !a_escape = !b_escape andalso
        equalsExp(a_lo, b_lo) andalso
        equalsExp(a_hi, b_hi) andalso
        equalsExp(a_body, b_body)
    | equalsExp (BreakExp (_), BreakExp(_)) = true
    | equalsExp 
        ((LetExp {decs=a_decs,body=a_body,pos=_}),
         (LetExp {decs=b_decs,body=b_body, pos=_})) =
         let fun equalsDecs ((a:dec)::a_list, (b:dec)::b_list) = 
            equalsDec(a, b) andalso equalsDecs(a_list,b_list)
               | equalsDecs ([], []) = true
               | equalsDecs (a, []) = false
               | equalsDecs ([], b) = false
         in
           equalsDecs(a_decs, b_decs) andalso
           equalsExp(a_body, b_body)
         end
    | equalsExp (
        ArrayExp {typ=a_typ, size=a_size, init = a_init, pos=_},
        ArrayExp {typ=b_typ, size=b_size, init = b_init, pos=_}) =
        a_typ = b_typ andalso
        equalsExp(a_size, b_size) andalso
        equalsExp(a_init, b_init)
    | equalsExp (a, b) = false

  and equalsExpList ((a:exp)::a_list, (b:exp)::b_list) =
          equalsExp(a,b) andalso equalsExpList(a_list, b_list)
    | equalsExpList ([],[]) = true
    | equalsExpList (a,[]) = false
    | equalsExpList ([],b) = false
  
  and equalsDec (
          VarDec{name=a_name,escape=a_escape,typ=a_typ,init=a_init,pos=_}, 
          VarDec{name=b_name,escape=b_escape,typ=b_typ,init=b_init,pos=_}) =
        a_name=b_name andalso 
        !a_escape = !b_escape andalso
        equalsSymbolOption(a_typ, b_typ) andalso equalsExp(a_init, b_init)
    | equalsDec (FunctionDec(a_list), FunctionDec(b_list)) =
        let fun equalsFunDecs ((a:fundec)::a_list, (b:fundec)::b_list) =
          #name a = #name b andalso
          equalsFields(#params a, #params b) andalso
          equalsSymbolOption(#result a, #result b) andalso
          equalsExp (#body a, #body b) andalso
          equalsFunDecs (a_list, b_list)
              | equalsFunDecs([], []) = true
              | equalsFunDecs(a, []) = false
              | equalsFunDecs([], b) = false
        in
          equalsFunDecs(a_list,b_list)
        end
    | equalsDec (TypeDec (a_list), TypeDec (b_list)) =
        let fun equalsTypeDecs ((a:tydec)::a_list, (b:tydec)::b_list) =
            (#name a) = (#name b) andalso
            equalsTy ((#ty a), (#ty b)) andalso
            equalsTypeDecs (a_list, b_list)
          | equalsTypeDecs ([], []) = true
          | equalsTypeDecs (a, []) = false
          | equalsTypeDecs ([], b) = false
        in
          equalsTypeDecs (a_list,b_list)
        end

  and equalsTy (NameTy(a_symbol,_), NameTy(b_symbol,_)) = a_symbol=b_symbol 
    | equalsTy (RecordTy (a_list), RecordTy(b_list)) = equalsFields(a_list, b_list)
    | equalsTy (ArrayTy (a_symbol, _), ArrayTy(b_symbol, _)) = a_symbol=b_symbol 
    | equalsTy (a,b) = false

  and equalsOper (a, b)= a=b 
end
        
