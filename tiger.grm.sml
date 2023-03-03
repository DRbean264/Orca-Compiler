functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn

fun varChecking (A.VarExp var, pos) = var
  | varChecking (exp, pos) = 
  let
    val _ = ErrorMsg.error pos "The left value is of wrong format. \
    \Replacing it with dummy left value: bogus"
  in
    A.SimpleVar (Symbol.symbol "bogus", pos)
  end

fun expSeqChecking (exp1::exp2::exps, pos) = (exp1::exp2::exps)
  | expSeqChecking (exps, pos) = 
  let
    val _ = ErrorMsg.error pos "The length of expression sequence \
    \should be >= 2. Appending two dummy expressions 'nil'"
  in
    (A.NilExp, pos)::(A.NilExp, pos)::exps
  end

fun IdChecking (A.VarExp (A.SimpleVar (sym, sympos)), pos) = Symbol.name sym
  | IdChecking (exp, pos) = 
  let
    val _ = ErrorMsg.error pos "Invalid ID of declaration. \
    \Replacing it with dummy id: bogus"
  in
    "bogus"
  end

fun mergeDecs (A.FunctionDec fundec, (A.FunctionDec fundecs)::decs) = (A.FunctionDec ((hd fundec)::fundecs))::decs
  | mergeDecs (A.TypeDec tydec, (A.TypeDec tydecs)::decs) = (A.TypeDec ((hd tydec)::tydecs))::decs
  | mergeDecs (dec, decs) = dec::decs


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\153\000\005\000\153\000\007\000\153\000\008\000\031\000\
\\009\000\153\000\010\000\030\000\011\000\153\000\012\000\029\000\
\\013\000\153\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\025\000\153\000\026\000\153\000\
\\027\000\153\000\030\000\153\000\031\000\153\000\034\000\153\000\
\\035\000\153\000\037\000\153\000\038\000\153\000\042\000\153\000\
\\043\000\153\000\044\000\153\000\000\000\
\\001\000\001\000\154\000\005\000\154\000\007\000\154\000\008\000\031\000\
\\009\000\154\000\010\000\030\000\011\000\154\000\012\000\029\000\
\\013\000\154\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\025\000\154\000\026\000\154\000\
\\027\000\154\000\030\000\154\000\031\000\154\000\034\000\154\000\
\\035\000\154\000\037\000\154\000\038\000\154\000\042\000\154\000\
\\043\000\154\000\044\000\154\000\000\000\
\\001\000\001\000\155\000\005\000\155\000\007\000\155\000\008\000\031\000\
\\009\000\155\000\010\000\030\000\011\000\155\000\012\000\029\000\
\\013\000\155\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\025\000\155\000\026\000\155\000\
\\027\000\155\000\030\000\155\000\031\000\155\000\034\000\155\000\
\\035\000\155\000\037\000\155\000\038\000\155\000\042\000\155\000\
\\043\000\155\000\044\000\155\000\000\000\
\\001\000\001\000\156\000\005\000\156\000\007\000\156\000\008\000\031\000\
\\009\000\156\000\010\000\030\000\011\000\156\000\012\000\029\000\
\\013\000\156\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\025\000\156\000\026\000\156\000\
\\027\000\156\000\030\000\156\000\031\000\156\000\034\000\156\000\
\\035\000\156\000\037\000\156\000\038\000\156\000\042\000\156\000\
\\043\000\156\000\044\000\156\000\000\000\
\\001\000\001\000\157\000\005\000\157\000\007\000\157\000\008\000\031\000\
\\009\000\157\000\010\000\030\000\011\000\157\000\012\000\029\000\
\\013\000\157\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\025\000\157\000\026\000\157\000\
\\027\000\157\000\030\000\157\000\031\000\157\000\034\000\157\000\
\\035\000\157\000\037\000\157\000\038\000\157\000\042\000\157\000\
\\043\000\157\000\044\000\157\000\000\000\
\\001\000\001\000\158\000\005\000\158\000\007\000\158\000\008\000\031\000\
\\009\000\158\000\010\000\030\000\011\000\158\000\012\000\029\000\
\\013\000\158\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\025\000\158\000\026\000\158\000\
\\027\000\158\000\030\000\158\000\031\000\158\000\034\000\158\000\
\\035\000\158\000\037\000\158\000\038\000\158\000\042\000\158\000\
\\043\000\158\000\044\000\158\000\000\000\
\\001\000\001\000\161\000\005\000\161\000\007\000\161\000\008\000\031\000\
\\009\000\161\000\010\000\030\000\011\000\161\000\012\000\029\000\
\\013\000\161\000\014\000\028\000\015\000\027\000\016\000\026\000\
\\017\000\025\000\018\000\024\000\019\000\023\000\020\000\022\000\
\\021\000\021\000\022\000\020\000\023\000\019\000\024\000\018\000\
\\025\000\017\000\026\000\016\000\030\000\161\000\031\000\161\000\
\\034\000\161\000\035\000\161\000\037\000\161\000\038\000\161\000\
\\042\000\161\000\043\000\161\000\044\000\161\000\000\000\
\\001\000\002\000\014\000\003\000\013\000\004\000\012\000\008\000\011\000\
\\016\000\010\000\029\000\009\000\032\000\008\000\033\000\007\000\
\\036\000\006\000\040\000\005\000\041\000\004\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\002\000\058\000\000\000\
\\001\000\002\000\068\000\000\000\
\\001\000\002\000\069\000\000\000\
\\001\000\002\000\070\000\000\000\
\\001\000\002\000\099\000\012\000\098\000\028\000\097\000\000\000\
\\001\000\002\000\101\000\000\000\
\\001\000\002\000\118\000\000\000\
\\001\000\002\000\119\000\000\000\
\\001\000\002\000\124\000\000\000\
\\001\000\002\000\128\000\000\000\
\\001\000\002\000\134\000\000\000\
\\001\000\002\000\138\000\000\000\
\\001\000\006\000\086\000\027\000\085\000\000\000\
\\001\000\006\000\115\000\000\000\
\\001\000\006\000\123\000\019\000\122\000\000\000\
\\001\000\006\000\137\000\000\000\
\\001\000\008\000\031\000\010\000\030\000\011\000\079\000\012\000\029\000\
\\014\000\028\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\025\000\017\000\
\\026\000\016\000\027\000\015\000\000\000\
\\001\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\030\000\073\000\000\000\
\\001\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\034\000\104\000\000\000\
\\001\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\035\000\072\000\000\000\
\\001\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\035\000\125\000\000\000\
\\001\000\008\000\087\000\000\000\
\\001\000\009\000\074\000\000\000\
\\001\000\009\000\080\000\000\000\
\\001\000\009\000\114\000\000\000\
\\001\000\013\000\077\000\000\000\
\\001\000\013\000\120\000\000\000\
\\001\000\019\000\078\000\000\000\
\\001\000\019\000\084\000\000\000\
\\001\000\019\000\126\000\000\000\
\\001\000\019\000\133\000\000\000\
\\001\000\027\000\071\000\000\000\
\\001\000\027\000\113\000\000\000\
\\001\000\037\000\067\000\000\000\
\\001\000\038\000\095\000\000\000\
\\001\000\039\000\111\000\000\000\
\\141\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\039\000\093\000\000\000\
\\148\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\000\000\
\\149\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\017\000\025\000\018\000\024\000\000\000\
\\150\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\017\000\025\000\018\000\024\000\000\000\
\\151\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\000\000\
\\152\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\000\000\
\\159\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\000\000\
\\160\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\000\000\
\\162\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\163\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\031\000\105\000\000\000\
\\164\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\165\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\172\000\000\000\
\\173\000\002\000\060\000\000\000\
\\174\000\000\000\
\\175\000\005\000\108\000\008\000\031\000\010\000\030\000\012\000\029\000\
\\014\000\028\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\025\000\017\000\
\\026\000\016\000\027\000\015\000\000\000\
\\176\000\000\000\
\\177\000\002\000\014\000\003\000\013\000\004\000\012\000\008\000\011\000\
\\016\000\010\000\029\000\009\000\032\000\008\000\033\000\007\000\
\\036\000\006\000\040\000\005\000\041\000\004\000\000\000\
\\178\000\000\000\
\\179\000\007\000\076\000\008\000\031\000\010\000\030\000\012\000\029\000\
\\014\000\028\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\025\000\017\000\
\\026\000\016\000\027\000\015\000\000\000\
\\180\000\000\000\
\\181\000\002\000\014\000\003\000\013\000\004\000\012\000\008\000\011\000\
\\016\000\010\000\029\000\009\000\032\000\008\000\033\000\007\000\
\\036\000\006\000\040\000\005\000\041\000\004\000\000\000\
\\182\000\000\000\
\\183\000\005\000\082\000\008\000\031\000\010\000\030\000\012\000\029\000\
\\014\000\028\000\015\000\027\000\016\000\026\000\017\000\025\000\
\\018\000\024\000\019\000\023\000\020\000\022\000\021\000\021\000\
\\022\000\020\000\023\000\019\000\024\000\018\000\025\000\017\000\
\\026\000\016\000\027\000\015\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\042\000\038\000\043\000\037\000\044\000\036\000\000\000\
\\188\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\189\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\190\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\191\000\008\000\031\000\010\000\030\000\012\000\029\000\014\000\028\000\
\\015\000\027\000\016\000\026\000\017\000\025\000\018\000\024\000\
\\019\000\023\000\020\000\022\000\021\000\021\000\022\000\020\000\
\\023\000\019\000\024\000\018\000\025\000\017\000\026\000\016\000\
\\027\000\015\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\002\000\103\000\000\000\
\\198\000\000\000\
\\199\000\005\000\130\000\000\000\
\"
val actionRowNumbers =
"\008\000\046\000\047\000\064\000\
\\085\000\009\000\008\000\008\000\
\\008\000\075\000\049\000\048\000\
\\050\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\010\000\071\000\
\\008\000\079\000\085\000\085\000\
\\085\000\043\000\011\000\012\000\
\\013\000\041\000\029\000\027\000\
\\053\000\032\000\077\000\007\000\
\\059\000\058\000\006\000\005\000\
\\004\000\003\000\002\000\001\000\
\\057\000\056\000\055\000\054\000\
\\051\000\035\000\037\000\026\000\
\\033\000\081\000\083\000\084\000\
\\082\000\075\000\038\000\022\000\
\\031\000\008\000\008\000\008\000\
\\065\000\074\000\008\000\068\000\
\\008\000\052\000\066\000\078\000\
\\008\000\044\000\014\000\008\000\
\\015\000\095\000\028\000\062\000\
\\061\000\077\000\073\000\008\000\
\\081\000\067\000\090\000\045\000\
\\095\000\091\000\088\000\042\000\
\\034\000\023\000\008\000\008\000\
\\076\000\070\000\016\000\069\000\
\\080\000\017\000\036\000\008\000\
\\024\000\018\000\030\000\060\000\
\\039\000\093\000\092\000\089\000\
\\008\000\019\000\097\000\008\000\
\\008\000\086\000\040\000\094\000\
\\020\000\063\000\073\000\008\000\
\\025\000\072\000\087\000\021\000\
\\097\000\096\000\000\000"
val gotoT =
"\
\\001\000\001\000\002\000\138\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\033\000\010\000\032\000\011\000\031\000\012\000\030\000\000\000\
\\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\042\000\003\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\043\000\000\000\
\\001\000\044\000\000\000\
\\001\000\045\000\000\000\
\\001\000\046\000\000\000\
\\001\000\047\000\000\000\
\\001\000\048\000\000\000\
\\001\000\049\000\000\000\
\\001\000\050\000\000\000\
\\001\000\051\000\000\000\
\\001\000\052\000\000\000\
\\001\000\053\000\000\000\
\\001\000\054\000\000\000\
\\001\000\055\000\000\000\
\\000\000\
\\007\000\057\000\000\000\
\\001\000\059\000\000\000\
\\001\000\061\000\005\000\060\000\000\000\
\\009\000\062\000\010\000\032\000\011\000\031\000\012\000\030\000\000\000\
\\009\000\063\000\010\000\032\000\011\000\031\000\012\000\030\000\000\000\
\\009\000\064\000\010\000\032\000\011\000\031\000\012\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\042\000\003\000\081\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\086\000\000\000\
\\001\000\087\000\000\000\
\\001\000\088\000\000\000\
\\000\000\
\\000\000\
\\001\000\089\000\000\000\
\\000\000\
\\001\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\092\000\000\000\
\\000\000\
\\015\000\094\000\000\000\
\\001\000\098\000\000\000\
\\000\000\
\\013\000\100\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\104\000\000\000\
\\008\000\105\000\000\000\
\\001\000\107\000\000\000\
\\006\000\108\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\110\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\114\000\000\000\
\\001\000\115\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\119\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\125\000\000\000\
\\000\000\
\\014\000\127\000\000\000\
\\001\000\129\000\000\000\
\\001\000\130\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\133\000\000\000\
\\001\000\134\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\137\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 139
val numrules = 59
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | ty of unit ->  (A.ty)
 | tyfieldshd of unit ->  (A.field list)
 | tyfields of unit ->  (A.field list) | vardec of unit ->  (A.dec)
 | tydec of unit ->  (A.dec) | fundec of unit ->  (A.dec)
 | decs of unit ->  (A.dec list)
 | recordfieldshd of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | recordfields of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | expseqcomhd of unit ->  (A.exp list)
 | expseqcom of unit ->  (A.exp list)
 | expseqsemihd of unit ->  ( ( A.exp * A.pos )  list)
 | expseqsemi of unit ->  ( ( A.exp * A.pos )  list)
 | program of unit ->  (A.exp) | exp of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
 $$ (T 18),nil
 $$ (T 26))::
(nil
 $$ (T 26),nil
 $$ (T 18))::
(nil
 $$ (T 30) $$ (T 6),nil
 $$ (T 30))::
(nil
,nil
 $$ (T 37) $$ (T 2) $$ (T 36))::
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp INT)
end)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp (STRING, STRINGleft))
end)
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.VarExp (A.SimpleVar (Symbol.symbol ID, IDleft)))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 val  (ID as ID1) = ID1 ()
 in (
A.VarExp (A.FieldVar (varChecking (exp, expleft), Symbol.symbol ID, expleft))
)
end)
 in ( LrTable.NT 0, ( result, exp1left, ID1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.VarExp (A.SubscriptVar (varChecking (exp1, exp1left), exp2, exp1left))
)
end)
 in ( LrTable.NT 0, ( result, exp1left, RBRACK1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp {left = (A.IntExp 0), oper = A.MinusOp, right = exp, pos = MINUSleft}
)
end)
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.PlusOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.MinusOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.TimesOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.DivideOp, right = exp2, pos = exp1left}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.EqOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.NeqOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.LtOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.LeOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.GtOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.OpExp {left = exp1, oper = A.GeOp, right = exp2, pos = exp1left})

end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp {test = exp1, then' = exp2, else' = SOME (A.IntExp 0), pos = exp1left}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp {test = exp1, then' = (A.IntExp 1), else' = SOME exp2, pos = exp1left}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 val  exp2 = exp2 ()
 in (
A.AssignExp {var = (varChecking (exp1, exp1left)), exp = exp2, pos = exp1left}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.IfExp {test = exp1, then' = exp2, else' = SOME (exp3), pos = IFleft}
)
end)
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1 = 
exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp {test = exp1, then' = exp2, else' = NONE, pos = IFleft})

end)
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp {test = exp1, body = exp2, pos = WHILEleft})
end)
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp {var = (Symbol.symbol ID), escape = ref false, lo = exp1, hi = exp2, body = exp3, pos = FORleft}
)
end)
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 25, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp BREAKleft))
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expseqsemi 
expseqsemi1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  (expseqsemi as 
expseqsemi1) = expseqsemi1 ()
 in (A.SeqExp expseqsemi)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expseqcom 
expseqcom1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, (expleft as 
exp1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 val  (expseqcom as expseqcom1) = expseqcom1 ()
 in (
A.CallExp {func = Symbol.symbol (IdChecking (exp, expleft)), args = expseqcom, pos = expleft}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 28, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expseqsemi 
expseqsemi1, _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _,
 ( _, (LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (decs as decs1) = decs1 ()
 val  (expseqsemi as expseqsemi1) = expseqsemi1 ()
 in (A.LetExp {decs = decs, body = A.SeqExp expseqsemi, pos = LETleft}
)
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 29, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.recordfields
 recordfields1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, (expleft as 
exp1left), _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 val  (recordfields as recordfields1) = recordfields1 ()
 in (
A.RecordExp {fields = recordfields, typ = Symbol.symbol (IdChecking (exp, expleft)), pos = expleft}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, RBRACE1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left
, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val 
 exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ArrayExp {typ = Symbol.symbol (IdChecking (exp1, exp1left)), size = exp2, init = exp3, pos = exp1left}
)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp3right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.recordfieldshd recordfieldshd1, _, 
recordfieldshd1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _
, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val 
 result = MlyValue.recordfields (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (recordfieldshd as recordfieldshd1) = recordfieldshd1 ()
 in ((Symbol.symbol ID, exp, IDleft)::recordfieldshd)
end)
 in ( LrTable.NT 6, ( result, ID1left, recordfieldshd1right), rest671)

end
|  ( 32, ( rest671)) => let val  result = MlyValue.recordfields (fn _
 => ([]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 33, ( ( _, ( MlyValue.recordfieldshd recordfieldshd1, _, 
recordfieldshd1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _
, ( MlyValue.ID ID1, IDleft, _)) :: ( _, ( _, COMMA1left, _)) :: 
rest671)) => let val  result = MlyValue.recordfieldshd (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (recordfieldshd as recordfieldshd1) = recordfieldshd1 ()
 in ((Symbol.symbol ID, exp, IDleft)::recordfieldshd)
end)
 in ( LrTable.NT 7, ( result, COMMA1left, recordfieldshd1right), 
rest671)
end
|  ( 34, ( rest671)) => let val  result = MlyValue.recordfieldshd (fn
 _ => ([]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 35, ( ( _, ( MlyValue.expseqsemihd expseqsemihd1, _, 
expseqsemihd1right)) :: ( _, ( MlyValue.exp exp1, (expleft as exp1left
), _)) :: rest671)) => let val  result = MlyValue.expseqsemi (fn _ =>
 let val  (exp as exp1) = exp1 ()
 val  (expseqsemihd as expseqsemihd1) = expseqsemihd1 ()
 in ((exp, expleft)::expseqsemihd)
end)
 in ( LrTable.NT 2, ( result, exp1left, expseqsemihd1right), rest671)

end
|  ( 36, ( rest671)) => let val  result = MlyValue.expseqsemi (fn _ =>
 ([]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.expseqsemihd expseqsemihd1, _, 
expseqsemihd1right)) :: ( _, ( MlyValue.exp exp1, expleft, _)) :: ( _,
 ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = 
MlyValue.expseqsemihd (fn _ => let val  (exp as exp1) = exp1 ()
 val  (expseqsemihd as expseqsemihd1) = expseqsemihd1 ()
 in ((exp, expleft)::expseqsemihd)
end)
 in ( LrTable.NT 3, ( result, SEMICOLON1left, expseqsemihd1right), 
rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.expseqsemihd (fn _
 => ([]))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.expseqcomhd expseqcomhd1, _, 
expseqcomhd1right)) :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: 
rest671)) => let val  result = MlyValue.expseqcom (fn _ => let val  (
exp as exp1) = exp1 ()
 val  (expseqcomhd as expseqcomhd1) = expseqcomhd1 ()
 in (exp::expseqcomhd)
end)
 in ( LrTable.NT 4, ( result, exp1left, expseqcomhd1right), rest671)

end
|  ( 40, ( rest671)) => let val  result = MlyValue.expseqcom (fn _ =>
 ([]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( MlyValue.expseqcomhd expseqcomhd1, _, 
expseqcomhd1right)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, 
COMMA1left, _)) :: rest671)) => let val  result = MlyValue.expseqcomhd
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (expseqcomhd as expseqcomhd1) = expseqcomhd1 ()
 in (exp::expseqcomhd)
end)
 in ( LrTable.NT 5, ( result, COMMA1left, expseqcomhd1right), rest671)

end
|  ( 42, ( rest671)) => let val  result = MlyValue.expseqcomhd (fn _
 => ([]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (fundec as fundec1) = fundec1
 ()
 val  (decs as decs1) = decs1 ()
 in (mergeDecs (fundec, decs))
end)
 in ( LrTable.NT 8, ( result, fundec1left, decs1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.decs (fn _ => let val  (vardec as vardec1) = vardec1
 ()
 val  (decs as decs1) = decs1 ()
 in (mergeDecs (vardec, decs))
end)
 in ( LrTable.NT 8, ( result, vardec1left, decs1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.tydec tydec1, tydec1left, _)) :: rest671)) => let val  result
 = MlyValue.decs (fn _ => let val  (tydec as tydec1) = tydec1 ()
 val  (decs as decs1) = decs1 ()
 in (mergeDecs (tydec, decs))
end)
 in ( LrTable.NT 8, ( result, tydec1left, decs1right), rest671)
end
|  ( 46, ( rest671)) => let val  result = MlyValue.decs (fn _ => ([]))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671))
 => let val  result = MlyValue.fundec (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec [{name = Symbol.symbol ID, params = tyfields, result = NONE, body = exp, pos = FUNCTIONleft}]
)
end)
 in ( LrTable.NT 9, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.FunctionDec [{name = Symbol.symbol ID1, params = tyfields, result = SOME (Symbol.symbol ID2, ID2left), body = exp, pos = FUNCTIONleft}]
)
end)
 in ( LrTable.NT 9, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec {name = Symbol.symbol ID, escape = ref false, typ = NONE, init = exp, pos = VARleft}
)
end)
 in ( LrTable.NT 11, ( result, VAR1left, exp1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec {name = Symbol.symbol ID1, escape = ref false, typ = SOME (Symbol.symbol ID2, ID2left), init = exp, pos = VARleft}
)
end)
 in ( LrTable.NT 11, ( result, VAR1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in (A.TypeDec [{name = Symbol.symbol ID, ty = ty, pos = TYPEleft}])

end)
 in ( LrTable.NT 10, ( result, TYPE1left, ty1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy (Symbol.symbol ID, IDleft))
end)
 in ( LrTable.NT 14, ( result, ID1left, ID1right), rest671)
end
|  ( 53, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy tyfields)
end)
 in ( LrTable.NT 14, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 54, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy (Symbol.symbol ID, ARRAYleft))
end)
 in ( LrTable.NT 14, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.tyfieldshd tyfieldshd1, _, tyfieldshd1right
)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.tyfields (fn _
 => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfieldshd as tyfieldshd1) = tyfieldshd1 ()
 in (
({name = Symbol.symbol ID1, escape = ref false, typ = Symbol.symbol ID2, pos = ID1left})::tyfieldshd
)
end)
 in ( LrTable.NT 12, ( result, ID1left, tyfieldshd1right), rest671)

end
|  ( 56, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 57, ( ( _, ( MlyValue.tyfieldshd tyfieldshd1, _, tyfieldshd1right
)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  
result = MlyValue.tyfieldshd (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfieldshd as tyfieldshd1) = tyfieldshd1 ()
 in (
({name = Symbol.symbol ID1, escape = ref false, typ = Symbol.symbol ID2, pos = ID1left})::tyfieldshd
)
end)
 in ( LrTable.NT 13, ( result, COMMA1left, tyfieldshd1right), rest671)

end
|  ( 58, ( rest671)) => let val  result = MlyValue.tyfieldshd (fn _ =>
 ([]))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
