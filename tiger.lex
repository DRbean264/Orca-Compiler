type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0

fun strProc str =
    let
        val str = String.substring (str, 1, (size str) - 2)
        val chLst = String.explode str

        fun isDigit c = #"0" <= c andalso c <= #"9"

        fun fetchHd [] = NONE (* raise exception here *)
          | fetchHd (e::lst) = SOME (e, lst)

        fun ignoreSequence (#"\n"::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#" "::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#"\t"::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#"\f"::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#"\\"::chLst, resStr) = (chLst, resStr)
          (* raise exception here *)
          | ignoreSequence (chLst, resStr) = (chLst, resStr)
                                    
        (* add empty list support *)
        fun escapeHelper (#"n"::chLst, resStr) =
            (chLst, resStr ^ "\n")
          | escapeHelper (#"t"::chLst, resStr) = 
            (chLst, resStr ^ "\t")
          | escapeHelper (#"\\"::chLst, resStr) =
            (chLst, resStr ^ "\\")
          | escapeHelper (#"\""::chLst, resStr) = 
            (chLst, resStr ^ "\"")
          | escapeHelper (#"^"::c::chLst, resStr) =
          (* now we just skip the control sequence, implement this *)
            (chLst, resStr)
          | escapeHelper (chLst, resStr) =
                case isDigit (hd chLst) of
                    true =>
                    let
                        val SOME (d1, chLst) = fetchHd chLst
                        val SOME (d2, chLst) = fetchHd chLst
                        val SOME (d3, chLst) = fetchHd chLst
                        val SOME num = Int.fromString (String.implode [d1, d2, d3])
                        (* TODO: check the range of the num is within printable char *)
                    in
                        (chLst, resStr ^ (Char.toString (chr num)))
                    end
                  | false =>
                    ignoreSequence (chLst, resStr)
            
        fun helper ([], resStr) = resStr
          | helper (#"\\"::chLst, resStr) = helper (escapeHelper (chLst, resStr))
          (* let *)
            (*     val () = print "Call escape helper\n" *)
            (*     val (chLst, resStr) = escapeHelper (chLst, resStr) *)
            (* in *)
            (*     helper (chLst, resStr) *)
            (* end *)
          | helper (c::chLst, resStr) = helper (chLst, resStr ^ (Char.toString c))
            (* let *)
            (*     val () = print ("Got an " ^ (Char.toString c) ^ ", call normal helper\n") *)
            (* in *)
            (*     helper (chLst, resStr ^ (Char.toString c))  *)
            (* end *)
    in
        helper (chLst, "")
    end

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 

%s COMMENT;
escape_sequence = \\([nt\\"]|\^[@A-Z\[\\\]\^_]|[0-9]{3}|[\ \n\t\f]+\\);
printable = [\ -\[]|[\]-~];

%%
<INITIAL>\n	                                     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>var  	                                 => (Tokens.VAR (yypos, yypos+3));
<INITIAL>type                                    => (Tokens.TYPE (yypos, yypos+4));
<INITIAL>function                                => (Tokens.FUNCTION (yypos, yypos+8));
<INITIAL>break                                   => (Tokens.BREAK (yypos, yypos+5));
<INITIAL>of                                      => (Tokens.OF (yypos, yypos+2));
<INITIAL>end                                     => (Tokens.END (yypos, yypos+3));
<INITIAL>in                                      => (Tokens.IN (yypos, yypos+2));
<INITIAL>nil                                     => (Tokens.NIL (yypos, yypos+3));
<INITIAL>let                                     => (Tokens.LET (yypos, yypos+3));
<INITIAL>do                                      => (Tokens.DO (yypos, yypos+2));
<INITIAL>to                                      => (Tokens.TO (yypos, yypos+2));
<INITIAL>for                                     => (Tokens.FOR (yypos, yypos+3));
<INITIAL>while                                   => (Tokens.WHILE (yypos, yypos+5));
<INITIAL>else                                    => (Tokens.ELSE (yypos, yypos+4));
<INITIAL>then                                    => (Tokens.THEN (yypos, yypos+4));
<INITIAL>if                                      => (Tokens.IF (yypos, yypos+2));
<INITIAL>array                                   => (Tokens.ARRAY (yypos, yypos+5));
<INITIAL>":="                                    => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL>"|"                                     => (Tokens.OR (yypos, yypos+1));
<INITIAL>"&"                                     => (Tokens.AND (yypos, yypos+1));
<INITIAL>">="                                    => (Tokens.GE (yypos, yypos+2));
<INITIAL>">"                                     => (Tokens.GT (yypos, yypos+1));
<INITIAL>"<="                                    => (Tokens.LE (yypos, yypos+2));
<INITIAL>"<"                                     => (Tokens.LT (yypos, yypos+1));
<INITIAL>"<>"                                    => (Tokens.NEQ (yypos, yypos+2));
<INITIAL>"="                                     => (Tokens.EQ (yypos, yypos+1));
<INITIAL>"/"                                     => (Tokens.DIVIDE (yypos, yypos+1));
<INITIAL>"*"                                     => (Tokens.TIMES (yypos, yypos+1));
<INITIAL>"-"                                     => (Tokens.MINUS (yypos, yypos+1));
<INITIAL>"+"                                     => (Tokens.PLUS (yypos, yypos+1));
<INITIAL>"."                                     => (Tokens.DOT (yypos, yypos+1));
<INITIAL>"}"                                     => (Tokens.RBRACE (yypos, yypos+1));
<INITIAL>"{"                                     => (Tokens.LBRACE (yypos, yypos+1));
<INITIAL>"]"                                     => (Tokens.RBRACK (yypos, yypos+1));
<INITIAL>"["                                     => (Tokens.LBRACK (yypos, yypos+1));
<INITIAL>")"                                     => (Tokens.RPAREN (yypos, yypos+1));
<INITIAL>"("                                     => (Tokens.LPAREN (yypos, yypos+1));
<INITIAL>";"                                     => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL>":"                                     => (Tokens.COLON (yypos, yypos+1));
<INITIAL>","                 	                 => (Tokens.COMMA (yypos, yypos+1));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]*                   => (Tokens.ID (yytext, yypos, yypos + (size yytext)));
<INITIAL>[0-9]+	                                 => (Tokens.INT (Option.valOf (Int.fromString yytext), yypos, yypos + (size yytext)));
<INITIAL>\"(\ |{printable}|{escape_sequence})*\"	    => (print (Int.toString(size yytext)); Tokens.STRING (strProc yytext, yypos, yypos + (size yytext)));
<INITIAL>(" "|"\n"|"\t")                         => (continue());
<INITIAL>"/*"                                    => (print "comment start\n"; commentDepth := !commentDepth + 1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"                                    => (print "comment end\n"; commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT>"/*"                                    => (print "comment start\n"; commentDepth := !commentDepth + 1; continue());
<COMMENT>(.|"\n")                                => (continue());
.                                                => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

