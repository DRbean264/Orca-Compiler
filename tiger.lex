type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type pos = int
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ErrorMsg.commentDepth
fun err(p1,p2) = ErrorMsg.error p1

fun strProc (str, pos) =
    let
        val str = String.substring (str, 1, (size str) - 2)
        val chLst = String.explode str

        fun isDigit c = #"0" <= c andalso c <= #"9"

        fun fetchHd [] = ErrorMsg.impossible "The ascii code escaping sequence should be of \\ddd format"
          | fetchHd (e::lst) = (e, lst)

        fun ignoreSequence (#"\n"::chLst, resStr) = 
          let
            val _ = lineNum := !lineNum+1
            val _ = linePos := pos :: !linePos
          in
            ignoreSequence (chLst, resStr)
          end
          | ignoreSequence (#" "::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#"\t"::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#"\f"::chLst, resStr) = ignoreSequence (chLst, resStr)
          | ignoreSequence (#"\\"::chLst, resStr) = (chLst, resStr)
          | ignoreSequence (chLst, resStr) = ErrorMsg.impossible "The ignoring escaping sequence should be only of format \\f___f\\, in which f__f means formating characters like space, tab, newline, formfeed"
                                    
        fun escapeHelper ([], resStr) = ErrorMsg.impossible "A single use of \\ is not allowed"
          | escapeHelper (#"n"::chLst, resStr) =
            (chLst, resStr ^ "\n")
          | escapeHelper (#"t"::chLst, resStr) = 
            (chLst, resStr ^ "\t")
          | escapeHelper (#"\\"::chLst, resStr) =
            (chLst, resStr ^ "\\")
          | escapeHelper (#"\""::chLst, resStr) = 
            (chLst, resStr ^ "\"")
          | escapeHelper (#"^"::c::chLst, resStr) =
          (* now we just skip the control sequence, maybe implement this later *)
            (chLst, resStr)
          | escapeHelper (chLst, resStr) =
                case isDigit (hd chLst) of
                    true =>
                    let
                        val (d1, chLst) = fetchHd chLst
                        val (d2, chLst) = fetchHd chLst
                        val (d3, chLst) = fetchHd chLst
                        val _ = if (isDigit d1) andalso (isDigit d2) andalso (isDigit d3) then () else ErrorMsg.impossible "The ascii code escaping sequence should be of \\ddd format"
                        val SOME num = Int.fromString (String.implode [d1, d2, d3])
                        (* check validity of ascii code *)
                        val _ = if num >= 0 andalso num <= 255 then () else ErrorMsg.impossible "The \\ddd escaping sequence should only be in range 000 to 255"
                    in
                        (chLst, resStr ^ (String.str (chr num)))
                    end
                  | false =>
                    ignoreSequence (chLst, resStr)
            
        fun helper ([], resStr) = resStr
          | helper (#"\\"::chLst, resStr) = helper (escapeHelper (chLst, resStr))
          | helper (c::chLst, resStr) = helper (chLst, resStr ^ (Char.toString c))
    in
        helper (chLst, "")
    end

fun eof () = 
    let 
        val pos = hd(!linePos)

        (* check if there's unmatched comment *)
        val () = if !commentDepth = 0
                then () 
                (* 
                  I reset everything. Because if I first use this lexer on
                  one file and there're errors in there. The next time I use 
                  this to lex another file, the error state would still be there.
                 *)
                else 
                  ErrorMsg.error pos ("Unmatched comment. " ^ "Comment depth " ^ (Int.toString (!commentDepth)))
    in 
        Tokens.EOF(pos,pos) 
    end

fun generateErr (pos, "\"") = ErrorMsg.error pos ("Unclosed string")
  | generateErr (pos, str) = ErrorMsg.error pos ("illegal character " ^ str)

%% 

%s COMMENT;
escape_sequence = \\([nt\\"]|\^[@A-Z\[\\\]\^_]|[0-9]{3}|[\ \n\t\f]+\\);
printable = [\ !#-\[]|[\]-~];

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

%%
<INITIAL,COMMENT>\n	                             => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>var  	                                 => (Tokens.VAR (yypos, yypos + (size yytext)));
<INITIAL>type                                    => (Tokens.TYPE (yypos, yypos + (size yytext)));
<INITIAL>function                                => (Tokens.FUNCTION (yypos, yypos + (size yytext)));
<INITIAL>break                                   => (Tokens.BREAK (yypos, yypos + (size yytext)));
<INITIAL>of                                      => (Tokens.OF (yypos, yypos + (size yytext)));
<INITIAL>end                                     => (Tokens.END (yypos, yypos + (size yytext)));
<INITIAL>in                                      => (Tokens.IN (yypos, yypos + (size yytext)));
<INITIAL>nil                                     => (Tokens.NIL (yypos, yypos + (size yytext)));
<INITIAL>let                                     => (Tokens.LET (yypos, yypos + (size yytext)));
<INITIAL>do                                      => (Tokens.DO (yypos, yypos + (size yytext)));
<INITIAL>to                                      => (Tokens.TO (yypos, yypos + (size yytext)));
<INITIAL>for                                     => (Tokens.FOR (yypos, yypos + (size yytext)));
<INITIAL>while                                   => (Tokens.WHILE (yypos, yypos + (size yytext)));
<INITIAL>else                                    => (Tokens.ELSE (yypos, yypos + (size yytext)));
<INITIAL>then                                    => (Tokens.THEN (yypos, yypos + (size yytext)));
<INITIAL>if                                      => (Tokens.IF (yypos, yypos + (size yytext)));
<INITIAL>array                                   => (Tokens.ARRAY (yypos, yypos + (size yytext)));
<INITIAL>":="                                    => (Tokens.ASSIGN (yypos, yypos + (size yytext)));
<INITIAL>"|"                                     => (Tokens.OR (yypos, yypos + (size yytext)));
<INITIAL>"&"                                     => (Tokens.AND (yypos, yypos + (size yytext)));
<INITIAL>">="                                    => (Tokens.GE (yypos, yypos + (size yytext)));
<INITIAL>">"                                     => (Tokens.GT (yypos, yypos + (size yytext)));
<INITIAL>"<="                                    => (Tokens.LE (yypos, yypos + (size yytext)));
<INITIAL>"<"                                     => (Tokens.LT (yypos, yypos + (size yytext)));
<INITIAL>"<>"                                    => (Tokens.NEQ (yypos, yypos + (size yytext)));
<INITIAL>"="                                     => (Tokens.EQ (yypos, yypos + (size yytext)));
<INITIAL>"/"                                     => (Tokens.DIVIDE (yypos, yypos + (size yytext)));
<INITIAL>"*"                                     => (Tokens.TIMES (yypos, yypos + (size yytext)));
<INITIAL>"-"                                     => (Tokens.MINUS (yypos, yypos + (size yytext)));
<INITIAL>"+"                                     => (Tokens.PLUS (yypos, yypos + (size yytext)));
<INITIAL>"."                                     => (Tokens.DOT (yypos, yypos + (size yytext)));
<INITIAL>"}"                                     => (Tokens.RBRACE (yypos, yypos + (size yytext)));
<INITIAL>"{"                                     => (Tokens.LBRACE (yypos, yypos + (size yytext)));
<INITIAL>"]"                                     => (Tokens.RBRACK (yypos, yypos + (size yytext)));
<INITIAL>"["                                     => (Tokens.LBRACK (yypos, yypos + (size yytext)));
<INITIAL>")"                                     => (Tokens.RPAREN (yypos, yypos + (size yytext)));
<INITIAL>"("                                     => (Tokens.LPAREN (yypos, yypos + (size yytext)));
<INITIAL>";"                                     => (Tokens.SEMICOLON (yypos, yypos + (size yytext)));
<INITIAL>":"                                     => (Tokens.COLON (yypos, yypos + (size yytext)));
<INITIAL>","                 	                   => (Tokens.COMMA (yypos, yypos + (size yytext)));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]*                   => (Tokens.ID (yytext, yypos, yypos + (size yytext)));
<INITIAL>[0-9]+	                                 => (Tokens.INT (Option.valOf (Int.fromString yytext), yypos, yypos + (size yytext)));
<INITIAL>\"(\ |{printable}|{escape_sequence})*\"	    => (Tokens.STRING (strProc (yytext, yypos), yypos, yypos + (size yytext)));
<INITIAL>(" "|"\n"|"\t")                         => (continue());
<INITIAL>"/*"                                    => (commentDepth := !commentDepth + 1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"                                    => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT>"/*"                                    => (commentDepth := !commentDepth + 1; continue());
<COMMENT>.                                       => (continue());
.                                                => (generateErr (yypos, yytext); continue());

