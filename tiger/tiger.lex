type pos = int;

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) Tokens.token;

val lineNum = ErrorMsg.lineNum;
val linePos = ErrorMsg.linePos;
val commentDepth: int ref = ref 0;
val stringVal: string ref = ref "";

fun inc_ref ref_ n = ref_ := !ref_ + n
fun dec_ref ref_ n = ref_ := !ref_ - n

fun err pos msg = ErrorMsg.error pos msg;

fun eof() = let
                val pos = hd(!linePos)
                val ln = !lineNum
                val commentError = "Comments are not balanced."
            in
                (if !commentDepth <> 0
                then err pos commentError else ();
                Tokens.EOF(pos, pos))
            end;

%%
%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));

ws    = [\ \t];
digit = [0-9];
digits = [0-9]+;
alpha = [a-zA-Z];
alphas = {alpha}+;
strings = "\""(\\.|[^\\"])*"\"";
newline = (\n | \r | \r\n | \n\r);
%s COMMENT;

%%

<INITIAL,COMMENT> {newline} =>              (inc_ref lineNum 1;  linePos := yypos :: !linePos; lex());
<INITIAL> {ws}+ =>                          (lex());
<INITIAL,COMMENT> "/*" =>                   (YYBEGIN COMMENT; commentDepth := !commentDepth + 1; lex());
<COMMENT> "*/" =>                           (commentDepth := !commentDepth - 1; if !commentDepth = 0 then YYBEGIN INITIAL else (); lex());
<COMMENT> . =>                              (lex());
<INITIAL> array =>                          (Tokens.ARRAY (yypos, yypos + 5));
<INITIAL> if =>                             (Tokens.IF (yypos, yypos + 2));
<INITIAL> then =>                           (Tokens.THEN (yypos, yypos + 4));
<INITIAL> else =>                           (Tokens.ELSE (yypos, yypos + 4));
<INITIAL> while =>                          (Tokens.WHILE (yypos, yypos + 5));
<INITIAL> for =>                            (Tokens.FOR (yypos, yypos + 3));
<INITIAL> to =>                             (Tokens.TO (yypos, yypos + 2));
<INITIAL> do =>                             (Tokens.DO (yypos, yypos + 2));
<INITIAL> let =>                            (Tokens.LET (yypos, yypos + 3));
<INITIAL> in =>                             (Tokens.IN (yypos, yypos + 2));
<INITIAL> end =>                            (Tokens.END (yypos, yypos + 3));
<INITIAL> of =>                             (Tokens.OF (yypos, yypos + 2));
<INITIAL> break =>                          (Tokens.BREAK (yypos, yypos + 5));
<INITIAL> nil =>                            (Tokens.NIL (yypos, yypos + 3));
<INITIAL> function =>                       (Tokens.FUNCTION (yypos, yypos + 8));
<INITIAL> var =>                            (Tokens.VAR (yypos, yypos + 3));
<INITIAL> type =>                           (Tokens.TYPE (yypos, yypos + 4));
<INITIAL> primitive =>                      (Tokens.PRIMITIVE (yypos, yypos + 9));
<INITIAL> class =>                          (Tokens.CLASS (yypos, yypos + 3));
<INITIAL> extends =>                        (Tokens.EXTENDS (yypos, yypos + 7));
<INITIAL> method =>                         (Tokens.METHOD (yypos, yypos + 6));
<INITIAL> new =>                            (Tokens.NEW (yypos, yypos + 3));
<INITIAL> "," =>                            (Tokens.COMMA (yypos, yypos + 1));
<INITIAL> ":" =>                            (Tokens.COLON (yypos, yypos + 1));
<INITIAL> ";" =>                            (Tokens.SEMI_COLON (yypos, yypos + 1));
<INITIAL> "{" =>                            (Tokens.LCURLY (yypos, yypos + 1));
<INITIAL> "}" =>                            (Tokens.RCURLY (yypos, yypos + 1));
<INITIAL> "(" =>                            (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL> ")" =>                            (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL> "[" =>                            (Tokens.LSQUARE (yypos, yypos + 1));
<INITIAL> "]" =>                            (Tokens.RSQUARE (yypos, yypos + 1));
<INITIAL> "." =>                            (Tokens.DOT (yypos, yypos + 1));
<INITIAL> "+" =>                            (Tokens.PLUS (yypos, yypos + 1));
<INITIAL> "-" =>                            (Tokens.MINUS (yypos, yypos + 1));
<INITIAL> "*" =>                            (Tokens.MUL (yypos, yypos + 1));
<INITIAL> "/" =>                            (Tokens.DIV (yypos, yypos + 1));
<INITIAL> "=" =>                            (Tokens.EQ (yypos, yypos + 1));
<INITIAL> "<>" =>                           (Tokens.NE (yypos, yypos + 2));
<INITIAL> "<" =>                            (Tokens.LT (yypos, yypos + 1));
<INITIAL> "<=" =>                           (Tokens.LE (yypos, yypos + 2));
<INITIAL> ">" =>                            (Tokens.GT (yypos, yypos + 1));
<INITIAL> ">=" =>                           (Tokens.GE (yypos, yypos + 2));
<INITIAL> "&" =>                            (Tokens.AND (yypos, yypos + 1));
<INITIAL> "|" =>                            (Tokens.OR (yypos, yypos + 1));
<INITIAL> ":=" =>                           (Tokens.ASSIGN (yypos, yypos + 2));
<INITIAL> {digits} =>                       (Tokens.INT(valOf (Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL> {alpha}({alpha}|{digits}|"_")* => (Tokens.ID (Symbol.symbol yytext, yypos, yypos + size(yytext)));
<INITIAL> {strings} =>                      (Tokens.STRING(String.substring(yytext, 1, String.size(yytext) - 2), yypos, yypos + size(yytext)));
. =>                                        (err yypos ("Illegal character '" ^ yytext ^ "' found"); linePos := yypos :: !linePos; eof());