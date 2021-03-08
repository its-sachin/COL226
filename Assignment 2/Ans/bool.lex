structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))

  val say = fn s => TextIO.output(TextIO.stdOut,s)
  val list = []
  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
";"      => (Tokens.EOF(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
{alpha}+ => (if yytext = "AND" then (Tokens.AND(!pos, !pos))
            else if yytext = "OR" then Tokens.OR(!pos,!pos)
            else if yytext = "XOR" then Tokens.XOR(!pos,!pos)
            else if yytext = "EQUALS" then Tokens.EQUALS(!pos,!pos)
            else if yytext = "IF" then Tokens.IF(!pos,!pos)
            else if yytext = "THEN" then Tokens.THEN(!pos,!pos)
            else if yytext = "ELSE" then Tokens.ELSE(!pos,!pos)
            else if yytext = "IMPLIES" then Tokens.IMPLIES(!pos,!pos)
            else if yytext = "NOT" then Tokens.NOT(!pos,!pos)
            else if yytext = "TRUE" then Tokens.CONST(yytext,!pos,!pos)
            else if yytext = "FALSE" then Tokens.CONST(yytext,!pos,!pos)
            else Tokens.ID(yytext,!pos,!pos));
"."      => (error ("Unknown token:"^yytext,!pos,!pos);
             lex());

