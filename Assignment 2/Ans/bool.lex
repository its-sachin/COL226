structure Tokens= Tokens
  
  type pos = int
  type line = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val line = ref 0;
  val eof = fn () => Tokens.EOF(!pos, !pos)
  fun error(s:string,a:int, b:int) = TextIO.output(TextIO.stdOut,"Unknown token:"^(Int.toString(a))^":"^(Int.toString(b))^":"^s^ "\n")

  fun inc(pos,num) = pos := (!pos) + num;

  val list = ref "["
  fun update(s,a) = list := !list^s^" \""^a^"\""^",";

  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n       => (inc(line,1); lex());
{ws}+    => (inc(pos,1); lex());
";"      => (inc(pos,1); update("EOS",";"); Tokens.EOF(!pos,!line));
"("      => (inc(pos,1); update("LPAREN",")"); Tokens.LPAREN(!pos,!line));
")"      => (inc(pos,1); update("RPAREN","("); Tokens.RPAREN(!pos,!line));
{alpha}+ => (if yytext = "AND" then (inc(pos,3); update("AND","AND"); Tokens.AND(!pos, !line))
            else if yytext = "OR" then (inc(pos,2); update("OR","OR"); Tokens.OR(!pos,!line))
            else if yytext = "XOR" then (inc(pos,3); update("XOR","XOR"); Tokens.XOR(!pos,!line))
            else if yytext = "EQUALS" then (inc(pos,6); update("EQUALS","EQUALS"); Tokens.EQUALS(!pos,!line))
            else if yytext = "IF" then (inc(pos,2); update("IF","IF"); Tokens.IF(!pos,!line))
            else if yytext = "THEN" then (inc(pos,4);  update("THEN","THEN");Tokens.THEN(!pos,!line))
            else if yytext = "ELSE" then (inc(pos,5); update("ELSE","ELSE"); Tokens.ELSE(!pos,!line))
            else if yytext = "IMPLIES" then (inc(pos,7); update("IMPLIES","IMPLIES"); Tokens.IMPLIES(!pos,!line))
            else if yytext = "NOT" then (inc(pos,3); update("NOT","NOT"); Tokens.NOT(!pos,!line))
            else if yytext = "TRUE" then (inc(pos,4);  update("CONST","TRUE");Tokens.CONST(yytext,!pos,!line))
            else if yytext = "FALSE" then (inc(pos,5); update("CONST","FALSE"); Tokens.CONST(yytext,!pos,!line))
            else (inc(pos,String.size(yytext)); update("ID",yytext); Tokens.ID(yytext,!pos,!line)));
.      => (error(yytext,!pos,!line); lex());

