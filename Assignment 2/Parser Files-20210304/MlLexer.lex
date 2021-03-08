datatype lexresult= DIV of string| EOF | EOS | ID of string | LPAREN |
                    NUM of int | PLUS | PRINT | RPAREN | SUB | TIMES | EQ
val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF
fun refinc x =  (x := !x + 1; !x)
val list = []
  
%%
  
%structure SimpLex
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t \n];
%%x
\n => (refinc linenum; lex());
{ws}+ => (lex());
";" => (EOF);
"/" => (DIV yytext);
";" => (EOS::list; EOS);
"(" => (LPAREN);
"=" => (EQ);
{digit}+ => (NUM  (List.foldr (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext)));
")" => (RPAREN);
"+" => (PLUS);
{alpha}+ => (if yytext="print" then PRINT else ID yytext);
"-" => (SUB);
"*" => (TIMES);
. => (error ("calc: ignoring bad character "^yytext); lex());
