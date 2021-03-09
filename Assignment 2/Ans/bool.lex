structure Tokens= Tokens
  
  type pos = int
  type line = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token


  val pos = ref 0
  val line = ref 0;
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (s,a:int, _) => TextIO.output(TextIO.stdOut,"Unknown token:"^(Int.toString(a))^":"^(Int.toString(!line))^":"^s^ "\n")

  fun inc(pos,num) = pos := (!pos) + num;
  fun setzero(pos) = pos := (!pos) - (!pos);

  val a=  Array.array(0,Term.IF("a"))
  val array = ref a;
  (* fun update(s,a) = list := !list^s^" \""^a^"\""^","; *)
  fun aTol arr = Array.foldr (op ::) [] arr;
  fun update(s) = 
    let 
      val list = (aTol (!array))@[s]
    in
      array := Array.fromList(list)
    end

  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n       => (inc(line,1); setzero(pos); lex());
{ws}+    => (inc(pos,1); lex());
";"      => (inc(pos,1); update(Term.EOF("EOS")); Tokens.EOF(!pos,!line));
"("      => (inc(pos,1); update(Term.LPAREN("(")); Tokens.LPAREN(!pos,!line));
")"      => (inc(pos,1); update(Term.RPAREN(")")); Tokens.RPAREN(!pos,!line));
{alpha}+ => (if yytext = "AND" then (inc(pos,3); update(Term.AND("AND")); Tokens.AND(!pos, !line))
            else if yytext = "OR" then (inc(pos,2); update(Term.OR("OR")); Tokens.OR(!pos,!line))
            else if yytext = "XOR" then (inc(pos,3); update(Term.XOR("XOR")); Tokens.XOR(!pos,!line))
            else if yytext = "EQUALS" then (inc(pos,6); update(Term.EQUALS("EQUALS")); Tokens.EQUALS(!pos,!line))
            else if yytext = "IF" then (inc(pos,2); update(Term.IF("IF")); Tokens.IF(!pos,!line))
            else if yytext = "THEN" then (inc(pos,4);  update(Term.THEN("THEN"));Tokens.THEN(!pos,!line))
            else if yytext = "ELSE" then (inc(pos,5); update(Term.ELSE("ELSE")); Tokens.ELSE(!pos,!line))
            else if yytext = "IMPLIES" then (inc(pos,7); update(Term.IMPLIES("IMPLIES")); Tokens.IMPLIES(!pos,!line))
            else if yytext = "NOT" then (inc(pos,3); update(Term.IMPLIES("IMPLIES")); Tokens.NOT(!pos,!line))
            else if yytext = "TRUE" then (inc(pos,4);  update(Term.CONST("TRUE")); Tokens.CONST(yytext,!pos,!line))
            else if yytext = "FALSE" then (inc(pos,5); update(Term.CONST("FALSE")); Tokens.CONST(yytext,!pos,!line))
            else (inc(pos,String.size(yytext)); update(Term.ID(yytext)); Tokens.ID(yytext,!pos,!line)));
.      => (error(yytext,!pos,!line); lex());

