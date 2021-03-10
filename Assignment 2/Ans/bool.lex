structure Tokens= Tokens
  
  type pos = int
  type line = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token


  val pos = ref 0
  val line = ref 1;
  val isError = ref false;
  val isLast = ref false;
  val eof = fn () =>( isLast:= true; Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, _) =>(isError := true; TextIO.output(TextIO.stdOut,"Unknown token:"^(Int.toString(!line))^":"^(Int.toString(l))^":"^e^ "\n"))

  val newline = ref false;


  fun inc(a,num) = 
    if (!newline = true) then (newline := false; a := num)
    else (a := (!a) + num)


  val a=  Array.array(0,Term.IF("a"))
  val array = ref a;
  (* fun update(s,a) = list := !list^s^" \""^a^"\""^","; *)
  fun aTol arr = Array.foldr (op ::) [] arr;

  fun update(s) = 
    let 
      val list = (aTol (!array))@[s]
      (* fun up(s) = if (!isFirst = true) then (isFirst := false; TextIO.output(TextIO.stdOut,"["^Term.value(s))) 
                  else TextIO.output(TextIO.stdOut,","^Term.value(s)) *)
    in
      (* (up(s); *)
      array := Array.fromList(list)
    end

  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%

\n       => ((line := (!line) +1;
             newline := true); 
             lex());

{ws}+    => (inc(pos,1);
             lex());

";"      => ((inc(pos,1); 
             update(Term.TERM(";")));             
            Tokens.TERM(!pos,!pos));

"("      => ((inc(pos,1);
             update(Term.LPAREN("(")));              
             Tokens.LPAREN(!pos,!pos));

")"      => ((inc(pos,1); 
            update(Term.RPAREN(")")));            
             Tokens.RPAREN(!pos,!pos));

{alpha}+ => (if yytext = "AND" then ((inc(pos,3);
                                    update(Term.AND("AND")));              
                                    Tokens.AND(!pos, !pos))

            else if yytext = "OR" then ((inc(pos,2); 
                                        update(Term.OR("OR")));                                        
                                        Tokens.OR(!pos,!pos))

            else if yytext = "XOR" then ((inc(pos,3); 
                                        update(Term.XOR("XOR")));                                         
                                        Tokens.XOR(!pos,!pos))

            else if yytext = "EQUALS" then ((inc(pos,6);
                                             update(Term.EQUALS("EQUALS")));                                              
                                            Tokens.EQUALS(!pos,!pos))

            else if yytext = "IF" then ((inc(pos,2); 
                                      update(Term.IF("IF")));                                      
                                       Tokens.IF(!pos,!pos))

            else if yytext = "THEN" then ((inc(pos,4);
                                          update(Term.THEN("THEN")));                                          
                                          Tokens.THEN(!pos,!pos))

            else if yytext = "ELSE" then ((inc(pos,5);
                                         update(Term.ELSE("ELSE")));                                          
                                        Tokens.ELSE(!pos,!pos))

            else if yytext = "IMPLIES" then ((inc(pos,7);
                                         update(Term.IMPLIES("IMPLIES")));                                          
                                        Tokens.IMPLIES(!pos,!pos))

            else if yytext = "NOT" then ((inc(pos,3);
                                         update(Term.IMPLIES("IMPLIES")));                                         
                                         Tokens.NOT(!pos,!pos))

            else if yytext = "TRUE" then ((inc(pos,4);
                                          update(Term.CONST("TRUE")));                                           
                                          Tokens.CONST(yytext,!pos,!pos))

            else if yytext = "FALSE" then ((inc(pos,5);
                                         update(Term.CONST("FALSE")));                                          
                                        Tokens.CONST(yytext,!pos,!pos))

            else ((inc(pos,String.size(yytext));
                   update(Term.ID(yytext)));                    
                   Tokens.ID(yytext,!pos,!pos)));

.      => ((inc(pos,1);
             error(yytext,!pos,!line));             
             lex());

