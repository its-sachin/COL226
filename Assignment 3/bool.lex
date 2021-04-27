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

  fun strToInt input = List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode input);

  fun inc(a,num) = 
    if (!newline = true) then (newline := false; a := num)
    else (a := (!a) + num)


  val a=  Array.array(0,"a")
  val array = ref a;
  (* fun update(s,a) = list := !list^s^" \""^a^"\""^","; *)
  fun aTol arr = Array.foldr (op ::) [] arr;

  fun update(s,a) = 
    let 
      val list = (aTol (!array))@[s^" \""^a^"\""]
      (* fun up(s) = if (!isFirst = true) then (isFirst := false; TextIO.output(TextIO.stdOut,"["^Term.value(s))) 
                  else TextIO.output(TextIO.stdOut,","^Term.value(s)) *)
    in
      (* (up(s); *)
      array := Array.fromList(list)
    end

  val tokenList = 
  [
    ("if", Tokens.IF),
    ("then", Tokens.THEN),
    ("else", Tokens.ELSE),
    ("fi", Tokens.FI),
    ("IMPLIES", Tokens.IMPLIES),
    ("AND", Tokens.AND),
    ("OR", Tokens.OR),
    ("XOR", Tokens.XOR),
    ("EQUALS", Tokens.EQUALS),    
    ("NOT", Tokens.NOT),
    ("let", Tokens.LET),
    ("in", Tokens.IN),
    ("end", Tokens.END),
    ("PLUS", Tokens.PLUS),
    ("MINUS", Tokens.MINUS),
    ("TIMES", Tokens.TIMES),
    ("NEGATE", Tokens.NEGATE),
    ("GREATERTHAN", Tokens.GREATERTHAN),
    ("LESSTHAN", Tokens.LESSTHAN),
    ("fn", Tokens.FN),
    ("fun", Tokens.FUN),
    ("int", Tokens.INT),
    ("bool", Tokens.BOOL)

  ]

  fun findToken(str:string, pos1:pos, pos2:pos) =
    case List.find (fn (s, _) => s = str )  tokenList of 
        SOME (_, tk) => 
          
          ((inc(pos,size(str));
            update(str, str));
            tk(pos1, pos2))

      | NONE => ((inc(pos,size(str));
                  update("ID", str));
                  Tokens.ID(str, pos1, pos2))

  
%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha=[A-Za-z][0-9]*;
ws = [\ \t];
num=[0-9];
lb = [\n \r\n];
%%

{lb}      => ((line := (!line) +1;
             newline := true); 
             lex());

{ws}+    => (inc(pos,1);
             lex());

";"      => ((inc(pos,1); 
             update("TERM", ";"));             
            Tokens.TERM(!pos,!pos));

"("      => ((inc(pos,1);
             update("LPAREN", "("));              
             Tokens.LPAREN(!pos,!pos));

")"      => ((inc(pos,1); 
            update("RPAREN", ")"));            
             Tokens.RPAREN(!pos,!pos));

{num}+    => ((inc(pos,size(yytext));
              update("NUM", Int.toString(strToInt(yytext))));
              Tokens.NUM(strToInt(yytext), !pos, !pos));

":"       =>  ((inc(pos,1);
                update("SEMICOLON", ":"));                                          
                Tokens.SEMICOLON(!pos,!pos));

"=>"       => ((inc(pos,2);
              update("DOUBLEARROW", "=>"));                                          
              Tokens.DOUBLEARROW(!pos,!pos));

"->"       => ((inc(pos,2);
              update("ARROW", "->"));                                          
              Tokens.ARROW(!pos,!pos));

"="       => ((inc(pos,1);
              update("EQDEC", "="));                                          
              Tokens.EQDEC(!pos,!pos));

{alpha}+  => (if (yytext = "TRUE" orelse yytext = "FALSE") then 

              ((inc(pos,size(yytext));
              update("CONST", yytext));                                          
              Tokens.CONST(yytext, !pos,!pos))

              else findToken(yytext,!pos,!pos));

.         => ((inc(pos,1);
                error(yytext,!pos,!line));             
                lex());

