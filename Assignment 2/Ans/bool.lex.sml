functor BoolLexFun(structure Tokens:Bool_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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

  
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\009\011\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\009\003\003\003\003\003\003\003\008\007\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\006\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 15)], trans = 0},
{fin = [(N 13),(N 15)], trans = 4},
{fin = [(N 13)], trans = 4},
{fin = [(N 6),(N 15)], trans = 0},
{fin = [(N 10),(N 15)], trans = 0},
{fin = [(N 8),(N 15)], trans = 0},
{fin = [(N 4),(N 15)], trans = 9},
{fin = [(N 4)], trans = 9},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((line := (!line) +1;
             newline := true); 
             lex())
| 10 => ((inc(pos,1); 
            update(Term.RPAREN(")")));            
             Tokens.RPAREN(!pos,!pos))
| 13 => let val yytext=yymktext() in if yytext = "AND" then ((inc(pos,3);
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
                   Tokens.ID(yytext,!pos,!pos)) end
| 15 => let val yytext=yymktext() in (inc(pos,1);
             error(yytext,!pos,!line));             
             lex() end
| 4 => (inc(pos,1);
             lex())
| 6 => ((inc(pos,1); 
             update(Term.TERM(";")));             
            Tokens.TERM(!pos,!pos))
| 8 => ((inc(pos,1);
             update(Term.LPAREN("(")));              
             Tokens.LPAREN(!pos,!pos))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
