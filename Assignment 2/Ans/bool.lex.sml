functor BoolLexFun(structure Tokens:Bool_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
"\000\000\000\000\000\000\000\000\000\008\009\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\000\000\000\000\000\000\000\007\006\000\000\000\000\005\000\
\\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000"
),
 (3, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\000\000\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\008\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
{fin = [(N 13)], trans = 3},
{fin = [(N 6)], trans = 0},
{fin = [(N 15)], trans = 0},
{fin = [(N 10)], trans = 0},
{fin = [(N 8)], trans = 0},
{fin = [(N 4)], trans = 8},
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

  1 => (pos := (!pos) + 1; lex())
| 10 => (Tokens.RPAREN(!pos,!pos))
| 13 => let val yytext=yymktext() in if yytext = "AND" then (Tokens.AND(!pos, !pos))
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
            else Tokens.ID(yytext,!pos,!pos) end
| 15 => let val yytext=yymktext() in error ("Unknown token:"^yytext,!pos,!pos);
             lex() end
| 4 => (lex())
| 6 => (Tokens.EOF(!pos,!pos))
| 8 => (Tokens.LPAREN(!pos,!pos))
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
