functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\006\000\002\000\005\000\011\000\004\000\000\000\
\\001\000\001\000\020\000\000\000\
\\001\000\003\000\011\000\004\000\010\000\005\000\009\000\006\000\008\000\
\\010\000\007\000\013\000\023\000\000\000\
\\001\000\009\000\000\000\000\000\
\\001\000\010\000\022\000\000\000\
\\001\000\012\000\019\000\000\000\
\\001\000\014\000\013\000\000\000\
\\026\000\003\000\011\000\004\000\010\000\005\000\009\000\006\000\008\000\
\\010\000\007\000\000\000\
\\027\000\003\000\011\000\004\000\010\000\005\000\009\000\006\000\008\000\
\\010\000\007\000\000\000\
\\028\000\000\000\
\\029\000\000\000\
\\030\000\004\000\010\000\006\000\008\000\000\000\
\\031\000\004\000\010\000\006\000\008\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\003\000\011\000\004\000\010\000\005\000\009\000\006\000\008\000\000\000\
\"
val actionRowNumbers =
"\000\000\007\000\006\000\009\000\
\\010\000\000\000\000\000\000\000\
\\000\000\000\000\005\000\001\000\
\\016\000\014\000\012\000\013\000\
\\011\000\000\000\004\000\002\000\
\\000\000\015\000\008\000\003\000"
val gotoT =
"\
\\001\000\001\000\002\000\023\000\000\000\
\\000\000\
\\003\000\010\000\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\000\000\
\\001\000\013\000\000\000\
\\001\000\014\000\000\000\
\\001\000\015\000\000\000\
\\001\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\019\000\000\000\
\\000\000\
\\000\000\
\\001\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 24
val numrules = 10
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | DECL of unit ->  (AST.decl) | START of unit ->  (AST.exp)
 | EXP of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 8) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "SUB"
  | (T 5) => "DIV"
  | (T 6) => "RPAREN"
  | (T 7) => "LPAREN"
  | (T 8) => "EOF"
  | (T 9) => "EQ"
  | (T 10) => "LET"
  | (T 11) => "IN"
  | (T 12) => "END"
  | (T 13) => "VAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1)
 = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671)) => let
 val  result = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.ValDecl(ID, EXP))
end)
 in ( LrTable.NT 2, ( result, VAL1left, EXP1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671))
 => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Add, EXP1,  EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Sub,  EXP1,  EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Mul,  EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Div, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _))
 :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (
DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AST.LetExp(DECL, EXP))
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (AST.BinExp(AST.Eq, EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
end
end
