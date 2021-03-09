functor BoolLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Bool_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* user declarations*)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\011\000\002\000\010\000\003\000\009\000\009\000\008\000\
\\013\000\007\000\000\000\
\\001\000\001\000\011\000\002\000\010\000\003\000\009\000\013\000\007\000\000\000\
\\001\000\010\000\026\000\000\000\
\\001\000\011\000\028\000\000\000\
\\001\000\012\000\025\000\000\000\
\\001\000\014\000\000\000\000\000\
\\031\000\000\000\
\\032\000\001\000\011\000\002\000\010\000\003\000\009\000\009\000\008\000\
\\013\000\007\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\004\000\016\000\005\000\015\000\006\000\014\000\007\000\013\000\
\\008\000\012\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\"
val actionRowNumbers =
"\007\000\016\000\011\000\009\000\
\\006\000\000\000\001\000\001\000\
\\018\000\017\000\001\000\001\000\
\\001\000\001\000\001\000\004\000\
\\002\000\020\000\015\000\014\000\
\\013\000\012\000\010\000\019\000\
\\000\000\003\000\000\000\008\000\
\\005\000"
val gotoT =
"\
\\001\000\004\000\002\000\003\000\003\000\002\000\004\000\001\000\
\\005\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\015\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\002\000\016\000\003\000\002\000\004\000\001\000\000\000\
\\004\000\017\000\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\000\000\
\\004\000\019\000\000\000\
\\004\000\020\000\000\000\
\\004\000\021\000\000\000\
\\002\000\022\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\025\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\001\000\027\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 29
val numrules = 15
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
 | CONST of unit ->  (string) | ID of unit ->  (string)
 | START of unit ->  (string list) | F4 of unit ->  (string list)
 | F3 of unit ->  (string list) | F2 of unit ->  (string list)
 | F1 of unit ->  (string list)
end
type svalue = MlyValue.svalue
type result = string list
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
fn (T 13) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "CONST"
  | (T 2) => "NOT"
  | (T 3) => "IMPLIES"
  | (T 4) => "AND"
  | (T 5) => "OR"
  | (T 6) => "XOR"
  | (T 7) => "EQUALS"
  | (T 8) => "IF"
  | (T 9) => "THEN"
  | (T 10) => "ELSE"
  | (T 11) => "RPAREN"
  | (T 12) => "LPAREN"
  | (T 13) => "EOF"
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
of  ( 0, ( ( _, ( MlyValue.F1 F11, F11left, F11right)) :: rest671)) =>
 let val  result = MlyValue.START (fn _ => let val  (F1 as F11) = F11
 ()
 in (F1)
end)
 in ( LrTable.NT 4, ( result, F11left, F11right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.START (fn _ => ([]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.F1 F12, _, F12right)) :: _ :: ( _, ( 
MlyValue.F1 F11, _, _)) :: _ :: ( _, ( MlyValue.F2 F21, _, _)) :: ( _,
 ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.F1 (fn _
 => let val  (F2 as F21) = F21 ()
 val  (F1 as F11) = F11 ()
 val  F12 = F12 ()
 in (
 ["IF IF"]@F21@["THEN THEN"]@F11@["ELSE ELSE"]@F12@["F1 -> IF F2 THEN F1 ELSE F1"] 
)
end)
 in ( LrTable.NT 0, ( result, IF1left, F12right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.F2 F21, F21left, F21right)) :: rest671)) =>
 let val  result = MlyValue.F1 (fn _ => let val  (F2 as F21) = F21 ()
 in (F2@["F1 -> F2"])
end)
 in ( LrTable.NT 0, ( result, F21left, F21right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.F2 F21, _, F21right)) :: _ :: ( _, ( 
MlyValue.F3 F31, F31left, _)) :: rest671)) => let val  result = 
MlyValue.F2 (fn _ => let val  (F3 as F31) = F31 ()
 val  (F2 as F21) = F21 ()
 in ( F31@["IMPLIES IMPLIES"]@F21@["F2 -> F3 IMPLIES F2"] )
end)
 in ( LrTable.NT 1, ( result, F31left, F21right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.F3 F31, F31left, F31right)) :: rest671)) =>
 let val  result = MlyValue.F2 (fn _ => let val  (F3 as F31) = F31 ()
 in (F3@["F2 -> F3"])
end)
 in ( LrTable.NT 1, ( result, F31left, F31right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.F4 F41, _, F41right)) :: _ :: ( _, ( 
MlyValue.F3 F31, F31left, _)) :: rest671)) => let val  result = 
MlyValue.F3 (fn _ => let val  (F3 as F31) = F31 ()
 val  (F4 as F41) = F41 ()
 in ( F31@["AND AND"]@F41@["F3 -> F3 AND F4"] )
end)
 in ( LrTable.NT 2, ( result, F31left, F41right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.F4 F41, _, F41right)) :: _ :: ( _, ( 
MlyValue.F3 F31, F31left, _)) :: rest671)) => let val  result = 
MlyValue.F3 (fn _ => let val  (F3 as F31) = F31 ()
 val  (F4 as F41) = F41 ()
 in (F31@["OR OR"]@F41@["F3 -> F3 XOR F4"] )
end)
 in ( LrTable.NT 2, ( result, F31left, F41right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.F4 F41, _, F41right)) :: _ :: ( _, ( 
MlyValue.F3 F31, F31left, _)) :: rest671)) => let val  result = 
MlyValue.F3 (fn _ => let val  (F3 as F31) = F31 ()
 val  (F4 as F41) = F41 ()
 in ( F31@["XOR XOR"]@F41@["F3 -> F3 EQUALS F4"])
end)
 in ( LrTable.NT 2, ( result, F31left, F41right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.F4 F41, _, F41right)) :: _ :: ( _, ( 
MlyValue.F3 F31, F31left, _)) :: rest671)) => let val  result = 
MlyValue.F3 (fn _ => let val  (F3 as F31) = F31 ()
 val  (F4 as F41) = F41 ()
 in ( F31@["EQUALS EQUALS"]@F41@["F3 -> F3 EQUALS F4"] )
end)
 in ( LrTable.NT 2, ( result, F31left, F41right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.F4 F41, F41left, F41right)) :: rest671)) =>
 let val  result = MlyValue.F3 (fn _ => let val  (F4 as F41) = F41 ()
 in (F4@["F3 -> F4"])
end)
 in ( LrTable.NT 2, ( result, F41left, F41right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.F4 (fn _ => let val  (ID as ID1) = ID1 ()
 in (["ID "^ ID1, "F4 -> ID"])
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.F4 (fn _ => let val  (CONST
 as CONST1) = CONST1 ()
 in (["CONST "^CONST1, "F4 -> CONST"])
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 13, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.F1 F11, _, _
)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.F4 (fn _ => let val  (F1 as F11) = F11 ()
 in ( ["LPAREN ("]@F11@["RPAREN )"]@["F4 -> (F1)"] )
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.F4 F41, _, F41right)) :: ( _, ( _, NOT1left
, _)) :: rest671)) => let val  result = MlyValue.F4 (fn _ => let val 
 (F4 as F41) = F41 ()
 in (["NOT NOT"]@F41@["F4 -> NOT F4"] )
end)
 in ( LrTable.NT 3, ( result, NOT1left, F41right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Bool_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
end
end
