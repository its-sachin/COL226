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
\\001\000\001\000\017\000\002\000\016\000\003\000\015\000\009\000\014\000\
\\013\000\052\000\014\000\013\000\020\000\012\000\023\000\011\000\
\\024\000\010\000\029\000\009\000\030\000\008\000\000\000\
\\001\000\001\000\017\000\002\000\016\000\003\000\015\000\009\000\014\000\
\\013\000\054\000\014\000\013\000\020\000\012\000\023\000\011\000\
\\024\000\010\000\029\000\009\000\030\000\008\000\000\000\
\\001\000\001\000\017\000\002\000\016\000\003\000\015\000\009\000\014\000\
\\014\000\013\000\020\000\012\000\023\000\011\000\024\000\010\000\
\\029\000\009\000\030\000\008\000\000\000\
\\001\000\001\000\017\000\002\000\016\000\003\000\015\000\014\000\013\000\
\\020\000\012\000\023\000\011\000\000\000\
\\001\000\001\000\029\000\000\000\
\\001\000\001\000\031\000\000\000\
\\001\000\001\000\049\000\000\000\
\\001\000\001\000\056\000\000\000\
\\001\000\010\000\055\000\000\000\
\\001\000\011\000\066\000\000\000\
\\001\000\012\000\078\000\000\000\
\\001\000\013\000\069\000\031\000\068\000\000\000\
\\001\000\013\000\073\000\031\000\068\000\000\000\
\\001\000\013\000\076\000\031\000\068\000\000\000\
\\001\000\014\000\030\000\000\000\
\\001\000\014\000\048\000\000\000\
\\001\000\014\000\064\000\033\000\063\000\034\000\062\000\000\000\
\\001\000\016\000\000\000\000\000\
\\001\000\025\000\065\000\000\000\
\\001\000\026\000\077\000\000\000\
\\001\000\027\000\050\000\000\000\
\\001\000\028\000\057\000\000\000\
\\001\000\028\000\060\000\000\000\
\\001\000\028\000\075\000\000\000\
\\001\000\028\000\079\000\000\000\
\\001\000\031\000\068\000\032\000\082\000\000\000\
\\001\000\031\000\068\000\032\000\083\000\000\000\
\\087\000\000\000\
\\088\000\015\000\028\000\000\000\
\\089\000\001\000\017\000\002\000\016\000\003\000\015\000\009\000\014\000\
\\014\000\013\000\020\000\012\000\023\000\011\000\024\000\010\000\
\\029\000\009\000\030\000\008\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\031\000\068\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\004\000\027\000\005\000\026\000\006\000\025\000\007\000\024\000\
\\008\000\023\000\017\000\022\000\018\000\021\000\019\000\020\000\
\\021\000\019\000\022\000\018\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\"
val actionRowNumbers =
"\002\000\051\000\041\000\035\000\
\\028\000\027\000\004\000\014\000\
\\005\000\053\000\003\000\002\000\
\\002\000\003\000\054\000\052\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\029\000\015\000\
\\006\000\020\000\057\000\000\000\
\\001\000\008\000\056\000\050\000\
\\049\000\048\000\047\000\046\000\
\\045\000\044\000\043\000\042\000\
\\040\000\030\000\007\000\021\000\
\\002\000\059\000\058\000\060\000\
\\055\000\002\000\022\000\016\000\
\\018\000\009\000\016\000\011\000\
\\039\000\038\000\016\000\002\000\
\\002\000\012\000\016\000\023\000\
\\013\000\019\000\010\000\024\000\
\\036\000\016\000\037\000\031\000\
\\032\000\016\000\025\000\026\000\
\\002\000\002\000\033\000\034\000\
\\017\000"
val gotoT =
"\
\\001\000\084\000\002\000\005\000\004\000\004\000\005\000\003\000\
\\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\030\000\000\000\
\\004\000\032\000\005\000\003\000\006\000\002\000\007\000\001\000\
\\008\000\031\000\000\000\
\\004\000\033\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\007\000\034\000\000\000\
\\000\000\
\\000\000\
\\007\000\035\000\000\000\
\\007\000\036\000\000\000\
\\007\000\037\000\000\000\
\\007\000\038\000\000\000\
\\007\000\039\000\000\000\
\\007\000\040\000\000\000\
\\007\000\041\000\000\000\
\\007\000\042\000\000\000\
\\007\000\043\000\000\000\
\\005\000\044\000\006\000\002\000\007\000\001\000\000\000\
\\002\000\045\000\004\000\004\000\005\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\049\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\051\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\056\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\057\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\003\000\059\000\000\000\
\\000\000\
\\000\000\
\\003\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\068\000\000\000\
\\004\000\069\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\070\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\003\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\079\000\000\000\
\\000\000\
\\000\000\
\\004\000\082\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\004\000\083\000\005\000\003\000\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 85
val numrules = 34
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
 | NUM of unit ->  (int) | CONST of unit ->  (string)
 | ID of unit ->  (string) | app of unit ->  (AST.exp)
 | variable of unit ->  (AST.exp) | operation of unit ->  (AST.exp)
 | statement of unit ->  (AST.exp) | exp of unit ->  (AST.exp)
 | types of unit ->  (AST.types) | formula of unit ->  (AST.formula)
 | START of unit ->  (AST.formula)
end
type svalue = MlyValue.svalue
type result = AST.formula
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
fn (T 15) => true | _ => false
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
  | (T 11) => "FI"
  | (T 12) => "RPAREN"
  | (T 13) => "LPAREN"
  | (T 14) => "TERM"
  | (T 15) => "EOF"
  | (T 16) => "PLUS"
  | (T 17) => "MINUS"
  | (T 18) => "TIMES"
  | (T 19) => "NEGATE"
  | (T 20) => "GREATERTHAN"
  | (T 21) => "LESSTHAN"
  | (T 22) => "NUM"
  | (T 23) => "LET"
  | (T 24) => "IN"
  | (T 25) => "END"
  | (T 26) => "EQDEC"
  | (T 27) => "SEMICOLON"
  | (T 28) => "FN"
  | (T 29) => "FUN"
  | (T 30) => "ARROW"
  | (T 31) => "DOUBLEARROW"
  | (T 32) => "INT"
  | (T 33) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.formula formula1, formula1left, 
formula1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (formula as formula1) = formula1 ()
 in (formula)
end)
 in ( LrTable.NT 0, ( result, formula1left, formula1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (exp as exp1)
 = exp1 ()
 in (AST.OneExp(exp))
end)
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 2, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.exp exp1, 
exp1left, _)) :: rest671)) => let val  result = MlyValue.formula (fn _
 => let val  (exp as exp1) = exp1 ()
 in (AST.OneExp(exp))
end)
 in ( LrTable.NT 1, ( result, exp1left, TERM1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  
result = MlyValue.formula (fn _ => let val  (exp as exp1) = exp1 ()
 val  formula1 = formula1 ()
 in (AST.AllExp(exp,formula1))
end)
 in ( LrTable.NT 1, ( result, exp1left, formula1right), rest671)
end
|  ( 4, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.exp exp2, _, _))
 :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result
 = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.LetExp(ID,exp1,exp2))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 5, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.exp exp3, _, _))
 :: _ :: ( _, ( MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp 
exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (AST.IfExp(exp1,exp2,exp3))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.types types2, _, _)) :: _ :: _ :: ( _, ( MlyValue.types 
types1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _
, FN1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (ID as ID1) = ID1 ()
 val  types1 = types1 ()
 val  types2 = types2 ()
 val  (exp as exp1) = exp1 ()
 in (AST.FnAbs(ID,types1,types2,exp))
end)
 in ( LrTable.NT 3, ( result, FN1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.types types2, _, _)) :: _ :: _ :: ( _, ( MlyValue.types 
types1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.exp (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  types1 = types1 ()
 val  types2 = types2 ()
 val  (exp as exp1) = exp1 ()
 in (AST.FunAbs(ID1,ID2,types1,types2,exp))
end)
 in ( LrTable.NT 3, ( result, FUN1left, exp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (statement as statement1) = statement1 ()
 in (statement)
end)
 in ( LrTable.NT 3, ( result, statement1left, statement1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.types types2, _, types2right)) :: _ :: ( _, 
( MlyValue.types types1, types1left, _)) :: rest671)) => let val  
result = MlyValue.types (fn _ => let val  types1 = types1 ()
 val  types2 = types2 ()
 in (AST.Arrow(types1, types2))
end)
 in ( LrTable.NT 2, ( result, types1left, types2right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.types types1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.types (fn _ => let val  (types as types1) = types1 ()
 in (types)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 11, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.types (fn _ => (AST.Int))
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 12, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.types (fn _ => (AST.Bool))
 in ( LrTable.NT 2, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.statement (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (statement as statement1) = statement1 ()
 in (AST.BbinopExp(AST.Implies,operation,statement))
end)
 in ( LrTable.NT 4, ( result, operation1left, statement1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.operation operation1, operation1left, 
operation1right)) :: rest671)) => let val  result = MlyValue.statement
 (fn _ => let val  (operation as operation1) = operation1 ()
 in (operation)
end)
 in ( LrTable.NT 4, ( result, operation1left, operation1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.BbinopExp(AST.And, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 16, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.BbinopExp(AST.Or, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 17, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.BbinopExp(AST.Xor, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 18, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.BbinopExp(AST.Equals, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 19, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.IbinopExp(AST.Plus, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 20, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.IbinopExp(AST.Minus, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 21, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.IbinopExp(AST.Times, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 22, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.BbinopExp(AST.Greaterthan, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 23, ( ( _, ( MlyValue.variable variable1, _, variable1right)) ::
 _ :: ( _, ( MlyValue.operation operation1, operation1left, _)) :: 
rest671)) => let val  result = MlyValue.operation (fn _ => let val  (
operation as operation1) = operation1 ()
 val  (variable as variable1) = variable1 ()
 in (AST.BbinopExp(AST.Lessthan, operation, variable))
end)
 in ( LrTable.NT 5, ( result, operation1left, variable1right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.variable variable1, variable1left, 
variable1right)) :: rest671)) => let val  result = MlyValue.operation
 (fn _ => let val  (variable as variable1) = variable1 ()
 in (variable)
end)
 in ( LrTable.NT 5, ( result, variable1left, variable1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.variable (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.variable (fn _ => let val  (NUM as 
NUM1) = NUM1 ()
 in (AST.NumExp(NUM))
end)
 in ( LrTable.NT 6, ( result, NUM1left, NUM1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.variable (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (AST.ConstExp(AST.strToConst(CONST)))
end)
 in ( LrTable.NT 6, ( result, CONST1left, CONST1right), rest671)
end
|  ( 28, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.variable (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.variable variable1, _, variable1right)) :: 
( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.variable (fn _ => let val  (variable as variable1) = 
variable1 ()
 in (AST.UniopExp(AST.Not, variable))
end)
 in ( LrTable.NT 6, ( result, NOT1left, variable1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.variable variable1, _, variable1right)) :: 
( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.variable (fn _ => let val  (variable as variable1) = 
variable1 ()
 in (AST.UniopExp(AST.Negate, variable))
end)
 in ( LrTable.NT 6, ( result, NEGATE1left, variable1right), rest671)

end
|  ( 31, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.app app1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.variable (fn _ => let val  (app as app1) = app1 ()
 in (app)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.app app1, app1left, _)) :: rest671)) => let val  result = 
MlyValue.app (fn _ => let val  (app as app1) = app1 ()
 val  (exp as exp1) = exp1 ()
 in (AST.AppExp(app,exp))
end)
 in ( LrTable.NT 7, ( result, app1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.app (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AST.AppExp(exp1,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
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
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun EQDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLEARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
