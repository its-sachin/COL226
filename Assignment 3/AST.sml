structure AST =
struct

exception TypeError

type id = string

datatype types = Int
	| Bool
	| Arrow of types * types

datatype intop = Plus | Minus | Times 
datatype boolop = Implies | And | Or | Xor | Equals | Greaterthan | Lessthan
datatype uniop = Not | Negate

datatype const = True | False

and exp = NumExp of int
		(*  *)
    	| VarExp of id
		(* look in symbol table for id and replace it with the value of it. *)
		| ConstExp of const
		(*  *)

		| UniopExp of uniop * exp
		(* if uniop is not then exp should be bool, and if negate then it should be int *)
		| IbinopExp of intop * exp * exp
		(* both exp shouldd be int *)
        | BbinopExp of boolop * exp * exp
		(* both exp should be bool *)

		| FnAbs of id * types * types * exp
		| FunAbs of id * id * types * types * exp
		| AppExp of exp * exp
		| LetExp of id * exp * exp
		(* add id and its value in symbol table *)
		| IfExp of exp * exp * exp
		(* exp1 should be bool and exp2 and ex3 should be of same types *)

datatype formula = OneExp of exp
	| AllExp of exp * formula

datatype value = IntVal of int
	       		| BoolVal of const
				| FunVal of id*types*types*exp

type symbolTable = (id * value) list

type typeTable = (id*types) list


fun strToConst a = 
	case a of
		"TRUE" => True
		| "FALSE" => False
		| _ => raise TypeError

fun checkNum(a:exp):exp = 
	case a of 
		IbinopExp(oper, NumExp i1, NumExp i2) =>
			(case oper of
				Plus => NumExp(i1+i2)
				| Minus => NumExp(i1-i2)
				| Times => NumExp (i1*i2))

		| UniopExp(Negate, NumExp i) => NumExp(~1*i)

		| _ => raise TypeError



fun appendID (var:id, v:value, list:symbolTable) = (var,v)::list
fun appendType(var:id, t:types, list:typeTable) = (var,t)::list


fun findSymbol(var:id, env:symbolTable) =
    case List.find(fn (x, _) => x = var) env of
		SOME (x, v)   => v
	|   NONE => raise TypeError
	

end


