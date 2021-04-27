structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun evalExp(e:exp, env:environment):value =
    case e of
	    NumExp i                    => IntVal i
      | StringExp s                 => StringVal s
      | VarExp x                    => envLookup (x, env) 				  
      | BinExp (b, e1, e2)          => evalBinExp(b, e1, e2, env)
      | LetExp(ValDecl(x, e1), e2)  =>
                                        let
                                            val v1 = evalExp (e1, env)
                                        in
                                            evalExp(e2, envAdd (x, v1, env))
                                            end		   
and


evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
    case (b, evalExp(e1, env), evalExp(e2, env))  of
        (Add, IntVal i1, IntVal i2)      => IntVal (i1+i2)
    |   (Sub, IntVal i1, IntVal i2)      => IntVal (i1-i2)
    |   (Mul, IntVal i1, IntVal i2)      => IntVal (i1*i2)
    |   (Div, IntVal i1, IntVal i2)      => IntVal (i1 div i2)
    |   (Eq, IntVal i1, IntVal i2)       => BoolVal (i1 = i2)
    |   (Eq, StringVal s1, StringVal s2) => BoolVal (s1 = s2)
    |   _  => raise brokenTypes  					    
end
