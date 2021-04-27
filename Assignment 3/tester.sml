structure TESTER  =
struct
open AST

fun operateInt oper a b=
    case oper of 
        Plus => a+b
        | Minus => a-b
        | Times => a*b


and

boolAnd(a:const, b:const):const = 
    case a of 
        False => False
        | _ => b 

and 

boolOr(a:const, b:const):const = 
    case a of 
        True => True
        | _ => b 

and 

boolXor(a:const, b:const):const = 
    if (a = b) then False else True

and 

boolEquals(a:const, b:const):const = 
    if (a = b) then True else False

and 

boolImplies(a:const, b:const):const = 
    case (a,b) of (True,False) => False
    | _ =>True

and

boolNot(a:const) = 
    case a of 
        True => False
        | False => True

and 

operateBool oper a b =
    case oper of 
        Implies => boolImplies(a,b)
        | And => boolAnd(a,b)
        | Or => boolOr(a,b)
        | Xor => boolXor(a,b)
        | Equals => boolEquals(a,b)
        | _ => raise TypeError
and
checkExp(e:exp, s:symbolTable):value =
    case e of
	    NumExp(i) => IntVal(i)
        | VarExp(i) => 	findSymbol(i,s)
        | ConstExp(c) => BoolVal(c)

        | UniopExp(oper,e1) => 
            (case checkExp(e1,s) of BoolVal(c) => 
                (case oper of 
                    Not => BoolVal(boolNot(c))
                    | Negate => raise TypeError)
            | IntVal(i) => 
                (case oper of 
                    Not => raise TypeError
                    | Negate => IntVal(~1*i))
            | _ => raise TypeError)
        
        | IbinopExp(oper, e1,e2) =>
            (case checkExp(e1,s) of IntVal(i1) => 
               (case checkExp(e2,s) of IntVal(i2) => IntVal(operateInt oper i1 i2)
               | _ => raise TypeError)
            | _ => raise TypeError)
        
        | BbinopExp(oper, e1,e2) => 
            (case oper of 
                Greaterthan => (case checkExp(e1,s) of 
                        IntVal(i1) => (case checkExp(e2,s) of 
                            IntVal(i2) => if (i1>i2) then BoolVal(True) else BoolVal(False)
                            | _ => raise TypeError)
                        | _ => raise TypeError)
                | Lessthan => (case checkExp(e1,s) of 
                        IntVal(i1) => (case checkExp(e2,s) of 
                            IntVal(i2) => if (i1<i2) then BoolVal(True) else BoolVal(False)
                            | _ => raise TypeError)
                        | _ => raise TypeError)
                | _ => (case checkExp(e1,s) of BoolVal(c1) => 
                            (case checkExp(e2,s) of BoolVal(c2) =>BoolVal(operateBool oper c1 c2)
                            | _ => raise TypeError)
                        | _ => raise TypeError))
        
        | FnAbs(i,t1,t2,e1) => FunVal(i,t1,t2,e1)

        | FunAbs(_,_,_,_,_) => raise TypeError 

        | LetExp(i1,e1,e2) => 
            (case e1 of
            FunAbs(_,_,_,_,_) => raise TypeError
            | _ => checkExp(e2, appendID(i1,checkExp(e1,s),s)))
        
        | IfExp(e1,e2,e3) => 
            (case checkExp(e1,s) of 
            IntVal(i) => raise TypeError
            | BoolVal(c) => 
                (case checkExp(e2,s) of 
                IntVal(i1) => 
                    (case checkExp(e3,s) of 
                    IntVal(i2) => 
                        (case c of 
                        True => IntVal(i1)
                        | False => IntVal(i2))
                    | _ => raise TypeError)
                | BoolVal(c1) => 
                    (case checkExp(e3,s) of 
                    BoolVal(c2) => 
                    (case c of 
                        True => BoolVal(c1)
                        | False => BoolVal(c2))
                    | _ => raise TypeError)
                | FunVal(i21,t21,t22,e21) => 
                    (case checkExp(e3,s) of
                    FunVal(i31,t31,t32,e31) => if ( t22 = t32) then 
                        (case c of 
                        True => FunVal(i21,t21,t22,e21)
                        | False => FunVal(i31,t31,t32,e31))
                        else raise TypeError
                    | _ => raise TypeError))) 

        | _ => IntVal(0)


             


and

evalFile(f:formula, s:symbolTable):value list = case f of 
    OneExp(e) => (case e of 
        FunAbs(i1,i2,t1,t2,e1) => [FunVal(i1,t1,t2,e1)]
        | _ => [checkExp(e,s)])

    | AllExp(e,f1) => (case e of 
        FunAbs(i1,i2,t1,t2,e1) => 
            (let val func = FunVal(i1,t1,t2,e1) 
                in 
                    func::evalFile(f1,appendID(i1,func,s)) 
                end)
        | _ => (checkExp(e,s)::evalFile(f1,s)))


end