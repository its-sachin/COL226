structure EVALUATOR  =
struct
open AST

fun boolAnd(a:const, b:const):const = 
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

operateInt oper a b =
    case oper of 
        Plus => a+b
        | Minus => a-b
        | Times => a*b


and

cutOut(t1:types, t2:types):types = 

let 

    fun cutOutRec(t1:types ,t2:types, ans:types, i:int):types = 
        case t1 of 
        Arrow(t11,t12) => 
            if (t12 = t2) then 
                (if (i = 0) then t11 else Arrow(t11,ans))
            else 
            if (i =0) then cutOutRec(t12,t2,t11,1) else cutOutRec(t12,t2,Arrow(t11,ans),1)
        | _ => raise TypeError

    fun reverse(t1:types,ans:types, i:int):types = 
        case t1 of
        Int => (if (i=0) then Int else Arrow(Int,ans))
        | Bool => (if (i=0) then Bool else Arrow(Bool,ans))
        | Arrow(a,b) => (if (i=0) then reverse(b,a,1) else reverse(b,Arrow(a,ans),1))

in

reverse(cutOutRec(t1,t2,Int,0),Int,0)

end

and

checkFun(e:exp,t:typeTable,t2:types,s:symbolTable):bool= 
    case t1 of 
    Int => (
        case checkExp(e,appendID(i,IntVal(0),s)) of 
        IntVal(i1) => (if (t2 = Int) then true
            else  false)
        | BoolVal(c) => (if (t2 = Bool) then true
            else  false)
        | FunVal(i1,t11,t12,e11) => (if (t2 = t12) then true
            else  false)
    )

    | Bool => (
        case checkExp(e,appendID(i,BoolVal(True),s)) of 
        IntVal(i1) => (if (t2 = Int) then true
            else  false)
        | BoolVal(c) => (if (t2 = Bool) then true
            else  false)
        | FunVal(i1,t11,t12,e11) => (if (t2 = t12) then true
            else  false)
    )

    | Arrow(t11,t12) => (
        case e of 
        AppExp(e1,e2) =>(
            (case e1 of 
            VarExp(a) => if (i=a) then checkFun(e2,i,t1,cutOut(t1,t2),s) else
                (case findSymbol(a,s) of
                FunVal(i2,t21,t22,e21) => (if (t22 = t2) then 
                    checkFun(e2,i,t1,t21,s)
                    else false)
                | _ => false)
            | FnAbs(i2,t21,t22,e2) => (if (t22 = t2) then 
            checkFun(e2,i,t1,t21,s)
            else false)
            | _ => false)
        )
        | NumExp(a) => if (t2 = Int) then true else false
        | ConstExp(a) => if (t2 = Bool) then true else false
        | UniopExp(oper,e1) => 
            (case oper of 
            NOT => if (t2 = Bool) then checkFun(e1,i,t1,Bool,s)
                else false
            | NEGATE => if (t2 = Int) then checkFun(e1,i,t1,Int,s)
                else false)
        | VarExp(a) => if (i=a) then (if (t1=t2) then true else false) else 
            (case findSymbol(a,s) of 
            IntVal(i1) => if (t2=Int) then true else false
            | BoolVal(c) => if (t2=Bool) then true else false
            | FunVal(i2,t21,t22,e21) => if (t22 = t2) then checkFun())
        | IbinopExp(oper, e1,e2) => if (t2 = Int) then checkFun(e1,i,t1,Int,s) andalso checkFun(e2,i,t1,Int,s) else false
        | BbinopExp(oper, e1,e2) => if (t2 = Bool) then checkFun(e1,i,t1,Bool,s) andalso checkFun(e2,i,t1,Bool,s) else false
        | LetExp(i1,e1,e2) => (case e1 of
            FunAbs(_,_,_,_,_) => raise TypeError
            | _ => checkFun(e2, i,t1,t2,appendID(i1,checkExp(e1,s),s)))
        | FnAbs(i,t1,t2,e1) => ..
        | IfExp(e1,e2,e3) => checkFun(e1,i,t1,Bool,s) andalso ((checkFun(e2,i,t1,Bool,s) andalso checkFun(e3,i,t1,Bool,s)) orelse ((checkFun(e2,i,t1,Int,s) andalso checkFun(e3,i,t1,Int,s))) 
        | _ => false
    )

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
        
        | FnAbs(i,t1,t2,e1) => if (checkFun(e1,i,t1,t2,s)) then FunVal(i,t1,t2,e1) else raise TypeError

        | FunAbs(_,_,_,_,_) => raise TypeError 

        | AppExp(e1,e2) => 
            (case checkExp(e1,s) of 
            FunVal(id1,t11,t12,e11) =>
                (case checkExp(e2,s) of
                IntVal(i1) => if (t11 = Int) then 
                    checkExp(e11,appendID(id1,IntVal(i1),s))
                    else raise TypeError
                | BoolVal(c) => if (t11 = Bool) then 
                    checkExp(e11,appendID(id1,BoolVal(c),s))
                    else raise TypeError
                | FunVal(id2,t21,t22,e21) => if (t11 = Arrow(t21,t22)) then
                    checkExp(e11, appendID(id1,FunVal(id2,t21,t22,e21),s))
                    else raise TypeError
                )
            | _ => raise TypeError
            )

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


             


and

evalFile(f:formula, s:symbolTable):value list = case f of 
    OneExp(e) => (case e of 
        FunAbs(i1,i2,t1,t2,e1) => if (checkFun(e1,i2,t1,t2,s)) then [FunVal(i2,t1,t2,e1)]
            else raise TypeError
        | _ => [checkExp(e,s)])

    | AllExp(e,f1) => (case e of 
        FunAbs(i1,i2,t1,t2,e1) => if (checkFun(e1,i2,t1,t2,s)) then
            (let val func = FunVal(i2,t1,t2,e1) 
                in 
                    func::evalFile(f1,appendID(i1,func,s)) 
                end)
            else raise TypeError
        | _ => (checkExp(e,s)::evalFile(f1,s)))

end
