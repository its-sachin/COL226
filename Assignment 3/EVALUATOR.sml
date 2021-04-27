structure EVALUATOR  =
struct
open AST

fun checkType(e:exp,t:typeTable,s:symbolTable):types= 
    (
        (* TextIO.output(TextIO.stdOut,"Type: "^printTypeTable(t)^"\n"); *)
    case e of 
    NumExp(_) => Int

    | VarExp(i) => findType(i,t,s)

    | ConstExp(_) => Bool

    | UniopExp(oper,e1) => (
        case oper of 
        Not => let val given = checkType(e1,t,s) in
            if (given = Bool) then Bool else 
                handleEx ("Type mismatch for NOT: expected: [bool] given: [" ^ typeToStr(given) ^ "]")
            end
        | Negate => let val given = checkType(e1,t,s) in
            if (given = Int) then Int else 
                handleEx ("Type mismatch for NEGATE: expected: [int] given: [" ^ typeToStr(given) ^ "]")
            end
    )

    | IbinopExp(oper,e1,e2) => if ((checkType(e1,t,s) =Int) andalso (checkType(e2,t,s) =Int)) then Int else 
        handleEx ("Type mismatch for " ^ ioperToStr(oper))

    | BbinopExp(oper,e1,e2) => 
        if (oper = Greaterthan orelse oper = Lessthan) then 
            if ((checkType(e1,t,s) =Int) andalso (checkType(e2,t,s) =Int)) then Bool else 
            handleEx ("Type mismatch for " ^ boperToStr(oper))
        else if (oper = Equals) then 
            if (((checkType(e1,t,s) =Int) andalso (checkType(e2,t,s) =Int)) orelse ((checkType(e1,t,s) =Bool) andalso (checkType(e2,t,s) =Bool))) then Bool else 
            handleEx ("Type mismatch for " ^ boperToStr(oper))
        else
            if ((checkType(e1,t,s) =Bool) andalso (checkType(e2,t,s) =Bool)) then Bool else 
            handleEx ("Type mismatch for " ^ boperToStr(oper))

    | LetExp(i,e1,e2) => (case e1 of
        FunAbs(_,_,_,_,_) =>   
            handleEx ("Named function declaration inside expression")
        | _ => checkType(e2,appendType(i,checkType(e1,t,s),t),s))

    | IfExp(e1,e2,e3) => let 
            val g1 = checkType(e1,t,s) 
        in
            if (g1 = Bool) then (
            let val a = checkType(e2,t,s)
                val b = checkType(e3,t,s)
            in
            if (a=b) then a else 
                handleEx ("Type mismatch for if expression: e2 = [" ^ typeToStr(a) ^ "] ,e3 = [" ^ typeToStr(b) ^ "]")
            end
            )else handleEx ("Type mismatch for if expression for e1 expected: [bool] given: ["^ typeToStr(g1) ^"]")
        end

    | FnAbs(i,t21,t22,e2) => let 
            val g = checkType(e2,appendType(i,t21,t),s) 
        in
            if ( g= t22) then t22 else 
            handleEx ("Type mismatch for FN: expected: [" ^ typeToStr(t22) ^"] given: [" ^ typeToStr(g) ^ "]")
        end

    | FunAbs(_,_,_,_,_) => handleEx ("Named function declaration inside expression")

    | AppExp(e1,e2) => (
        case checkType(e1,t,s) of 
        Arrow(t11,t12) => (
            let val t2 = checkType(e2,t,s)
            in
                cutOut(t11,t12,t2)
            end
        )
        | _ => handleEx ("Function expected for application")
        )
    )
and



evalExp(e:exp, s:symbolTable):value =
    let 

        val temp=  Array.array(0,("a",BoolVal(True)))
        val global = ref temp;
        fun aTol arr = Array.foldr (op ::) [] arr;

        fun updateGlobal(var:id, v:value) = 
            let 
            val list = (var,v)::(aTol (!global))
            in
            global := Array.fromList(list)
            end

        fun findAll(var,loc) = 
            case List.find(fn (x, _) => x = var) loc of
            SOME (x, v)   => v
            | NONE => 
                let val s = aTol (!global)
                in
                    (case List.find(fn (x, _) => x = var) s of
                    SOME (x,v) => v
                    | NONE => handleEx( "Unidentified variable " ^ var))
                end

    fun checkExp(e,s) = 
    (
        (* TextIO.output(TextIO.stdOut,"Symbol: "^printTable(s)^"\n"^"Global: "^printTable(aTol(!global))^"\n"); *)
    case e of
	    NumExp(i) => IntVal(i)
        | VarExp(i) => 	findAll(i,s)
        | ConstExp(c) => BoolVal(c)

        | UniopExp(oper,e1) => 
            (case checkExp(e1,s) of 
            BoolVal(c) => 
                (case oper of 
                    Not => BoolVal(boolNot(c))
                    | Negate => 
                        handleEx ("Type mismatch for NEGATE: expected: [int] given: [bool]"))
            | IntVal(i) => 
                (case oper of 
                    Not => 
                        handleEx ("Type mismatch for NOT: expected: [bool] given: [int]")
                    | Negate => IntVal(~1*i))
            | _ => handleEx ("Type mismatch for " ^ uoperToStr(oper) ^": expected: [bool] or [int]" ))
        
        | IbinopExp(oper, e1,e2) =>
            (case checkExp(e1,s) of IntVal(i1) => 
               (case checkExp(e2,s) of IntVal(i2) => IntVal(operateInt oper i1 i2)
               | _ => handleEx ("Type mismatch for " ^ ioperToStr(oper) ^": expected: [int]" ))
            | _ => handleEx ("Type mismatch for " ^ ioperToStr(oper)^ ": expected: [int]" ))
        
        | BbinopExp(oper, e1,e2) => 
            (case oper of 
                Greaterthan => (case checkExp(e1,s) of 
                        IntVal(i1) => (case checkExp(e2,s) of 
                            IntVal(i2) => if (i1>i2) then BoolVal(True) else BoolVal(False)
                            | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [int]" ))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [int]" ))
                | Lessthan => (case checkExp(e1,s) of 
                        IntVal(i1) => (case checkExp(e2,s) of 
                            IntVal(i2) => if (i1<i2) then BoolVal(True) else BoolVal(False)
                            | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [int]" ))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [int]" ))

                | Equals => (case checkExp(e1,s) of 
                    IntVal(i1) => (case checkExp(e2,s) of 
                        IntVal(i2) =>if (i1=i2) then BoolVal(True) else BoolVal(False)
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [int]" ))
                    | BoolVal(c1) => (case checkExp(e2,s) of 
                        BoolVal(c2) => BoolVal(boolEquals(c1,c2))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [bool]" ))
                    | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [int] or [bool]" ))

                | _ => (case checkExp(e1,s) of BoolVal(c1) => 
                            (case checkExp(e2,s) of BoolVal(c2) =>BoolVal(operateBool oper c1 c2)
                            | _ => handleEx ("Type mismatch for " ^ boperToStr(oper)^ ": expected: [bool]" ))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^": expected: [bool]" )))
        
        | FnAbs(i,t1,t2,e1) => 
            let val g = checkType(e1,initType(i,t1,s,aTol(!global)),s)
            in
                if ( g= t2) 
                then FunVal(i,t1,t2,e1) 
                else handleEx ("Type mismatch for fn abstraction: given: [" ^typeToStr(g)^ "] expected: [" ^ typeToStr(t2)^ "]")
            end

        | FunAbs(_,_,_,_,_) => handleEx ("Named function declaration inside expression")

        | AppExp(e1,e2) => 
            (case checkExp(e1,s) of 
            FunVal(id1,t11,t12,e11) =>
                (case checkExp(e2,s) of
                IntVal(i1) => if (t11 = Int) then 
                    (updateGlobal(id1,IntVal(i1)); checkExp(e11,s))
                    else handleEx ("Type mismatch for function application: expected: ["^ typeToStr(t11) ^"] given: [int]")
                | BoolVal(c) => if (t11 = Bool) then 
                    (updateGlobal(id1,BoolVal(c)); checkExp(e11,s))
                    else handleEx ("Type mismatch for function application: expected: ["^ typeToStr(t11) ^"] given: [bool]")
                | FunVal(id2,t21,t22,e21) => if (t11 = Arrow(t21,t22)) then
                    (updateGlobal(id1,FunVal(id2,t21,t22,e21)); checkExp(e11,s))
                    else handleEx ("Type mismatch for function application: expected: ["^ typeToStr(t11) ^"] given: [" ^typeToStr(Arrow(t21,t22))^ "]")
                )
            | _ => handleEx ("Function expected for application"))
            

        | LetExp(i1,e1,e2) => 
            (case e1 of
            FunAbs(_,_,_,_,_) => handleEx ("Named function declaration inside expression")
            | _ => checkExp(e2, appendID(i1,checkExp(e1,s),s)))
        
        | IfExp(e1,e2,e3) => 
            let 
            val temp = checkType(e,initTypeEmp(s,aTol(!global)),s) 
            val BoolVal(c) = checkExp(e1,s)
            in
                (case c of 
                True => checkExp(e2,s)
                | False => checkExp(e3,s))
            end
        )
        in

        checkExp(e,s)

        end


and

evalFile(f:formula, s:symbolTable):value list = case f of 
    OneExp(e) => (case e of 
        FunAbs(i1,i2,t1,t2,e1) => 
            let val g = checkType(e1,initTypeFun(i1,i2,t1,t2,s),s)
            in
                if (g = t2) 
                then [FunVal(i2,t1,t2,e1)]
                else handleEx ("Type mismatch for fun abstraction: given: [" ^typeToStr(g)^ "] expected: [" ^ typeToStr(t2)^ "]")
            end
        | _ => [evalExp(e,s)])

    | AllExp(e,f1) => (case e of 
        FunAbs(i1,i2,t1,t2,e1) => 
            let val g = checkType(e1,initTypeFun(i1,i2,t1,t2,s),s)
            in
                if (g = t2) then
                    (let val func = FunVal(i2,t1,t2,e1) 
                        in 
                            func::evalFile(f1,appendID(i1,func,s)) 
                        end)
                else handleEx ("Type mismatch for fun abstraction: given: [" ^typeToStr(g)^ "] expected: [" ^ typeToStr(t2)^ "]")
            end
        | _ => (evalExp(e,s)::evalFile(f1,s)))


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
        | _ => handleEx "Invalid Operator for bool"
and

operateInt oper a b =
    case oper of 
        Plus => a+b
        | Minus => a-b
        | Times => a*b


and

cutOut(t11:types, t12:types,t2:types):types = 

let 

    fun cutOutRec(t11:types ,t12:types, t2:types):types = 
        case t2 of 
        Int => if (t11 = Int) then t12 else handleEx "Type Mismatch for function application"
        | Bool => if (t11 = Bool) then t12 else handleEx "Type Mismatch for function application"
        | Arrow(t21,t22) => if (t11 =t12) then (
            case t12 of 
            Arrow(t121,t122) => cutOutRec(t121,t122,t22) 
            | _ => handleEx "Type Mismatch for function application"
        )else handleEx "Type Mismatch for function application"

    (* fun reverse(t1:types,ans:types, i:int):types = 
        case t1 of
        Int => (if (i=0) then Int else Arrow(Int,ans))
        | Bool => (if (i=0) then Bool else Arrow(Bool,ans))
        | Arrow(a,b) => (if (i=0) then reverse(b,a,1) else reverse(b,Arrow(a,ans),1)) *)

in
cutOutRec(t11,t12,t2)

end

and

typeToStr(t:types) =
    case t of 
    Int => "int"
    | Bool => "bool"
    | Arrow(t1,t2) => "Arrow("^typeToStr(t1)^","^typeToStr(t2)^")"

and

boperToStr(a:boolop) =
    case a of
    Implies => "IMPLIES"
    | And => "AND"
    | Or => "OR"
    | Xor => "XOR"
    | Equals => "EQUALS"
    | Greaterthan =>  "GREATERTHAN"
    | Lessthan => "LESSTHAN"

and

ioperToStr(a:intop) =
    case a of
    Plus => "PLUS" 
    | Minus => "MINUS"
    | Times  => "TIMES"

and

uoperToStr(a:uniop) =
    case a of
    Not => "NOT" 
    | Negate => "NEGATE"
and

getType(var:value):types = 
    case var of 
    IntVal(i) => Int
    | BoolVal(i) => Bool
    | FunVal(i,t1,t2,e) => Arrow(t1,t2)

and

findType(var:id, env:typeTable, s:symbolTable):types = 
	case List.find(fn (x, _) => x = var) env of
		SOME (x, v)   => v
	|   NONE => 
        (case List.find(fn (x, _) => x = var) s of
        SOME (x,v) => 
            (case v of
            IntVal(i) => Int
            | BoolVal(i) => Bool
            | FunVal(_,t1,t2,_) => Arrow(t1,t2))
        | NONE => handleEx( "Unidentified variable " ^ var))
and

initTypeEmp(env:symbolTable, global:symbolTable) = 
	let 
		fun initTypeRec(env,ans) = 
			case env of
			[] => ans
			| [(x,y)] => (x,getType(y))::ans
            | (x,y)::xs  => initTypeRec(xs,(x,getType(y))::ans) 

	in

    initTypeRec(env,initTypeRec(global,[]))

    end

and

initType(var:id, t:types, env:symbolTable, global:symbolTable) = 

    appendType(var,t,initTypeEmp(env,global))

and

initTypeFun(name:id, var:id, t:types,out:types, env:symbolTable) = 
	let 
		fun initTypeRec(env,ans) = 
			case env of
			[] => ans
			| [(x,y)] => (x,getType(y))::ans
            | (x,y)::xs  => initTypeRec(xs,(x,getType(y))::ans) 

	in

    appendType(name,Arrow(t,out),appendType(var,t,initTypeRec(env,[])))

    end

and


printVal(v:value) = 
    case v of
    IntVal(i) => Int.toString(i)
    | BoolVal(c) => (
        case c of 
        True => "TRUE"
        | False => "FALSE"
    )
    | FunVal(i,t1,t2,e) => i^","^typeToStr(t1)^","^typeToStr(t2)^" ,some exp"

and

printTable(s:symbolTable) =
    case s of
        [] => ""
        | [(x,y)] => x^","^printVal(y)
        | (x,y)::xs  => x^","^printVal(y)^" | "^printTable(xs)

and

printTypeTable(s:typeTable) =
    case s of
        [] => ""
        | [(x,y)] => x^","^typeToStr(y)
        | (x,y)::xs  => x^","^typeToStr(y)^" | "^printTypeTable(xs)


end
