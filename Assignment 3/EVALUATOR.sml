structure EVALUATOR  =
struct
open AST

fun checkType(e:exp,t:typeTable,s:symbolTable):types= 
    (
        (* TextIO.output(TextIO.stdOut,"Type: "^typeTabelToStr(t)^"\n"); *)
    case e of 
    NumExp(_) => Int

    | VarExp(i) => findType(i,t,s)

    | ConstExp(_) => Bool

    | UniopExp(oper,e1) => (
        case oper of 
        Not => let val given = checkType(e1,t,s) in
            if (given = Bool) then Bool else 
                handleEx ("Type mismatch for NOT at \"" ^ expToStr(e)^"\"\nexpected: [bool]\ngiven: ["^ typeToStr(given) ^"]")
            end
        | Negate => let val given = checkType(e1,t,s) in
            if (given = Int) then Int else 
                handleEx ("Type mismatch for NEGATE at \"" ^ expToStr(e)^"\"\nexpected: [int]\ngiven: ["^ typeToStr(given) ^"]")
            end
    )

    | IbinopExp(oper,e1,e2) => 
        let val t1 = checkType(e1,t,s)
            val t2 = checkType(e2,t,s)
        in
            if ((t1 =Int) andalso (t2 =Int)) 
            then Int 
            else handleEx ("Type mismatch for " ^ ioperToStr(oper) ^ " at \"" ^ expToStr(e) ^ "\"\nexpected: [int]*[int] \ngiven: [" ^typeToStr(t1) ^"]*[" ^typeToStr(t2)^"]")
        end

    | BbinopExp(oper,e1,e2) => 
        let val t1 = checkType(e1,t,s)
            val t2 = checkType(e2,t,s)
        in
            if (oper = Greaterthan orelse oper = Lessthan) then 
                if ((t1 =Int) andalso (t2 =Int)) then Bool else 
                handleEx ("Type mismatch for " ^ boperToStr(oper) ^ " at \"" ^ expToStr(e)^ "\"\nexpected: [int]*[int] \ngiven: [" ^typeToStr(t1) ^"]*[" ^typeToStr(t2)^"]" )
            else if (oper = Equals) then 
                if (((t1 =Int) andalso (t2 =Int)) orelse ((t1 =Bool) andalso (t2 =Bool))) then Bool else 
                handleEx ("Type mismatch for " ^ boperToStr(oper) ^ " at \"" ^ expToStr(e)^ "\"\nexpected: [int]*[int] or [bool]*[bool] \ngiven: [" ^typeToStr(t1) ^"]*[" ^typeToStr(t2)^"]")
            else
                if ((t1=Bool) andalso (t2 =Bool)) then Bool else 
                handleEx ("Type mismatch for " ^ boperToStr(oper) ^ " at \"" ^ expToStr(e)^ "\"\nexpected: [bool]*[bool] \ngiven: [" ^typeToStr(t1) ^"]*[" ^typeToStr(t2)^"]")
        end

    | LetExp(i,e1,e2) => (case e1 of
        FunAbs(_,_,_,_,_) =>   
            handleEx ("Named function declaration inside expression at \""^expToStr(e)^"\"")
        | _ => checkType(e2,appendType(i,checkType(e1,t,s),t),s))

    | IfExp(e1,e2,e3) => let 
            val g1 = checkType(e1,t,s) 
        in
            if (g1 = Bool) then (
            let val a = checkType(e2,t,s)
                val b = checkType(e3,t,s)
            in
            if (a=b) then a else 
                handleEx ("Type mismatch for then and else expression at \"" ^expToStr(e)^ "\"\nthen: [" ^ typeToStr(a) ^ "]\nelse = [" ^ typeToStr(b) ^ "]")
            end
            )else handleEx ("Type mismatch for if expression at if \""^expToStr(e1) ^"\"\nexpected: [bool]\ngiven: ["^ typeToStr(g1) ^ "]")
        end

    | FnAbs(i,t21,t22,e2) => let 
            val g = checkType(e2,appendType(i,t21,t),s) 
        in
            if ( g= t22) then Arrow(t21,t22) else 
            handleEx ("Type mismatch for FN at \""^ expToStr(e)^"\"\nexpected: [" ^ typeToStr(t22) ^"]\ngiven: [" ^ typeToStr(g) ^ "]")
        end

    | FunAbs(_,_,_,_,_) => handleEx ("Named function declaration inside expression at \""^expToStr(e)^"\"")

    | AppExp(e1,e2) => (
        case checkType(e1,t,s) of 
        Arrow(t11,t12) => (
            let val t2 = checkType(e2,t,s)
            in
                if (t11 = t2) then t12 
                else handleEx ("Type mismatch for function application at \"" ^ expToStr(e)^"\"\nexpected: ["^ typeToStr(t11) ^"]\ngiven: [" ^typeToStr(t2)^ "]")
            end
        )
        | _ => handleEx ("Function expected for application at \""^expToStr(e)^"\"")
        )
    )
and



evalExp(e:exp, s:symbolTable):value =
    let 

    fun tackleFree(e:exp,i:id,change:value):exp = 
        (case e of 
        VarExp(i1) => 
            if (i1 = i) then (
                case change of
                IntVal(c) => NumExp(c)
                | BoolVal(c) => ConstExp(c)
                | FunVal(c,t1,t2,e1) => FnAbs(c,t1,t2,e1)
            ) else e
        | UniopExp(oper,e1) => UniopExp(oper,tackleFree(e1,i,change))
        | BbinopExp(oper,e1,e2) => BbinopExp(oper,tackleFree(e1,i,change),tackleFree(e2,i,change))
        | IbinopExp(oper,e1,e2) => IbinopExp(oper,tackleFree(e1,i,change),tackleFree(e2,i,change))
        | FnAbs(i2,t1,t2,e1) => FnAbs(i2,t1,t2,tackleFree(e1,i,change))
        | FunAbs(_,_,_,_,_) => handleEx("Function declaration inside expression at \""^expToStr(e)^"\"")
        | AppExp(e1,e2) => AppExp(tackleFree(e1,i,change), tackleFree(e2,i,change))
        | LetExp(i2,e1,e2) => LetExp(i2,tackleFree(e1,i,change),tackleFree(e2,i,change))
        | IfExp(e1,e2,e3) => IfExp(tackleFree(e1,i,change),tackleFree(e2,i,change),tackleFree(e3,i,change))        
        | _ => e)



    fun appendFree(v:value,i:id,change:value):value =
        (case v of
        FunVal(i1,t1,t2,e1) => FunVal(i1,t1,t2,tackleFree(e1,i,change))
        | _ => v)


    fun checkExp(e,s) = 
    (
        (* TextIO.output(TextIO.stdOut,"Symbol: "^tabelToStr(s)^"\n"); *)
    case e of
	    NumExp(i) => IntVal(i)
        | VarExp(i) => 	findSymbol(i,s)
        | ConstExp(c) => BoolVal(c)

        | UniopExp(oper,e1) => 

        let val e1check = checkExp(e1,s)
        in
            (case e1check of 
            BoolVal(c) => 
                (case oper of 
                    Not => BoolVal(boolNot(c))
                    | Negate => 
                        handleEx ("Type mismatch for NEGATE at \"" ^ expToStr(e)^"\"\nexpected: [int]\ngiven: [bool]"))
            | IntVal(i) => 
                (case oper of 
                    Not => 
                        handleEx ("Type mismatch for NOT at \"" ^ expToStr(e)^"\"\nexpected: [bool]\ngiven: [int]")
                    | Negate => IntVal(~1*i))
            | _ => handleEx ("Type mismatch for " ^ uoperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [bool] or [int]\ngiven: ["^typeToStr(getType(e1check))^"]" ))
        end

        | IbinopExp(oper, e1,e2) =>
        let val e1check = checkExp(e1,s)
        in
            (case e1check of IntVal(i1) =>

            let val e2check = checkExp(e2,s)
            in 
               (case e2check of IntVal(i2) => IntVal(operateInt oper i1 i2)
               | _ => handleEx ("Type mismatch for " ^ ioperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]"^"\ngiven: ["^typeToStr(getType(e1check))^"]*["^typeToStr(getType(e2check))^"]" ))
            end

            | _ => handleEx ("Type mismatch for " ^ ioperToStr(oper)^ " at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]"^"\ngiven: ["^typeToStr(getType(e1check))^"]*[_]" ))
        end

        | BbinopExp(oper, e1,e2) =>

        let val e1check = checkExp(e1,s)
            val e2check = checkExp(e2,s)
        in 
            (case oper of 
                Greaterthan => (case e1check of 
                        IntVal(i1) => (case e2check of 
                            IntVal(i2) => if (i1>i2) then BoolVal(True) else BoolVal(False)
                            | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))
                | Lessthan => (case e1check of 
                        IntVal(i1) => (case e2check of 
                            IntVal(i2) => if (i1<i2) then BoolVal(True) else BoolVal(False)
                            | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))

                | Equals => (case e1check of 
                    IntVal(i1) => (case e2check of 
                        IntVal(i2) =>if (i1=i2) then BoolVal(True) else BoolVal(False)
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))
                    | BoolVal(c1) => (case e2check of 
                        BoolVal(c2) => BoolVal(boolEquals(c1,c2))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [bool]*[bool]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))
                    | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [int]*[int] or [bool]*[bool]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))

                | _ => (case e1check of BoolVal(c1) => 
                            (case e2check of BoolVal(c2) =>BoolVal(operateBool oper c1 c2)
                            | _ => handleEx ("Type mismatch for " ^ boperToStr(oper)^ " at \"" ^ expToStr(e)^"\"\nexpected: [bool]*[bool]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" ))
                        | _ => handleEx ("Type mismatch for " ^ boperToStr(oper) ^" at \"" ^ expToStr(e)^"\"\nexpected: [bool]*[bool]\ngiven: ["^typeToStr(getType(e1check)) ^"]*[" ^ typeToStr(getType(e2check)) ^"]" )))
        end
        
        | FnAbs(i,t1,t2,e1) => 
            let val g = checkType(e1,initType(i,t1,s),s)
            in
                if ( g= t2) 
                then FunVal(i,t1,t2,e1) 
                else handleEx ("Type mismatch for fn abstraction at \"" ^ expToStr(e)^"\"\ngiven: [" ^typeToStr(g)^ "]\nexpected: [" ^ typeToStr(t2)^ "]")
            end

        | FunAbs(_,_,_,_,_) => handleEx ("Named function declaration inside expression at \"" ^ expToStr(e)^"\"")

        | AppExp(e1,e2) => 
            (* let val temp = checkType(e,initTypeEmp(s),s) in *)
                (case checkExp(e1,s) of 
                FunVal(id1,t11,t12,e11) =>
                    let val v2 = checkExp(e2,s)
                    in
                    (case v2 of
                    IntVal(i1) => if (t11 = Int) then 
                        (appendFree(checkExp(e11,appendID(id1,IntVal(i1),s)),id1,v2))
                        else handleEx ("Type mismatch for function application at \"" ^ expToStr(e)^"\"\nexpected: ["^ typeToStr(t11) ^"]\ngiven: [int]")
                    | BoolVal(c) => if (t11 = Bool) then 
                        (appendFree(checkExp(e11,appendID(id1,BoolVal(c),s)),id1,v2))
                        else handleEx ("Type mismatch for function application at \"" ^ expToStr(e)^"\"\nexpected: ["^ typeToStr(t11) ^"]\ngiven: [bool]")
                    | FunVal(id2,t21,t22,e21) => if (t11 = Arrow(t21,t22)) then
                        (appendFree(checkExp(e11,appendID(id1,FunVal(id2,t21,t22,e21),s)),id1,v2))
                        else handleEx ("Type mismatch for function application at \"" ^ expToStr(e)^"\"\nexpected: ["^ typeToStr(t11) ^"]\ngiven: [" ^typeToStr(Arrow(t21,t22))^ "]")
                    )
                    end
                | _ => handleEx ("Function expected for application at \"" ^ expToStr(e)^"\""))
            (* end *)
            

        | LetExp(i1,e1,e2) => 
            (case e1 of
            FunAbs(_,_,_,_,_) => handleEx ("Named function declaration inside expression at \"" ^ expToStr(e)^"\"")
            | _ => checkExp(e2, appendID(i1,checkExp(e1,s),s)))
        
        | IfExp(e1,e2,e3) => 
            let 
            val temp = checkType(e,initTypeEmp(s),s) 
            val e1check = checkExp(e1,s)
            in
                (case e1check of
                BoolVal(c) => 
                    (case c of 
                    True => checkExp(e2,s)
                    | False => checkExp(e3,s))
                | _ => handleEx("Type mismatch for if expression at if \""^expToStr(e1) ^"\" expected: [bool]\ngiven: [" ^ typeToStr(getType(e1check)) ^ "]")
                )
            end
        )
        in

        checkExp(e,s)

        end


and

evalFile(f:formula):value list = 

    let fun evalFileR(f:formula, s:symbolTable):value list = 
        case f of 
        OneExp(e) => (case e of 
            FunAbs(i1,i2,t1,t2,e1) => 
                let val g = checkType(e1,initTypeFun(i1,i2,t1,t2,s),s)
                in
                    if (g = t2) 
                    then [FunVal(i2,t1,t2,e1)]
                    else handleEx ("Type mismatch for fun abstraction at \"" ^ expToStr(e)^"\"\ngiven: [" ^typeToStr(g)^ "]\nexpected: [" ^ typeToStr(t2)^ "]")
                end
            | _ => [evalExp(e,s)])

        | AllExp(e,f1) => (case e of 
            FunAbs(i1,i2,t1,t2,e1) => 
                let val g = checkType(e1,initTypeFun(i1,i2,t1,t2,s),s)
                in
                    if (g = t2) then
                        (let val func = FunVal(i2,t1,t2,e1) 
                            in 
                                func::evalFileR(f1,appendID(i1,func,s)) 
                            end)
                    else handleEx ("Type mismatch for fun abstraction at \"" ^ expToStr(e)^"\"\ngiven: [" ^typeToStr(g)^ "]\nexpected: [" ^ typeToStr(t2)^ "]")
                end
            | _ => (evalExp(e,s)::evalFileR(f1,s)))

    in

    evalFileR(f,[])

    end


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

typeToStr(t:types) =
    case t of 
    Int => "int"
    | Bool => "bool"
    | Arrow(t1,t2) => "Arrow("^typeToStr(t1)^", "^typeToStr(t2)^")"

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

initTypeEmp(env:symbolTable) = 
	let 
		fun initTypeRec(env,ans) = 
			case env of
			[] => ans
			| [(x,y)] => (x,getType(y))::ans
            | (x,y)::xs  => initTypeRec(xs,(x,getType(y))::ans) 

	in

    initTypeRec(env,[])

    end

and

initType(var:id, t:types, env:symbolTable) = 

    appendType(var,t,initTypeEmp(env))

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

resToStr(res:value list) =
    case res of 
    [] => ""
    | [x] => valToStr(x)
    | x::xs => valToStr(x)^"\n"^resToStr(xs)

and

expToStr(e:exp) = 
    case e of
    NumExp(i) => Int.toString(i)
    | VarExp(i1) => i1
    | ConstExp(i) => (case i of True =>"TRUE" | _ => "FALSE")
    | UniopExp(oper,e1) => uoperToStr(oper)^" " ^expToStr(e1)
    | IbinopExp(oper,e1,e2) => expToStr(e1)^" "^ioperToStr(oper) ^ " "^expToStr(e2)
    | BbinopExp(oper,e1,e2) => expToStr(e1)^" "^boperToStr(oper) ^ " "^expToStr(e2)
    | FnAbs(i,t1,t2,e1) => "fn("^i^":"^typeToStr(t1)^")"^": "^typeToStr(t2)^" => "^expToStr(e1)
    | FunAbs(i1,i2,t1,t2,e1) => "fun " ^ i1^ "("^i2^":"^typeToStr(t1)^")"^": "^typeToStr(t2)^" => "^expToStr(e1)
    | AppExp(e1,e2) => "("^expToStr(e1) ^" " ^ expToStr(e2) ^") "
    | LetExp(i1,e1,e2) => "let " ^ i1 ^" = " ^ expToStr(e1) ^" in " ^ expToStr(e2)
    | IfExp(e1,e2,e3) => "if " ^expToStr(e1) ^" then " ^ expToStr(e2) ^" else " ^ expToStr(e3)

and


valToStr(v:value) = 
    case v of
    IntVal(i) =>"[IntVal " ^Int.toString(i)^"]"
    | BoolVal(c) => (
        case c of 
        True => "[BoolVal TRUE]"
        | False => "[BoolVal TRUE]"
    )
    | FunVal(i,t1,t2,e) => "[FunVal("^i^", "^typeToStr(t1)^", "^typeToStr(t2)^", "^expToStr(e)^")]"

and

tabelToStr(s:symbolTable) =
    case s of
        [] => ""
        | [(x,y)] => x^","^valToStr(y)
        | (x,y)::xs  => x^","^valToStr(y)^"         |        "^tabelToStr(xs)

and

typeTabelToStr(s:typeTable) =
    case s of
        [] => ""
        | [(x,y)] => x^","^typeToStr(y)
        | (x,y)::xs  => x^","^typeToStr(y)^"        |        "^typeTabelToStr(xs)


end
