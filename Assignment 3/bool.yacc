 (* user declarations*)

%%
(* required declarations *)

%name Bool

%term
    ID of string| CONST of string | NOT | IMPLIES | AND | OR | XOR 
    | EQUALS | IF | THEN | ELSE | FI | RPAREN | LPAREN | TERM | EOF 
    | PLUS | MINUS | TIMES | NEGATE | GREATERTHAN | LESSTHAN | NUM of int 
    | LET | IN | END | EQDEC | COLON | FN | FUN | ARROW | DOUBLEARROW
    | INT | BOOL

%nonterm 
        START of AST.formula | formula of AST.formula | types of AST.types | exp of AST.exp 
        | statement of AST.exp | operation of AST.exp | variable of AST.exp | app of AST.exp

%pos int

(* optional declarations *)

%eop EOF
%noshift EOF


%left EQUALS XOR OR AND 
%left GREATERTHAN LESSTHAN

%left MINUS PLUS
%left TIMES

%right ELSE THEN IF IMPLIES NOT
%right ARROW 

%nonassoc EQDEC

%start START
%verbose

%%

START: formula (formula)

formula: exp 
        (AST.OneExp(exp))
    
    | exp TERM
        (AST.OneExp(exp))

    | exp TERM formula 
        (AST.AllExp(exp,formula1))


exp: LET ID EQDEC exp  IN exp END 
        (AST.LetExp(ID,exp1,exp2))

    | IF exp THEN exp ELSE exp FI
        (AST.IfExp(exp1,exp2,exp3))

    | FN LPAREN ID COLON types RPAREN COLON types DOUBLEARROW exp 
        (AST.FnAbs(ID,types1,types2,exp))

    | FUN ID LPAREN ID COLON types RPAREN COLON types DOUBLEARROW exp 
        (AST.FunAbs(ID1,ID2,types1,types2,exp))

    | statement 
        (statement)


types: types ARROW types 
        (AST.Arrow(types1, types2))

    | LPAREN types RPAREN 
        (types)

    | INT 
        (AST.Int)

    | BOOL 
        (AST.Bool)



statement: operation IMPLIES statement 
        (AST.BbinopExp(AST.Implies,operation,statement)) 

    | operation 
        (operation)


operation: operation PLUS operation 
        (AST.IbinopExp(AST.Plus, operation1, operation2))

    | operation MINUS operation 
        (AST.IbinopExp(AST.Minus, operation1, operation2))

    | operation TIMES operation 
        (AST.IbinopExp(AST.Times, operation1, operation2))

    | operation GREATERTHAN operation 
        (AST.BbinopExp(AST.Greaterthan, operation1, operation2))

    | operation LESSTHAN operation 
        (AST.BbinopExp(AST.Lessthan, operation1, operation2))


    | operation AND variable 
        (AST.BbinopExp(AST.And, operation, variable))

    | operation OR variable 
        (AST.BbinopExp(AST.Or, operation, variable))

    | operation XOR variable 
        (AST.BbinopExp(AST.Xor, operation, variable))

    | operation EQUALS variable 
        (AST.BbinopExp(AST.Equals, operation, variable))



    | variable 
        (variable)


variable: ID 
        (AST.VarExp(ID))

    | NUM 
        (AST.NumExp(NUM))

    | CONST 
        (AST.ConstExp(AST.strToConst(CONST)))

    | LPAREN exp RPAREN 
        (exp)

    | NOT variable 
        (AST.UniopExp(AST.Not, variable))

    | NEGATE variable 
        (AST.UniopExp(AST.Negate, variable))

    | LPAREN app RPAREN
        (app) 

app: app exp (AST.AppExp(app,exp))
    | exp exp (AST.AppExp(exp1,exp2))
