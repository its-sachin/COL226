 (* user declarations*)

%%
(* required declarations *)

%name Bool

%term
    ID of string| CONST of string | NOT | IMPLIES | AND | OR | XOR 
    | EQUALS | IF | THEN | ELSE | FI | RPAREN | LPAREN | TERM | EOF 
    | PLUS | MINUS | TIMES | NEGATE | GREATERTHAN | LESSTHAN | NUM of int 
    | LET | IN | END | EQDEC | SEMICOLON | FN | FUN | ARROW | DOUBLEARROW
    | INT | BOOL

%nonterm 
        START of AST.formula | formula of AST.formula | types of AST.types | exp of AST.exp 
        | statement of AST.exp | operation of AST.exp | variable of AST.exp | app of AST.exp

%pos int

(* optional declarations *)

%eop EOF
%noshift EOF

%right ELSE THEN IF IMPLIES NOT
%right ARROW 

%left EQUALS XOR OR AND
%left MINUS PLUS
%left GREATERTHAN LESSTHAN TIMES


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

    | FN LPAREN ID SEMICOLON types RPAREN SEMICOLON types DOUBLEARROW exp 
        (AST.FnAbs(ID,types1,types2,exp))

    | FUN ID LPAREN ID SEMICOLON types RPAREN SEMICOLON types DOUBLEARROW exp 
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


operation: operation AND variable 
        (AST.BbinopExp(AST.And, operation, variable))

    | operation OR variable 
        (AST.BbinopExp(AST.Or, operation, variable))

    | operation XOR variable 
        (AST.BbinopExp(AST.Xor, operation, variable))

    | operation EQUALS variable 
        (AST.BbinopExp(AST.Equals, operation, variable))


    | operation PLUS variable 
        (AST.IbinopExp(AST.Plus, operation, variable))

    | operation MINUS variable 
        (AST.IbinopExp(AST.Minus, operation, variable))

    | operation TIMES variable 
        (AST.IbinopExp(AST.Times, operation, variable))

    | operation GREATERTHAN variable 
        (AST.BbinopExp(AST.Greaterthan, operation, variable))

    | operation LESSTHAN variable 
        (AST.BbinopExp(AST.Lessthan, operation, variable))


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
