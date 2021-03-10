 (* user declarations*)

%%
(* required declarations *)

%name Bool

%term
    ID of string| CONST of string | NOT | IMPLIES | AND | OR | XOR 
    | EQUALS | IF | THEN | ELSE | RPAREN | LPAREN | TERM | EOF

%nonterm  statement of string list | implication of string  list 
    | operation of string list | variable of string list 
    | formula of string list

%pos int

(* optional declarations *)

%eop EOF
%noshift EOF

%right ELSE THEN IF IMPLIES NOT

%left EQUALS XOR OR AND


%start formula
%verbose


%%

formula: statement TERM (statement@["TERM ;","formula -> statement;"])
    | ([])

statement: IF statement THEN statement ELSE statement ( ["IF IF"]@statement1@["THEN THEN"]@statement2@["ELSE ELSE"]@statement3@["statement -> IF statement THEN statement ELSE statement"] )
    | implication (implication@["statement -> implication"])

implication: operation IMPLIES implication ( operation1@["IMPLIES IMPLIES"]@implication1@["implication -> operation IMPLIES implication"] ) 
    | operation (operation@["implication -> operation"])

operation: operation AND variable ( operation1@["AND AND"]@variable1@["operation -> operation AND variable"] )
    | operation OR variable (operation1@["OR OR"]@variable1@["operation -> operation OR variable"] )
    | operation XOR variable ( operation1@["XOR XOR"]@variable1@["operation -> operation XOR variable"])
    | operation EQUALS variable ( operation1@["EQUALS EQUALS"]@variable1@["operation -> operation EQUALS variable"] )
    | variable (variable@["operation -> variable"])

variable: ID (["ID "^ ID1, "variable -> ID"])
    | CONST (["CONST "^CONST1, "variable -> CONST"])
    | LPAREN statement RPAREN ( ["LPAREN ("]@statement1@["RPAREN )"]@["variable -> (statement)"] )
    | NOT variable (["NOT NOT"]@variable1@["variable -> NOT variable"] )