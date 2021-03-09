(* user declarations*)

%%
(* required declarations *)

%name Bool

%term
    ID of string| CONST of string | NOT | IMPLIES | AND | OR | XOR 
    | EQUALS | IF | THEN | ELSE | RPAREN | LPAREN | TERM | EOF

%nonterm  statement of string list | IMPLICATION of string  list 
    | OPERATION of string list | VARIABLE of string list 
    | START of string list
(* why options?? and "of sring" when to apply*)

%pos int

(* optional declarations *)

%eop EOF
%noshift EOF


%left EQUALS OR AND
%left XOR

%right IMPLIES NOT
%right IF THEN ELSE

%start START
%verbose


%%

START: statement TERM (statement@["TERM ;"])
    | ([])

statement: IF IMPLICATION THEN statement ELSE statement ( ["IF IF"]@IMPLICATION1@["THEN THEN"]@statement1@["ELSE ELSE"]@statement2@["statement -> IF IMPLICATION THEN statement ELSE statement"] )
    | IMPLICATION (IMPLICATION@["statement -> IMPLICATION"])

IMPLICATION: OPERATION IMPLIES IMPLICATION ( OPERATION1@["IMPLIES IMPLIES"]@IMPLICATION1@["IMPLICATION -> OPERATION IMPLIES IMPLICATION"] ) 
    | OPERATION (OPERATION@["IMPLICATION -> OPERATION"])

OPERATION: OPERATION AND VARIABLE ( OPERATION1@["AND AND"]@VARIABLE1@["OPERATION -> OPERATION AND VARIABLE"] )
    | OPERATION OR VARIABLE (OPERATION1@["OR OR"]@VARIABLE1@["OPERATION -> OPERATION XOR VARIABLE"] )
    | OPERATION XOR VARIABLE ( OPERATION1@["XOR XOR"]@VARIABLE1@["OPERATION -> OPERATION EQUALS VARIABLE"])
    | OPERATION EQUALS VARIABLE ( OPERATION1@["EQUALS EQUALS"]@VARIABLE1@["OPERATION -> OPERATION EQUALS VARIABLE"] )
    | VARIABLE (VARIABLE@["OPERATION -> VARIABLE"])

VARIABLE: ID (["ID "^ ID1, "VARIABLE -> ID"])
    | CONST (["CONST "^CONST1, "VARIABLE -> CONST"])
    | LPAREN statement RPAREN ( ["LPAREN ("]@statement1@["RPAREN )"]@["VARIABLE -> (statement)"] )
    | NOT VARIABLE (["NOT NOT"]@VARIABLE1@["VARIABLE -> NOT VARIABLE"] )

