(* user declarations*)

%%
(* required declarations *)

%name Bool

%term
    ID of string| CONST of string | NOT | IMPLIES | AND | OR | XOR 
    | EQUALS | IF | THEN | ELSE | RPAREN | LPAREN | TERM | EOF

%nonterm  statement of string list | variable of string list 
    | formula of string list

%pos int

(* optional declarations *)

%eop EOF
%noshift EOF


%right IF THEN ELSE IMPLIES NOT
%left EQUALS XOR OR AND

%start formula
%verbose


%%

formula: statement TERM (statement@["TERM ;"]@["formula -> statement;"])
    | ([])

statement: IF statement THEN statement ELSE statement ( ["IF IF"]@statement1@["THEN THEN"]@statement2@["ELSE ELSE"]@statement3@["statement -> IF statement THEN statement ELSE statement"] )
    | statement IMPLIES statement ( statement1@["IMPLIES IMPLIES"]@statement2@["statement -> statement IMPLIES statement"] )
    | statement AND statement ( statement1@["AND AND"]@statement2@["statement -> statement AND statement"] )
    | statement OR statement (statement1@["OR OR"]@statement2@["statement -> statement XOR statement"] )
    | statement XOR statement ( statement1@["XOR XOR"]@statement2@["statement -> statement EQUALS statement"])
    | statement EQUALS statement ( statement1@["EQUALS EQUALS"]@statement2@["statement -> statement EQUALS statement"] )
    | variable (variable@["statement -> variable"])

variable: ID (["ID "^ ID1, "variable -> ID"])
    | CONST (["CONST "^CONST1, "variable -> CONST"])
    | LPAREN statement RPAREN ( ["LPAREN ("]@statement1@["RPAREN )"]@["variable -> (statement)"] )
    | NOT variable (["NOT NOT"]@variable1@["variable -> NOT variable"] )

