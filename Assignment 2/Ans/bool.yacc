(* user declarations*)

%%
(* required declarations *)

%name Bool

%term
    ID of string| CONST of string | NOT | IMPLIES | AND | OR | XOR 
    | EQUALS | IF | THEN | ELSE | RPAREN | LPAREN | EOF

%nonterm  F1 of string list | F2 of string  list 
    | F3 of string list | F4 of string list 
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

START: F1 (F1)
    | ([])

F1: IF F2 THEN F1 ELSE F1 ( ["IF IF"]@F21@["THEN THEN"]@F11@["ELSE ELSE"]@F12@["F1 -> IF F2 THEN F1 ELSE F1"] )
    | F2 (F2@["F1 -> F2"])

F2: F3 IMPLIES F2 ( F31@["IMPLIES IMPLIES"]@F21@["F2 -> F3 IMPLIES F2"] ) 
    | F3 (F3@["F2 -> F3"])

F3: F3 AND F4 ( F31@["AND AND"]@F41@["F3 -> F3 AND F4"] )
    | F3 OR F4 (F31@["OR OR"]@F41@["F3 -> F3 XOR F4"] )
    | F3 XOR F4 ( F31@["XOR XOR"]@F41@["F3 -> F3 EQUALS F4"])
    | F3 EQUALS F4 ( F31@["EQUALS EQUALS"]@F41@["F3 -> F3 EQUALS F4"] )
    | F4 (F4@["F3 -> F4"])

F4: ID (["ID "^ ID1, "F4 -> ID"])
    | CONST (["CONST "^CONST1, "F4 -> CONST"])
    | LPAREN F1 RPAREN ( ["LPAREN ("]@F11@["RPAREN )"]@["F4 -> (F1)"] )
    | NOT F4 (["NOT NOT"]@F41@["F4 -> NOT F4"] )

