(* user declarations*)
fun lookup "special" = 1000
    | lookup s = 0

%%
(* required declarations *)

%name Calc

%term
    ID of string | NUM of int
    | PLUS | SUB | TIMES | DIV | RPAREN | LPAREN | EOF

%nonterm EXP of int | START of int option

%pos int

(* optional declarations *)

%eop EOF
%noshift EOF

(* header -----> allows funtors -----> lex yacc back forth functioning *) 

%left SUB PLUS
%left TIMES DIV

(* right *)

%start START
%verbose


%%

START: EXP (SOME EXP)
    | (NONE)

EXP: NUM (NUM)
    | ID (lookup ID)
    | EXP PLUS EXP (EXP1 + EXP2)
    | EXP SUB EXP (EXP1 - EXP2)
    | EXP TIMES EXP (EXP1 * EXP2)
    | EXP DIV EXP (EXP1 div EXP2)