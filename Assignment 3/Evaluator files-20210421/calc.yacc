(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int
| PLUS | TIMES | SUB | DIV  | RPAREN | LPAREN | EOF
| EQ | LET | IN | END | VAL|  STRING of string

%nonterm EXP of AST.exp | START of AST.exp | DECL of AST.decl 

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%left EQ  
%left SUB PLUS
%left TIMES DIV
(* %right *)
%start START

%verbose

%%

  START: EXP (EXP)

  DECL: VAL ID EQ EXP (AST.ValDecl(ID, EXP))

  EXP: NUM (AST.NumExp(NUM))
  | ID (AST.VarExp(ID))
  | STR (AST.StringExp (STR))
  | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP SUB  EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES  EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))
  | EXP DIV  EXP (AST.BinExp(AST.Div, EXP1, EXP2))
  | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
  | EXP EQ EXP (AST.BinExp(AST.Eq, EXP1, EXP2))
  
  

