structure Term =
struct
  datatype term = ID of string| CONST of string | NOT of string
    | IMPLIES of string| AND of string| OR of string| XOR of string 
    | EQUALS of string| IF of string| THEN of string| ELSE of string
    | RPAREN of string| LPAREN of string| EOF of string
end