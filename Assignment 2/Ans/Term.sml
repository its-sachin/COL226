structure Term =
struct
  datatype term = ID of string| CONST of string | NOT of string
    | IMPLIES of string| AND of string| OR of string| XOR of string 
    | EQUALS of string| IF of string| THEN of string| ELSE of string
    | RPAREN of string| LPAREN of string| EOF of string | TERM of string

    fun value(a:term) =
        case a of 
        ID(a) => "ID \""^a^"\""
        | CONST(a) => "CONST \""^a^"\""
        | NOT(a) => "NOT \""^a^"\""
        | IMPLIES(a) => "IMPLIES \""^a^"\""
        | AND(a) => "AND \""^a^"\""
        | OR(a) => "OR \""^a^"\""
        | XOR(a) => "XOR \""^a^"\""
        | EQUALS(a) => "EQUALS \""^a^"\""
        | IF(a) => "IF \""^a^"\""
        | THEN(a) => "THEN \""^a^"\""
        | ELSE(a) => "ELSE \""^a^"\""
        | RPAREN(a) => "RPAREN \""^a^"\""
        | LPAREN(a) => "LPAREN \""^a^"\""
        | EOF(a) => "EOF \""^a^"\""
        | TERM(a) => "TERM \""^a^"\""
end