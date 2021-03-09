use "MlLexer.lex.sml";

val instream = TextIO.openIn "g"
val lexer = SimpLex.makeLexer(fn _ => TextIO.input instream)

val l = [];
val l = l@[lexer()];
val l = l@[lexer()];
val l = l@[lexer()];