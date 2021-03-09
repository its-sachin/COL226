exception LexerError

structure BoolLrVals = 	BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex 	 = 	BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser = 	Join(structure LrParser = LrParser
     	       				structure ParserData = BoolLrVals.ParserData
     	       				structure Lex = BoolLex)

fun initLex() = 
	BoolLex.UserDeclarations.pos := 0;
	BoolLex.UserDeclarations.line := 1;
	BoolLex.UserDeclarations.isError := false;
	BoolLex.UserDeclarations.array := Array.array(0,Term.IF("a"));

fun readFile infile =
    let val done = ref false
		val instream = TextIO.openIn infile
    	val lexer =  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true; TextIO.input instream))
    in
	(initLex();
	lexer)
    end

fun lexerRes() = 
	let 
		fun aTol arr = Array.foldr (op ::) [] arr;
		val list = aTol (!BoolLex.UserDeclarations.array)
	in
		list
	end

fun invoke lexstream =
			let val l = BoolLex.UserDeclarations.line
				val b = !l
				fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:"^ (Int.toString b) ^ ":" ^(Int.toString pos) ^":"^"''concerened production rule''" ^ "\n")
		in
		    BoolParser.parse(0,lexstream,print_error,())
		end


fun parse (lexer) =
    let val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = BoolParser.Stream.get lexer
    in
		if (!BoolLex.UserDeclarations.isError = true) then raise LexerError
        else (result)
    end


val parseString =   parse o readFile



