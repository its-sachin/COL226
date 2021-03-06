structure BoolLrVals = BoolLrValsFun(structure Token = LrParser.Token)
structure CalcLex = BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BoolLrVals.ParserData
     	       structure Lex = CalcLex)

fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    CalcParser.parse(0,lexstream,print_error,())
		end

fun readFile infile =
    let val done = ref false
		val instream = TextIO.openIn infile
    	val lexer =  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;TextIO.input instream))
    in
	lexer
    end

val list = []	
		
fun parse (lexer) =
    let val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o readFile



