exception LexerError

structure BoolLrVals = 	BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex 	 = 	BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser = 	Join(structure LrParser = LrParser
     	       				structure ParserData = BoolLrVals.ParserData
     	       				structure Lex = BoolLex)


fun initLex() = 
	((((BoolLex.UserDeclarations.pos := 0;
	BoolLex.UserDeclarations.line := 1);
	BoolLex.UserDeclarations.isError := false);
	BoolLex.UserDeclarations.array := Array.array(0,Term.IF("a"))))

fun readFile infile =
    let val done = ref false
		val instream = TextIO.openIn infile
    	val lexer =  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true; TextIO.input instream))
    in
	(initLex();
	lexer)
    end

val lexerRes = fn _ =>
	let 
		fun aTol arr = Array.foldr (op ::) [] arr;
		val list = aTol (!BoolLex.UserDeclarations.array)
	in
		if (!BoolLex.UserDeclarations.isError = true) then raise LexerError
		else list
	end

fun invoke lexstream =
			let val l = BoolLex.UserDeclarations.line
				val b = !l
				fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:"^ (Int.toString b) ^ ":" ^(Int.toString pos) ^":"^"''concerened production rule''" ^ "\n")
		in
		    BoolParser.parse(0,lexstream,print_error,())
		end


fun parseRes(lexer) =
    let
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = BoolParser.Stream.get lexer
    in
		if (!BoolLex.UserDeclarations.isError = true) then raise LexerError
        else (result)
    end

fun printLex(list,str) = 
	case list of 
	[] => TextIO.output(TextIO.stdOut,"["^str^"]\n")
	| [x] => printLex([],str^Term.value(x))
	| x::xs => printLex(xs,str^Term.value(x)^",")

fun printParse(list,str) = 
		case list of 
		[] => TextIO.output(TextIO.stdOut,"["^str^"]\n")
		| [x] => printParse([],str^"\""^x^"\"")
		(* | x::xs => printParse(xs,str^"\""^x^"\",") *)
		| x::xs => printParse(xs,str^x^",")
		

fun compile input = 
	let 
		val lexer = readFile input
		val parseRes = parseRes(lexer)
		val lexerRes = lexerRes 0

	in
	(printLex(lexerRes,"");
	printParse(parseRes,""))
	end

