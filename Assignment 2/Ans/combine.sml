exception LexerError

structure BoolLrVals = 	BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex 	 = 	BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser = 	Join(structure LrParser = LrParser
     	       				structure ParserData = BoolLrVals.ParserData
     	       				structure Lex = BoolLex)

val doneOnce = ref false;

fun initLex() = 
	let fun a _ = (BoolLex.UserDeclarations.pos := 0; BoolLex.UserDeclarations.line := 1)
		fun b _ = (a(); BoolLex.UserDeclarations.isError := false)
		fun c _ = (b(); BoolLex.UserDeclarations.isLast := false)
		fun d _ = (c(); BoolLex.UserDeclarations.newline := false)
		fun e _ = (d(); doneOnce := false)
	in
	(e();
	BoolLex.UserDeclarations.array := Array.array(0,Term.IF("a")))
	end



fun readFile infile =
    let 
		val instream = TextIO.openIn infile
    in
	(initLex();
	instream)
    end


val lexerRes = fn _ =>
	let 
		fun aTol arr = Array.foldr (op ::) [] arr;
		val list = aTol (!BoolLex.UserDeclarations.array)
	in
		if (!BoolLex.UserDeclarations.isError = true) then raise LexerError
		else list
	end

fun printLex(list) = 

	let fun printLexR(list,str) = 
		case list of 
		[] => TextIO.output(TextIO.stdOut,"["^str^"]\n")
		| [x] => printLexR([],str^Term.value(x))
		| x::xs => printLexR(xs,str^Term.value(x)^",")
	in
	printLexR(list,"")
	end


fun printParse(list) = 
	let fun printParseR(list,str) = 
			case list of 
			[] => TextIO.output(TextIO.stdOut,"["^str^"]\n")
			(* | [x] => printParseR([],str^"\""^x^"\"") *)
			| [x] => printParseR([],str^x)
			(* | x::xs => printParseR(xs,str^"\""^x^"\",") *)
			| x::xs => printParseR(xs,str^x^",")
		in
		printParseR(list,"")
		end


fun invoke instream =
			let 
				val done = ref false
				val syntaxError = ref ""
				val lexstream =  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true; TextIO.input instream))
				fun print_error (s,pos:int, _) =
				(	if (!BoolLex.UserDeclarations.isLast = true) then (printLex(lexerRes ()); TextIO.output(TextIO.stdOut, !syntaxError ^ "Syntax Error:" ^ (Int.toString (!BoolLex.UserDeclarations.line))^":"^(Int.toString pos) ^":"^s ^ "\n"))
					else syntaxError := !syntaxError ^ "Syntax Error:" ^ (Int.toString (!BoolLex.UserDeclarations.line))^":"^(Int.toString pos) ^":"^s ^ "\n")
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
		

fun compile input =
	let val parseRes = parseRes(readFile input)
		val lexerRes = lexerRes()
	in
	if (!doneOnce = false) then (printLex(lexerRes); printParse(parseRes))
	else printParse(parseRes)
	end
 	

