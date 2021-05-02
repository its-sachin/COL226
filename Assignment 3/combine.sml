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
	BoolLex.UserDeclarations.array := Array.array(0,"a"))
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


fun print(list) = 
	let fun printR(list,str) = 
			case list of 
			[] => TextIO.output(TextIO.stdOut,"["^str^"]\n")
			(* | [x] => printR([],str^"\""^x^"\"") *)
			| [x] => printR([],str^x)
			(* | x::xs => printR(xs,str^"\""^x^"\",") *)
			| x::xs => printR(xs,str^x^",")
		in
		printR(list,"")
		end


fun invoke instream =
			let 
				val done = ref false
				val syntaxError = ref ""
				val lexstream =  BoolParser.makeLexer (fn _ =>( TextIO.input instream))
				fun print_error (s,pos:int, _) =
				(	if (!BoolLex.UserDeclarations.isLast = true andalso !done = false) then 
					(done := true;
					(* TextIO.output(TextIO.stdOut,"\nLexer output: \n");
					print(lexerRes ());  *)
					TextIO.output(TextIO.stdOut, "\n"^ !syntaxError ^ "Syntax Error:" ^ (Int.toString (!BoolLex.UserDeclarations.line))^":"^(Int.toString pos) ^":"^s ^ "\n"))
					else (syntaxError := !syntaxError ^ "Syntax Error:" ^ (Int.toString (!BoolLex.UserDeclarations.line))^":"^(Int.toString pos) ^":"^s ^ "\n"))
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
		
(* 
fun compile input =
	let val parseRes = parseRes(readFile input)
		val lexerRes = lexerRes()
	in
	if (!doneOnce = false) then (print(lexerRes); print(parseRes))
	else print(parseRes)
	end *)


fun parse input =
	let val parseRes = parseRes(readFile input)
		val lexerRes = lexerRes()
	in
	(
	(* TextIO.output(TextIO.stdOut,"\nLexer output: \n"); 
	print(lexerRes); 
	TextIO.output(TextIO.stdOut,"\n");  *)
	parseRes)
	end

fun compile input = 
	let val parsed = parse input
	in
	EVALUATOR.evalFile(parsed)
	end

fun printRes input =
	let val compiled = compile input
	in
	TextIO.output(TextIO.stdOut, EVALUATOR.resToStr(compiled)^ "\n")
	end

val args = CommandLine.arguments()
val a = case args of [] => ""
	| [x] => x
	| x::xs => x

val _ = 
	if (a = "") then TextIO.output(TextIO.stdOut,"Enter the path of file.\n")
	else (printRes a)

 	

