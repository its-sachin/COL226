structure Calc =
 struct
   open SimpLex
   open UserDeclarations
   exception Error

   fun parse strm =
    let
      val say = fn s => TextIO.output(TextIO.stdOut,s)
      val lexer = SimpLex.makeLexer (fn n => case TextIO.inputLine strm of
      	  	  		    	     SOME line => line
					    |NONE      => "")
      val nexttok = ref (lexer())
      val advance = fn () => (nexttok := lexer(); !nexttok)
      val error = fn () => (say ("calc: syntax error on line" ^
                           (Int.toString(!linenum)) ^ "\n"); raise Error); 
      val lookup = fn i =>
        if i = "a" then 1
        else if i = "b" then 2
        else  (say ("calc: unknown identifier '" ^ i ^ "'\n"); raise Error)


     fun STMT_LIST () =
         case !nexttok of
            EOF => ()
          | _ => (STMT(); STMT_LIST())
        
        
     and STMT() =
         (case !nexttok
           of EOS  => ()
            | PRINT => (advance(); say ((Int.toString (E():int)) ^ "\n"); ())
            | _ => (E(); ());
         case !nexttok
           of EOS => (advance())
            | _ => error())

     and E () = E' (T())

     and E' (i : int ) =
         case !nexttok of
            PLUS => (advance (); E'(i+T()))
          | SUB => (advance (); E'(i-T()))
          | RPAREN => i
          | EOF => i
          | EOS => i
          | _ => error()

     and T () =  T'(F())

     and T' i =
        case !nexttok of
            PLUS => (say("operator: + \n"); i)
          | SUB => (say("operator: - \n"); i)
          | TIMES => (say("operator: * \n"); advance(); T'(i*F()))
          | DIV => (say("operator: / \n"); advance (); T'(i div F()))
          | EOF => i
          | EOS => i
          | RPAREN => (say("rparen: ) \n"); i)
          | _ => error()

     and F () =
        case !nexttok of
            ID i => (say("identifier: " ^ i^"\n"); advance(); lookup i)
          | LPAREN =>
              let val v = (advance(); E())
              in if !nexttok = RPAREN then (say("lparen: ( \n"); advance (); v) else error()
              end
          | NUM i => (say("number: " ^ Int.toString(i)^"\n"); advance(); i)
          | _ => error()
    in STMT_LIST () handle Error => parse strm
    end
 end
