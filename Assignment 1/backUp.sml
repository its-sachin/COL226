fun changer(inLine, newline1,newline2) = 
            let
                val len1 = String.size(newline1)
                val len = String.size(inLine)
            in
            String.substring(inLine,0, len-len1) ^ newline2
            end









fun convertDelimiters(infilename, delim1, outfilename, delim2) =
    let 
    	fun inToList (infile:string) =
        let 
            val input= TextIO.openIn infile
            fun inToListRec(input) =
                case TextIO.inputLine input of
                        SOME line => line :: inToListRec(input)
                        | NONE      => []
        in
        inToListRec(input) before TextIO.closeIn input
        end

        fun changer(inStr,c,d) = 
            let 
                val len = String.size(inStr)
                fun changerRec(inStr, i,outStr) = 
                    if (i = len) then outStr
                    else if (String.sub(inStr,i) = #"\\") then 
                        if (String.sub(inStr,i+1) = c) then changerRec(inStr,i+2,outStr^String.str(c))
                        else if (String.sub(inStr,i+1) = d) then changerRec(inStr,i+2,outStr^ "\\" ^ "\\" ^ String.str(d))
                        else changerRec(inStr,i+2,outStr^"\\")
                    else if (String.sub(inStr,i) = d) then  changerRec(inStr,i+1,outStr^"\\"^String.str(d))
                    else if (String.sub(inStr,i) = c) then  changerRec(inStr,i+1,outStr^String.str(d))
                    else changerRec(inStr,i+1,outStr^String.str(String.sub(inStr,i)))
            in
            changerRec(inStr,0,"")
            end
            
        fun printChar(str,output) =
            let 
                val charList = String.explode(str)
                fun printCharRec(charList) =
                case charList of
                [] => NONE
                | x::xs  => (TextIO.output1(output,x); printCharRec(xs))
        in
            printCharRec(charList)
        end

    in

    let
        val inList = inToList(infilename)
        val output = TextIO.openOut outfilename
        fun doAll(delim1, delim2, inList) = 
            case inList of 
                [] => (TextIO.closeOut output)
                | x::xs => (printChar(changer(x,delim1,delim2),output); doAll(delim1,delim2,xs))

    in
    doAll(delim1,delim2,inList)
    end
    end

fun csv2tsv(infilename, outfilename) = 
    convertDelimiters(infilename, #",", outfilename, #"\t") 

fun tsv2csv(infilename, outfilename) = 
    convertDelimiters(infilename, #"\t", outfilename, #",") 







fun convertNewlines(infilename, newline1, outfilename, newline2) = 
    let 
    	fun inToList (infile:string) =
        let 
            val input= TextIO.openIn infile
            fun inToListRec(input) =
                case TextIO.inputLine input of
                        SOME line => line :: inToListRec(input)
                        | NONE      => []
        in
        inToListRec(input) before TextIO.closeIn input
        end

        fun changer(inLine, newline1,newline2) = 
		    let
		        val len1 = String.size(newline1)
		        val len = String.size(inLine)
		    in
		    let 
		        fun changerRec(inLine,i,outLine) =
		            if (i >= len) then outLine
		            else 
		                if (String.substring(inLine, i, len1) = newline1) then changerRec(inLine, i+len1, outLine^newline2)
		                else changerRec(inLine, i+1, outLine^String.substring(inLine,i,1))
		    in
		    changerRec(inLine,0,"")
		    end
		    end


        fun printChar(str,output) =
            let 
                val charList = String.explode(str)
                fun printCharRec(charList) =
                case charList of
                [] => NONE
                | x::xs  => (TextIO.output1(output,x); printCharRec(xs))
	        in
	            printCharRec(charList)
	        end

    in

    let
        val inList = inToList(infilename)
        val output = TextIO.openOut outfilename
        fun doAll(inList) = 
            case inList of 
                [] => (TextIO.closeOut output)
                | x::xs => (printChar(changer(x,newline1,newline2),output); doAll(xs))

    in
    doAll(inList)
    end
    end     


fun unix2dos(infilename, outfilename) = 
	convertNewlines(infilename, "\n", outfilename, "\r\n")

fun dos2unix(infilename, outfilename) = 
	convertNewlines(infilename, "\r\n", outfilename, "\n")





        fun counter(inStr) =
        	let 
        		fun counterRec(inStr, i, j) =
        			if (i >= String.size(inStr)) then j
        			else if (String.sub(inStr,i) = #",") then
        				if (String.sub(inStr,i+1) = delim1) then counterRec(inStr,i+2,j)
        				else counterRec(inStr,i+1,j)
        			else if (String.sub(inStr,i) = delim1) then counterRec(inStr,i+1,j+1)
        			else counterRec(inStr,i+1,j)
        	in
        		counterRec(inStr,0,1)
        	end

		fun fieldCheck(inList) = 
        	let 
        		fun firstCount(inList) = 
        			case inList of 
        				[] => 0
        				| x::xs => counter(x)
        	in
        	let
        		val first = firstCount(inList)
				fun checkAti(i,line) = 
					if (i = first) then ()
					else (print ("(* Exception Raised when number of fields is uneven in the input file. \n Return line number, expected and actual number of fields *) \n");
						print("exception UnevenFields of string \n");
						print("(* \"Expected: " ^ Int.toString(first)^ " fields, Present: " ^ Int.toString(i) ^ " fields on Line " ^Int.toString(line)^ "\"*) \n")
						)

				fun fieldCheckRec(inList,line) = 
					case inList of 
						[] => ()
						| x::xs => (checkAti(counter(x),line); fieldCheckRec(xs,line+1))
			in
			fieldCheckRec(inList,1)
			end
			end






























	fun convertDelimiters(infilename, delim1, outfilename, delim2) =
    let 
    	exception emptyInputFile
		exception ss

    	fun inToList (infile:string) =
        let 
            val input= TextIO.openIn infile
            fun inToListRec(input) =
                case TextIO.inputLine input of
                        SOME line => line :: inToListRec(input)
                        | NONE      => []
        in
        if (inToListRec(input) = []) then raise emptyInputFile
        else inToListRec(input) before TextIO.closeIn input
        end

        fun changer(inStr,c,d) = 
            let 
                val len = String.size(inStr)
                fun changerRec(inStr, i,outStr) = 
                    if (i >= len) then outStr
                    else if (String.sub(inStr,i) = #"\\") then 
                        if (String.sub(inStr,i+1) = c) then changerRec(inStr,i+2,outStr^String.str(c))
                        else if (String.sub(inStr,i+1) = d) then changerRec(inStr,i+2,outStr^ "\\" ^ "\\" ^ String.str(d))
                        else changerRec(inStr,i+2,outStr^"\\")
                    else if (String.sub(inStr,i) = d) then  changerRec(inStr,i+1,outStr^"\\"^String.str(d))
                    else if (String.sub(inStr,i) = c) then  changerRec(inStr,i+1,outStr^String.str(d))
                    else changerRec(inStr,i+1,outStr^String.str(String.sub(inStr,i)))
            in
            changerRec(inStr,0,"")
            end
            
        fun printChar(str,output) =
            let 
                val charList = String.explode(str)
                fun printCharRec(charList) =
                case charList of
                [] => NONE
                | x::xs  => (TextIO.output1(output,x); printCharRec(xs))
        in
            printCharRec(charList)
        end

        fun counter(inStr) =
        	let 
        		fun counterRec(inStr, i, j) =
        			if (i >= String.size(inStr)) then j
        			else if (String.sub(inStr,i) = delim1) then counterRec(inStr,i+1,j+1)
        			else counterRec(inStr,i+1,j)
        	in
        		counterRec(inStr,0,1)
        	end
    in

    let
        val inList = inToList(infilename)
        val output = TextIO.openOut outfilename

        fun check(inList,line) = 
        	let 
        		fun firstCount(inList) = 
        			case inList of 
        				[] => 0
        				| x::xs => counter(x)
        	in
        	let
        		val first = firstCount(inList)
				fun checkAti(i) = 
					if (i = first) then ()
					else raise ss
			in
			case inList of 
				[] => ()
				| x::xs => (checkAti(counter(x)); check(xs,line+1))
			end
			end

                				
                	

        fun doAll(delim1, delim2, inList) = 
            case inList of 
                [] => (TextIO.closeOut output)
                | x::xs => (printChar(changer(x,delim1,delim2),output); doAll(delim1,delim2,xs))

    in
    check(inList,1);
    doAll(delim1,delim2,inList)
    end
    end

fun csv2tsv(infilename, outfilename) = 
    convertDelimiters(infilename, #",", outfilename, #"\t") 

fun tsv2csv(infilename, outfilename) = 
    convertDelimiters(infilename, #"\t", outfilename, #",") 







fun convertNewlines(infilename, newline1, outfilename, newline2) = 
    let 
    	fun inToList (infile:string) =
        let 
            val input= TextIO.openIn infile
            fun inToListRec(input) =
                case TextIO.inputLine input of
                        SOME line => line :: inToListRec(input)
                        | NONE      => []
        in
        inToListRec(input) before TextIO.closeIn input
        end

        fun changer(inLine, newline1,newline2) = 
		    let
		        val len1 = String.size(newline1)
		        val len = String.size(inLine)
		    in
		    let 
		        fun changerRec(inLine,i,outLine) =
		            if (i >= len) then outLine
		            else 
		                if (String.substring(inLine, i, len1) = newline1) then changerRec(inLine, i+len1, outLine^newline2)
		                else changerRec(inLine, i+1, outLine^String.substring(inLine,i,1))
		    in
		    changerRec(inLine,0,"")
		    end
		    end


        fun printChar(str,output) =
            let 
                val charList = String.explode(str)
                fun printCharRec(charList) =
                case charList of
                [] => NONE
                | x::xs  => (TextIO.output1(output,x); printCharRec(xs))
	        in
	            printCharRec(charList)
	        end

    in

    let
        val inList = inToList(infilename)
        val output = TextIO.openOut outfilename
        fun doAll(inList) = 
            case inList of 
                [] => (TextIO.closeOut output)
                | x::xs => (printChar(changer(x,newline1,newline2),output); doAll(xs))

    in
    doAll(inList)
    end
    end     


fun unix2dos(infilename, outfilename) = 
	convertNewlines(infilename, "\n", outfilename, "\r\n")

fun dos2unix(infilename, outfilename) = 
	convertNewlines(infilename, "\r\n", outfilename, "\n")