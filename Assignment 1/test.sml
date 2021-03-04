exception UnevenFields
exception Quotation_inside_field_without_escaping
exception Quotation_inside_fields_without_enclosing
exception emptyInputFile

fun inToStr (infile:string) =
   let 
    	val input = TextIO.openIn infile
		fun inToStrRec input =
			case TextIO.input1 input of
					SOME char => String.str(char) ^ inToStrRec(input)
					| NONE      => ""
    in
	 	inToStrRec(input) before TextIO.closeIn input
    end

fun unevenPrint(f,n,l) = 
	(print ("(* Exception Raised when number of fields is uneven in the input file. \n Return line number, expected and actual number of fields *) \n");
	print("exception UnevenFields of string \n");
	print("(* \"Expected: " ^ Int.toString(n)^ " fields, Present: " ^ Int.toString(f) ^ " fields on Line " ^Int.toString(l)^ "\"*) \n"); ""
	)

val delim1 = #"\""
val delim2 = #"\""

fun changer(inStr) = 
	let 
		val len = String.size(inStr)

		fun changerRec(inStr,out,i,found,f,n,l) = 

			if (i >= len) then
				if (f<>n) then (raise UnevenFields; unevenPrint(f,n,l))
				else out

			else 

				if (String.sub(inStr,i) = #"\"") then
					if (found = false) then
						if (i=0) then changerRec(inStr,out,i+1,true,f,n,l)					
						else if ((String.sub(inStr,i-1) = delim1) orelse (String.sub(inStr,i-1) = #"\n") ) then
							changerRec(inStr,out^"\"",i+1,true,f,n,l)
						else raise Quotation_inside_fields_without_enclosing
					else 
						if (String.sub(inStr,i+1) = delim1) then
							changerRec(inStr,out^"\""^String.str(delim2),i+2,false,f+1,n,l)
						else if (String.sub(inStr,i+1) = #"\"") then
							changerRec(inStr,out^"\""^"\"",i+2,true,f,n,l)
						else if (String.sub(inStr,i+1) = #"\n") then
							changerRec(inStr,out^"\"",i+1,false,f,n,l)
						else raise Quotation_inside_field_without_escaping


				else if (String.sub(inStr,i) = delim1) then
					if (found = false) then changerRec(inStr,out^"\""^String.str(delim2),i+1,false,f+1,n,l)
					else changerRec(inStr,out^String.str(delim1),i+1,true,f,n,l)


				else if (String.sub(inStr,i) = #"\n") then
					if (found = false) then
						if (n = ~1) then
							changerRec(inStr,out^"\n",i+1,false,1,f,l+1) 
						else 
							if (f <> n) then (raise UnevenFields; unevenPrint(f,n,l)) 
							else changerRec(inStr,out^"\n",i+1,false,1,n,l+1)
					else changerRec(inStr,out^"\n",i+1,true,f,n,l)

				else changerRec(inStr,out^String.str(String.sub(inStr,i)),i+1,found,f,n,l)

	in
	if inStr = "" then raise emptyInputFile
    else changerRec(inStr,"\"",0,false,1,~1,1)
	end 









