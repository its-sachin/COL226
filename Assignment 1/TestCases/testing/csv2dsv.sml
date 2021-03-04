(* @SACHIN *)
(* 2019CS10722 *)

(* Declaration of originality: complete code written solely by me, without any help from anyone or any source online. *)



(* Exceptions self explanatory with their names *)
exception emptyInputFile
exception UnevenFields
exception Quotation_inside_field_without_escaping
exception Quotation_inside_fields_without_enclosing
exception Termination_without_EOL


(* Function that reads the input file and give string of text of that file as output *)
fun inToStr (infilename) =
   let 
    	val input = TextIO.openIn infilename
		fun inToStrRec input =
			case TextIO.input1 input of
					SOME char => String.str(char) ^ inToStrRec(input)
					| NONE      => ""
    in
	 	inToStrRec(input) before TextIO.closeIn input
    end


(* Helper function that prints the appropriate line when uneven fields is encountered and throws exception *)
fun unevenPrint(f,n,l) = 
	(
	print("Expected: " ^ Int.toString(n)^ " fields, Present: " ^ Int.toString(f) ^ " fields on Line " ^Int.toString(l)^ "\n"); raise UnevenFields
	)


(* Functions asked to make in question *)
fun convertDelimiters(infilename, delim1, outfilename, delim2) =
    let 
		exception UnevenFields

        fun changer(inStr) = 
            let 
                val len = String.size(inStr)

                fun changerRec(inStr,out,i,found,f,n,l) = 

                    if (i >= len) then
                        if (n = ~1 orelse f = n) then
                            if (String.sub(inStr,len-1) = #"\n") then out
                            else raise Terminate_without_EOL
                        else (unevenPrint(f,n,l))  

                    else 

                        if (String.sub(inStr,i) = #"\"") then
                            if (found = false) then
                                if (i=0) then changerRec(inStr,out,i+1,true,f,n,l)

                                else if (String.sub(inStr,i-1) = #"\n")  then
                                    changerRec(inStr,out^"\"",i+1,true,f,n,l)

                                else if (String.sub(inStr,i-1) = delim1) then 
                                    changerRec(inStr,out,i+1,true,f,n,l)

                                else raise Quotation_inside_fields_without_enclosing

                            else 
                                if (i = len-1) then raise Terminate_without_EOL

                                else if (String.sub(inStr,i+1) = delim1) then
                                    changerRec(inStr,out^"\""^String.str(delim2)^"\"",i+2,false,f+1,n,l)

                                else if (String.sub(inStr,i+1) = #"\"") then
                                    changerRec(inStr,out^"\""^"\"",i+2,true,f,n,l)

                                else if (String.sub(inStr,i+1) = #"\n") then
                                    changerRec(inStr,out,i+1,false,f,n,l)

                                else raise Quotation_inside_field_without_escaping


                        else if (String.sub(inStr,i) = delim1) then
                            if (found = false) then changerRec(inStr,out^"\""^String.str(delim2)^"\"",i+1,false,f+1,n,l)

                            else changerRec(inStr,out^String.str(delim1),i+1,true,f,n,l)


                        else if (String.sub(inStr,i) = #"\n") then
                            if (found = false) then

                                if (n = ~1) then 
                                    changerRec(inStr,out^"\""^"\n",i+1,false,1,f,l+1) 

                                else 

                                    if (f <> n) then (unevenPrint(f,n,l))
                                    else if (i = len-1) then changerRec(inStr,out^"\""^"\n",i+1,false,f,n,l+1)
                                    else changerRec(inStr,out^"\""^"\n",i+1,false,1,n,l+1)

                            else changerRec(inStr,out^"\n",i+1,true,f,n,l)

                        else
                            if (i<>0 andalso String.sub(inStr,i-1) = #"\n" andalso found = false) then changerRec(inStr,out^"\""^String.str(String.sub(inStr,i)),i+1,found,f,n,l)
                            else changerRec(inStr,out^String.str(String.sub(inStr,i)),i+1,found,f,n,l)

            in
            if( inStr = "") then raise emptyInputFile
            else changerRec(inStr,"\"",0,false,1,~1,1)
            end
            
        fun printChar(str,output) =
            let 
                val charList = String.explode(str)
                fun printCharRec(charList) =
                case charList of
                [] => TextIO.closeOut output
                | x::xs  => (TextIO.output1(output,x); printCharRec(xs))
        in
            printCharRec(charList)
        end


    in

    let
        val inStr = inToStr(infilename)
        val changed = changer(inStr)
        val output = TextIO.openOut outfilename

    in
    printChar(changed, output)
    end
    end

fun csv2tsv(infilename, outfilename) = 
    convertDelimiters(infilename, #",", outfilename, #"\t") 

fun tsv2csv(infilename, outfilename) = 
    convertDelimiters(infilename, #"\t", outfilename, #",") 

