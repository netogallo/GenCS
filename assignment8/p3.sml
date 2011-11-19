(*
 Ernesto Rodriguez

 GenCS Fall 2011, Jacobs University
 Assignment 8: Encoding Programs as Strings
 Problem 2
*)

(* 
 Clone of the Haskell >> operator
| Allows functions to be called in
| sequence as if they where imperative
| actions.
*)
infixr 2 >> fun a >> b = b;

(*
 Read every line of a given text file 
| check if the given string patt is in that line 
| if that's the case write that line into the 
| output file, if no read next line and continue
| until the file ends.
*)

fun grep patt fileIn fileOut=let 
    (* Open the files *)
    val input=TextIO.openIn fileIn
    val output=TextIO.openOut fileOut
    (* Writhe the given string a to the output file*)
    fun writeLine a=TextIO.outputSubstr (output,Substring.extract (a,0,NONE))
    (* Test the given line a has the substring patt
     if so write the line to the output file *)
    fun testLine a=if String.isSubstring patt a then
		       writeLine a
		   else
		       ()
    (* Read the next line, if there is text
      | test if it contains patt
      | and read next line
      | if there is no more lines
      | close file and return
     *)
    fun copyLine (SOME l)=(testLine l) >> (copyLine (TextIO.inputLine input))
      | copyLine _=TextIO.closeOut output
in
    copyLine (TextIO.inputLine input)
end;
