(*
 Ernesto Rodriguez

 GenCS Fall 2011, Jacobs University
 Assignment 8: Encoding Programs as Strings
 Problem 2
*)

(*
 Read every line of the input stream,
 parse the line as a number (if possible)
 return a list of numbers.
*)
fun readFile b=let val line=TextIO.inputLine b
		   fun readInt (SOME i)=[i]
		     | readInt _=[]
		   fun readLine (SOME a)=(readInt (Int.fromString a))@readFile b
		     | readLine _=[]		   
	       in
		   readLine line
	       end;

(* 
 Merge-Sort algorithm for 
 numbrs using < as the 
 order.
*)
fun sort [] = []
  | sort (a::nil)=[a]
  | sort (a::tail)=let val (l,e,g)=(foldl (fn(v,(l,e,g))=>if (v < e) then (v::l,e,g) 
							      else (l,e,v::g)) ([],a,[]) tail) in
			   (sort l)@[a]@(sort g)
		       end;

exception noneMissing;
exception youSeriouslyWroteMoreThan1000Numbers
exception indexOutOfBounds
exception emptyFile

fun findMissing file=let 
    val (n::nums)=(readFile (TextIO.openIn file))
    fun missing (a::b::t)=if a+1=b orelse a=b then
			      (*
			       If repeated number,
			       ignore and keep searching
			       *)
			      missing (b::t)
			  else
			      a+1
      | missing (a::nil)=if a=n then 
			     raise noneMissing
			 else
			     a+1
      (*
       If no numbers where given, 
       1 is the first missing number
       *)
      | missing _=1 
in
    if n>1000 then
	raise youSeriouslyWroteMoreThan1000Numbers
    else if n<1 then
	raise indexOutOfBounds
    else
	let 
	    val one::t=sort nums
	in
	    if one=1 then
		missing t
	    else
		1
	end
end
    handle Bind=>raise emptyFile
