(*
 Ernesto Rodriguez

 GenCS Fall 2011, Jacobs University
 Assignment 8: Encoding Programs as Strings
 Problem 4
*)

(* All morse code characters separated by \n *)
val morse="A  .-\nB  -...\nC  -.-.\nD  -..\nE  .\nF  ..-.\nG  --.\nH  ....\nI  ..\nJ  .---\nK  -.-\nL  .-..\nM  --\nN  -.\nO  ---\nP  .--.\nQ  --.-\nR  .-.\nS  ...\nT  -\nU  ..-\nV  ...-\nW  .--\nX  -..-\nY  -.--\nZ  --..\n1  .----\n2  ..---\n3  ...--\n4  ....-\n5  .....\n6  -....\n7  --...\n8  ---..\n9  ----.\n0  -----\n,  --..--\n.  .-.-.-\n?  ..--..\n;  -.-.-\n:  ---...\n/  -..-.\n-  -....-\n'  .----.\n( -.--.-\n) -.--.-\n_  ..--.-\n";

infixr 2 >> fun a >> b = b;

(* 
 Read the list of morse code characters and
|return a list of pairs (latin,morse).
*)
val morseMap=let
    val input=TextIO.openString morse
    fun parseLine l=let
	val latin=Substring.takel (fn(x)=>not(x=(#" "))) (Substring.extract (l,0,NONE))
	val morse=Substring.taker (fn(x)=>not(x=(#" "))) (Substring.extract (l,0,NONE))
    in
	(Substring.string latin,Substring.string (Substring.takel (fn(x)=>not(x=(#"\n"))) morse))
    end
    fun copyLine (SOME l)=(parseLine l)::(copyLine (TextIO.inputLine input))
      | copyLine _=TextIO.closeIn input >> []
in
    (" ",".......")::(copyLine (TextIO.inputLine input))
end;

(*
 Convert all letters to uppercase, since there 
 is no case in morse code, this saves characters.
*)
fun toUpper s=let
    val (h::t)=explode s
in
    implode ((Char.toUpper h)::(explode (toUpper (implode t))))
end
    handle Bind=>"";

(*
 Search in the list of characters the corresponding morse code
 of the latin character.
*)
	      
fun toMorse a=List.find (fn((latin,morse))=>(toUpper a)=latin)  morseMap;

(*
 Search for the given code and return the
 matching latin character.
*)
fun toLatin a=List.find (fn((latin,morse))=>(toUpper a)=morse)  morseMap;

(*
 Morse code encoder, basically maps
 every character of a list to it's
 corresponding morse code character.
*)		     
fun encode' (h::str)=let    
    fun fetch (SOME l)=l
      | fetch _=("","")
    val (l,m)=fetch (toMorse (implode [h]))
in
    (explode m)@(encode' str)
end
  | encode' _=[];

(*
 Encoder wrapper, simply implodes and 
 explodes the string for easier manipulation
*)
fun encode s=implode (encode' (explode s));

(*
 The biggest ammount of characters that will
 be attempted to be decoded as a latin character
*)			

val lookAhead=7;

(*
 All possibilities will be modeled as a tree since
 we can think of it like a initial latin character
 and the child nodes will be all the possible 
 decodings after that character.


         ---C ...
     A---|
     |   ---R ...
 ""--B---
     |
     C---

 *)
datatype 'a tree = empty | tree of ('a*'a tree list);

(*
 Morse code decoder it returns a character list tree,
 This function creates a new tree on each iteration
 with a decoded characer as a root, and adds all
 the leaves which represent possibilities recursively.
 Then it tries decoding different characters and
 generate it's respective trees.
 *)
fun decode' 0 _ (tree (v,l))=tree (v,empty::l)
  | decode' _ nil (tree (v,l))=tree (v,empty::l)
  | decode' n list (tree (v,l))=let
	val test=List.take(list,n)
	val rel=toLatin (implode test)
	fun replace (SOME (latin,_))=let
	    val tr=tree (v,(decode' lookAhead (List.drop (list,n)) (tree (explode latin,[])))::l)
	in
	    decode' (n-1) list tr
	end
	  | replace _=decode' (n-1) list (tree (v,l))
    in
	replace rel
    end
    handle Subscript=>(decode' (n-1) list (tree (v,l)));

(* Convert the tree to a character list list, by
 appending the value of the root node to the value
 of all it's child nodes and then appending that 
 value to the parent node.
*)
fun toList (tree (v,(a::nil)))=[v]
  |toList (tree (v,l))=foldl (fn(a,b)=>(map (fn(x)=>v@x) (toList a))@b) [] l
  |toList _=[];

(*Decoder wrapper, provides the appropiate input
 for all the functions involved in decoding
 and showing the result
*)
fun decode str=map (fn(x)=>implode x) (toList (decode' lookAhead (explode str) (tree (explode "",[]))));

fun decodeNPrint msg=map (fn(x)=>print x >> print "\n") (decode (encode msg));
