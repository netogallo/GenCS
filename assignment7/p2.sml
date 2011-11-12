(*
 Ernesto Rodriguez

 GenCS Fall 2011, Jacobs University
 Assignment 7: SML Playground
*)

exception lineOutOfBounds;

(*
 Split the given message into lines containing k letters.

 raises a lineOutOfBouds exception if a word more than
 k letters long is found.				      
*)
fun split msg k=foldr (fn(a,b)=>(implode a)::b) [] (splitHelper (explode msg) k);

fun splitHelper [] k=[]
  | splitHelper msg k=let val (h,t)=chop [] msg k in
			  [h]@splitHelper t k
		      end;

fun reverse nil=[]
  | reverse (h::t)=(reverse t )@[h];

fun chop h nil _=(reverse h,nil)
  | chop nil _ 0=raise lineOutOfBounds
  | chop (l::h) (a::b::t) 1=if b=(#" ") then (reverse (a::l::h),t)
			    else chop (l::h) (a::b::t) 0 
  | chop (l::h) (n::t) 0=if n=(#" ") then (reverse (l::h),t)
		    else chop h (l::n::t) 0
  | chop h (n::t) k=if n=(#" ") then chop (n::h) t k
		    else chop (n::h) t (k-1);
    
