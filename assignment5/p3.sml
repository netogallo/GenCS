(*
 Ernesto Rodriguez

 Sort the mushrooms	 

 red of int, int is the number of dots.
 green of bool, true if it has a black dot (or eatable) false otherwise
 yellow real*rea, given as weight*volume				 
*)

datatype mushroom = red of int | green of bool | yellow of real*real;

fun cmp (red a) (red b)=if a>b then 1 else if b<a then ~1 else 0							       
  | cmp (green a) (green b)=if a andalso b then 0 else if a then 1 else ~1
  | cmp (yellow (w1,v1)) (yellow (w2,v2))=if (w1/v1)>(w2/v2) then ~1 else if (w1/v1)<(w2/v2) then 1 else 0
  | cmp (red a) (green b)= ~1
  | cmp (red a) (yellow b)= ~1
  | cmp (green a) (red b)=1
  | cmp (green a) (yellow b)= ~1
  | cmp (yellow a) (red b)=1
  | cmp (yellow a) (green b)=1;


(*
 Use quicksort and defined cmp comparison function to sort the mushrooms

 sort [red 2,green false, red 5, yellow (3.0,6.0) , green true, yellow (2.0,1.0)]
*)
fun sort [] = []
  | sort (a::nil)=[a]
  | sort (a::tail)=let val (l,e,g)=(foldl (fn(v,(l,e,g))=>if (cmp v e)=0 then (v::l,e,g) else if (cmp v e)=1 then (l,e,v::g) else (v::l,e,g)) ([],a,[]) tail) in
			    (sort l)@[a]@(sort g)
			    end;

sort [red 2,green false, red 5, yellow (3.0,6.0) , green true, yellow (2.0,1.0)];
