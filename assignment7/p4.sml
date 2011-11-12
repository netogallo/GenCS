(*
 Ernesto Rodriguez

 GenCS Fall 2011, Jacobs University
 Assignment 7: SML Playground
*)

fun sort [] = []
  | sort (a::nil)=[a]
  | sort (a::tail)=let val (l,e,g)=(foldl (fn(v,(l,e,g))=>if (v < e) then (v::l,e,g) 
							      else (l,e,v::g)) ([],a,[]) tail) in
			   (sort l)@[a]@(sort g)
		       end;

exception funNotBijective;
fun index a nil=raise funNotBijective
  | index a (h::t)=if h=a then 1
		   else 1+(index a t);

fun getIndex i nil=raise funNotBijective
  |getIndex 0 _=raise funNotBijective
  |getIndex 1 (a::b)=a
  |getIndex i (a::b)=getIndex (i-1) b;

fun findPower' l m i=if l=i then 1
		   else 1+ (findPower' (foldl (fn(a,b)=>b@[getIndex a m]) [] l) m i);

fun findPower l=findPower' l l (sort l);

