(*
 This function accepts a list an and a element and returns a list
 of pairs of the given element and each element in the list.
*)
fun pair a (b::nil)=[(a,b)]
| pair a (b::tail)=[(a,b)] @ pair a tail;

(*
 Use the the pair function to create a list of pairs
 containing all the pairs of a list.
*)
fun allPairs(a::b::nil)=[(a,b)]
  | allPairs(a::tail) = pair a tail @ allPairs tail;

(* 
 Given a list of pairs, return the pair of the
 two closest elements.
*)
fun comp ((a,b)::nil)=(a,b)
|comp ((a,b)::tail) = 
  let val (c,d)=comp tail in
      if abs(d-c)>abs(a-b) then
	  (a,b)
      else
	  (c,d)
  end;

(*
 Wrapper function to combine the all pairs
 function with the comp function.
*)
fun closestTwoElements list=comp(allPairs list);
