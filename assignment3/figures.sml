(*
 Author: Ernesto Rodriguez
 Written for the GenCS fall 2011 Jacobs University
*)

(*
 Generate a list of 1's with n elements
*)
fun square 1 =[1]
  | square n = [1]@square(n-1);

(*
 Generate a list with n times the given list inside
*)
fun square2 1 list=[list]
  | square2 n list=[list] @ (square2 (n-1) list);

(*
 Wrapper for square & square2
*)
fun squares n = square2 n (square n);

(*
 Append 1 to every list inside a list
*)
fun append(h::nil)=[h @ [1]]
  | append(h::tail)=[h @ [1]] @ append(tail);

(*
 Generate a diamond by adding a list with [1] at the
 end-points and adding one one to every list already
 inside.
*)
fun diamond 1=[[1]]
  | diamond n= [[1]] @ append(diamond(n-1)) @ [[1]];

(*
 Given a list of lenght n, generate a list of the same length
 with all it's elements set to 0.
*)
fun flat1(_::nil)=[0]
  | flat1(_::tail)=flat1(tail) @ [0];

(*
 Add 1 to the list, make it all zero and convert the end points to one
 (Sort of)
*)
fun cons1(_::nil)=[1,1]
  | cons1(_::_::nil)=[1,0,1]
  | cons1(_::tail)=[1]@flat1(tail)@[1];

fun append1(h::nil)=[cons1(h)]
  | append1(h::tail)=[cons1(h)] @ append1(tail);

fun cdiamond 1=[[1]]
  | cdiamond n=[[1]] @ append1(cdiamond(n-1)) @ [[1]];


