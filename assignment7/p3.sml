(*
 Ernesto Rodriguez

 GenCS Fall 2011, Jacobs University
 Assignment 7: SML Playground
*)

exception functionNotDefined;

fun H 0=0
  | H n=if n>0 then n-(H (H (H (n-1))))
	else raise functionNotDefined;

fun Q 1=1
  | Q 2=1
  | Q n=if n>2 then (Q (n-(Q (n-1))))+(Q (n-(Q (n-2))))
	else raise functionNotDefined;

fun male 0=0
  | male n=if n>0 then n-(female (male (n-1)))
	   else raise functionNotDefined
and female 0=1
  |female n=if n>0 then n-(female (male (n-1)))
	    else raise functionNotDefined;

fun a 0=0
  | a 1=2
  | a n=if n>0 then (a (n-2))*(Q (n+1))
	else raise functionNotDefined
and b 0=c 1
  | b 1=c 2
  | b n=if n>1 then c (n-2)
	else raise functionNotDefined
and c 0=1
  | c n=if n>0 then (a (n-1))*(c (n-1))
	else raise functionNotDefined
and d 0=0
  | d n=if n>0 then (b (a n))
	else raise functionNotDefined;
