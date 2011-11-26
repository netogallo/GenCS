(*
 Ernesto Rodriguez
 Jacobs University Bremen
 General Computer Science Fall 2011
 Assignment 9: Boolean Expressions
*)

fun eval _ zero=zero
  | eval _ one=one
  | eval psi (var a)=let
	val element=List.find (fn((x,_))=>a=x) psi
	fun match (SOME (_,value))=value
	  | match _ = var a
    in
	match element
    end
  | eval psi (times (a,b)) = if (eval psi a)=one andalso (eval psi b)=one then one else zero
  | eval psi (plus (a,b)) = if (eval psi a)=one orelse (eval psi b)=one then one else zero
  | eval psi (compl a) = if (eval psi a)=one then zero else one;

fun booleval a b=eval b a;
