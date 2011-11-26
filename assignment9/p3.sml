(*
 Ernesto Rodriguez
 Jacobs University Bremen
 General Computer Science Fall 2011
 Assignment 9: Boolean Expressions
*)

datatype boolexp = zero | one
		 | plus of boolexp * boolexp
		 | times of boolexp * boolexp
		 | compl of boolexp
		 | var of int;

fun inspect (plus (a,b))=(inspect a)@(inspect b)
  | inspect (times (a,b))=(inspect a)@(inspect b)
  | inspect (compl a) = inspect a
  | inspect (var a) = [a]
  | inspect _ =[];

fun rmDuplicates l=foldl (fn(a,b)=>if (List.exists (fn(x)=>a=x)  b) then b else a::b) [] l;

exception parseError;    

fun eval _ zero=0
  | eval _ one=1
  | eval psi (var a)=let
	val element=List.find (fn((x,_))=>a=x) psi
	fun match (SOME (_,value))=value
	  | match _ = raise parseError
    in
	match element
    end
  | eval psi (times (a,b)) = if (eval psi a)=1 andalso (eval psi b)=1 then 1 else 0
  | eval psi (plus (a,b)) = if (eval psi a)=1 orelse (eval psi b)=1 then 1 else 0
  | eval psi (compl a) = if (eval psi a)>0 then 0 else 1;

fun appendX(x, []) = []
  | appendX(x, a::l) = (x::a) :: appendX(x, l);

fun generate(0) = []
  | generate(1) = [[1], [0]]
  | generate(n) = appendX(0, generate(n-1)) @ appendX(1, generate(n-1));

fun genTable expr=let
    val vars=rmDuplicates (inspect expr)
    fun pairs nil nil=[]
      | pairs (a::t) (b::t2) = (a,b)::(pairs t t2)
      | pairs _ _= raise parseError 
in 
    (foldl (fn(a,b)=>(var a)::b) [] vars,
     foldr (fn(a,b)=>(a,(eval (pairs vars a) expr))::b) [] (generate (List.length vars)))
end;

