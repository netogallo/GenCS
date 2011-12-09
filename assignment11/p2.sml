(*
 Ernesto Rodriguez
 Jacobs University Bremen
 General Computer Science Fall 2011
 Assignment 11: Propositional Logic and Quine McCluskey
*)

(*Custom booleans to handle truth tables, NONE indicates a space that was resolved*)
datatype mybool = T | F | NONE;

(*Errors raised in case invalid truth tables are given*)
exception notABool;
exception inconsistentExp;

(*Convert an integer list to a boolean list, also ensure only 1 and 0 are in the list*)
fun convert nil=nil
  | convert (h::t)=if h=0 then 
		       F::(convert t)
		   else if h=1 then
		       T::(convert t)
		   else
		       raise notABool;

(*Check the tt, return the dnf, validate consistency of the tt*)
fun validateNConvert nil = (nil,nil)
  | validateNConvert (h::t)= let
	val (a,b)=h
	val (prev,prevf)=validateNConvert t
	val boolExp = convert a
	val res = if b=0 then 
		      false 
		  else if b=1 then
		      true
		  else
		      raise notABool;
	val existsT=List.exists (fn(a)=>a=boolExp) prev
	val existsF=List.exists (fn(a)=>a=boolExp) prevf
    in
	if res then
	    if existsT then
		(prev,prevf)
	    else if existsF then
		raise inconsistentExp
	    else
		(boolExp::prev,prevf)
	else
	    (prev,boolExp::prevf)
    end;

(*In case list with different ammount of variables are found*)
exception inconsistentLen;
exception noResolution;

(*Try to resolve the two given polinomials, rais exception on failure.*)
fun resolve' nil nil 1=nil
  | resolve' nil nil _=raise noResolution
  | resolve' nil _ _=raise inconsistentLen
  | resolve' _ nil _=raise inconsistentLen
  | resolve' (a::t) (b::t2) n=if a=b then
				  a::resolve' t t2 n
			     else 
				 NONE::resolve' t t2 (n+1)

(*Return the resolution of two polinomials, nil if there is no resolution*)
fun resolve a b=resolve' a b 0
    handle noResolution=> nil;

(*Resolve a polinomial with all polinomials in the list, return all the resolutions*)
fun resolveAll a (nil)=nil
  | resolveAll a (b::tail)=let
	val res=resolve a b
    in
	if res=nil then
	    resolveAll a tail
	else
	    res::(resolveAll a tail)
    end;

(*Remove duplicates from a list*)
fun filter nil=nil
  | filter (a::t)=if List.exists (fn(x)=>x=a) t then
		      filter t
		  else
		      a::(filter t);

(*Helper function for resolveMulti, read below*)
fun resolveMulti' nil _=(nil,nil)
  | resolveMulti' (pol::t) polis=let
	val (p,m)=resolveMulti' t polis
	val res=resolveAll pol polis
    in 
	if res=nil then
	    if List.exists (fn(a)=>a=pol)  p then
		(p,m)
	    else
		(pol::p,m)
	else
	    (p,filter (m@res))
    end;

(*Resolve all the polinomials in a list among themselves, return a tuple
with the found primes (no resolution found) and the resolved polinomials*)
fun resolveMulti a=resolveMulti' a a;

(*Apply resolution iteravely until no resolution can be found, return a 
tuple of primes and nil, which should be discarded cause the second argument
is used for service*)
fun megaResolve a=let 
    
    val (p,m)=resolveMulti a
    val (p1,m1)=if m=nil then
		    (p,nil)
		else
		    megaResolve m
in 
    (filter (p@p1), m1)
end

(*Convert a list of polinomials in dnf (boolean list) into a string that looks nice*)
fun mkPretty nil=""
  | mkPretty (a::tail)=let 
	fun handleIn a (b,c)=if a=T then 
				 (String.concat([b,"X",Int.toString c]),c+1)
			     else if a=F then
				 (String.concat([b,"(-X",Int.toString c,")"]),c+1)
			     else
				 (b,c+1)
	val (str,_)=foldl (fn(a,b)=>handleIn a b) ("",1) a
    in 
	if tail=nil then
	    str
	else
	    String.concat [str," + ",(mkPretty tail)]
    end;

(*Wrapper function to call everyting as the sml type system commands

Note: I didn't validate completenes of the TT, but this is essentially generating
all possible combinations of T,F values (aka. The truth table) and making sure
all members were given. 
*)
fun QuineMcCluskey tt = let 
    val (dnf,_)=validateNConvert tt
    val (p,_)=megaResolve (dnf)
in 
    mkPretty p
end;
			    
