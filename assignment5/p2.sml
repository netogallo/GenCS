(*
 Ernesto Rodriguez
 
 Coprime numbers in SML
 *)

(*
 Given two numbers returns the biggest one.
*)
fun max a b=if a>b then a else b;

(*
 Given a number, compute a list from 1 to that number.
*)
fun range 0=[]
  | range n=(range (n-1))@[n];

(*
 Function to increase the count of the coprime coefficients. We have the c,d are the coefficients, and a,b are the sum to say it that way.
 This means that x*c=a and y*d=b. So this function checks with sum is smaller at the moment, adds the coprime to the corresponding smaller
 sum and increases the coefficient by one.
*)
fun coprimeHelper (x,y) (a,b,c,d)=if a>b then (a,b+y,c,d+1) else (a+x,b,c+1,d);

(*
 Iterator, it gose from one to the larges possible ammount of iterations (the multiplication of both coprimes). It starts with the
 coefficients 1 and 1 and checks if the difference is one. If so we're done, if not increase the coefficient whose product with
 it's coprime number is smaller.	       
*)
fun coprimeIt (a,b)=foldl (fn(x,(i,j,k,l))=>if abs(i-j)=1 then (i,j,k,l) else (coprimeHelper (a,b) (i,j,k,l))) (a,b,1,1)  (range(a*b));

(*
 Try to find coprime coefficients, in case of failure return the original values.
*)
fun coprimeCoeff (a,b)=let val (i,j,k,l)=coprimeIt (a,b) in if abs(i-j)=1 then (k,l) else (a,b) end;
