(*
 Jacobs Univeristy Bremen
 GenCS Fall 2011

 Author: Ernesto Rodriguz
 Assignment 4, Problem2

*)

fun modulo den num = if den>=num then (modulo (den-num) num) else den;

fun msgLen msg = size (foldl (^)  "" msg);

fun largest (x::nil)=msgLen x
  | largest (x::tail)=let val l=largest(tail) in
			  if ((msgLen x) > (l)) then msgLen x else l end;

fun checkCD cd (h::nil)=if ((modulo (msgLen h) cd)=0) then true else false
  | checkCD cd (h::tail)=if ((modulo (msgLen h) cd)=0) andalso (checkCD cd tail) then true else false;

fun computeGCDService list n limit=if n>=limit then 
				       if checkCD n list then
					   n
				       else
					   1
				   else
				       let val ngcd=(computeGCDService list (n+1) limit) in
					   if (checkCD n list) then					   
					       if ngcd>n then
						   ngcd
					       else
						   n
					   else
					       ngcd
				       end

fun computeGCD list=computeGCDService list 1 (largest list);

(*
checkCD 5 ["kdfjlas","fjlks","jkldjasldkf","jflksdjf"];

checkCD 5 ["kkkkk","kkkkk","kkkkk","kkkkk"];

computeGCD [["games", "wire", "address", "linuxbox"],["foo","operation", "remote"]];
largest [["games", "wire", "address", "linuxbox"],["foo","operation", "remote"]];
checkCD 6 [["games", "wire", "address", "linuxbox"],["foo","operation", "remote"]];
*)
