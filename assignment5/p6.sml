(*
 Ernesto Rodriguez
*)
infixr ++;
datatype one = i;
datatype myNats = O | ++ of one*myNats;
datatype sweetness = sweetness of myNats;
datatype weight = weight of myNats;
datatype milkPart = milkPart of myNats*myNats; (* As a fraction a*b=a/b *)
datatype dimensions = dimensions of myNats*myNats*myNats;
datatype chocolate = chocolate of sweetness*weight*milkPart*dimensions;

(*  
 
 Addition for mynats

 plus (i++i++O) (i++i++O); 

 *)
fun plus O O=O
  | plus O (a ++ b)=a++b
  | plus (a ++ b) O=a++b
  | plus (a ++ b) (c ++ d)=a++c++(plus b d);

(*

 multiplication for my nats.

 mul (i++i++i++O) (i++i++O);

*)

fun mul O O=O
  | mul O a = O
  | mul a O = O
  | mul (a ++ O) (c ++ d)=c++d
  | mul (a ++ b) (c++O)=a ++ b
  | mul (a ++ b) (c++d) = plus (c++d) (mul b (c++d));

val ten=mul (i++i++i++i++i++O) (i++i++O);

val myChocolates=[chocolate(sweetness (i++i++i++O),weight (i++i++O),milkPart (i++i++i++i++O,ten), dimensions (i++i++O,i++i++O,i++O)),
		  chocolate(sweetness (i++i++i++i++O),weight (i++O),milkPart (i++i++i++i++i++O,ten), dimensions (i++i++i++i++O,i++i++O,i++O)),
		  chocolate(sweetness (i++O),weight (i++i++i++O),milkPart (i++i++i++i++i++i++O,ten), dimensions (i++i++i++i++O,i++i++O,i++i++O)),
		  chocolate(sweetness (i++i++i++O),weight (i++O),milkPart (i++i++i++i++i++O,ten), dimensions (i++O,i++i++O,i++i++O))];

(* 

 Returns a 4-tuple:
 first element: the total sweetness of the box,
 second element: the total weight of the box,
 third element: tuple (the milk percentage of the whole box) (a,b)=>a/b.
 forth element: The volume of the box.		

*)
fun box a=foldl (fn((chocolate (sweetness s,weight w,milkPart (p,q),dimensions (x,y,z))),(a,b,(c1,c2),d))=>(plus s a,plus w b,(plus p c1,plus q c2),plus (mul x (mul y z)) d)) (O,O,(O,O),O) a;

box myChocolates;
