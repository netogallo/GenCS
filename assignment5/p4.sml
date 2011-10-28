datatype position = GK | DF | MF | FW;
datatype player = player of string*int*position;
infixr add;
datatype team = empty | add of player*team;

fun showfs (add (pos,rest))=pos;

add (GK,add (FW,empty))

val myTeam=(player ("Yashin",1,GK)) 
	       add (player ("Carlos",6,DF))
	       add (player ("Maldini",3,DF))
	       add (player ("Cafu",2,DF))
	       add (player ("Hagi",10,MF))
	       add (player ("Matthaus",12,MF))
	       add (player ("Zidane",5,MF))	       
	       add (player ("Beckham",23,MF))
	       add (player ("Stoitchkov",8,FW))
	       add (player ("Ronaldo",9,FW))
	       add (player ("Pele",11,FW))
	       add empty;
