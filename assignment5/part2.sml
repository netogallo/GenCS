(*
 Jacobs Univeristy Bremen
 GenCS Fall 2011

 Author: Ernesto Rodriguz
 Assignment 4, Problem2

 Check if the given relations are symetric for the given elements. Check that
 for every of the given elements there exists a relation with itself.
 
 reflexive ([1,2,3],[(1,1),(3,3),(3,2)]);
*)
fun reflexive (list,rel)=foldl (fn(x,y)=>y andalso List.exists (fn(a)=>a=(x,x)) rel) true list;


(*
 Check that for all the given relations there exists a symetric version of the relation.
 Ej. given (1,2) it searches for (2,1).

 symetricService [1,2,3] [(1,3),(3,1),(2,1),(1,2)] nil;
*)
fun symetricService list ((a,b)::nil) relOl=List.exists (fn(x)=>(b,a)=x) relOl
  | symetricService list ((a,b)::tail) relOl=if foldl (fn(c,t)=>List.exists (fn(x)=>(b,a)=x) c orelse t) true [tail,relOl] then
						 symetricService list tail ([(a,b)] @ relOl)
					     else
						 false
(*
 Checks that all the given relations relate the elements of a given list (There is no nonsense relation.)

 relsConsistent([1,2,3],[(1,3),(3,1),(2,1),(1,2)]);
*)
fun relsConsistent (list,rels)=foldl (fn ((a,b),prev)=>if prev andalso List.exists (fn (x)=>if x=a orelse x=b then true else false) list then true else false) true rels;

(*
 Check that the relations are symetric and consistent. To determine if the relations are symetric for
 the given pairs.

 symmetric([1,2,3],[(1,3),(3,1),(2,1),(1,2)]);
*)
fun symmetric (list,rel)=if (symetricService list rel nil) andalso relsConsistent (list,rel) then true else false;

(*
 Given a relation, and a list of relations, it returns all the other that derive from such relation.
 Ej. if 1 relates to 2 and 2 relates 3, the relation (2,3) also applies for one. One must relate to 3.

 getSimilar (3,4) [(4,5),(3,5),(3,4),(2,1),(3,6)];
*)
fun getSimilar (a,b) list=foldl (fn ((r1,r2),list)=>if not ((a,b)=(r1,r2)) andalso r1=b then (list @ [(r1,r2)]) else list) [] list;

(*
 Get similar only returns the relations that can be inferred directly from the pair, this function
 also returns the relations that can be inferred from the pairs that relate to a pair. 

 getCompleteSimilar (1,2) [(2,3),(3,4),(2,6),(5,6),(7,8)];
*)
fun getCompleteSimilar (a,b) list=let val sim=getSimilar (a,b) list in
				      foldl (fn ((c,d),res)=>res @ (getSimilar (c,d) list)) sim sim end;

(*

 Check if a given relation is transitive for the rest of relations. For example if 
 (1,2) was given and the pair (2,3) exists then (1,3) should exist. This this function
 automatically expands the relations so if pair (1,2) is given and the relations
 (2,3) and (3,4) exist then (1,4) must also exist.								       

 chkTransitive (1,2) [(2,3),(3,4),(2,6),(5,6),(7,8),(1,4),(1,6),(1,3)];
*)
fun chkTransitive (a,b) list=foldl (fn ((r1,r2),prev)=> if prev andalso List.exists (fn(rel)=>(a,r2)=rel) list then true else false) true (getCompleteSimilar (a,b) list);

(*

 relPairExists (3,4) [(2,5),(4,3),(1,2)]
*)
fun relPairExists (a,b) rels=List.exists (fn (n)=>if (a,b)=n orelse (b,a)=n then true else false) rels;

(*
 Calls the chkTransitive function for every pair in the list then checks that all relations
 are related to the given paris

 transitive([1,2,3],[(1,2),(2,3),(1,3),(3,4),(1,4),(2,4)]);
*)
fun transitive (list,rels)=foldl (fn (rel,prev)=>if prev andalso (chkTransitive rel rels) then true else false) true rels;

(*
 Checks given an element a list and relations, if there exists a relation of that element to every other element (totality).
*)
fun isRelatedToAll a list rels=foldl (fn (elem,prev)=>if elem=a then true else relPairExists (a,elem) rels) true list;

(*
 Given a list of elements and a list of relations, it checks that all elements are related to each other (totality).
*)
fun allRelated (list,rels)=if foldl (fn (elem,prev)=>prev andalso isRelatedToAll elem list rels) true list then true else false;

(*
 Checks for transitivity or consistency to say it that way. If a<b b<c then c<a cannot exist. This function
 is given a number which should be the "biggest" number of a list of relations. This list of relations
 is computed using the getCompleteSimilar2 which given a relations it returns all the relations
 that should be smaller.
*)
fun isMax num list=foldl (fn ((a,b),last)=>if last andalso not (a=num) then true else false) true list;

(*
 Given a relation and a list of relations it retrieves all the elements that relate to it 
 transively.

 getSimilar2 (4,5) [(1,4),(2,3),(1,3),(2,4),(1,2),(0,1)];
*)
fun getSimilar2 (a,b) list=foldl (fn ((r1,r2),list)=>if a=r2 then list @ [(r1,r2)] else list) [] list;

(*
 Recursively use getSimilar2 to get all the transitive relations

*)
fun getCompleteSimilar2 (a,b) list=let val sim=getSimilar2 (a,b) list in
				      foldl (fn ((c,d),res)=>res @ (getSimilar2 (c,d) list)) sim sim end;

(* 
 Checks for consistency in the relation if 1<2 and 2<3 then 3<1 cannot exist

 isConsistent [(1,2),(1,3),(2,3)] 
*)
fun isConsistent rels=foldl (fn ((a,b),prev)=>if prev andalso (isMax b (getCompleteSimilar2 (a,b) rels)) then true else false) true rels;

(*
 Check if order is linear by checking that all elements are related and that the relations are consistent.

 linearOrder([1,2,3,4],[(1,2),(1,3),(2,3)]);
*)
fun linearOrder (list,rels)=if (allRelated (list,rels)) andalso isConsistent rels then true else false;



