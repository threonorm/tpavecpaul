(*----------1----------*)
let miroir u = let n=string_length u in
let v = make_string n `a` in
for i=0 to n-1 do
	v.[n-1-i] <- u.[i]
done ;
v ;;
let palindrome u = let v=miroir u in u=v ;;

(*----------2----------*)
let rec f n = match n with
	|0 -> 2
	|_ -> let x = f (n-1) in x*x ;;
(*ou bien, avec récursivité terminale :*)
let f n =
	let rec g acc n = match n with
		|0 -> acc
		|_ -> g (acc*acc) (n-1)
	in
	g 2 n;;
(*ou autre...*)

(*----------3----------*)
let egal x y = match y with
	| x -> true
	| _ -> false;;
(* Caml répond "egal : 'a -> 'b -> bool = <fun>" ce qui devrait
vous alarmer : le type de x et y n'est pas le même. *)
egal 1 2;;
(* Caml répond "- : bool = true" ! En fait le x dans le matching
est muet. À retenir : on matche sur une structure de données,
par exemple "t::q", "(u,_)", "[0]",... *)

(*----------4----------*)
(* a) *)
let rec card l = match l with
	|[] -> 0
	|_::q -> 1 + (card q);;
(*ou bien*)
let rec card = function
	|[] -> 0
	|_::q -> 1 + (card q);;
(* b) *)
let rec estpresent x l = match l with
	|[] -> false
	|y::q when y=x -> true
	|_::q -> estpresent x q;;
(* c) *)
(*le plus simple à écrire, mais de complexité quadratique :*)
let rec miroir = function
	|[] -> []
	|x::q -> miroir q @ [x];;
(*le mieux car linéaire :*)
let miroir l =
	let rec retourne acc liste = match liste with
		|[] -> acc
		|x::q -> retourne (x::acc) q
	in
	retourne [] l ;;
(* d) *)
let rec applique f = function
	|[] -> []
	|x::q -> (f x) :: (applique f q);;
(* e) *)
let rec itere f a l = match l with
	|[] -> failwith "liste vide"
	|[b] -> f a b
	|b::q -> itere f (f a b) q;;
(* f) *)
let somme l = it_list (fun x y -> x+y) 0 l;;
(* g) *)
(*version avec du it_list et du map :*)
let prod l1 l2 =
	let colle liste x= liste @ (map (fun z -> (x,z)) l2) in
	it_list colle [] l1;;

(*----------5----------*)
(* a) *)
let rec insere x = function
	|[] -> [x]
	|y::q when y<x -> y::(insere x q)
	|y::q -> x::y::q;;
let rec tri_insertion = function
	|[] -> []
	|x::q -> insere x (tri_insertion q) ;;
(*ou bien, pour utiliser it_list :*)
let tri_insertion2 = it_list (fun acc x -> insere x acc) [] ;;
(* b) *)
let rec partition l = match l with
	|[] -> [],[]
	|x::q -> let (g,d) = partition l in x::d,g;;
let rec fusion = function
	|[],l2 -> l2
	|l1,[] -> l1
	|x1::l1,x2::l2 when x1<x2 -> x1::(fusion(l1, (x2::l2)))
	|x1::l1,x2::l2 -> x2::(fusion ((x1::l1),l2));;
let rec tri_fusion l = match l with
	|[] -> []
	|[x] -> [x]
	|_ -> let (g,d) = partition l in
				fusion (tri_fusion g, tri_fusion d);;
(* d) *)
(*par exemple, si les valeurs sont des entiers entre 0 inclus et m exclus :*)
let tri_comptage m l =
	let v = make_vect m 0 in
	it_list (fun _ x -> v.(x) <- v.(x) + 1) () l;
	let ll = ref [] in
	for i=m-1 downto 0 do
		for j=1 to v.(i) do
			ll := i::!ll
		done
	done;
	!ll;;

(*----------6----------*)
let rec croissanteR = function
	|[] -> true
	|[x] -> true
	|x::y::q when x > y -> false
	|x::q -> croissanteR q ;;

let croissanteI l = 
	let ll = ref l in
	let croissante = ref true in
	let taille = ref (card l) in
	while !taille >= 2 && !croissante do
		if hd(!ll) > hd(tl(!ll)) then croissante := false ;
		ll := tl(!ll) ;
		taille := !taille - 1
	done ;
	!croissante;;

exception pascroissante ;;

let croissanteR2 l = try
	let rec aux ll =
		if hd(ll) > hd(tl(ll)) then raise pascroissante else aux (tl(ll))
	in
	aux l
with 
	pascroissante -> false
	|_ -> true;;
	
let croissanteI2 l =
try
	let ll = ref l in
	while true do
		if hd(!ll) > hd(tl(!ll)) then raise pascroissante ;
		ll := tl(!ll)
	done;
	true
with 
	pascroissante -> false
	|_ -> true;;

(*----------7----------*)
let rec church_of_int n f x = match n with
	|0 -> x
	|_ -> f (church_of_int (n-1) f x);;
let int_of_church n = n (fun x -> x+1) 0;;
let ajoute n m f x = n f (m f x) ;;
let multiplie n m f x = n (m f) x ;;
let puissance n m = m n ;;
