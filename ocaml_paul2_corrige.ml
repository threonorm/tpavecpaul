(*----------1----------*)
(* Q1 *)
let fibo n =
  let rec f a b n =  match n with
  |0 -> a
  |_ -> f b (a+b) (n-1)
  in
  f 0 1 n ;; 

(* Q3 *)
let table = make_vect 17 [] ;;
let h n = n mod 17 ;;

let ajoute (k,e) = 
  let i = h k in
  table.(i) <- (k,e)::table.(i);;

exception pasla;;
let recherche k =
  let rec cherche elt liste = match liste with
    |[] -> raise pasla
    |(a,b)::q when elt=a -> b
    |_::q -> cherche elt q
  in
  cherche k table.(h k);;

(* Q4,5 *)
ajoute (0,0) ; ajoute (1,1) ;; (*pour traiter les cas terminaux*)
let rec fibo_memo n =
  try
    recherche n
  with pasla -> let f1 = fibo_memo (n-1) in
                let f2 = fibo_memo (n-2) in 
                let f = f1+f2 in ajoute (n,f); f;;
(*Avantages : si on appelle plusieurs fois de suite la fonction. De plus
c'est adapte a une classe beaucoup plus grande de fonction recursive ! *)



(*----------2.1----------*)
(*Moralement, une liste circulaire est une liste qui fait un cycle... Il
n'y a donc pas de "dernier element" et notamment on considerera :
[1, 2 , 3 , 4]_c = [2,3,4,1]_c 

-Avec un tableau de booleen (les protagonistes sont donc anonymises, on
stock seulement leur etat : vivant/mort. On fera nos calculs modulo la
taille du tableau

-Avec une file d'attente encodee par deux piles.

-Avec une liste doublement chainee (technique)
*)

(* Q.2 a Q.5, implementation par tableau de booleen. *)
let convertit l =
  let n = list_length l in
  make_vect n true;;
  
let estpresent i list_circ =
  list_circ.(i);;

let supprime i list_circ =
  list_circ.(i)<-false;;
  
let josephe n m =
  let v= make_vect n true in
    let j = ref 0 in 
    for k=n downto 2 do
      for i=1 to (m-1) do 
        j:= (!j+1) mod n;
          (*Technique d'evitement des elimines :*)
          while (v.(!j)=false) do
            j:= (!j+1) mod n;
          done;
          (*fin de l'evitement on est donc sur le vivant suivant.*) 
      done;
      supprime (!j) v; (*Ca y est on elimine le m-ieme*)
      while (v.(!j)=false) do (*puis on se place au suivant*)
        j:= (!j+1) mod n;
      done;
    done;
  j:=0;
  while v.(!j)=false do (*renvoyer le seul indice a true *)
    j:=!j + 1
  done;
  !j;;


(* Q.2 a Q.5, implementation par listes doublement chainees *)
type ldd = {mutable g :ldd; contenu :int ; mutable d:ldd};;

let cree_ldd i = let rec a={g=a; contenu=i ; d=a} in a ;;

let convertit l =
  let n = list_length l in
  let v = make_vect n (cree_ldd 0) in
  let rec aux n = function (*donne les bonnes valeurs aux contenus de la ldd*)
    | [] -> ()
    | t::q -> v.(n) <- cree_ldd t ; aux (n+1) q
  in
  aux 0 l;
  for i=0 to (n-2) do (*place les liens entre elements de la ldd*)
    v.(i).d <- v.(i+1); v.(i+1).g <- v.(i)
  done;
  v.(n-1).d <- v.(0); v.(0).g <- v.(n-1);
  v.(0);;

let est_seul ldd =
  ldd == ldd.d ;;
(*Dans cette derniere fonction, ecrite pour le plaisir, on teste l'egalite
physique des objets (==) pour eviter des problemes d'evaluation infinie *)

let suppr_prem ldd = (*Reçoit une ldd ref et y supprime le premier element *)
  if (est_seul !ldd) then failwith "ldd vide" else
    begin
      ldd:= !ldd.g; !ldd.d <- !ldd.d.d ; !ldd.d.g <- !ldd ;
    end;;
let supprime n ldd = (*supprime le nieme et se place sur le n-1 eme*)
  for i=1 to n do ldd:= !ldd.d done;
  suppr_prem ldd;;

let josephe n m =
  let rec liste_ent acc = function (* liste [0;1;...;n-1]*)
    |0-> acc
    |k-> liste_ent ((k-1)::acc) (k-1)
  in
  let c=ref (convertit (liste_ent [] n)) in
  for i=1 to n-1 do supprime m c done;
  !c.contenu;;


(*----------2.2----------*)
(* Q1 : reflechir a la position du premier elimine,
et a ce qui arrive aux n-1 restants *)
(* Q2 *)
let rec loup n m = match n with (*on utilise Q1*)
  |1 -> 0
  |_ -> let l = loup (n-1) m in if l < (m mod n) then l else l+1;;
(* Q3 : d'apres Q1, on obtient 0 ssi a toutes les etapes on avait n mod k > 0,
et ce pour tous les k<=n-1 (et >1), ce qui equivaut a dire qu'aucun entier de
]1;n-1[ ne divise n, i.e. n est premier *)
(* Q4 : on remarque que a n fixe, m->l(n,m) est periodique de periode 
ppcm(2,...n). On teste pour ces valeurs et on se rend compte que le loup a 
tendance a etre choisi parmi ceux qui sont pres du compteur.
Moralite : ne pas faire le compteur dans ma cour de recre. *)


(*----------3.1----------*)
(* Q1 *)
(*d'abord un algorithme glouton, sans l'indication *)
let max_fibo n = 
  let k = ref 0 in
  while fibo_memo !k <= n do
    k:= !k + 1
  done;
  fibo_memo (!k-1);; (*pas de honte a rappeler la fonction, c'est en memoire !*)
  
(*ou en utilisant l'indication :*)
let phi = (1. +. (sqrt 5.)) /. 2. ;;

let max_fibo n =
  let f x = int_of_float((log((float_of_int x) *. (sqrt 5.) +. 0.5))/.(log phi))
  in
  let k = f n in (*le terme est soit k soit k-1*)
  if fibo_memo k > n then fibo_memo (k-1) else fibo_memo k;;
  
(* Q2 *)
let rec zeckendorf n =
  let f = max_fibo n in
  if n=f then [f] else f::(zeckendorf (n-f));;


(*----------3.2----------*)
(* Q1 *)
let coup_gagnant p n =
  let c = hd(rev (zeckendorf n)) in
  if p=0 then begin
    if c != n then c else 1
  end
  else begin
    if c <= 2*p then c else 1
  end;;

(* Q2 *)
let partie n =
  let rec aux p n = match n with
    |0 -> [0]
    |_ -> let c = coup_gagnant p n in n::(aux c (n-c))
  in
  aux 0 n;;
(*avec un peu d'entrainement, on arrive tres bien a 
implementer zeckendorf dans sa tete..*)

let quigagne liste = 
  let n = list_length liste in
  if n mod 2 = 0 then print_string("Le premier joueur a gagne !")
  else print_string("Le second joueur a gagne !");
  print_newline();;
