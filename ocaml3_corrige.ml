type lettre == int;;
type mot == lettre vect;;
let A = 10;;
let q = 10;;
let p = 17;;

(*-------------------------------------1-------------------------------------*)
let naif (m:mot) (t:mot) = 
  let lm = vect_length m and lt = vect_length t in
  let trouve = ref (-1) in
  let i = ref 0 in
  while !trouve= -1 && !i<=lt-lm do
    if m = sub_vect t !i lm then trouve:= !i ;
    i:= !i + 1
  done;
  !trouve;;
(* Complexite facilement en O(|m||t|) *)
(* Une autre version pour les fous d'imperatif, et sans sub_vect *)
let naif2 (m:mot) (t:mot) = 
  let lm = vect_length m and lt = vect_length t in
  let i = ref 0 and j = ref 0 in
  while !j < lm && !i+ !j < lt do
    if m.(!j)=t.(!i+ !j) then j:= !j + 1 else
      begin
	i:=!i+1;
	j:= 0
      end;
  done;
  if !j = lm then (!i) else (-1) ;;
(* Si les lettres de m sont toutes differentes, on peut remplacer i:=!i+1 par 
i:=!i+ !j dans naif2. En fait on peut le faire des que la premiere lettre de 
m n'est pas presente ailleurs dans m. *)
(* Alors on a une complextie en O(|m|+|t|) car chaque lettre de t est lue au 
maximum 2 fois (pourquoi ?) *)

(*-------------------------------------2-------------------------------------*)
let rec puiss a b = match b with (* exponentiation rapide *)
  |0 -> 1
  |_ when b mod 2 = 0 -> let p = puiss a (b/2) in p*p
  |_ -> let p = puiss a (b/2) in a*p*p;;
let h m = 
  let lm = vect_length m in
  let somme = ref m.(0) in
  for i=1 to lm-1 do
    somme := q* !somme + m.(i)
  done;
  !somme mod p;;
let rabinkarp (m:mot) (t:mot) =
  let lm = vect_length m and lt = vect_length t in
  let hm = h m in
  let ht = ref (h (sub_vect t 0 lm)) in (* contient le hash du sous-mot de t *)
  let trouve = ref (-1) in
  let i = ref 0 in
  let qn = puiss q (lm-1) in
  
  while !trouve = -1 && !i <lt-lm do
    if !ht = hm then
      if m= sub_vect t !i lm then trouve:= !i ;
    ht:= (q*(!ht - qn*t.(!i)) + t.(!i+lm)) mod p ; (*on actualise le hash *)
    if !ht <0 then ht := !ht + p; (* mod peut renvoyer un nombre negatif *)
    i:= !i +1;
  done;
  if !trouve= (-1) && !ht = hm then (* dernier test pour la derniere position *)
    if m= sub_vect t !i lm then trouve:= !i ;
    
  !trouve;;

(*-------------------------------------3-------------------------------------*)
type adc == int vect vect * bool vect;;
let position ((aut,fin):adc) (t:mot) = 
  let lt = vect_length t in
  let etat = ref 0 in
  let i = ref 0 in
  let lm = (vect_length aut) -1 in (* on sait que l'automate a |m|+1 etats *)
  while fin.(!etat) = false && !i <lt do
    etat := aut.(!etat).(t.(!i));
    i:=!i + 1
  done;
  if fin.(!etat) then !i-lm else -1;;
(* s'execute en O(|t|) *)

let automate (m:mot) = 
  let lm = vect_length m in
  let aut = make_matrix (lm+1) A 0 in
  let b= ref 0 in (* retient le B(u) de notre etat u*)

  for a=0 to A-1 do (* remplir la premiere ligne *)
    aut.(0).(a) <- 0
  done;
  aut.(0).(m.(0)) <- 1;

  for i=1 to lm-1 do (* remplir la ieme ligne *)
    for a=0 to A-1 do 
      aut.(i).(a) <- aut.(!b).(a)
    done;
    aut.(i).(m.(i)) <- i+1;
    b := aut.(!b).(m.(i)) (* c'est bien le bon bord pour le rang suivant *)
  done;

  let fin = make_vect (lm+1) false in
  fin.(lm) <- true;
  aut,fin;;
(* automate est en O(|m||A|) *)

let kmp m t =
  position (automate m) t;;
(* en O(|m||A| + |t|) *)

(*-------------------------------------4-------------------------------------*)
(* D'abord on ecrit une fonciton qui construit la matrice donnant la derniere 
occurence d'une lettre a avant la position k dans m. *)
let dernocc m =
  let lm=vect_length m in
  let mat = make_matrix A lm (-1) in
  for i=0 to lm-1 do
    let a=m.(i) in
    for j=i to lm-1 do
      mat.(a).(j) <- i
    done
  done;
  mat;;
(* La fonction principale contiendra une sous-fonction teste qui effectue les 
tests dans l'ordre inverse. En cas d'echec, elle leve une exception "excp" 
qui donne le "p" dont parle l'enonce, sinon, elle modifie toute seule la valeur 
indiquant ou on a trouve le mot m. *)
exception excp of int;;
let bm m t =
  let lm = vect_length m and lt = vect_length t in
  let mat = dernocc m in
  let trouve = ref (-1) in
  let ind = ref 0 in
  let teste i =
    for k=lm-1 downto 0 do
      if m.(k) != t.(k+i) then raise (excp ( k-mat.(t.(i+k)).(k) ))
    done;
    trouve:=i
  in
  while !trouve=(-1) && !ind <= lt-lm do
    try teste !ind with excp p -> ind:=!ind + p
  done;
  !trouve;;
