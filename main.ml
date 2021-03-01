type domino = D of int * int;; (*Type domino, qui représente un domino.*)

type chain = E | S of int * string * int;; (*Type chain, qui représente la partie en cours. E si la partie démarre et S sinon*)

type player = H of int | B of int;; (*Type player, qui représente un joueur. H pour un Homme et B pour un Bot.*)

let string_of_domino (D(a, b)) = string_of_int a ^ "-" ^ string_of_int b;; (*Permet de récupérer la chaine de caractère d'un domino.*)

let flip (D(a,b)) = (D(b,a));; (*Fonction flip qui retourne un domino.*)

let length l = (*Retourne la taille d'une liste*)
  let rec urs count = function
    | [] -> count
    | _::l -> urs (count + 1 ) l
  in urs 0 l;;

let append = function (*Fonction append qui ajoute un domino à la chaine*)
  | (D(a,b), S(debut, str, fin), '<') -> (S(a, string_of_domino (D(a,b)) ^ " " ^ str, fin))
  | (D(a,b), S(debut, str, fin), '>') -> (S(debut, str ^ " " ^ string_of_domino (D(a,b)), b))
  | (D(a,b), E, '<') -> (S(a, string_of_domino (D(a,b)), b))
  | _ -> raise (Invalid_argument "Erreur dans l'utilisation de la fonction");;

let legal_adds (D(a,b)) = function (*Renvoie les chaînes de dominos résultant de toutes les poses légales*)
  | E -> [S(a, string_of_domino (D(a,b)), b)]
  | S(d, _, f) as chain when (a = d && a = f) -> [append (flip (D(a,b)), chain, '<'); append (D(a,b), chain, '>')]
  | S(d, _, f) as chain when (b = d && b = f) -> [append (D(a,b), chain, '<'); append (flip (D(a,b)), chain, '>')]
  | S(d, _, f) as chain when (a = d && b = f) -> [append (flip (D(a,b)), chain, '<'); append (flip (D(a,b)), chain, '>')]
  | S(d, _, f) as chain when (b = d && a = f) -> [append (D(a,b), chain, '<'); append (D(a,b), chain, '>')]
  | S(d, _, f) as chain when (a = d) -> [append (flip (D(a,b)), chain, '<')]
  | S(d, _, f) as chain when (a = f) -> [append (D(a,b), chain, '>')]
  | S(d, _, f) as chain when (b = d) -> [append (D(a,b), chain, '<')]
  | S(d, _, f) as chain when (b = f) -> [append (flip (D(a,b)), chain, '>')]
  | _ -> [];;

let possible_dominoes dominoes chain = (*Renvoie la liste de chacun des dominos d'une main donnée qui est plaçable au bout d'une chaîne donnée.*)
  if chain = E then dominoes
  else
  let rec urs = function
    | [] -> []
    | x::l when (legal_adds x chain) != [] -> x::(urs l)
    | _::l -> urs l
  in urs dominoes;;


let list = []
let s = 0

let char_list_of_string c = 
  let rec urs s l =
    if s < 0 
    then l 
    else urs (s - 1) (c.[s] :: l) in urs (String.length c - 1) [];;


let list_of_players = []
let i = 0

let player_ofstring s = 
  let rec urs i l =
    if i < 0 
    then l 
    else
      match s.[i] with 
      |'H' -> urs (i - 1) (H (i+1) :: l) 
      |'B' -> urs (i - 1) (B (i+1) :: l) 
      | -> failwith "erreur"
      in urs (String.length s - 1) [] ;;
      
let rec suppress d = function 
    | [] -> []
    | x::l when x = d || x = flip d -> l
    | x::l -> x::suppress d l;;

  
  let string_of_chain = function (*Renvoie la chaine représentant la partie en cours*)
    |E -> ""
    |S (nb_g, str, nb_d) -> str

  let string_of_state l = function (*Renvoie l'état d'un joueur*)
    |H(x) when l = [] -> "Joueur "^x^" (humain) : \t"
    |H(x)-> "Joueur "^x^" (humain) : \t"^string_of_dominoes(l)
    |B(x) when l = [] -> "Joueur "^x^" (bot) : \t"
    |B(x)-> "Joueur "^x^" (bot) : \t"^string_of_dominoes(l) 
