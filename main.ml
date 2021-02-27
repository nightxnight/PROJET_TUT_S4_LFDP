type domino = D of int * int;; (*Type domino, qui représente un domino.*)

type chain = E | S of int * string * int;; (*Type chain, qui représente la partie en cours. E si la partie démarre et S sinon*)

type player = H of int | B of int;; (*Type player, qui représente un joueur. H pour un Homme et B pour un Bot.*)

let string_of_domino (D(a, b)) = string_of_int a ^ "-" ^ string_of_int b;; (*Permet de récupérer la chaine de caractère d'un domino.*)

let flip (D(a,b)) = (D(b,a));; (*Fonction flip qui retourne un domino.*)

let append = function (*Fonction append qui ajoute un domino à la chaine*)
  | (D(a,b), S(debut, str, fin), '<') -> (S(a, string_of_domino (D(a,b)) ^ " " ^ str, fin))
  | (D(a,b), S(debut, str, fin), '>') -> (S(debut, str ^ " " ^ string_of_domino (D(a,b)), b))
  | (D(a,b), E, '<') -> (S(a, string_of_domino (D(a,b)), b))
  | (D(a,b), E, '>') -> (S(a, string_of_domino (D(a,b)), b))
  | _ -> raise (Invalid_argument "Erreur dans l'utilisation de la fonction");;

let legal_adds = function
  | (D(a,b), S(debut, str, fin)) ->
    if (a = debut && a = fin) then [append (flip (D(a,b)), S(debut, str, fin), '<'); append (D(a,b), S(debut, str, fin), '>')]
    else if (b = debut && b = fin) then [append (D(a,b), S(debut, str, fin), '<'); append (flip (D(a,b)), S(debut, str, fin), '>')]
    else if (a = debut && b = fin) then [append (flip (D(a,b)), S(debut, str, fin), '<'); append (flip (D(a,b)), S(debut, str, fin), '>')]
    else if (b = debut && a = fin) then [append (D(a,b), S(debut, str, fin), '<'); append (D(a,b), S(debut, str, fin), '>')]
    else if(a = debut) then [append (flip (D(a,b)), S(debut, str, fin), '<')]
    else if(a = fin) then [append (flip (D(a,b)), S(debut, str, fin), '>')]
    else if (b = debut) then [append (D(a,b), S(debut, str, fin), '<')]
    else if (b = fin) then [append (flip (D(a,b)), S(debut, str, fin), '<')]
    else []
  | (D(a,b), E) -> [S(a, string_of_domino (D(a,b)), b)];;