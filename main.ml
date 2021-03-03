(*
  _______                                          _              _ _       
 |__   __|                                        | |            (_) |      
    | |_   _ _ __   ___  ___    ___ ___  _ __  ___| |_ _ __ _   _ _| |_ ___ 
    | | | | | '_ \ / _ \/ __|  / __/ _ \| '_ \/ __| __| '__| | | | | __/ __|
    | | |_| | |_) |  __/\__ \ | (_| (_) | | | \__ \ |_| |  | |_| | | |_\__ \
    |_|\__, | .__/ \___||___/  \___\___/|_| |_|___/\__|_|   \__,_|_|\__|___/
        __/ | |                                                             
       |___/|_|                                                                   
 *)

type domino = D of int * int;; (*Type domino, qui représente un domino.*)

type chain = E | S of int * string * int;; (*Type chain, represente la partie en cours. Vaut E si la partie commence sinon S triplet d'une string representant le jeu et de deux entiers aux extemités : nombre placable.*)

type player = H of int | B of int;; (*Type player, qui représente un joueur. H pour un Homme et B pour un Bot.*)

type 'a option = None | Some of 'a;; (*Type option polymorphe afin d'accepter la valeur None.*)

(*
   _____           _   _                   _                                         _   __                        
  / ____|         | | (_)                 | |                                       | | /_/                        
 | |  __  ___  ___| |_ _  ___  _ __     __| | ___  ___    ___ ___  _   _ _ __  ___  | | ___  __ _  __ _ _   ___  __
 | | |_ |/ _ \/ __| __| |/ _ \| '_ \   / _` |/ _ \/ __|  / __/ _ \| | | | '_ \/ __| | |/ _ \/ _` |/ _` | | | \ \/ /
 | |__| |  __/\__ \ |_| | (_) | | | | | (_| |  __/\__ \ | (_| (_) | |_| | |_) \__ \ | |  __/ (_| | (_| | |_| |>  < 
  \_____|\___||___/\__|_|\___/|_| |_|  \__,_|\___||___/  \___\___/ \__,_| .__/|___/ |_|\___|\__, |\__,_|\__,_/_/\_\
                                                                        | |                  __/ |                 
                                                                        |_|                 |___/                  
*)

(* convertis un domino en chaine *)
let string_of_domino (D(a, b)) = Printf.sprintf ("%d-%d") a b;;

(* retourne un domino *)
let flip (D(a,b)) = (D(b,a));;

(* ajoute un domino a une extremite de la chain *)
let append = function
  | (D(a,b), S(debut, str, fin), '<') -> (S(a, string_of_domino (D(a,b)) ^ " " ^ str, fin))
  | (D(a,b), S(debut, str, fin), '>') -> (S(debut, str ^ " " ^ string_of_domino (D(a,b)), b))
  | (D(a,b), E, _) -> (S(a, string_of_domino (D(a,b)), b))
  | _ -> failwith "Erreur dans l'utilisation de la fonction";;

(* renvoie sous forme de liste les chain legales apres placement d'un domino *)
let legal_adds (D(a, b)) chain =
  match chain with
    | E -> [S(a, string_of_domino (D(a,b)), b)]
    | S(d, _, f) ->
      let place (D(a, b)) = function
        | S(d, _, f) as chain when b=d && a=f-> [append (D(a, b), chain, '<'); append (D(a, b), chain, '>')] 
        | S(d, _, _) as chain when b=d -> [append (D(a, b), chain, '<')] 
        | S(_, _, f) as chain when a=f -> [append (D(a, b), chain, '>')] 
        | _ -> []
      in List.concat [place (D(a, b)) chain;place (flip (D(a, b))) chain];;

(*
Anciennes versions de legal_adds 

let legal_adds (D(a,b)) = function (*Renvoie les chaînes de dominos résultant de toutes les poses légales*)
  | E -> [S(a, string_of_domino (D(a,b)), b)]
  | S(d, _, f) as chain when (a = d && b = f) -> [append (flip (D(a,b)), chain, '<'); append (flip (D(a,b)), chain, '>')]
  | S(d, _, f) as chain when (a = d && a = f) -> [append (flip (D(a,b)), chain, '<'); append (D(a,b), chain, '>')]
  | S(d, _, f) as chain when (b = d && b = f) -> [append (D(a,b), chain, '<'); append (flip (D(a,b)), chain, '>')]
  | S(d, _, f) as chain when (b = d && a = f) -> [append (D(a,b), chain, '<'); append (D(a,b), chain, '>')]
  | S(d, _, _) as chain when (a = d) -> [append (flip (D(a,b)), chain, '<')]
  | S(_, _, f) as chain when (b = f) -> [append (flip (D(a,b)), chain, '>')]
  | S(_, _, f) as chain when (a = f) -> [append (D(a,b), chain, '>')]
  | S(d, _, _) as chain when (b = d) -> [append (D(a,b), chain, '<')]
  | _ -> [];;

let legal_adds (D(a, b)) chain =
  if chain = E then [append ((D(a, b)), chain, ' ')]
  else if a = b then [append ((D(a, b)), chain, '>'); append ((D(a, b)), chain, '>')]
  else
    let place (D(a, b)) = function
      | S(d, _, f) as chain when b=d && a=f-> [append (D(a, b), chain, '<'); append (D(a, b), chain, '>')] 
      | S(d, _, _) as chain when b=d -> [append (D(a, b), chain, '<')] 
      | S(_, _, f) as chain when a=f -> [append (D(a, b), chain, '>')] 
      | _ -> []
    in List.concat [place (D(a, b)) chain;place (flip (D(a, b))) chain];;

*)

(* renvoie la sous-liste des dominos placable d'une liste de dominos et d'une chain donnee *)
let possible_dominoes dominoes chain =
  if chain = E then dominoes
  else
  let rec urs = function
    | [] -> []
    | x::l when (legal_adds x chain) != [] -> x::(urs l)
    | _::l -> urs l
  in urs dominoes;;

(*
   _____   __ _           _   _                   _                                  __       _                               
  / ____| /_/| |         | | (_)                 | |                                 \_\     (_)                              
 | (___   ___| | ___  ___| |_ _  ___  _ __     __| |_   _    ___ ___  _   _ _ __     __ _     _  ___  _   _  ___ _ __         
  \___ \ / _ \ |/ _ \/ __| __| |/ _ \| '_ \   / _` | | | |  / __/ _ \| | | | '_ \   / _` |   | |/ _ \| | | |/ _ \ '__|        
  ____) |  __/ |  __/ (__| |_| | (_) | | | | | (_| | |_| | | (_| (_) | |_| | |_) | | (_| |   | | (_) | |_| |  __/ |     _ _ _ 
 |_____/ \___|_|\___|\___|\__|_|\___/|_| |_|  \__,_|\__,_|  \___\___/ \__,_| .__/   \__,_|   | |\___/ \__,_|\___|_|    (_|_|_)
                                                                           | |              _/ |                              
                                                                           |_|             |__/                               
*)
(*input_valid*)

let rec suppress d = function 
    | [] -> []
    | x::l when x = d || x = flip d -> l
    | x::l -> x::suppress d l;;

(*input_move*)

(*input_bot_move*)

(*input_human_move*)

(*
   _____           _   _                                         _  __  _             _ _                                     
  / ____|         | | (_)                                       | | \_\| |           | ( )                                    
 | |  __  ___  ___| |_ _  ___  _ __     ___ ___  _ __ ___  _ __ | | ___| |_ ___    __| |/ _   _ _ __     ___ ___  _   _ _ __  
 | | |_ |/ _ \/ __| __| |/ _ \| '_ \   / __/ _ \| '_ ` _ \| '_ \| |/ _ \ __/ _ \  / _` | | | | | '_ \   / __/ _ \| | | | '_ \ 
 | |__| |  __/\__ \ |_| | (_) | | | | | (_| (_) | | | | | | |_) | |  __/ ||  __/ | (_| | | |_| | | | | | (_| (_) | |_| | |_) |
  \_____|\___||___/\__|_|\___/|_| |_|  \___\___/|_| |_| |_| .__/|_|\___|\__\___|  \__,_|  \__,_|_| |_|  \___\___/ \__,_| .__/ 
                                                          | |                                                          | |    
                                                          |_|                                                          |_|    
*)

(* convertis un joueur en chaine *)
let string_of_player = function
  | H(x) -> Printf.sprintf ("Joueur %d (%s)") x "humain"
  | B(x) -> Printf.sprintf ("Joueur %d (%s)   ") x "bot";;

(* transfert un nombre n de dominos de la source a la destination *)
let rec take destination n source = 
  match (n, source) with
    | (0, source) -> (destination, source)
    | (n, []) -> (destination, [])
    | (n, (D(a, b))::ts) -> take ((D(a, b))::destination) (n-1) ts;;

(* convertis une liste de domino en chaine *)
let rec string_of_dominoes = function
  | [] -> "" 
  | x::[] -> string_of_domino x
  | x::l -> Printf.sprintf ("%s %s") (string_of_domino x) (string_of_dominoes l);;

(*move*)

(*
  __  __ _                               _                      _ _                                     _   _      
 |  \/  (_)                             | |                    | ( )                                   | | (_)     
 | \  / |_ ___  ___    ___ _ __    _ __ | | __ _  ___ ___    __| |/ _   _ _ __   ___   _ __   __ _ _ __| |_ _  ___ 
 | |\/| | / __|/ _ \  / _ \ '_ \  | '_ \| |/ _` |/ __/ _ \  / _` | | | | | '_ \ / _ \ | '_ \ / _` | '__| __| |/ _ \
 | |  | | \__ \  __/ |  __/ | | | | |_) | | (_| | (_|  __/ | (_| | | |_| | | | |  __/ | |_) | (_| | |  | |_| |  __/
 |_|  |_|_|___/\___|  \___|_| |_| | .__/|_|\__,_|\___\___|  \__,_|  \__,_|_| |_|\___| | .__/ \__,_|_|   \__|_|\___|
                                  | |                                                 | |                          
                                  |_|                                                 |_|                          
*)

(* cree toutes les combinaisons de dominos de chiffre maximum n *)
let make_dominoes n = 
    let rec urs = function
    | (-1, -1) -> []
    | (x,-1) -> urs (x-1, x-1)
    | (x,y) -> (D(x,y))::urs (x, y-1)
    in urs (n, n);;

(* convertis un tableau en liste de caractère *)
let rec char_list_of_string str = 
match (String.length str) with
    | 0 -> []
    | n -> str.[0]::char_list_of_string (String.sub str 1 (n - 1));;

(* convertis une chaine en liste de joueur *)
let players_of_string str = 
    let rec urs n = function 
    | [] -> []
    | 'B'::l | 'b'::l -> (B(n))::urs (n + 1) l
    | 'H'::l | 'h'::l -> (H(n))::urs (n + 1) l
    | _::l -> failwith "Erreur dans l'utilisation de la fonction"
    in urs 1 (char_list_of_string str);;

(* retourne la taille de la main initiale en fonction du nombre de joueur *)
let hand_size = function 
    | 2 -> 7
    | 3 | 4 -> 6
    | _ -> failwith "Between 2 and 4 players, please!"

(*make_state_list*)

(*
       _                                                                  _         _ _ _   
      | |                                                                | |       | (_) |  
      | | ___ _   _   _ __  _ __ ___  _ __  _ __ ___ _ __ ___   ___ _ __ | |_    __| |_| |_ 
  _   | |/ _ \ | | | | '_ \| '__/ _ \| '_ \| '__/ _ \ '_ ` _ \ / _ \ '_ \| __|  / _` | | __|
 | |__| |  __/ |_| | | |_) | | | (_) | |_) | | |  __/ | | | | |  __/ | | | |_  | (_| | | |_ 
  \____/ \___|\__,_| | .__/|_|  \___/| .__/|_|  \___|_| |_| |_|\___|_| |_|\__|  \__,_|_|\__|
                     | |             | |                                                    
                     |_|             |_|                                                    
*)

(* convertis une chain en chaine *)
let string_of_chain = function
    | E -> ""
    | S(_, board, _) -> board

(* convertis un couple (dominos, joueur) en chaine *)
let string_of_state (dominoes, player) = Printf.sprintf ("%s:\n\t%s") (string_of_player player) (string_of_dominoes dominoes);;

(*list_shuffle*)

(*play*)