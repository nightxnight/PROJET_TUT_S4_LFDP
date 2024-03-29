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
let string_of_domino (D(a, b)) = Printf.sprintf "%d-%d" a b;;

(* retourne un domino *)
let flip (D(a,b)) = (D(b,a));;

(* ajoute un domino a une extremite de la chain *)
let append = function
  | D(a,b), E, _ -> S (a, string_of_domino (D(a, b)), b)
  | D(a, b), S(_, board, fin), '<' -> S(a, Printf.sprintf "%s %s" (string_of_domino (D(a, b))) board, fin)
  | D(a, b), S(debut, board, _), '>' -> S (debut, Printf.sprintf "%s %s" board (string_of_domino (D(a, b))), b)
  | _ -> failwith "Direction invalide";;

(* renvoie sous forme de liste les chain legales apres placement d'un domino *)
let legal_adds domino = function
  |E-> [(append(domino,E,'>'))]
  |S(d, _,f) as chain ->
    let place = function 
      | D(a,b) when d = b && f = a && a != b -> [append(domino, chain, '<'); append(domino, chain, '>')]
      | D(a,b) when d = a && f = b && a != b -> [append(flip domino, chain, '<') ; append(flip domino, chain, '>')]
      | D(a,b) when d = a -> [append(flip domino, chain,'<')]
      | D(a,b) when d = b -> [append(domino, chain, '<')]
      | D(a,b) when f = a -> [append(domino, chain, '>')]
      | D(a,b) when f = b -> [append(flip domino, chain,'>')]
      |_->[]
    in place domino


(*
Anciennes versions de legal_adds:

let legal_adds (D(a, b)) = function
  | E -> [append (D(a, b), E, ' ')] 
  | chain ->
    let place (D(a, b)) = function
      | S(d, _, f) as chain when d=b && f=a -> [append (D(a, b), chain, '<'); append (D(a, b), chain, '>');] 
      | S(_, _, f) as chain when a=f -> [append (D(a, b), chain, '>')] 
      | S(d, _, _) as chain when b=d -> [append (D(a, b), chain, '<')] 
      | _ -> []
    in 
    let is_double = function 
      | D(x, y) when x = y -> place (D(x, y)) chain
      | domino -> place domino chain @ place (flip domino) chain
    in is_double (D(a, b));;

let legal_adds (D(a, b)) chain =
  match chain with
  | E -> [S(a, string_of_domino (D(a,b)), b)] 
  | S(d, _, f) as chain when (a=d && a=f) || (b=d && b=f) -> [append (D(a, b), chain, '<'); append (D(a, b), chain, '>')] 
  | S(d, _, f) -> 
    let place (D(a, b)) = function
      | S(d, _, f) as chain when b=d && a=f-> [append (D(a, b), chain, '<'); append (D(a, b), chain, '>')] 
      | S(d, _, _) as chain when b=d -> [append (D(a, b), chain, '<')] 
      | S(_, _, f) as chain when a=f -> [append (D(a, b), chain, '>')] 
      | _ -> []
    in place (D(a, b)) chain @ place (flip (D(a, b))) chain;; 

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
    in (place (D(a, b)) chain) @ (place (flip (D(a, b))) chain);;
*)

(* renvoie la sous-liste des dominos placable d'une liste de dominos et d'une chain donnee *)
let possible_dominoes dominoes = function
  | E -> dominoes 
  | chain ->
    let rec urs = function
      | [] -> []
      | x::l when legal_adds x chain != [] -> x::urs l 
      | _::l -> urs l
    in urs dominoes;;

(*
Ancienne version:

let possible_dominoes dominoes chain =
  if chain = E then dominoes
  else
    let rec urs = function
      | [] -> []
      | x::l when (legal_adds x chain) != [] -> x::(urs l)
      | _::l -> urs l
    in urs dominoes;;
*)

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

(* convertis une chaine en domino *)
let domino_of_string str = 
  let x = int_of_string (List.nth (String.split_on_char '-' str) 0)
  and y = int_of_string (List.nth (String.split_on_char '-' str) 1) 
  in D(x,y);;

(* teste si une chaine donne est bien un domino *)
let is_domino str = 
  if String.contains str '-' = false then false
  else if String.index str '-' != String.rindex str '-' then false
  else try
      let result = function
        | x, y when 0 <= int_of_string x && 0 <= int_of_string y -> true
        | _ -> false  
      in result ((String.trim (List.nth (String.split_on_char '-' str) 0)),(String.trim (List.nth (String.split_on_char '-' str) 1)))
    with _ -> false;;

(* permet d'effectuer des saisies clavier avec predicat et cast *)
let read = ref read_line;;
let rec input_valid prompt is_valid cast =
  let rec urs () =
    let s = !read () in
    if is_valid s then cast s
    else
      let () = print_endline "Réessayez!" in
      urs ()
  in
  print_endline prompt; urs ();;

(* supprime un domino d'une liste de dominos *)
let rec suppress d = function 
  | [] -> []
  | x::l when x = d || x = flip d -> l
  | x::l -> x::suppress d l;;

(* fonction qui permet de jouer un domino parmis une liste de dominos, dans une chaine donnee*)
let input_move select_domino select_end chain dominoes = 
  let selected_end domino chain = 
    match (legal_adds domino chain) with
    | [] -> None
    | x::[] -> Some x
    | x::y::_ -> Some (select_end x y) 
  in
  let selected_domino dominoes chain = 
    match (possible_dominoes dominoes chain) with
    | [] -> print_string "\tAucun coup possible!\n"; None
    | x::[] -> print_string (Printf.sprintf ("\tCoup forcé:\t%s\n") (string_of_domino x)); Some x
    | l -> Some (select_domino l)  
  in 
  let result domino chain = 
    match (selected_end domino chain) with  
    | None -> None
    | Some chain -> Some ((suppress domino dominoes), chain)
  in 
  let exec dominoes chain = 
    match (selected_domino dominoes chain) with
    | None -> None
    | Some domino -> result domino chain
  in exec dominoes chain;;

(* melange une liste, fonction polymorphique *)
let list_shuffle l = 
  let rec urs = function
    | [] -> []
    | (n, x)::l -> x::urs l
  in urs (List.sort (fun x y -> compare (fst x) (fst y)) (List.map (function x -> (Random.bits (), x)) l));;

(* fonction a executer par les bots pour jouer *)
let input_bot_move chain dominoes = input_move 
    (function l -> let choosen_domino d = print_string (Printf.sprintf("\tà placer :  \t%s\n") (string_of_domino d)) ; d in choosen_domino (List.nth l (Random.int (List.length l))))
    (fun c1 c2 -> List.nth [c1;c2] (Random.int (List.length [c1;c2])))
    chain
    dominoes

(* fonction a executer par les humains pour jouer*)
let input_human_move chain dominoes = input_move
    (function dominoes -> input_valid
                            ("Quel domino voulez-vous poser ?")
                            (function str -> is_domino str && List.mem (domino_of_string str) dominoes)
                            domino_of_string)

    (fun c1 c2 -> if (input_valid
                        ("A quel bout ?")
                        (function str -> str = "<" || str = ">")
                        (function x -> x)) = "<" then c1 else c2)
    chain
    dominoes                                      

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
  | H x -> Printf.sprintf "Joueur %d (%s)" x "humain"
  | B x -> Printf.sprintf "Joueur %d (%s)   " x "bot";;

(* transfert un nombre n de dominos de la source a la destination *)
let rec take destination n source = 
  match n, source with
  | 0, source -> (destination, source)
  | n, [] -> (destination, [])
  | n, (D(a, b))::ts -> take ((D(a, b))::destination) (n-1) ts;;

(* convertis une liste de domino en chaine *)
let rec string_of_dominoes = function
  | [] -> "" 
  | x::[] -> string_of_domino x
  | x::l -> Printf.sprintf "%s %s" (string_of_domino x) (string_of_dominoes l);;

(* fonction qui fait jouer un joueur si il le peut, il piochera sinon *)
let move stack chain hand player = 
  print_string (Printf.sprintf ("%s:\n") (string_of_player player));
  print_string (Printf.sprintf ("\tmain : %s\n") (string_of_dominoes hand));
  let result chain dominoes f =
    match (f chain dominoes) with
    | None -> let draw (new_hand, new_stack) = (new_stack, chain, new_hand) in draw (take hand 2 stack)
    | Some(new_hand, new_chain) -> (stack, new_chain, new_hand)
  and
    input_x_move = function
    | B _ -> input_bot_move
    | H _ -> input_human_move
  in result chain hand (input_x_move player);;


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
    | 0, 0 -> [D(0,0)]
    | x, 0 -> (D(x,0))::urs (x-1, x-1)
    | x, y -> (D(x,y))::urs (x, y-1)
  in urs (n, n);;

(*
Ancienne version:

let make_dominoes n = 
  let rec urs = function
    | (-1, -1) -> []
    | (x,-1) -> urs (x-1, x-1)
    | (x,y) -> (D(x,y))::urs (x, y-1)
  in urs (n, n);;
*)

(* convertis un tableau en liste de caractère *)
let rec char_list_of_string str = 
  match String.length str with
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
let get_hand_size = function 
  | 2 -> 7
  | 3 | 4 -> 6
  | _ -> failwith "Between 2 and 4 players, please!"

(* cree un couple compose de la pioche et de couples d'un joueur et de ses dominos *)
let make_state_list players_str dominoes =
  let rec urs players dominoes to_take state_list = 
    match (players, take [] to_take dominoes) with
    | [], (taken, rest) -> (List.rev (rest @ taken), List.rev state_list)
    | p::l, (hand, stack) -> urs l stack to_take ((hand, p)::state_list)
  in urs (players_of_string players_str) dominoes (get_hand_size (String.length players_str)) [];;

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
  | S(_,board,_)->board

(* convertis un couple (dominos, joueur) en chaine *)
let string_of_state (dominoes, player) = Printf.sprintf ("%s:\t%s") (string_of_player player) (string_of_dominoes dominoes);;

(* permet de lancer une partie de dominos en fonction du nombre max des dominos ainsi que les joueurs sous forme de chaine *)
let play domino_max player_str = 
  let state_list = make_state_list player_str (list_shuffle (make_dominoes domino_max)) 
  in
  let player_play player_and_hand stack chain = 
    match (move stack chain (fst player_and_hand) (snd player_and_hand)) with 
    | (_, new_chain, new_hand) as result-> 
      print_string (Printf.sprintf "\tchaîne :    \t%s\n" (string_of_chain new_chain));
      let check_game_state hand = 
        match new_hand with
        | [] -> None 
        | _ when chain = new_chain -> Some (1, result)
        | _ -> Some (0, result)
      in check_game_state new_hand
  in
  let rec loop players_list stack chain = function
    | x when x = (List.length players_list) -> print_string "Match nul!\n"; ()
    | stop ->
      match player_play (List.hd players_list) stack chain with
      | None -> print_string (Printf.sprintf "%s a gagné!\n" (string_of_player (snd (List.hd players_list)))) ; ()
      | Some (pass, (new_stack, new_chain, hand)) -> loop ((List.tl players_list) @ [(hand, snd (List.hd players_list))]) new_stack new_chain ((stop + pass) * pass) 
  in 
  List.iter (function x -> print_string (Printf.sprintf "%s\n" (string_of_state x))) (snd state_list);
  loop (snd state_list) (fst state_list) E 0;;