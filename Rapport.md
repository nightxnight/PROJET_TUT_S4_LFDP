# DOMIN🁏CAML

## Soutenance de projet le 5 mars 2021

## Membres du groupe
#### MARSAL Remi
#### SERANO Waïan
#### RAHUEL Victor
#### SIMON Marco
#### AKNIOU Mehdi

## Sommaire
* **I**. Introduction
* **II**. Analyse du sujet
* **III**. Répartition des tâches
* **IV**. Choix et méthodes de travail
* **V**. Evolution du projet
* **VI**. Difficultés rencontrées
* **VII**. Conclusion

# I. Introduction
Dans le cadre de notre DUT Informatique, nous sommes souvent amenés à réaliser de nombreux projets afin d'appliquer les notions et les technologies vues en TD et TP. Durant les cours de **programmation fonctionnelle**, nous avons vu une multitude de choses en plus de la découverte du langage **OCaml**. Notre professeur Aristide Grange a donc pris le temps de rédiger, pour nous, un projet tuteuré afin d'appliquer ces notions. Dans ce rapport, vous trouverez tous les détails de ce projet.

# II. Analyse du sujet
Ce projet tuteuré implique l'utilisation d'**OCaml** pour créer un jeu de dominos. Ce langage a la particularité d'être un langage de **programmation fonctionnelle**. Il est donc différent des langages impératifs auxquels nous sommes habitués comme Java, C# ou encore PHP.

Le sujet se compose de plusieurs parties et sous-parties nous permettant de mieux comprendre les attentes du professeur. Afin de pouvoir traiter et développer efficacement les fonctions, elles ont été décrites en détail. De plus, un fichier de tests est fourni afin de nous permettre de vérifier que les fonctions ont été écrites correctement. Cela nous permet également de vérifier l'intégralité des cas d'utilisation de chaque fonction.

L'objectif de ce projet et de nous faire découvrir une nouvelle facette de la programmation ainsi que les **avantages** et **inconvéniants** de la programmation fonctionnelle.

# III. Répartition des tâches
Les tâches ont été reparties par **niveau** principalement. Cependant, afin que les membres les plus en difficultés puissent approfondir leurs connaissances, nous avons décidé de réaliser une partie des tâches les plus complexes en **groupe**. 
Waïan Serano a mis en place un **tableau Kanban** sur le repository **GitHub** du projet afin que l'on puisse suivre l'avancée du projet directement sur **GitHub**. C'est donc grâce à ce tableau que l'on a réparti les tâches qui a été globalement respectée. D'autant plus que l'aide de Remi Marsal, qui a le meilleur niveau du groupe en **OCaml**, a été d'une grande utilité dans la compréhension globale des fonctions. De plus, le **travail en équipe** nous a permis d'améliorer une grande partie des fonctions.

# IV. Choix et méthodes de travail
## a. Communication via Discord
Nous avons choisi **Discord** pour communiquer entre nous. Les **réunions quotidiennes** nous ont permis d'être **coordonnées** et particulièrement **efficaces** sur le développement du projet.

## b. Partage du code source via GitHub
Dans plusieurs projets auparavant, nous avons appris à utiliser GitHub. On a donc choisi cette méthode pour partager et mettre en commun ce projet. Cela nous permet de récupérer rapidement les changements qu'a pu effectuer un membre du groupe. De plus, le système de tableau Kanban intégré à GitHub nous a permis de répartir les tâches et de suivre l'évolution du projet.

## c. Compilateur OCaml
Afin de compiler et de tester les différentes fonctions du projet, nous avons utilisé **Jupyter** et **TryOCaml**.

# V. Evolution du projet
Il est important pour nous de conserver une bonne ambiance au sein de notre équipe de projet. En effet, tout le monde a fait l'effort de venir à chaque réunion que l'on s'était imposée en plus de la réunion de présentation avec le professeur.

L'intérêt que nous avons porté au projet nous a permis d'optimiser le programme tout en respectant les conseils du professeur concernant l'utilisation de la forme du ***curry***, la **correspondance de motifs** (**filtrage par motifs réservés**) et en supprimant les parenthèses inutiles.

Les **réunions** avec le professeur ont beaucoup aidé au développement du projet et à l'amélioration des fonctions. Cela nous a donc permis de **comprendre** une grande partie des notions avec lesquelles nous avions des difficultés.

# VI. Difficultés recontrées
Nous avons eu quelques problèmes lors de l'écriture de certaines fonctions. En effet, certaines fonctions ont été plus problématiques car plusieurs versions fonctionnaient.

## a. Fonction legal_adds
La fonction legal_adds est probablement la fonction qui a subis le plus de changement. On a pu distinguer deux versions bien distincts

### Version n°1
La première version comporte du **pattern-matching** classique
```ocaml
let legal_adds domino = function
  |E-> [(append(domino,E,'>'))]
  |S(d, _,f) as chain -> function
    | D(a,b) when (d = b && f = a) && a != b -> [append(domino, chain, '<'); append(domino, chain, '>')]
    | D(a,b) when (d = a && f = b) && a != b -> [append(flip domino, chain, '<') ; append(flip domino, chain, '>')]
    | D(a,b) when d = a -> [append(flip domino, chain,'<')]
    | D(a,b) when d = b -> [append(domino, chain, '<')]
    | D(a,b) when f = a -> [append(domino, chain, '>')]
    | D(a,b) when f = b -> [append(flip domino, chain,'>')]
    |_->[];;

val legal_adds : domino -> chain -> chain list = <fun>
```

### Version n°2
La deuxième version implique **l'utilisation de fonctions** à l'interieur de la fonction elle-même.
```ocaml
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

val legal_adds : domino -> chain -> chain list = <fun>
```

### Version n°3
Cette version réalise une **première partie des comparaisons avec les cas sans l'utilisation de la fonction flip puis avec**.
```ocaml
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

val legal_adds : domino -> chain -> chain list = <fun>
```
On constate alors que la **valeur** des trois fonctions est **identique**, donc on entre bien les mêmes **arguments** et on retourne bien le même **type**. La **version n°1** et la **version n°2** ont été gardé car elles possédaient une approche différente. La **version n°3** permettait de **réduire grandement le nombre de comparaisons**. lors de la réalisation des tests, il s'est avéré que dans certains cas, la **version n°2** et la **version n°3** comportaient des **erreurs**. Nous avons alors choisi la **version n°1** qui réalisait les tests avec **succès**.

## b. Fonction input_move
Cette fonction a, elle aussi, subit quelques changements.

### Version n°1
La principale particularité de cette version est qu'elle implique l'utilisation des **erreurs**.
```ocaml
let input_move select_domino select_end chain dominoes = 
  let selected_domino dominoes chain = 
    match (possible_dominoes dominoes chain) with
    | [] -> raise (Invalid_argument "No playable dominoes")
    | x::[] -> x
    | l -> (select_domino l) 
  in 
  let selected_end domino chain = 
    match (legal_adds domino chain) with
    | [] -> raise (Invalid_argument "No playable dominoes")
    | x::[] -> x
    | x::y::_ -> select_end x y
  in 
  let new_some new_dominoes chain = Some(new_dominoes, chain)
  in
  let resultat dominoes chain domino = try new_some (suppress domino dominoes) (selected_end domino chain) with _ -> None
  in try resultat  dominoes chain (selected_domino dominoes chain) with _ -> None;;

val input_move :
  (domino list -> domino) ->
  (chain -> chain -> chain) ->
  chain -> domino list -> (domino list * chain) option = <fun>
```

### Version n°2
Dans cette version, on a remplacé l'utilisation des **erreurs** par l'utilisation du **type option**.
```ocaml
let input_move select_domino select_end chain dominoes = 
  let selected_end domino chain = 
    match (legal_adds domino chain) with
    | [] -> None
    | x::[] -> Some x
    | x::y::_ -> Some (select_end x y) 
  in
  let selected_domino dominoes chain = 
    match (possible_dominoes dominoes chain) with
    | [] -> print_string "Aucun coup possible!\n"; None
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

val input_move :
  (domino list -> domino) ->
  (chain -> chain -> chain) ->
  chain -> domino list -> (domino list * chain) option = <fun>
```

Dans la **version  n°1**, l'éxecution des fonctions **selected_domino** et **selected_end** était dans le **mauvais ordre**. De plus, lors de l'écriture de cette version, le **type option** ne fonctionnait pas. C'est pour cela que dans la **version n°2**, on utilise le **type option** qui permet de réaliser les tests avec **succès**.

## c. Fonction make_dominoes
Dans cette fonction, il y a eu un léger changement.

### Version n°1
```ocaml
let make_dominoes n = 
  let rec urs = function
    | (-1, -1) -> []
    | (x,-1) -> urs (x-1, x-1)
    | (x,y) -> (D(x,y))::urs (x, y-1)
  in urs (n, n);;
```

### Version n°2
```ocaml
let make_dominoes n = 
  let rec urs = function
    | 0, 0 -> [D(0,0)]
    | x, 0 -> (D(x,0))::urs (x-1, x-1)
    | x, y -> (D(x,y))::urs (x, y-1)
  in urs (n, n);;
```

Dans la **version n°1**, l'**itération** a commencé à **-1**, ce qui n'était pas correct dans certains cas. De plus, le **pattern-matching** n'était pas optimale. Ces erreurs ont donc été corrigées avec la **version n°2**.

# VII. Conclusion
Grâce à la **programmation fonctionnelle** et à ce **projet**, nous avons **découvert** une nouvelle façon de programmer très différente d’un langage impératif tel que le PHP ou encore le C#.

Nous avons également pu **perfectionner** notre niveau en programmation que nous avons acquis grâce à notre parcours scolaire post-bac, nos compétences en **gestion de projet** (notamment avec l’utilisation de GitHub) et le **travail en équipe** qui est un **aspect essentiel** pour nous, puisque de nombreux projets collectifs seront surement présents dans notre future vie professionnelle.

Même si nous avons rencontré certaines **difficultés**, nous nous sommes **soutenus** afin d’atteindre les **objectifs attendus**. Au vu des résultats que nous avons aujourd’hui, nous pouvons affirmer que le **travail du groupe** a été une belle **réussite** dans tous les domaines. 

Enfin, nous remercions Monsieur Grange Artistide de nous avoir fait découvrir le langage **OCaml**, et de nous avoir aidé à **progresser** de jour en jour grâce à ses nombreux conseils.