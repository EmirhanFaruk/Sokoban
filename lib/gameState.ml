(* lib/GameState.ml *)

(*Représentation des éléments de la carte*)
type map = {
  
  wall : char;
  ground : char;
  box : char;
  boxGround : char;
  player : char;
}


(* Exception pour signaler qu'un niveau n'a pas été trouvé *)
exception Level_not_found of int

(* Fonction pour charger une carte d'un fichier texte *)
let loadMap filename niveau =

  (* Principe : Nous cherchons dans filename le niveau demandé en parcourant chaque block de niveau,
  une fois trouvé nous ajouton le block de niveau dans une liste puis inversion le contenu de la liste
  afin que cela soit dans l'ordre. Si le niveau n'existe pas on renvoit une erreur. *)

  let ic = open_in filename in
  let rec loop current_level current_map =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = ';' then
        (* Extraire le numéro du niveau à partir de la ligne *)
        let level_num = int_of_string (String.sub line 2 (String.length line - 2)) in
        if level_num = niveau then
          (* Si c'est le niveau désiré, initialiser la carte *)
          loop niveau []
        else
          (* Passer au niveau suivant si ce n'est pas le bon *)
          loop niveau current_map
      else if current_level = niveau then
        (* Ajouter la ligne à la carte du niveau *)
        loop niveau (line :: current_map)
      else
        (* Continuer à chercher le niveau *)
        loop current_level current_map
    with
    | End_of_file ->
        close_in ic;
        if current_map = [] then
          raise (Level_not_found niveau)
        else
          List.rev current_map  (* Inverser la liste pour retrouver l'ordre original *)
  in
  try
    loop (-1) []  (* On commence à un niveau "invalide" (-1) pour la recherche *)
  with
  | Level_not_found lvl ->
      Printf.printf "Le niveau %d n'a pas été trouvé dans le fichier.\n" lvl;
      []
  




