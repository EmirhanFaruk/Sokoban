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
        (* Si on trouve une nouvelle déclaration de niveau *)
        let level_num = int_of_string (String.sub line 2 (String.length line - 2)) in
        if level_num = niveau then
          (* Si le niveau est celui que l'on cherche, on continue à remplir `current_map` *)
          loop niveau []
        else if current_level = niveau then
          (* Si on a terminé de lire le niveau, on arrête la lecture *)
          List.rev current_map
        else
          (* Continuer la recherche du niveau *)
          loop level_num current_map
      else if current_level = niveau then
        (* Ajouter la ligne à la carte du niveau en cours *)
        loop niveau (line :: current_map)
      else
        (* Continuer le parcours du fichier *)
        loop current_level current_map
    with
    | End_of_file ->
        close_in ic;
        if current_map = [] then
          []  (* Niveau non trouvé, retourne une liste vide *)
        else
          List.rev current_map  (* Retourner la carte trouvée dans l'ordre *)
  in
  loop (-1) []  (* On démarre avec un niveau initial "invalide" (-1) *)



