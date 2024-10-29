(* Les différents éléments de la carte *)
type tile = Wall | Ground | Box | BoxGround | Player

(* Type qui représente la liste de la map modifiable quand on veut *)
type level_map = {
  mutable grid: tile list list;
}

(* Exception pour signaler qu'un niveau n'a pas été trouvé *)
exception Level_not_found of int
exception Isnt_in_the_list of (int * int)

(* Convertir une chaîne en liste de caractères *)
let string_to_char_list s =
  List.init (String.length s) (fun i -> s.[i])

(* Extraire le numéro de niveau à partir d'une ligne *)
let get_level_number line =
  let level_str = String.trim (String.sub line 2 (String.length line - 2)) in
  int_of_string level_str

(* Charge une ligne de caractères en une liste de tuiles *)
let load_line_to_tiles line =
  List.fold_right (fun c acc ->
    match c with
    | '#' -> Wall::acc
    | ' ' -> Ground::acc
    | '$' -> Box::acc
    | '.' -> BoxGround::acc
    | '@' -> Player::acc
    | _ -> acc
  ) (string_to_char_list line) []

(* Fonction pour charger une carte d'un fichier texte *)
let loadMap filename niveau =
  (* Principe : Nous cherchons dans filename le niveau demandé en parcourant chaque block de niveau,
  une fois trouvé nous ajoutons le block de niveau dans une liste puis inversions le contenu de la liste
  afin que cela soit dans l'ordre. Si le niveau n'existe pas on renvoit une erreur. *)
  let ic = open_in filename in
  let rec loop current_level current_map =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = ';' then
        (* Si on trouve une nouvelle déclaration de niveau *)
        let level_num = get_level_number line in
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
        let row = load_line_to_tiles line in
        loop niveau (row :: current_map)
      else
        (* Continuer le parcours du fichier *)
        loop current_level current_map
    with
    | End_of_file ->
        close_in ic;
        if current_map = [] then
          raise (Level_not_found niveau)   (* Si le niveau n'est pas trouvé alors on renvoit une erreur *)
        else
          List.rev current_map  (* Retourner la carte trouvée dans l'ordre *)
  in
  { grid = loop (-1) [] }  (* On retourne un level_map avec la grille construite *)

(* Fonction qui permet de modifier les éléments de la liste level_map *)
let modifyList (list_map : level_map) x y element =
  (* On vérifie que list_map.grid[x][y] fait bien partie de la liste puis on change l'élément de la list_map.grid[x][y] par element *)
  if x >= 0 && x < List.length list_map.grid then
    let row = List.nth list_map.grid x in
    if y >= 0 && y < List.length row then
      let new_row = List.mapi (fun i el -> if i = y then element else el) row in
      list_map.grid <- List.mapi (fun i r -> if i = x then new_row else r) list_map.grid;  (* Met à jour directement list_map.grid *)
    else
      raise (Isnt_in_the_list (x, y))  (* Lever une erreur pour indices invalides *)
  else
    raise (Isnt_in_the_list (x, y))  (* Lever une erreur pour indices invalides *)
