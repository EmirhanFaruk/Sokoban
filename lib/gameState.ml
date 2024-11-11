module GameState =
struct
  open Tile
  open Player

  (* Type qui représente la liste de la map modifiable quand on veut *)
  type level_map = {
    mutable grid: Tile.tile array array;
    mutable original : Tile.tile array array;
  }

  (* Exception pour signaler qu'un niveau n'a pas été trouvé *)
  exception Level_not_found of int

  (** Exception levée lorsque des coordonnées sont en dehors de la carte *)

  exception Isnt_in_the_list of (int * int)

  (* Extraire le numéro de niveau à partir d'une ligne *)
  let get_level_number line =
    let level_str = String.trim (String.sub line 2 (String.length line - 2)) in
    int_of_string level_str

  (* Fonction pour charger une carte d'un fichier texte *)
  let loadMap filename niveau (player : Player.player) =
    (* Principe : Nous cherchons dans filename le niveau demandé en parcourant chaque bloc de niveau.
      Une fois trouvé, nous ajoutons le bloc de niveau dans une liste puis inversons le contenu de la liste
      afin que cela soit dans l'ordre. Si le niveau n'existe pas, on renvoie une erreur. *)
    let ic = open_in filename in
    let rec loop current_level current_map y =
      try
        let line = input_line ic in
        if String.length line > 0 && line.[0] = ';' then
          (* Si on trouve une nouvelle déclaration de niveau *)
          let level_num = get_level_number line in
          if level_num = niveau then
            (* Si le niveau est celui que l'on cherche, on continue à remplir `current_map` *)
            loop niveau [] 0
          else if current_level = niveau then
            (* Si on a terminé de lire le niveau, on arrête la lecture *)
            List.rev current_map  (* Pas encore converti en array *)
          else
            (* Continuer la recherche du niveau *)
            loop level_num current_map y
        else if current_level = niveau then
          (* Ajouter la ligne à la carte du niveau en cours *)
          let row = Tile.load_line_to_tiles line y player in
          loop niveau (row :: current_map) (y + 1)
        else
          (* Continuer le parcours du fichier *)
          loop current_level current_map y
      with
      | End_of_file ->
          close_in ic;
          if current_map = [] then
            raise (Level_not_found niveau)   (* Si le niveau n'est pas trouvé alors on renvoie une erreur *)
          else
            List.rev current_map  (* Retourner la carte trouvée dans l'ordre *)
    in
    let map_list = loop (-1) [] 0 in
    (* Convertir la carte de liste de liste en array array *)
    let map = Array.of_list (List.map Array.of_list map_list) in
    { grid = map ; original = Array.map Array.copy map }   (* On retourne un level_map avec la grille construite *)

  (* Fonction qui renvoit la dimension de la map *)
  let get_dim grid =
    let height = Array.length grid in
    let width = 
      match grid with
      | [||] -> 0
      | h -> Array.length h.(0)
    in
    (width, height)
  (* Fonction pour obtenir la tuile au coordonnée (x,y) *) 
  let getTile (map : Tile.tile array array) x y =
    map.(y).(x)

  (* Fonction qui permet de modifier les éléments de la liste level_map *)
  let modifyList (list_map : level_map) x y element =
    (* On vérifie que list_map.grid[x][y] fait bien partie de la liste puis on change l'élément de la list_map.grid[x][y] par element *)
    if x>=0 && x < Array.length list_map.grid then
      let row = list_map.grid.(x) in
      if y>=0 && y < Array.length row then
        list_map.grid.(y).(x) <- element  (* Met à jour directement list_map.grid *)
      else
        raise (Isnt_in_the_list (x, y))  (* Lever une erreur pour indices invalides *)
      else
        raise (Isnt_in_the_list (x, y))  (* Lever une erreur pour indices invalides *)


  (* Fonction pour trouver toutes les coordonnées de BoxGround dans la carte d'origine *)
  let find_boxground_positions (map: level_map) =
    let positions = ref [] in
    Array.iteri (fun y row ->
      Array.iteri (fun x cell ->
        match cell with
        | Tile.BoxGround -> positions := (x, y) :: !positions
        | _ -> ()
      ) row
    ) map.original;
    !positions

  (* Fonction pour vérifier si toutes les positions de BoxGround dans original_map contiennent une Box *)
  let isAllBox (map: level_map) =
    let boxground_positions = find_boxground_positions map in
    List.for_all (fun (x, y) ->
      match (map.grid).(y).(x) with
      | Tile.BoxOnBoxGround -> true
      | _ -> false
    ) boxground_positions

  (* Fonction qui renvoie la copie d'une map*)
  let copyMap map =
    Array.map Array.copy map


  end