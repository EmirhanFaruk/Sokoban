
(* Les différents éléments de la carte *)
module GameState =
struct
  open Player
  type tile = Wall | Ground | Box | BoxGround | Player

  (* Type qui représente la liste de la map modifiable quand on veut *)
  type level_map = {
    mutable grid: tile array array;
    mutable original : tile array array;
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
  let load_line_to_tiles line y (player: Player.pos) =
    let _, tile_list = List.fold_right (fun c (x, acc) ->
      match c with
      | '#' -> (x - 1, Wall :: acc)
      | ' ' -> (x - 1, Ground :: acc)
      | '$' -> (x - 1, Box :: acc)
      | '.' -> (x - 1, BoxGround :: acc)
      | '@' ->
          (* Mettre à jour la position du joueur *)
          Player.updatePlayer player (x,y);
          (x - 1, Player :: acc)
      | _ -> (x - 1, acc)  (* Ignorer les caractères non spécifiés *)
    ) (string_to_char_list line) ((String.length line - 1), []) in
    tile_list

  (* Fonction pour charger une carte d'un fichier texte *)
  let loadMap filename niveau player =
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
          let row = load_line_to_tiles line y player in
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

    
  (* Fonction qui renvoit la dimension de la map *)
  let get_dim grid =
    let height = Array.length grid in
    let width = 
      match grid with
      | [||] -> 0
      | h -> Array.length h.(0)
    in
    (width, height)

  (* Fonction qui vérifie si les coordonnées correspondent à un chemin valide *)
  let isPath grid (x,y) =
    let (width,height) = get_dim grid in
    if x<0 || y<0 || x>= width|| y>= height then false
    else
      match grid.(y).(x) with
      | Wall -> false  
      | _ -> true
      
  (* Foncition qui met à jour la position du joueur dans la carte *)
  let updatePlayerPosition list_map (player : Player.pos) new_x new_y current_tile =
    modifyList list_map new_x new_y current_tile;  (* On met à jour la position du joueur dans la map *)
    Player.updatePlayer player (new_x, new_y)  (* On met à jour les coordonées joueur *)

  (* Fonction qui met à jour l'ancienne case du joueur *)
  let updateOriginalTile list_map old_x old_y =
    (* On réccupère la case d'origine à la position actuelle du joueur *)
    let old_original = list_map.original.(old_y).(old_x) in
    (* Si c'est un joueur ou une boite on change en Ground sinon on met l'original *)
    if old_original = Player || old_original = Box then
      modifyList list_map old_x old_y Ground
    else
      modifyList list_map old_x old_y old_original

  (* Fonction qui met à jour la carte, la position du joueur et le mouvement des boîtes *)
  let updateMap (list_map : level_map) (player : Player.pos) direction =
    let (width, height) = get_dim list_map.grid in
    (* On réccupère la position suivante du joueur *)
    let new_x, new_y = Player.get_next_pos (player.x, player.y) direction (width, height) in

    (* Si la prochaine position est un chemin *)
    if isPath list_map.grid (new_x, new_y) then
      let old_x, old_y = player.x, player.y in
      let current_tile = list_map.grid.(old_y).(old_x) in
      let next_tile = list_map.grid.(new_y).(new_x) in

      let isBoxBlocked = ref false in
      if next_tile = Box then
        (* On essaie de déplacer la boîte *)
        let box_new_x, box_new_y = Player.get_next_pos (new_x, new_y) direction (width, height) in
        (* On vérifie si la nouvelle position de la boîte est un chemin valide *)
        if isPath list_map.grid (box_new_x, box_new_y) && not (list_map.grid.(box_new_y).(box_new_x) = Box) then
          (* Ancienne position de la boîte *)
          let box_current_tile = list_map.grid.(new_y).(new_x) in
          
          (* On déplace la boîte *)
          modifyList list_map box_new_x box_new_y box_current_tile;
          
          (* On met à jour la position du joueur *)
          updatePlayerPosition list_map player new_x new_y current_tile;

          (* On met à jour de l'ancienne position du joueur *)
          updateOriginalTile list_map old_x old_y
        else 
          isBoxBlocked := true  (* On marque que la boîte est bloquée *)
      else 
        (* On déplace seulement le joueur s'il n'y a pas de boîte *)
        updatePlayerPosition list_map player new_x new_y current_tile;

      (* Si la boîte n'est pas bloquée, on met à jour l'ancienne position *)
      if not !isBoxBlocked then
        updateOriginalTile list_map old_x old_y;  
        list_map.grid  (*Carte mise à jour *)
    else
      list_map.grid  (*Carte sans modification sinon *)
end