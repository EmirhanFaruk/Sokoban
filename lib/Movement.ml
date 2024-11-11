module Movement =
struct
open Tile
open UndoRedo
open GameState
open Player

(* Fonction qui vérifie si les coordonnées correspondent à un chemin valide *)
let isPath grid (x,y) =
  let (width,height) = GameState.get_dim grid in
  if x<0 || y<0 || x>= width|| y>= height then false
  else
    match grid.(y).(x) with
    | Tile.Wall -> false
    | _ -> true

(* Foncition qui met à jour la position du joueur dans la carte *)
let updatePlayerPosition (list_map : GameState.level_map) (player : Player.player) new_x new_y =
  let save_list = (UndoRedo.makeSave new_x new_y (GameState.getTile list_map.grid new_x new_y) Tile.Player) :: [] in
  GameState.modifyList list_map new_x new_y Tile.Player;  (* On met à jour la position du joueur dans la map *)
  Player.updatePlayerPos player (new_x, new_y);  (* On met à jour les coordonées joueur *)
  Player.stat_upt player.stat;
  save_list

(* Fonction qui met à jour l'ancienne case du joueur *)
let updateOriginalTile (list_map : GameState.level_map) old_x old_y=
  (* On réccupère la case d'origine à la position actuelle du joueur *)
  let old_original = list_map.original.(old_y).(old_x) in
  let old_tile = GameState.getTile list_map.grid old_x old_y in
  let save_list = ref [] in
  (* Si c'est un joueur ou une boite on change en Ground sinon on met l'original *)
  if old_original = Player || old_original = Box then
    (save_list := (UndoRedo.makeSave old_x old_y old_tile Ground) :: !save_list;
    GameState.modifyList list_map old_x old_y Ground)
  else
    (save_list := (UndoRedo.makeSave old_x old_y old_tile old_original) :: !save_list;
    GameState.modifyList list_map old_x old_y old_original);

  !save_list

(* Fonction qui met à jour la carte, la position du joueur et le mouvement des boîtes *)
let updateMap (list_map : GameState.level_map) (player : Player.player) direction (stacks : UndoRedo.stacks)=
  let (width, height) = GameState.get_dim list_map.grid in
  (* On réccupère la position suivante du joueur *)
  let new_x, new_y = Player.get_next_pos (player.pos.x, player.pos.y) direction (width, height) in

  (* Si la prochaine position est un chemin *)
  if isPath list_map.grid (new_x, new_y) then
    let old_x, old_y = player.pos.x, player.pos.y in
    let next_tile = list_map.grid.(new_y).(new_x) in
    let save_list = ref [] in

    let isBoxBlocked = ref false in
    if next_tile = Box || next_tile = BoxOnBoxGround then
      (* On essaie de déplacer la boîte *)
      let box_new_x, box_new_y = Player.get_next_pos (new_x, new_y) direction (width, height) in
      (* On vérifie si la nouvelle position de la boîte est un chemin valide *)
      if isPath list_map.grid (box_new_x, box_new_y) && not (list_map.grid.(box_new_y).(box_new_x) = Box) then
        (* Ancienne position de la boîte *)
        let box_current_tile = if (GameState.getTile list_map.grid box_new_x box_new_y) = Tile.BoxGround
          then Tile.BoxOnBoxGround else Tile.Box in

        (* On déplace la boîte *)
        save_list := (UndoRedo.makeSave box_new_x box_new_y (GameState.getTile list_map.grid box_new_x box_new_y) box_current_tile)::!save_list;
        GameState.modifyList list_map box_new_x box_new_y box_current_tile;

        (* On met à jour la position du joueur *)
        save_list := List.append (updatePlayerPosition list_map player new_x new_y) !save_list;

        (* On met à jour de l'ancienne position du joueur *)
        save_list := List.append (updateOriginalTile list_map old_x old_y ) !save_list
      else
        isBoxBlocked := true  (* On marque que la boîte est bloquée *)
    else
      (* On déplace seulement le joueur s'il n'y a pas de boîte *)
      save_list := List.append (updatePlayerPosition list_map player new_x new_y) !save_list;

    (* Si la boîte n'est pas bloquée, on met à jour l'ancienne position *)
    if not !isBoxBlocked then
      save_list := List.append (updateOriginalTile list_map old_x old_y) !save_list;
      UndoRedo.addSavesToStack stacks !save_list;
      list_map.grid  (*Carte mise à jour *)
  else
    list_map.grid  (*Carte sans modification sinon *)
end