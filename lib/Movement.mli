(** Module Movement : gère le mouvement de joueur *)
module Movement : sig
    (* Fonction qui vérifie si les coordonnées correspondent à un chemin valide *)
    val isPath : GameState.level_map -> int * int -> bool

    (* Foncition qui met à jour la position du joueur dans la carte *)
    val updatePlayerPosition : GameState.level_map -> Player.player -> int -> int -> unit

    (* Fonction qui met à jour l'ancienne case du joueur *)
    val updateOriginalTile : GameState.level_map -> int -> int -> UndoRedo.save list

    (* Fonction qui met à jour la carte, la position du joueur et le mouvement des boîtes *)
    val updateMap : GameState.level_map -> Player.player -> Player.direction -> UndoRedo.stacks -> Tile.tile array array
end