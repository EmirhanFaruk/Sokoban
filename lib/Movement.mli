open GameState
open Player
open UndoRedo
open Tile

(** Module Movement : gère le mouvement de joueur *)
module Movement : sig
  (* Vérifie si les coordonnées (x, y) correspondent à un chemin valide *)
  val isPath : Tile.tile array array -> int * int -> bool

  (* Fonction qui met à jour la position du joueur sur la carte et retourne une liste de sauvegarde pour undo *)
  val updatePlayerPosition : GameState.level_map -> Player.player -> int -> int -> UndoRedo.save list

  (* Fonction qui met à jour l'ancienne case du joueur *)
  val updateOriginalTile : GameState.level_map -> int -> int -> UndoRedo.save list

  (* Fonction qui met à jour la carte, la position du joueur et le mouvement des boîtes,
  enregistre les modifications dans les piles de undo/redo. *)
  val updateMap : GameState.level_map -> Player.player -> Player.direction -> UndoRedo.stacks -> Tile.tile array array
end
