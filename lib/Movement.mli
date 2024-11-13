open GameState
open Player
open UndoRedo
open Tile

(** Module Movement : gère le mouvement de joueur *)
module Movement : sig

  (* Fonction qui met à jour la carte, la position du joueur et le mouvement des boîtes,
  enregistre les modifications dans les piles de undo/redo. *)
  val updateMap : GameState.level_map -> Player.player -> Player.direction -> UndoRedo.stacks -> Tile.tile array array

end
