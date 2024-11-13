open GameState
open Tile
open Player
(* Module pour l'affichage de l'état du jeu *)
module UndoRedo : sig
  (* Type représentant une sauvegarde d'une tuile, avec ses coordonnées et son état avant et après déplacement *)
  type save = { x : int; y : int; before : Tile.tile; after : Tile.tile }

  (* Type des piles de sauvegardes pour les actions de undo/redo *)
  type stacks = {
    mutable undoStack : save list list;
    mutable redoStack : save list list;
  }

  (* Initialiser les piles de undo et redo *)
  val initializeStacks : unit -> stacks

  (* Réinitialiser les piles de undo et redo *)
  val resetStacks : stacks -> unit

  (* Créer une sauvegarde d'une tuile avec ses coordonnées et son état avant/après *)
  val makeSave : int -> int -> Tile.tile -> Tile.tile -> save

  (* Ajouter une liste de sauvegardes à la pile undo et vider la pile redo *)
  val addSavesToStack : stacks -> save list -> unit

  (* Annuler la dernière action (undo) *)
  val undo : GameState.level_map -> Player.player -> stacks -> unit

  (* Rétablir la dernière action annulée (redo) *)
  val redo : GameState.level_map -> Player.player -> stacks -> unit

end
