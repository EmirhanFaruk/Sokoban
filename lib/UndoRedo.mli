(* Module pour l'affichage de l'état du jeu *)
module UndoRedo : sig
  (* Type représentant une sauvegarde avec les coordonnées et les tuiles avant et après déplacement *)
  type save = { x : int; y : int; before : Tile.tile; after : Tile.tile }

  (* Type représentant les piles de undo et redo représenté par une liste de liste de sauvegarde*)
  type stacks = {
    mutable undoStack : save list list;
    mutable redoStack : save list list;
  }

  (* Fonction pour initialiser les piles *)
  val initializeStacks : unit -> stacks

  (* Fonction pour reinitialiser les piles *)
  val resetStacks : stacks -> unit

  (* Fonction pour créer une sauvegarde *)
  val makeSave : int -> int -> Tile.tile -> Tile.tile -> save

  (* Fonction qui ajoute une liste de sauvegarde à la pile undo *)
  val addSavesToStack : stacks -> save list -> unit

  (* Fonction qui met à jour la map selon les sauvegardes *)
  val update : GameState.level_map -> Player.player -> save list -> bool -> save list

  (* Fonction pour retourner en arrière *)
  val undo : GameState.level_map -> Player.player -> stacks -> unit

  (* Fonction pour revenir en avant *)
  val redo : GameState.level_map -> Player.player -> stacks -> unit

end