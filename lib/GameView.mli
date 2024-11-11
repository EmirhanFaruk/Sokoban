open Tile
(* Module pour l'affichage de l'état du jeu *)
module GameView : sig

  (* Affichage de la carte par défaut *)
  val printMap : Tile.tile array array -> unit

  (* Fonction pour nettoyer le terminal *)
  val clear_terminal : unit -> unit

  (* Affiche le niveau actuel *)
  val showLevel : int -> unit

end