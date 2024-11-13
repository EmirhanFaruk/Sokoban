open GameState
open Player

(* Module Play : gère la logique principale du jeu Sokoban *)
module Play : sig

  (* Fonction qui met à jour le niveau au suivant et la map *)
  val updateMap : int ref -> GameState.level_map -> string -> Player.player -> unit

  (* Fonction qui s'occupe de la boucle du jeu *)
  val play : unit -> unit

end