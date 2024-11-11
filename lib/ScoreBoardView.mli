(** Module pour gérer l'affichage du tableau des scores dans le jeu *)
module ScoreboardView : sig

  (* Mettre un string au milieu par rapport a la longueur donné.
    Mettre des espaces autour de string. Mettre un string plus long
    que length va lever une exception, il faut éviter *)
  val center_text : string -> int -> string

    (* Afficher le scoreboard du niveau donné.
       Ça sera affiché comme niveau + 1 pour que ça soit
       synchronisé avec le jeu *)
  val print_level_scoreboard : int -> unit

  (* Afficher le menu de scoreboard avec le premier niveau disponible *)
  val scoreboard_menu : unit -> unit

end
