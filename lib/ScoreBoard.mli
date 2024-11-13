(** Module pour gérer le tableau des scores et la gestion des fichiers de scores *)
module Scoreboard : sig

  (** Type représentant un score avec un nom, un niveau et un nombre de mouvements *)
  type score = {
    name: string;  (** Nom du joueur *)
    level: int;    (** Niveau du jeu *)
    mutable moves: int; (** Nombre de mouvements *)
  }

  (* Sauvegarder le score dans le fichier *)
  val save_score : Player.stat -> int -> unit

  (* Ajouter dans une liste de string les 10 meilleures performance d'un niveau *)
  val get_level_scoreboard : int -> string list

  (* Mettre le resultat de get_level_scoreboard 0-999 dans une liste. Techniquement avoir tous les scores possibles *)
  val get_levels : unit -> 'a list

end
