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

  (* Lire le fichier donné *)
  val read_file : string -> string list

  (* Transformer une liste de string dans une liste de score en le triant par le niveau donné. Format de fichier: nom;niveau;deplacement *)  
  val list_to_score : string list -> int -> score array

  (* Avoir la data de score dans une liste d'array de score *)
  val get_score_data : string -> score array list

  (* Transformer le score en string. Ajout d'espaces autour du texte pour ameliorer affichage. *) 
  val score_to_str : score -> int -> int -> int -> string

  (* Avoir les 10 premiers elements d'un array *)
  val get_10_els : 'a array -> 'a array

  (* Ajouter dans une liste de string les 10 meilleures performance d'un niveau *)
  val get_level_scoreboard : int -> string list

  (* Mettre le resultat de get_level_scoreboard 0-999 dans une liste. Techniquement avoir tous les scores possibles *)
  val get_levels : unit -> int list

end
