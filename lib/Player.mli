(** Module pour gérer le joueur et ses interactions *)
module Player : sig

  (* Définition du type pour représenter la position du joueur *)
  type pos = { mutable x : int; mutable y : int }

  (** Définition du type pour représenter les statistiques du joueur *)
  type stat = { mutable name : string; mutable moves : int }

  (* Définition du type pour représenter les directions possibles *)
  type direction = Haut | Bas | Droite | Gauche

  (* Fonction pour obtenir la prochaine position en fonction de la direction et des dimensions *)
  val get_next_pos : int * int -> direction -> int * int -> int * int

  (* Fonction pour mettre à jour la position du joueur *)
  val updatePlayer : pos -> int * int -> unit

  (* Fonction pour copier un joueur *)
  val copyPlayer : pos -> pos

  (* Fonction pour réinitialiser les statistiques du joueur *)
  val reset_stat : stat -> unit

  (** Fonction pour mettre à jour le nombre de mouvements du joueur *)
  val stat_upt : stat -> unit

end