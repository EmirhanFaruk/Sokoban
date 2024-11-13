(** Module pour gérer le joueur et ses interactions *)
module Player : sig

  (* Définition du type pour représenter la position du joueur *)
  type pos = { mutable x : int; mutable y : int }

  (** Définition du type pour représenter les statistiques du joueur *)
  type stat = { mutable name : string; mutable moves : int }

  (* Définition l'ensemble de pos et stat: joueur *)
  type player = { pos : pos; stat : stat }

  (* Définition du type pour représenter les directions possibles *)
  type direction = Haut | Bas | Droite | Gauche

  (* Fonction pour obtenir la prochaine position en fonction de la direction et des dimensions *)
  val get_next_pos : int * int -> direction -> int * int -> int * int

  (* Fonction pour mettre à jour la position du joueur *)
  val updatePlayerPos : player -> int * int -> unit

  (* Fonction pour produire un player *)
  val makePlayer : pos -> stat -> player

  (* Fonction pour copier un player *)
  val copyPlayer : player -> player

  (* Fonction pour réinitialiser les statistiques du joueur *)
  val reset_stat : stat -> unit

  (* Fonction pour mettre à jour le nombre de mouvements du joueur *)
  val stat_upt : stat -> unit

  (* Fonction pour diminuer le nombre de mouvements de stat par 1 *)
  val stat_down : stat -> unit

end