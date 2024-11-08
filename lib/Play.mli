(** Module Play : gère la logique principale du jeu Sokoban *)
module Play : sig

  open GameState
  open Player

  (* Fonction qui met à jour le niveau au suivant et la map *)
  val updateMap : int ref -> GameState.level_map -> string -> Player.pos -> unit

  (* Fonction qui vérifie si le joueur a fini un niveau et le jeu*)
  val endGame : int ref -> GameState.level_map -> bool

  (* Fonction qui reinitialise la partie *)
  val restart : GameState.level_map -> Player.pos -> Player.pos -> unit

  (* Fonction qui vérifie si une chaîne de caractères contient uniquement des espaces *)
  val contains_only_spaces : string -> bool

  (* Fonction qui permet d'avoir le nom de joueur *)
  val get_name : unit -> string

  (* Fonction qui lis les entrées clavier sous Unix *)
  val readUnix : unit -> char

  (* Fonction qui lis les entrées clavier sous Windows *)
  val readWindows : unit -> char

  (* Fonction qui appelle la bonne fonction en fonction de l'OS afin de donner les déplacements a faire *)
  val readKey : string -> char

  (* Fonction qui permet d'afficher les touches en fonction de l'OS*)
  val affichageOS : string -> string

  (* Fonction qui s'occupe de la boucle du jeu *)   
  val play : unit -> unit

end