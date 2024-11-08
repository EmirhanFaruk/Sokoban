(* Module pour gérer le menu principal et les interactions *)
module Menu : sig

  (* Fonction pour nettoyer le terminal *)
  val clear_terminal : unit -> unit

  (* Affiche le menu principal et retourne le choix de l'utilisateur *)
  val showMenu : unit -> char

  (* Affiche les règles du jeu *)
  val showRules : unit -> unit

  (* Fonction principale qui affiche le menu et traite le choix du joueur *)
  val mainMenu : unit -> unit

end