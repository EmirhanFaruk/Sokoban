(* Jeu principal *)
let play () = 
  (* Fonction principale qui lance le jeu après l'affichage du menu. 
     1- On ne l'appelle que 1 fois au début après le menu.
     2- On va stocker la liste de listes ici (la map du jeu qu'on va modifier dans le futur).
     3- Un loop qui demande h24 au joueur ses actions.
     4- S'arrête que quand le joueur veut s'arrêter. *)

  let level = 1 inc (* La variable qui va représenter les niveaux*)
  let filename = "./assert/levels.txt" (* La variable qui représente le fichier de la map *) in
  let map = GameState.loadMap filename level (* La liste de liste qui va stocker la map qu'on va modifier*) in

  (* Fonction recursive qui représente le loop du jeu et qui va a chaque action indiqué modifier la carte et afficher la carte*)
  let rec loop () =
    GameView.showMap map.grid;
    (* Les actions du joueur, haut, bas, droite, gauche *)
    print_string "Action (h/b/d/g pour déplacer, q pour quitter) : ";
    flush stdout;
    let action = read_line () in
    match action with
    | "q" -> print_endline "Au revoir!"; exit 0
    | _ -> print_endline "Action non reconnue."; loop ()
  in
  loop ()
