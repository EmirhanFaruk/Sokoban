(* Jeu principal *)
open Player

let play () = 
  (* Fonction principale qui lance le jeu après l'affichage du menu. 
     1- On ne l'appelle que 1 fois au début après le menu.
     2- On va stocker la liste de listes ici (la map du jeu qu'on va modifier dans le futur).
     3- Un loop qui demande h24 au joueur ses actions.
     4- S'arrête que quand le joueur veut s'arrêter. *)

  let level = 1 in (* La variable qui va représenter les niveaux*)
  let filename = "./assert/levels.txt" (* La variable qui représente le fichier de la map *) in
  let player = { x = 0; y = 0} in
  let map = GameState.loadMap filename level player (* La liste de liste qui va stocker la map qu'on va modifier*) in

  (* Fonction recursive qui représente le loop du jeu et qui va a chaque action indiqué modifier la carte et afficher la carte*)
  let rec loop () =
    GameView.printMap map.grid;
    print_string "\x1b[1mAction (h/b/d/g pour déplacer, q pour quitter) : ";
    flush stdout;
    let action = read_line () in
    match action with
    | "q" -> print_endline "Au revoir!"; exit 0
    | "h" | "b" | "d" | "g" as dir ->
        let direction = match dir with
          | "h" -> Haut
          | "b" -> Bas
          | "d" -> Droite
          | "g" -> Gauche
          | _ -> failwith "Impossible"  (* Ne devrait jamais arriver *)
        in 
        map.grid <- GameState.updateMap map player direction;
        loop ()
    | _ -> print_endline "Action non reconnue."; loop ()
  in
  loop ()
