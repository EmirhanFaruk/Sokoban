(* Jeu principal *)
open Player
open GameState
open GameView
open Solia

module Play =
struct
  let play () = 
    (* Fonction principale qui lance le jeu après l'affichage du menu. 
      1- On ne l'appelle que 1 fois au début après le menu.
      2- On va stocker la liste de listes ici (la map du jeu qu'on va modifier dans le futur).
      3- Un loop qui demande h24 au joueur ses actions.
      4- S'arrête que quand le joueur veut s'arrêter. *)

    let level = 1 in (* La variable qui va représenter les niveaux*)
    let filename = "./assert/levels.txt" (* La variable qui représente le fichier de la map *) in
    let (player : Player.pos) = { x = 0; y = 0} in
    let map = GameState.loadMap filename level player (* La liste de liste qui va stocker la map qu'on va modifier*) in

    (* Fonction recursive qui représente le loop du jeu et qui va a chaque action indiqué modifier la carte et afficher la carte*)
    let rec loop () =
      GameView.showLevel level;
      GameView.printMap map.grid;
      print_string "\x1b[1mAction (z/s/d/q pour se déplacer, x pour quitter) : ";
      flush stdout;
      let action = read_line () in
      match action with
      | "x" -> print_endline "Au revoir!"; exit 0
      | "z" | "s" | "d" | "q" as dir ->
          let direction = 
            match dir with
            | "z" -> (Haut : Player.direction)
            | "s" -> (Bas : Player.direction)
            | "d" -> (Droite : Player.direction)
            | "q" -> (Gauche : Player.direction)
            | _ -> failwith "Impossible"  (* Ne devrait jamais arriver *)
          in 
          map.grid <- GameState.updateMap map player direction;
          loop ()
      | _ -> print_endline "Action non reconnue."; GameView.showLevel level; loop ()
    in
    loop ()

end