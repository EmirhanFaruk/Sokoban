(* Jeu principal *)
let endGame level map filename =
  (* Pour déterminer si le joueur a fini le niveay nous allons tout simplement passer au niveau suivant si il n'y a plus de BoxGround sur la carte*)
  let level_completed = not (List.exists (List.exists ((=) GameState.BoxGround)) map.GameState.grid) in
  if level_completed then
    let new_level = level + 1 in
    let new_map = GameState.loadMap filename new_level in
    (new_level, new_map)  (* Renvoie le nouveau niveau et la nouvelle carte *)
  else
    (level, map)  (* Si le niveau n'est pas terminé, on retourne le niveau et la carte actuels *)


let play () = 
  (* Fonction principale qui lance le jeu après l'affichage du menu. 
     1- On ne l'appelle que 1 fois au début après le menu.
     2- On va stocker la liste de listes ici (la map du jeu qu'on va modifier dans le futur).
     3- Un loop qui demande h24 au joueur ses actions.
     4- S'arrête que quand le joueur veut s'arrêter. *)

  let level = 1 in (* La variable qui va représenter les niveaux*)
  let filename = "./assert/levels.txt" in (* La variable qui représente le fichier de la map *) 
  let map = GameState.loadMap filename level in(* La liste de liste qui va stocker la map qu'on va modifier*) 

  (* Fonction recursive qui représente le loop du jeu et qui va a chaque action indiqué modifier la carte et afficher la carte*)
  let rec loop level map =
    GameView.printMap map.GameState.grid;
    (* Les actions du joueur, haut, bas, droite, gauche *)
    print_string "Action (h/b/d/g pour déplacer, q pour quitter) : ";
    flush stdout;
    let action = read_line () in
    match action with
    | "q" -> print_endline "Au revoir!"; exit 0
    | _ ->
        print_endline "Action non reconnue.";
        let (new_level, new_map) = endGame level map filename in
        loop new_level new_map  
  in
  loop level map  



