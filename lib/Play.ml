module Play =
struct 
  open Player
  open GameState
  open GameView

  (* Fonction qui met à jour le niveau au suivant et la map *)
  let updateMap (level : int ref) (map : GameState.level_map) filename (player: Player.pos)  =
    let new_level = !level + 1 in
        (* Charge la nouvelle carte pour le niveau suivant *)
        let new_map = GameState.loadMap filename new_level player in
        map.grid <- new_map.grid;
        map.original <- new_map.original;
        level := new_level  (*On met à jour le niveau*)

  (* Fonction qui vérifie si le joueur a fini un niveau et le jeu*)
  let endGame (level : int ref) (map : GameState.level_map) =
    (* On vérifie si toutes les cases BoxGround ont été recouverte par une boîte *)
    let level_completed = 
      GameState.isAllBox map in
  
    if !level == 999 && level_completed then begin
      print_endline "Félicitations ! Vous avez terminé tous les niveaux disponibles.";
      exit 0
    end else level_completed
  
   (* Fonction qui s'occupe de la boucle du jeu *)   
  let play () = 
    let level = ref 1 in
    let filename = "./assert/levels.txt" in
    let (player : Player.pos) = { x = 0; y = 0 } in
    let map = GameState.loadMap filename !level player in

    let rec loop () =
      GameView.showLevel !level;
      GameView.printMap map.grid;
      print_string "\x1b[1mAction (z/s/d/q pour se déplacer, x pour quitter) : ";
      flush stdout;
      let action = read_line () in
      match action with
      | "x" -> print_endline "Au revoir!"; exit 0
      | "z" | "s" | "d" | "q" as dir ->
          let direction = 
            match dir with
            | "z" -> Player.Haut
            | "s" -> Player.Bas
            | "d" -> Player.Droite
            | "q" -> Player.Gauche
            | _ -> failwith "Impossible"
          in 
          (* Met à jour la carte en fonction de la direction *)
          map.grid <- GameState.updateMap map player direction;
          (* Appelle la fonction endGame pour vérifier si le niveau/jeu est terminé *)
          if endGame level map then updateMap level map filename player 
          else ();
          loop ()
      | _ -> print_endline "Action non reconnue."; GameView.showLevel !level; loop ()
    
    in
    loop ()
end
