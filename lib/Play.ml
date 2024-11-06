module Play =
struct 
  open Player
  open GameState
  open GameView
  open Scoreboard
  open Canonique

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
    
  (* Fonction qui reinitialise la partie *)
  let restart (map : GameState.level_map) (player : Player.pos) (playerCopy : Player.pos) =
    map.grid <- GameState.copyMap map.original;
    Player.updatePlayer player (playerCopy.x,playerCopy.y)

  (* I HATE 2 SPACE TABS *)
  (* Avoir le nom de joueur *)
  let get_name () =
    if Sys.os_type <> "Unix"
    then
      (
        if input_line stdin = "" then ()
      );
    GameView.clear_terminal ();
    print_string "Entrez votre nom: ";
    flush stdout;
    let name = read_line () in
    GameView.clear_terminal ();
    name
  
   (* Fonction qui s'occupe de la boucle du jeu *)   
  let play () =
    Canonique.makeCanonique ();
    let (stat : Player.stat) = { name = get_name (); moves = 0 } in
    Canonique.makeNoCanonique (); (* For the get_name func *)
    let level = ref 0 in
    let filename = "./assert/levels.txt" in
    let (player : Player.pos) = { x = 0; y = 0 } in
    let map = GameState.loadMap filename !level player in
    let playerCopy = Player.copyPlayer player in

    (try

    let rec loop () =
      GameView.showLevel !level;
      GameView.printMap map.grid;
      print_endline ("Deplacements: " ^ (string_of_int stat.moves));
      print_string "\x1b[1m\n- z/s/d/q pour se déplacer.\n- r pour recommencer le niveau.\n- x pour quitter\nAction : ";
      flush stdout;
      let action =
      if Sys.os_type = "Unix"  (* Lit l'entrée du terminal *)
      then
          input_char stdin
      else
        let user_input = read_line () in
        if String.length user_input > 0
        then
          user_input.[0]
        else
          'a' in (* Une lettre au hasard pour que ça fasse rien *)
      match action with
      | 'x' -> ()
      | 'r' -> restart map player playerCopy; Player.reset_stat stat; loop () (* On relance le loop avec la map reset *)
      | 'z' | 's' | 'd' | 'q' as dir ->
          let direction = 
            match dir with
            | 'z' -> Player.Haut
            | 's' -> Player.Bas
            | 'd' -> Player.Droite
            | 'q' -> Player.Gauche
            | _ -> failwith "Impossible"
          in 
          (* Met à jour la carte en fonction de la direction *)
          map.grid <- GameState.updateMap map player direction stat;
          (* Appelle la fonction endGame pour vérifier si le niveau/jeu est terminé *)
          if endGame level map then
            (
            Scoreboard.save_score stat !level;
            Player.reset_stat stat;
            updateMap level map filename player;
            Player.updatePlayer playerCopy (player.x,player.y))
          else ();
          loop ()
      | _ -> print_endline "Action non reconnue."; GameView.showLevel !level; loop ()
    
    in
    loop ()

  with
  | Exit -> Canonique.makeCanonique (); ()  (* Gère la sortie normale *)
  | exn -> Canonique.makeCanonique (); prerr_endline ("Erreur inattendue : " ^ Printexc.to_string exn)
  );
end


