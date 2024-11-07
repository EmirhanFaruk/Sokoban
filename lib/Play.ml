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
  


    let readUnix () =
      let buf = Bytes.create 3 in
      let n = Unix.read Unix.stdin buf 0 3 in
      if n = 1 then
        (* Si un seul caractère est saisi, vérifie s’il s’agit de 'r' ou 'x' *)
        let key = Bytes.get buf 0 in
        if key = 'x' || key = 'r' || key = 'X' || key = 'R' then key else ' '
      else if n = 3 then (
        (* Sous Unix/Linux, les séquences de flèches sont précédées par \x1b *)
        match Bytes.sub_string buf 0 3 with
        | "\x1b[A" -> 'H'  (* Flèche haut *)
        | "\x1b[B" -> 'B'  (* Flèche bas *)
        | "\x1b[C" -> 'D'  (* Flèche droite *)
        | "\x1b[D" -> 'G'  (* Flèche gauche *)
        | _ -> ' '
      )
      else ' '
    
      (* Lecture du *)
      let readWindows () =
        let user_input = read_line () in
        if String.length user_input > 0 then 
          let first_char = user_input.[0] in
          match first_char with
          | 'x' | 'r' | 'X' | 'R' -> first_char (* Capture des touches 'r' et 'x' *)
          | 'Z' | 'z' | 'W' | 'w' -> 'H'  (* Flèche haut *)
          | 'Q' | 'q' | 'A' | 'a' -> 'B' (* Flèche bas *)
          | 'S' | 's' -> 'D'  (* Flèche droite *)
          | 'D' | 'd' -> 'G' (* Flèche gauche *)
          | _ -> ' '   
        else ' '
      
      (* Fonction principale qui appelle la bonne fonction en fonction de l'OS afin de donner les déplacements a faire *)
      let readKey systeme =
         (* On regarde si on est sur Unix *)
        if systeme = "Unix" then
          readUnix () 
        
          (* On regarde si on est sur Windows *)
        else if systeme = "Win32" then
          readWindows ()  
        else ' '

    

  (* Fonction qui permet d'afficher les touches en fonction de l'OS*)
  let affichageOS systeme =  
    if systeme = "Unix" then 
       "\x1b[1m\n- ↑↓←→ flèches directionnelles pour se déplacer.\n- r pour recommencer le niveau.\n- x pour retourner au menu.\nAction : " 
    else "\x1b[1m\n- Haut: z/w |Bas: q/a |Droite: s |Gauche: d  pour se déplacer.\n- r pour recommencer le niveau.\n- x pour retourner au menu.\nAction : " 



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
    let systeme = Sys.os_type in
    let affichageTouche = (affichageOS systeme) in


    (try

    let rec loop () =
      GameView.showLevel !level; (* Affiche le niveau courant *)
      GameView.printMap map.grid; (* Affiche la carte du niveau courant *)
      print_endline ("Deplacements: " ^ (string_of_int stat.moves));
      print_string affichageTouche;
      flush stdout;
      let action = readKey systeme in (* Lit l'action du joueur *)

      match action with
      | 'x'|'X' -> ()
      | 'r'|'R' -> restart map player playerCopy; Player.reset_stat stat; loop () (* On relance le loop avec la map reset *)
      | 'H' | 'B' | 'D' | 'G' as dir ->
          let direction = 
            match dir with
            | 'H' -> Player.Haut
            | 'B' -> Player.Bas
            | 'D' -> Player.Droite
            | 'G' -> Player.Gauche
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
  | Exit -> ()  (* Gère la sortie normale *)
  | exn -> prerr_endline ("Erreur inattendue : " ^ Printexc.to_string exn)

  );
end


