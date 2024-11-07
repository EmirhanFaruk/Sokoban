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
  

    (* Fonction qui converti l'entrée du terminal en une direction/erreur *)
  let read_key () =
    (* On stock le contenu dans une variabke afin de savoir*)
    let buf = Bytes.create 3 in
    let n = Unix.read Unix.stdin buf 0 3 in

    (* On regarde il y a combien d'entrée *)
    if n = 1 then

      (* Si on a un seul caractère, on verifie que c'est soit x ou soit r, sinon on renvoit rien *)
       let key = Bytes.get buf 0 in
        if key = 'x' || key = 'r' || key = 'X' || key = 'R' then key else ' '

     (* Si il y a 3 en entrée alors on vérifie que c'est bien une fleche *)  
    else if n = 3 then 
      match Bytes.sub_string buf 0 3 with
      | "\x1b[A" -> 'H'  (* Flèche haut *)
      | "\x1b[B" -> 'B'  (* Flèche bas *)
      | "\x1b[C" -> 'D'  (* Flèche droite *)
      | "\x1b[D" -> 'G'  (* Flèche gauche *)
      | _ -> ' '

      

      (* Si il y a 2 entrée alors on vérifie que c'est des fleches de Windows *)
  else if n = 2 && Bytes.get buf 0 = '\xE0' then 
    match Bytes.get buf 1 with
    | '\x48' -> 'H'  (* Flèche haut sous Windows *)
    | '\x50' -> 'B'  (* Flèche bas sous Windows *)
    | '\x4D' -> 'D'  (* Flèche droite sous Windows *)
    | '\x4B' -> 'G'  (* Flèche gauche sous Windows *)
    | _ -> ' '

     else ' '


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
      GameView.showLevel !level; (* Affiche le niveau courant *)
      GameView.printMap map.grid; (* Affiche la carte du niveau courant *)
      print_endline ("Deplacements: " ^ (string_of_int stat.moves));
      print_string "\x1b[1m\n- ↑↓←→ flèches directionnelles pour se déplacer.\n- r pour recommencer le niveau.\n- x pour retourner au menu.\nAction : ";
      flush stdout;
      let action = read_key() in (* Lit l'action du joueur *)

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


