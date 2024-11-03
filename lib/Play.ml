module Play = 
struct 
  open Player
  open GameState
  open GameView

(* Fonction qui vérifie si le joueur a fini la partie et si il a fini un niveau *)
  let endGame level map filename =
    (* Vérifie si toutes les cases BoxGround ont été recouvertes par une boîte *)
    let level_completed = 
      not (Array.exists (Array.exists ((=) GameState.BoxGround)) map.GameState.grid) in
  
    if level > 2000 && level_completed then begin
      print_endline "Félicitations ! Vous avez terminé tous les niveaux disponibles.";
      exit 0
    end else if level_completed then
      let new_level = level + 1 in
      (* Charge la nouvelle carte pour le niveau suivant *)
      let new_map = GameState.loadMap filename new_level { x = 0; y = 0 } in
      (new_level, new_map)  (* Retourne le nouveau niveau et la nouvelle carte *)
    else
      (level, map)  (* Retourne le niveau actuel et la carte inchangée *)
  

  let play () = 
    let level = ref 1 in
    let filename = "./assert/levels.txt" in
    let player = { x = 0; y = 0 } in
    let map = loadMap filename !level player in

    let rec loop () =
      GameView.showLevel !level;
      GameView.printMap map.grid;  (* Assurez-vous que map est du bon type *)
      print_string "\x1b[1mAction (z/s/d/q pour se déplacer, x pour quitter) : ";
      flush stdout;
      let action = read_line () in
      match action with
      | "x" -> print_endline "Au revoir!"; exit 0
      | "z" | "s" | "d" | "q" as dir ->
          let direction = 
            match dir with
            | "z" -> Haut
            | "s" -> Bas
            | "d" -> Droite
            | "q" -> Gauche
            | _ -> failwith "Impossible"  (* Ne devrait jamais arriver *)
          in 
          (* Met à jour la carte en fonction de la direction *)
          let new_map = GameState.updateMap map player direction in  
          (* Appelle la fonction endGame pour vérifier si le niveau est terminé *)
          let (new_level, updated_map) = endGame !level new_map filename in  
          (* Mettez à jour la carte et le niveau courant *)
          map.grid <- updated_map.grid;  (* Assurez-vous que updated_map est du bon type *)
          level := new_level;  (* Mettre à jour le niveau courant *)
          loop ()
      | _ -> print_endline "Action non reconnue."; GameView.showLevel !level; loop ()
    
    in
    loop ()
end
