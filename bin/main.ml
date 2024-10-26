(* bin/main.ml *)


open Sokoban_lib


let () = 
  let filename = "./assert/levels.txt" in
  let niveau = 1 in
  
  match GameState.loadMap filename niveau with
  | Some map -> 
      GameView.getMap map  (* Assurez-vous que cette fonction est bien définie dans GameView.ml *)
  | None -> 
      Printf.eprintf "Erreur : Impossible de charger le niveau %d depuis %s\n" niveau filename