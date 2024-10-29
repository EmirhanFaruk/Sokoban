(* bin/main.ml *)


open Sokoban_lib


let () = 
  let filename = "./assert/levels.txt" in
  let niveau = 5 in
  
  let map = GameState.loadMap filename niveau in
  if map = [] then
    Printf.eprintf "Erreur : Impossible de charger le niveau %d depuis %s\n" niveau filename
  else
    GameView.getMap map