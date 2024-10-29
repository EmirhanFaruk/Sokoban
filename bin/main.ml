(* bin/main.ml *)

open Sokoban_lib

let () =
  let filename = "./assert/levels.txt" in
  let niveau = 1 in

  try
    (* Charger la carte pour le niveau spécifié *)
    let level_map = GameState.loadMap filename niveau in

    (* Afficher la carte initiale *)
    Printf.printf "Carte initiale:\n";
    GameView.getMap level_map.grid;

    (* Modifier un élément à une position spécifique *)
    GameState.modifyList level_map 1 2 '$';  (* Modifier l'élément à la position (1, 2) en '$' *)

    (* Afficher la carte mise à jour *)
    Printf.printf "\nCarte mise à jour:\n";
    GameView.getMap level_map.grid;

  with
  | GameState.Level_not_found lvl ->
      Printf.eprintf "Le niveau %d n'a pas été trouvé.\n" lvl
  | GameState.Isnt_in_the_list (x, y) ->
      Printf.eprintf "Indices invalides : (%d, %d).\n" x y;
