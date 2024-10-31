open GameState

(* Convertit une tuile en un caractère *)
let cell_to_emoji cell = 
  match cell with
  | Wall -> "\xF0\x9F\x9F\xA8"
  | Ground -> "\xE2\xAC\x9B"
  | Box -> "\xF0\x9F\x93\xA6"
  | BoxGround ->  "\xF0\x9F\x9F\xA9"
  | Player -> "\xF0\x9F\x9A\xB6"

(* Affichage de la carte par défaut *)
let printMap (map: tile array array) =
  Array.iter (fun row ->
    Array.iter (fun cell -> print_string (cell_to_emoji cell)) row;  (* Affiche chaque cellule *)
    print_newline ()) map  (* On passe à la ligne suivante *)

