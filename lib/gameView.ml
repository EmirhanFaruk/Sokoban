open GameState

(* Convertit une tuile en un caractère *)
let cell_to_emoji cell = 
  match cell with
  | Wall -> "\xF0\x9F\x9F\xa8"
  | Ground -> "\xE2\xAC\x9B"
  | Box -> "\xF0\x9F\x93\xA6"
  | BoxGround ->  "\xF0\x9F\x9F\xA2"
  | Player -> "\xF0\x9F\x9A\xB6"

(* Affichage de la carte par défaut *)
let showMap (map: tile list list) =
  List.iter (fun row ->
    List.iter (fun cell -> print_string (cell_to_emoji cell)) row;  (* Correctement terminer la fonction interne *)
    print_newline ()) map
