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
      (* Ajouter "    " au début de chaque ligne *)
      print_string "         ";  (* Affiche les espaces au début de la ligne *)
      Array.iter (fun cell -> print_string (cell_to_emoji cell)) row;  (* Affiche chaque cellule *)
      print_newline ()) map  (* On passe à la ligne suivante *)
  
(* Fonction pour nettoyer le terminal *)
let clear_terminal () =
  let command = 
  if Sys.os_type = "Unix" then "clear" 
  else "cls" in
  ignore (Unix.system command)

(* Affiche le niveau actuel *)
let showLevel level =
  clear_terminal ();
  let level_str = Printf.sprintf "%d" level in
  let length = String.length level_str in
  let border_length = length + 30 in  (* 28 pour le texte de bordure, espace et formatage *)

  print_string "\x1b[1m";  (* Met le texte en gras *)
  Printf.printf "\n    %s\n" (String.make border_length '-');  (* Affiche la bordure *)
  Printf.printf "    |           NIVEAU %s          |\n" level_str;  (* Affiche le niveau *)
  Printf.printf "    %s\n" (String.make border_length '-');  (* Affiche la bordure *)
  print_string "\x1b[0m";  (* Réinitialise les styles de texte *)
  print_newline ()
