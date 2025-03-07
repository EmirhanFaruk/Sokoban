module GameView =
struct
  open Tile
  (* Affichage de la carte par défaut *)
  let printMap (map: Tile.tile array array) =
    Array.iter (fun row ->
        print_string "         ";  (* Affiche les espaces au début de la ligne *)
        Array.iter (fun cell -> print_string (Tile.cell_to_emoji cell)) row;  (* Affiche chaque cellule *)
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
    let level_str = Printf.sprintf "%d" (level + 1) in
    let length = String.length level_str in
    let border_length = length + 30 in  (* 28 pour le texte de bordure, espace et formatage *)

    print_string "\x1b[1m";  (* Met le texte en gras *)
    Printf.printf "\n    %s\n" (String.make border_length '-');  (* Affiche la bordure *)
    Printf.printf "    |           NIVEAU %s          |\n" level_str ;  (* Affiche le niveau *)
    Printf.printf "    %s\n" (String.make border_length '-');  (* Affiche la bordure *)
    print_string "\x1b[0m";  (* Réinitialise les styles de texte *)
    print_newline ()

end
