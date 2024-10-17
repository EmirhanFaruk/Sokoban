(* gameState.ml *)

type map = {
  
  wall : char;
  ground : char;
  box : char;
  boxGround : char;
  player : char;
}


(* Lire les niveaux Sokoban depuis un fichier .txt *)
let read_sokoban_levels filename =
  let ic = open_in filename in
  let rec loop acc current_level =
    try
      let line = input_line ic in
      if String.length line > 0 && line.[0] = ';' then
        (* Nouvelle définition de niveau *)
        let level_num = String.sub line 2 (String.length line - 2) in
        (* Sauvegarder le niveau précédent si non vide *)
        let acc = if current_level <> [] then current_level :: acc else acc in
        loop acc []  (* Commence un nouveau niveau vide *)
      else
        (* Ajouter une ligne au niveau en cours *)
        loop acc (line :: current_level)
    with
    | End_of_file ->
        close_in ic;
        List.rev (if current_level <> [] then current_level :: acc else acc)
  in
  loop [] []



