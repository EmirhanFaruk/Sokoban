(*Affichage la map par défaut*)
let getMap (map: char list list) =
  List.iter (fun row ->
    List.iter print_char row;
    print_newline ()) map;

