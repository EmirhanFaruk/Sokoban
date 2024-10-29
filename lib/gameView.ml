(*Affichage la map par défaut*)
let showMap (map: char list list) =
  List.iter (fun row ->
    List.iter print_char row;
    print_newline ()) map;

