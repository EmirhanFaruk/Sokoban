open Player

module Tile =
struct

  type tile = Wall | Ground | Box | BoxGround | Player | BoxOnBoxGround

  (* Convertit une tuile en un caractère *)
  let cell_to_emoji (cell : tile) = 
    match cell with
    | Wall -> "\xF0\x9F\x9F\xA8"
    | Ground -> "\xE2\xAC\x9B"
    | Box -> "\xF0\x9F\x93\xA6"
    | BoxGround ->  "\xF0\x9F\x9F\xA9"
    | Player -> "\xF0\x9F\x9A\xB6"
    | BoxOnBoxGround -> "\xF0\x9F\x90\x92"
  
  (* Convertir une chaîne en liste de caractères *)
  let string_to_char_list s =
    List.init (String.length s) (fun i -> s.[i])

  (* Charge une ligne de caractères en une liste de tuiles *)
  let load_line_to_tiles line y (player: Player.player) =
    let _, tile_list = List.fold_right (fun c (x, acc) ->
      match c with
      | '#' -> (x - 1, Wall :: acc)
      | ' ' -> (x - 1, Ground :: acc)
      | '$' -> (x - 1, Box :: acc)
      | '.' -> (x - 1, BoxGround :: acc)
      | '@' ->
          (* Mettre à jour la position du joueur *)
          Player.updatePlayerPos player (x,y);
          (x - 1, Player :: acc)
      | _ -> (x - 1, acc)  (* Ignorer les caractères non spécifiés *)
    ) (string_to_char_list line) ((String.length line - 1), []) in
    tile_list
end