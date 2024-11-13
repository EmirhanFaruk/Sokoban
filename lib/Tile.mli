open Player
(* Module pour définir les objets du jeu *)
module Tile : sig

  type tile = Wall | Ground | Box | BoxGround | Player | BoxOnBoxGround

  (* Convertit une tuile en un caractère *)
  val cell_to_emoji : tile -> string

  (* Charge une ligne de caractères en une liste de tuiles *)
  val load_line_to_tiles : string -> int -> Player.player -> tile list

end