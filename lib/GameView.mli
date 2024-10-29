open GameState

(* Convertit une tuile en un string d'emoji*)
val cell_to_emoji : tile -> string

(* Affichage de la carte *)
val printMap : tile list list -> unit
