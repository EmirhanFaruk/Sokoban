(* Module responsable de charger la carte et plupart de ses modifications *)
module GameState : sig

  open Tile
  open Player

  (* Type représentant la carte d'un niveau avec une grille modifiable et une copie originale *)
  type level_map = {
    mutable grid: Tile.tile array array;
    mutable original: Tile.tile array array;
  }

  (* Exception levée si un niveau n'est pas trouvé *)
  exception Level_not_found of int

  (* Exception levée si les coordonnées sont hors limites *)
  exception Isnt_in_the_list of (int * int)

  (* Fonction pour extraire le numéro de niveau à partir d'une ligne *)
  val get_level_number : string -> int

  (* Fonction pour charger une carte à partir d'un fichier texte
     Renvoie une structure de type `level_map` *)
  val loadMap : string -> int -> Player.player -> level_map

  (* Fonction qui retourne les dimensions (width, height) de la grille *)
  val get_dim : Tile.tile array array -> int * int

  (* Fonction pour obtenir la tuile aux coordonnées (x, y) dans une grille donnée *)
  val getTile : Tile.tile array array -> int -> int -> Tile.tile

  (* Fonction pour modifier un élément dans `level_map.grid` aux coordonnées (x, y) *)
  val modifyList : level_map -> int -> int -> Tile.tile -> unit

  (* Fonction pour trouver toutes les coordonnées de BoxGround dans la carte originale *)
  val find_boxground_positions : level_map -> (int * int) list

  (* Fonction pour vérifier si toutes les positions de BoxGround contiennent une Box *)
  val isAllBox : level_map -> bool

  (* Fonction qui renvoie une copie de la carte *)
  val copyMap : Tile.tile array array -> Tile.tile array array

end
