(* Module pour gérer l'état du jeu et les cartes *)
module GameState : sig

  (* Type représentant les différents éléments de la carte *)
  type tile = Wall | Ground | Box | BoxGround | Player

  (* Type représentant une carte de niveau modifiable quand on veut *)
  type level_map = {
    mutable grid: tile array array;
    mutable original : tile array array;
  }

  (* Exception levée lorsqu'un niveau n'est pas trouvé *)
  exception Level_not_found of int

  (* Exception levée lorsque des coordonnées sont en dehors de la carte *)
  exception Isnt_in_the_list of (int * int)

  (* Convertit une chaîne de caractères en une liste de caractères *)
  val string_to_char_list : string -> char list

  (* Extraire le numéro de niveau à partir d'une ligne *)
  val get_level_number : string -> int

  (* Charge une ligne de caractères en une liste de tuiles *)
  val load_line_to_tiles : string -> int -> Player.pos -> tile list

  (* Fonction pour charger une carte d'un fichier texte *)
  val loadMap : string -> int -> Player.pos -> level_map

  (* Fonction qui permet de modifier les éléments de la liste level_map *)
  val modifyList : level_map -> int -> int -> tile -> unit

  (* Fonction qui renvoit la dimension de la map *)
  val get_dim : tile array array -> int * int

  (* Fonction qui vérifie si les coordonnées correspondent à un chemin valide *)
  val isPath : tile array array -> int * int -> bool

  (* Foncition qui met à jour la position du joueur dans la carte *)
  val updatePlayerPosition : level_map -> Player.pos -> int -> int -> tile -> Player.stat -> unit

  (* Fonction qui met à jour l'ancienne case du joueur *)
  val updateOriginalTile : level_map -> int -> int -> unit

  (* Fonction qui met à jour la carte, la position du joueur et le mouvement des boîtes *)
  val updateMap : level_map -> Player.pos -> Player.direction -> Player.stat -> tile array array

  (* Fonction pour trouver toutes les coordonnées de BoxGround dans la carte d'origine *)
  val find_boxground_positions : level_map -> (int * int) list

  (* Fonction pour vérifier si toutes les positions de BoxGround dans original_map contiennent une Box *)
  val isAllBox : level_map -> bool

  (* Fonction qui renvoie la copie d'une map*)
  val copyMap : tile array array -> tile array array

end