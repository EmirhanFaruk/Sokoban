(* Type qui représente la liste de la map modifiable quand on veut *)
type tile = Wall | Ground | Box | BoxGround | Player

type level_map = {
  mutable grid: tile list list;  (* Modèle de carte avec type de tuile *)
}

(* Exceptions pour signaler qu'un niveau n'a pas été trouvé ou que les coordonnées sont invalides *)
exception Level_not_found of int
exception Isnt_in_the_list of (int * int)

(* Fonction pour charger une carte d'un fichier texte *)
val loadMap : string -> int -> level_map

(* Fonction qui permet de modifier les éléments de la liste level_map *)
val modifyList : level_map -> int -> int -> tile -> unit
