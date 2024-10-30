type pos = { mutable x : int; mutable y : int }

type direction = Haut | Bas | Droite | Gauche

let get_next_pos (x,y) dir (width,height) =
  match dir with
  | Haut when y > 0 -> (x, y-1) 
  | Bas when y < height-1 -> (x, y+1)
  | Droite when x < width-1 -> (x+1, y) 
  | Gauche when x > 0 -> (x-1, y)
  | _ -> (x, y)  

(* Fonction pour mettre à jour la position du joueur*)
let updatePlayer player (x,y)=
  player.x <-x; player.y <-y;
  