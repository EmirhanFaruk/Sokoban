module Player = struct

  (* Définition du type pour représenter la position du joueur *)
  type pos = { mutable x : int; mutable y : int }

  (* Définition du type pour représenter les statistiques du joueur *)
  type stat = { mutable name : string; mutable moves : int }

  (* Définition l'ensemble de pos et stat: joueur *)
  type player = { pos : pos; stat : stat }

  (* Définition du type pour représenter les directions possibles *)
  type direction = Haut | Bas | Droite | Gauche

  (* Fonction pour obtenir la prochaine position en fonction de la direction et des dimensions *)
  let get_next_pos (x, y) dir (width, height) =
    match dir with
    | Haut when y > 0 -> (x, y - 1)  (* Déplacement vers le haut quand c'est possible *)
    | Bas when y < height - 1 -> (x, y + 1) (* Déplacement vers le bas quand c'est possible *)
    | Droite when x < width - 1 -> (x + 1, y) (* Déplacement vers la droite quand c'est possible *)
    | Gauche when x > 0 -> (x - 1, y) (* Déplacement vers la gauche quand c'est possible *)
    | _ -> (x, y)

  (* Fonction pour mettre à jour la position du joueur *)
  let updatePlayerPos (player : player) (x, y) =
    player.pos.x <- x;
    player.pos.y <- y

  (* Fonction pour copier une position *)
  let copyPos (pos : pos) =
    {x = pos.x; y = pos.y}

  (* Fonction pour copier un stat *)
  let copyStat (stat : stat) =
    {name = stat.name; moves = stat.moves}

  (* Fonction pour produire un player *)
  let makePlayer (pos : pos) (stat : stat) =
    {pos = copyPos pos; stat = stat}

  (* Fonction pour copier un player *)
  let copyPlayer (player : player) =
    makePlayer (copyPos player.pos) (copyStat player.stat)

  (* Fonction pour réinitialiser les statistiques du joueur *)
  let reset_stat stat =
    stat.moves <- 0
    
  (* Fonction pour mettre à jour le nombre de mouvements du joueur *)
  let stat_upt stat =
    stat.moves <- stat.moves + 1

  (* Fonction pour diminuer nombre de mouvements du joueur *)
  let stat_down stat =
    if(stat.moves > 0) then
      stat.moves <- stat.moves - 1

end
