open QCheck
open Sokoban_lib.Player

(* Test pour vérifier que get_next_pos déplace le joueur correctement *)
let test_get_next_pos =
  Test.make
    ~name:"get_next_pos déplace le joueur correctement"
    (triple int int (QCheck.oneofl ["Haut"; "Bas"; "Droite"; "Gauche"]))
    (fun (x, y, dir) ->
      let width, height = (10, 10) in
      let direction = match dir with
        | "Haut" -> Player.Haut
        | "Bas" -> Bas
        | "Droite" -> Droite
        | "Gauche" -> Gauche
        | _ -> Haut
      in
      let (new_x, new_y) = Player.get_next_pos (x, y) direction (width, height) in
      match direction with
      | Haut -> if y > 0 then new_y = y - 1 else new_y = y
      | Bas -> if y < height - 1 then new_y = y + 1 else new_y = y
      | Droite -> if x < width - 1 then new_x = x + 1 else new_x = x
      | Gauche -> if x > 0 then new_x = x - 1 else new_x = x
    )

(* Test pour vérifier que update_player_pos met à jour la position correctement *)
let test_update_player_pos =
  Test.make
    ~name:"updatePlayerPos met à jour la position correctement"
    (triple int int (pair int int))
    (fun (initial_x, initial_y, (new_x, new_y)) ->
      let (pos : Player.pos) = { x = initial_x; y = initial_y } in
      let (stat : Player.stat) = { name = "Test"; moves = 0 } in
      let (player : Player.player) = { pos; stat } in
      Player.updatePlayerPos player (new_x, new_y);
      player.pos.x = new_x && player.pos.y = new_y
    )


(* Test pour vérifier que makePlayer crée un joueur correctement *)
let test_make_player =
  Test.make
    ~name:"makePlayer créer un joueur correctement"
    (pair (pair int int) (pair string int))
    (fun ((x, y), (name, moves)) ->
      let (pos : Player.pos) = { x; y } in
      let (stat : Player.stat) = { name; moves } in
      let (player : Player.player) = Player.makePlayer pos stat in
      player.pos.x = x && player.pos.y = y && player.stat.name = name && player.stat.moves = moves
    )

(* Test pour vérifier que copyPlayer copie correctement un joueur *)
let test_copy_player =
  Test.make
    ~name:"copyPlayer copie correctement un joueur"
    (pair (pair int int) (pair string int))
    (fun ((x, y), (name, moves)) ->
      let (pos : Player.pos) = { x; y } in
      let (stat : Player.stat) = { name; moves } in
      let (player : Player.player) = { pos; stat } in
      let (copied : Player.player) = Player.copyPlayer player in
      copied.pos.x = player.pos.x &&
      copied.pos.y = player.pos.y &&
      copied.stat.name = player.stat.name &&
      copied.stat.moves = player.stat.moves &&
      copied != player
    )

(* Test pour vérifier que reset_stat réinitialise les mouvements à 0 *)
let test_reset_stat =
  Test.make
    ~name:"reset_stat reinitialise les mouvements à 0"
    (pair string int)
    (fun (name, moves) ->
      let (stat : Player.stat) = { name; moves } in
      Player.reset_stat stat;
      stat.moves = 0
    )

(* Test pour vérifier que stat_upt augmente les mouvements *)
let test_stat_upt =
  Test.make
    ~name:"stat_upt incrémente les mouvements correctement"
    (pair string int)
    (fun (name, moves) ->
      let (stat : Player.stat) = { name; moves } in
      Player.stat_upt stat;
      stat.moves = moves + 1
    )

(* Test pour vérifier que stat_down diminue les mouvements sans aller en dessous de 0 *)
let test_stat_down =
  Test.make
    ~name:"stat_down décrémente les mouvements correctement"
    (pair string int)
    (fun (name, moves) ->
      let (stat : Player.stat) = { name; moves } in
      Player.stat_down stat;
      if moves > 0 then stat.moves = moves - 1
      else stat.moves = moves
    )

let () =
  QCheck_runner.run_tests_main [
    test_get_next_pos;
    test_update_player_pos;
    test_make_player;
    test_copy_player;
    test_reset_stat;
    test_stat_upt;
    test_stat_down;
  ]