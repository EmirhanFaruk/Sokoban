open Player
open GameState

module Solia =
struct

    (* The state of the map, player and moves throughout the whole algorithm *)
    type current_state =
    {
        level_map: GameState.level_map;
        player: Player.pos;
        moves: Player.pos list;
    }

    (* Returns true if the given position is BoxGround in the original(untouched) map *)
    let is_box_on_ground (box_pos : Player.pos) (map : GameState.tile array array) =
        map.(box_pos.y).(box_pos.x) = GameState.BoxGround


    (* Checks if the game is done *)
    let is_finished (state : current_state) =
        let res = ref true in
        let map = state.level_map.grid in
        let omap = state.level_map.original in
        for y = 0 to Array.length map.grid - 1 do
        	for x = 0 to Array.length map.grid.(y) - 1 do
        	    (* If the object is a box and it is not on a boxground,
        	       then the game is not finished *)
        		if map.(y).(x) = GameState.Box &&
        		not (is_box_on_ground ({Player.x = x; Player.y = y} : Player.pos) omap) then
        		    res := false
        	done
        done;
        !res


    (* Checks if the given box is cornered and cannot be moved *)
    let box_cornered (box_pos : Player.pos) (map : GameState.tile array array) =
        let x = box_pos.x in
        let y = box_pos.y in
        let width = Array.length map.(0) in
        let height = Array.length map in

        (* Getting the objects around the box.
           If out of boundries(which should not happen), it counts it as a wall *)
        let left = if x > 0 then map.(y).(x - 1) else GameState.Wall in
        let right = if x < width - 1 then map.(y).(x + 1) else GameState.Wall in
        let up = if y > 0 then map.(y - 1).(x) else GameState.Wall in
        let down = if y < height - 1 then map.(y + 1).(x) else GameState.Wall in

        (* Checking if the boundries are walls or not.
           Not counting if they are boxes because
           there are cases where the adjacent cases can be pushed
           and the box can be freed from being cornered. Ex:

             B B
             B
                 *)
        let left_wall = (left = GameState.Wall) in
        let right_wall = (right = GameState.Wall) in
        let up_wall = (up = GameState.Wall) in
        let down_wall = (down = GameState.Wall) in

        (* If adjacent directions are walls, then box is cornered *)
        let cornered =
        (left_wall && up_wall) ||
        (left_wall && down_wall) ||
        (right_wall && up_wall) ||
        (right_wall && down_wall) in

        cornered

    (* Checks if the state has a box cornered *)
    let has_box_cornered (state : current_state) =
        let res = ref false in
        let map = state.level_map.grid in
        for y = 0 to Array.length map.grid - 1 do
            for x = 0 to Array.length map.grid.(y) - 1 do
                (* If the object is a box and it is cornered,
                   then res = true *)
                if box_cornered ({Player.x = x; Player.y = y} : Player.pos) map then
                    res := true
            done
        done;
        !res




    (* Compares maps *)
    let are_equal_maps (map1 : GameState.tile array array) (map2 : GameState.tile array array) =
        Array.for_all2 (fun row1 row2 -> Array.for_all2 ( = ) row1 row2) map1 map2

    (* Gets the possible neighbors. Ik its extremely expensive, gonna opt it later(never) *)
    let get_neighbors (state : current_state) =
        let res = ref [] in
        let directions = [|Player.Gauche; Player.Droite; Player.Haut; Player.Bas|] in
        for i = 0 to 3 do
            let direction = directions.(i) in
            let temp_player = { Player.x = state.player.x; Player.y = state.player.y } in
        	let temp_map = GameState.updateMap temp_map state temp_player direction in

        	if not (are_equal_maps temp_map state.level_map.grid) then
        	    let new_moves = temp_player :: state.moves in
                let new_state = { level_map = temp_map; player = temp_player; moves = new_moves } in
                res := new_state :: !res
        done
        !res



    (* Turns tile array into string(to change later if its too expensive) *)
    let arr_to_st (row : GameState.tile array) =
        Array.fold_left (fun obj ->
                            match obj with
                            | GameState.Wall -> acc ^ "W"
                            | GameState.Player -> acc ^ "P"
                            | GameState.BoxGround -> acc ^ "X"
                            | GameState.Box -> acc ^ "B"
                            | _ -> "N"
                         ) "" row

    (* Makes a unique key for the map(basically the whole thing in a string lol)
       which makes it "better" to find the right key in hash table ig *)
    let make_hash_key (state : current_state) =
        (* Player part of the key. Easier to find the key starting from the player *)
        let kp = (string_of_int state.player.x) ^ "," ^ (string_of_int state.player.y) in

        (* Map part of the key. I really hope this part
           does not make it really bad in optimisation *)
        let km = Array.fold_left (fun acc row -> acc ^ (arr_to_st row))
                    "" state.level_map.grid in

        kp ^ "|" ^ km

    (* Breadth first search algorithm to solve the given state *)
    let bfs (state : current_state) =
        let queue = Queue.create in
        (* To not revisit the visited states *)
        let visited = Hashtbl.create 10000 in
        Queue.add state queue;
        Hashtbl.add visited (make_hash_key state) state;

        let rec search () =
            if Queue.is_empty queue then None
            else
                let cs = Queue.take queue in
                if is_solved cs then Some (List.rev cs.steps)
                else
                    let neighbors = get_neighbors current_state in
                    List.iter (fun neighbor ->
                        let key = make_hash_key neighbor in
                        if not (Hashtbl.mem visited key) then
                            (
                                Queue.add neighbor queue;
                                Hashtbl.add visited key true
                            )
                        ) neighbors;
                    search ()
        in
        search ()


end