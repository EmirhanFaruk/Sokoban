open Player
open GameState

module Solia =
struct

    (* The state of the map, player and moves throughout the whole algorithm *)
    type current_state =
    {
        level_map = GameState.level_map;
        player = Player.pos;
        moves = Player.pos list;
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


    (* Makes a unique key for the map(basically the whole thing in a string lol)
       which makes it "better" to find the right key in hash table ig *)
    let make_hash_key (state : current_state) =
        (* Player part of the key. Easier to find the key starting from the player *)
        let kp = (string_of_int state.player.x) ^ "," ^ (string_of_int state.player.y) in

        (* Map part of the key. I really hope this part
           does not make it really bad in optimisation *)
        let km = Array.fold_left (fun st -> acc ^ st)
        (
            Array.fold_left (fun obj ->
                                match obj with
                                | GameState.Wall -> acc ^ "W"
                                | GameState.Player -> acc ^ "P"
                                | GameState.BoxGround -> acc ^ "X"
                                | GameState.Box -> acc ^ "B"
                                | _ -> "N"
                             ) "" state.level_map.grid
        ) ""

    (* Breadth first search algorithm to solve the given state *)
    let bfs (state : current_state) =
        let queue = Queue.create in
        (* To not revisit the visited states *)
        let visited = Hashtbl.create 10000 in
        Queue.add state queue;
        Hashtbl.add


end