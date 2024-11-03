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
    let is_box_on_ground (box_pos : Player.pos) (map : GameState.level_map) =
        map.(box_pos.x).(box_pos.y) = GameState.BoxGround


    (* Checks if the game is done *)
    let is_finished (state : current_state) =
        let mutable res = true in
        let map = state.level_map.grid in
        let omap = state.level_map.original in
        for y = 0 to Array.length map.grid - 1 do
        	for x = 0 to Array.length map.grid.(y) - 1 do
        		if map.(y).(x) = GameState.Box then
        		if omap.(y).(x) <> GameState.BoxGround then
        		    res <- false
        	done
        done;
        res



end