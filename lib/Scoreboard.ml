module Scoreboard =
struct
    type score =
    {
        name: string;
        level: int;
        mutable moves: int;
    }



    (* Reads the given file *)
    let read_file filename =
    let chnl = open_in filename in
    let rec loop () =
        try
            let next = input_line f in
            next :: loop ()
        with End_of_file ->
            close_in f;
            []
        in loop ()


    (* Turns list of strings into list of sorted scores of given level.
       File format: name;level;moves *)
    let list_to_score list level =
        let res = List.fold_left
        (fun acc line ->
            let parts = Str.split (Str.regexp ";") line in
            if int_of_string (List.nth parts 1) = level then
            let new_score = {
                                name = List.nth parts 0;
                                level = int_of_string (List.nth parts 1);
                                moves = int_of_string (List.nth parts 2);
                            } in
            new_score::res
        ) [] list in
        let arres = Array.of_list res in
        Array.sort (fun s1 s2 -> compare s1.moves s2.moves) arres;
        arres




    (* Gets score data in a list of array of score. *)
    let get_score_data filename =
        let lines = read_file "./assert/levels.txt" in
        let score_arr_list = ref [] in
        for i = 0 to List.length lines do
        	let score_arr = list_to_score lines i in
        	if Array.length score_arr <> 0 then
        	score_arr_list := score_arr :: !score_arr_list
        done
        !score_arr_list


    (* Gets top 10 of a level and puts a format to it *)
    let print_level_scoreboard level =
        let all_scores = get_score_data "./assert/levels.txt" in
        let wanted_scores_full =
            List.filter (fun s -> s.level = level) all_scores in

        let top_10_scores =
            List.filteri (fun i _ -> i < 10) wanted_scores_full in


        let longest_name_len =
        List.fold_left
        (fun acc el ->
            if (String.length el.name) > acc then
            acc = (String.length el.name)
        ) 0 top_10_scores



    (* Prints given level's scoreboard. Level will be printed as if it's +1 *)
end