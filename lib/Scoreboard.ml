module Scoreboard =
struct
    open Player

    type score =
    {
        name: string;
        level: int;
        mutable moves: int;
    }


    let save_score (stat : Player.stat) level =
        let ochnl =
        open_out_gen [Open_append; Open_creat] 0o666 "./assert/scores.txt" in
        let res =
        stat.name ^ ";" ^
        (string_of_int level) ^ ";" ^
        (string_of_int stat.moves) ^ "\n" in

        output_string ochnl res;
        close_out ochnl


    (* Reads the given file *)
    let read_file filename =
    let chnl = open_in filename in
    let rec loop () =
        try
            let next = input_line chnl in
            next :: loop ()
        with End_of_file ->
            close_in chnl;
            []
        in loop ()


    (* Turns list of strings into list of sorted scores of given level.
       File format: name;level;moves *)
    let list_to_score list level =
        let res = List.fold_left
        (fun acc line ->
            if String.length line > 5
            then
                try
                    let parts = Str.split (Str.regexp ";") line in
                    if int_of_string (List.nth parts 1) = level then
                        let new_score = {
                                            name = List.nth parts 0;
                                            level = int_of_string (List.nth parts 1);
                                            moves = int_of_string (List.nth parts 2);
                                        } in
                        new_score::acc
                    else
                        acc
                with _ -> acc
            else
                acc
        ) [] list in
        let arres = Array.of_list res in
        Array.sort (fun s1 s2 -> compare s2.moves s1.moves) arres;
        arres




    (* Gets score data in a list of array of score. *)
    let get_score_data filename =
        let lines = read_file filename in
        let score_arr_list = ref [] in
        for i = 0 to (List.length lines) - 1 do
        	let score_arr = list_to_score lines i in
        	if Array.length score_arr <> 0 then
        	score_arr_list := score_arr :: !score_arr_list
        done;
        !score_arr_list



    (* Turns score into str for scoreboard.
       Adds enough space after and before the variables to make them look good *)
    let score_to_str score max_name max_move extra_padding =
        let name_rest =
        if max_name > (String.length score.name)
        then
            max_name - (String.length score.name)
        else
            0
        in


        let move_rest =
        if max_move > String.length (string_of_int score.moves)
        then
            max_move - String.length (string_of_int score.moves)
        else
            0
        in

        let name = score.name ^ (String.make name_rest ' ') in
        let score = (String.make move_rest ' ') ^ (string_of_int score.moves) in

        name ^ (String.make extra_padding ' ') ^ score


    (* Gets first 10 elements of an array *)
    let get_10_els arr =
        let len = Array.length arr in
        let res = Array.make len (arr.(0)) in
        for i = 0 to len - 1 do
        	Array.set res i (arr.(i))
        done;
        res

    (* Gets top 10 of a level and puts them in a string list *)
    let get_level_scoreboard level =
        let all_scores = get_score_data "./assert/scores.txt" in
        let wanted_list =
        List.filter (fun x -> x.(0).level = level) all_scores in

        if List.length wanted_list <> 0
        then
            begin
                let top_10_scores =
                    Array.to_list (get_10_els (List.hd wanted_list)) in


                let longest_name_len =
                List.fold_left
                (fun acc el ->
                    if (String.length el.name) > acc
                    then
                        (String.length el.name)
                    else
                        acc
                ) 0 top_10_scores in

                let longest_move_len =
                List.fold_left
                (fun acc el ->
                    let len = String.length (string_of_int el.moves) in
                    if len > acc
                    then
                        len
                    else
                        acc
                ) 0 top_10_scores in

                let total_len = 41 in
                let diff = (total_len - longest_move_len - longest_name_len) in
                let border = diff / 2 in
                let padding =
                if diff mod 2 = 1
                then
                    3
                else
                    2 in



                let res = Array.fold_left
                (
                    fun acc score ->
                    let scstr =
                    (String.make border ' ') ^
                    (score_to_str score longest_name_len longest_move_len padding) ^
                    (String.make border ' ') in
                    scstr :: acc

                ) [] (Array.of_list top_10_scores) in
                res
            end
        else
            []


    let get_levels () =
        let res = ref [] in
        for i = 0 to 999 do
            if List.length (get_level_scoreboard i) > 0 then
                res := i :: !res
        done;
        !res


end