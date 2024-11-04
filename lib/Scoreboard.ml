module Scoreboard =
struct
    open GameView

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



    (* Turns score into str for scoreboard.
       Adds enough space after and before the variables to make them look good *)
    let score_to_str score max_name max_move extra_padding =
        let name_rest = 0 in
        if max_name > (String.length score.name) then
            name_rest = max_name - (String.length score.name)

        let move_rest = 0 in
        if max_move > String.length (string_of_int el.move) then
            move_rest = max_move - String.length (string_of_int el.move)

        let name = score.name ^ (String.make name_rest ' ') in
        let score = (String.make move_rest ' ') ^ (string_of_int el.move) in

        name ^ (String.make extra_padding ' ') ^ score


    (* Gets top 10 of a level and puts them in a string list *)
    let get_level_scoreboard level =
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
        ) 0 top_10_scores in

        let longest_move_len =
        List.fold_left
        (fun acc el ->
            let len = String.length (string_of_int el.move) in
            if len > acc then
            acc = len
        ) 0 top_10_scores in

        let total_len = 41 in
        let diff = (total_len - longest_move_len - longest_name_len) in
        let border = diff / 2 in
        let padding = 2 in
        if diff mod 2 = 1 then
        padding = 3


        let res = Array.fold_left
        (
            fun acc score ->
            let scstr =
            (String.make border ' ') ^
            (score_to_str score max_name max_move padding) ^
            (String.make border ' ') in
            scstr :: acc

        ) [] top_10_scores in
        res




    let center_text text length =
        let len = String.length text in
        let border = (length - len) / 2 in
        (String.make border ' ') ^
        text ^
        (String.make border ' ')


    (* Prints given level's scoreboard. Level will be printed as if it's +1 *)
    let print_level_scoreboard level =
    let scores = get_level_scoreboard level in
    let sb_text = "Top 10 of level " ^ (string_of_int (level + 1))

    print_string "\x1b[1m";
    print_endline (center_text sb_text 41);

    let lines_left = 10 - List.length scores in

    let print_scores scores =
        match scores with
        | [] -> ()
        | s :: xs -> print_endline s; print_scores xs
    in print_scores scores

    for i = 0 to lines_left do
    	print_endline "";
    done
    print_string "\x1b[0m";
    flush stdout


    let get_levels () =
    let res = [] in
    for i = 0 to 999 do
    	if List.length (get_level_scoreboard i) > 0 then
    	res = i :: res
    done
    res

    let scoreboard_menu () =
        let levels = Array.of_list get_levels in
        let index = 0 in
        if Array.length levels = 0
        then
            print_endline "No scores found"
        else
            while index <> -1 do
                GameView.clear_terminal ();
                print_level_scoreboard levels.(index);
                print_endline "(Previous: b, Next: n, Quit: x) : ";
                let choice = read_line () in
                match choice with
                | b -> if index = 0
                        then index = (Array.length levels) - 1
                        else index = index - 1
                | n -> if index = (Array.length levels) - 1
                        then index = 0
                        else index = index + 1
                | x -> GameView.clear_terminal (); index = -1
                | _ -> GameView.clear_terminal ()
            done

end