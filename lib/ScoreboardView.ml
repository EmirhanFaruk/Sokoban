module ScoreboardView =
struct
    open Scoreboard
    open GameView

    let center_text text length =
        let len = String.length text in
        let border = (length - len) / 2 in
        (String.make border ' ') ^
        text ^
        (String.make border ' ')


    (* Prints given level's scoreboard. Level will be printed as if it's +1 *)
    let print_level_scoreboard level =
        let scores = Scoreboard.get_level_scoreboard level in
        let sb_text = "Top 10 of level " ^ (string_of_int (level + 1)) in

        print_string "\x1b[1m";
        print_endline (center_text sb_text 41);

        let lines_left = 10 - List.length scores in

        let rec print_scores scores =
            match scores with
            | [] -> ()
            | s :: xs -> print_endline s; print_scores xs
        in print_scores scores;

        for _ = 0 to lines_left - 1 do
            print_endline "";
        done;
        print_string "\x1b[0m";
        flush stdout


    let scoreboard_menu () =
        (* For some reason the levels are reversed *)
        let levels = Array.of_list (Scoreboard.get_levels ()) in
        let index = ref ((Array.length levels) - 1) in
        if Array.length levels = 0
        then
            print_endline "No scores found"
        else
            while !index <> -1 do
                GameView.clear_terminal ();
                print_level_scoreboard levels.(!index);
                print_string "(Previous: b, Next: n, Quit: x) : ";
                let choice = read_line () in
                if String.equal choice "b"
                then
                    if !index = (Array.length levels) - 1
                    then
                        index := 0
                    else
                        index := !index + 1
                else if String.equal choice "n"
                then
                    if !index = 0
                    then
                        index := (Array.length levels) - 1
                    else
                        index := !index - 1
                else if String.equal choice "x"
                then
                    index := -1
            done;
            GameView.clear_terminal ()


end