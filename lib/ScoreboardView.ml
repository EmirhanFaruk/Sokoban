module ScoreboardView =
struct
    open Scoreboard
    open GameView


    (* Mettre un string au milieu par rapport au longueur donné.
       Mettre espace autour de string. Mettre un string plus long
       que length va lever une exception, il faut l'éviter *)
    let center_text text length =
        let len = String.length text in
        let border = (length - len) / 2 in
        if (length - len) mod 2 = 0
        then
            (String.make border ' ') ^
            text ^
            (String.make border ' ')
        else
            (String.make (border + 1) ' ') ^
            text ^
            (String.make border ' ')


    (* Afficher le scoreboard de niveau donné.
       Ça sera affiché comme niveau + 1 pour que ça soit
       syncronisé avec le jeu *)
    let print_level_scoreboard level =
        (* Avoir top 10 d'un niveau dans une liste de string *)
        let scores = Scoreboard.get_level_scoreboard level in
        let sb_text = "Top 10 de niveau " ^ (string_of_int (level + 1)) in

        print_string "\x1b[1m";
        print_endline (center_text sb_text 41);

        (* Calcul des lignes restant. A la place d'afficher les choix
           juste après les scores pendant qu'il y a 2-3 scores,
           on l'affiche comme s'il y avait 10 scores *)
        let lines_left = 10 - List.length scores in

        (* Afficher les scores préfaites un par un *)
        let rec print_scores scores =
            match scores with
            | [] -> ()
            | s :: xs -> print_endline s; print_scores xs
        in print_scores scores;

        (* Sauter les lignes restants *)
        for _ = 0 to lines_left - 1 do
            print_endline "";
        done;
        print_string "\x1b[0m";
        flush stdout


    (* Afficher le menu de scoreboard avec le premier niveau disponible *)
    let scoreboard_menu () =
        let levels = Array.of_list (Scoreboard.get_levels ()) in
        (* Index de array de score *)
        let index = ref ((Array.length levels) - 1) in
        if Array.length levels = 0
        then
            (* Si pas de score, sortir de function et aller au menu *)
            print_endline "Aucun score trouvé."
        else
            (* On continue à afficher le scoreboard jusqu'à l'utilisateur veut quitter *)
            while !index <> -1 do
                GameView.clear_terminal ();
                (* Afficher le niveau *)
                print_level_scoreboard levels.(!index);
                print_endline "(Précédent: b, Suivant: n, Retour au menu: x): ";
                (* Gérer les choix. On compte a l'invers car la liste est à l'inverse. *)
                let choice = if Sys.os_type = "Unix"
                then
                    input_char stdin
                else
                    let user_input = read_line () in
                    if String.length user_input > 0
                    then
                        user_input.[0]
                    else
                        'a' in
                match choice with
                | 'b' -> if !index = (Array.length levels) - 1
                         then
                             index := 0
                         else
                             index := !index + 1
                | 'n' -> if !index = 0
                         then
                             index := (Array.length levels) - 1
                         else
                             index := !index - 1
                | 'x' -> index := -1 (* Pour quitter, on rend false le seul condition de while *)
                | _ -> () (* Faire rien si touché à une autre touche *)
            done;
            GameView.clear_terminal ()


end