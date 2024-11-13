module Scoreboard =
struct
    open Player

    (* Codes de couleur *)
    let yellow = "\x1b[33m"
    let grey = "\x1b[90m"
    let maroon = "\x1b[38;5;52m"  (* Approximation du marron *)
    let reset = "\x1b[0m"


    (* Type de score *)
    type score =
    {
        name: string;
        level: int;
        mutable moves: int
    }

    (* Sauvegarder le score dans le fichier *)
    let save_score (stat : Player.stat) level =
        (* Ouvrir une chaine de sortie pour écrire dans le fichier *)
        let ochnl =
        open_out_gen [Open_append; Open_creat] 0o644 "./asset/scores.txt" in
        (* Preparation de string à écrire *)
        let res =
        stat.name ^ ";" ^
        (string_of_int level) ^ ";" ^
        (string_of_int stat.moves) ^ "\n" in

        (* Ecriture et ferméture de fichier *)
        output_string ochnl res;
        close_out ochnl


    (* Lire le fichier donné *)
    let read_file filename =
    (* Ouvrir une chaine d'entrée pour lire le fichier *)
    let chnl = open_in filename in
    (* Boucle qui met les lignes dans une liste jusqu'à la fin de fichier *)
    let rec loop () =
        try
            let next = input_line chnl in
            next :: loop ()
        with End_of_file ->
            close_in chnl;
            []
        in loop ()


    (* Transformer une liste de string dans une liste de score
       en le triant par le niveau donné.
       Format de fichier: nom;niveau;deplacement *)
    let list_to_score list level =
        (* Transformer la liste donnée(fait par read_file) dans une liste de score *)
        let res = List.fold_left
        (fun acc line ->
            if String.length line > 5
            then
                try
                    (* Séparer le string par rapport au délimiteur ';'*)
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
        (* Transformer la liste dans un array et
           le trier par rapport aux déplacements *)
        let arres = Array.of_list res in
        Array.sort (fun s1 s2 -> compare s2.moves s1.moves) arres;
        arres




    (* Avoir la data de score dans une liste d'array de score *)
    let get_score_data filename =
        (* Lire le fichier *)
        let lines = read_file filename in
        (* Production de la variable à renvoyer *)
        let score_arr_list = ref [] in
        for i = 0 to (List.length lines) - 1 do
            (* Transformation de strings de score de niveau i en array de score *)
            let score_arr = list_to_score lines i in
            if Array.length score_arr <> 0
            then
                (* S'il y a des scores existant, on le met dans la variable à renvoyer *)
                score_arr_list := score_arr :: !score_arr_list
        done;
        !score_arr_list



    (* Transformer le score en string.
       Ajout d'espaces autour du texte pour ameliorer affichage. *)
    let score_to_str score max_name max_move extra_padding =
        Printf.sprintf "%-*s %*d%s" max_name score.name max_move score.moves (String.make extra_padding ' ')

    (* Avoir les 10 premiers elements d'un array *)
    let get_10_els arr =
        let len = min 10 (Array.length arr) in
        Array.sub arr 0 len (* On retourne un sous tableau de 10 éléments au maximum *)


    (* Ajouter dans une liste de string les 10 meilleures performance d'un niveau *)
    let get_level_scoreboard level =
        (* Avoir le data de score dans une liste d'array de score *)
        let all_scores = get_score_data "./asset/scores.txt" in
        (* Avoir que les scores avec le niveau voulu.
           Type de variable toujours le même malgré inconvénience *)
        let wanted_list =
        List.filter (fun x -> x.(0).level = level) all_scores in

        (* S'il y a des scores au niveau voulu, on continue aux calcules *)
        if List.length wanted_list <> 0
        then
            begin
                (* Avoir le top 10 *)
                let top_10_scores =
                    Array.to_list (get_10_els (List.hd wanted_list)) in

                (* Avoir le nom le plus long pour calcul d'affichage *)
                let longest_name_len =
                List.fold_left
                (fun acc el ->
                    if (String.length el.name) > acc
                    then
                        (String.length el.name)
                    else
                        acc
                ) 0 top_10_scores in

                (* Avoir le déplacement le plus long en string pour calcul d'affichage *)
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

                (* Longueur d'une ligne. Toute la somme de longueur de nom etc
                   doit être plus petit ce nombre *)
                let total_len = 41 in
                (* Calcul d'espace entre les bordures et le nom/déplacement *)
                let diff = (total_len - longest_move_len - longest_name_len) in
                let border = diff / 2 in
                let padding =
                if diff mod 2 = 1
                then
                    3
                else
                    2 in

                (*ajouter les couleurs en fonction de la position *)
                let add_color index str =
                    let color =
                        match index with
                        | 0 -> yellow
                        | 1 -> grey
                        | 2 -> maroon
                        | _ -> ""
                    in
                    if color <> "" then color ^ str ^ reset else str
                in

                (* Préparation des strings de score avec les espaces calculées et couleurs *)
                let res =
                    top_10_scores
                    |> List.rev
                    |> List.mapi (fun idx score ->
                        let scstr =
                        (String.make border ' ') ^
                        (add_color idx (score_to_str score longest_name_len longest_move_len padding)) ^
                        (String.make border ' ') in
                        scstr
                    )
                in
                res
            end
        else
            []

    (* Mettre le resultat de get_level_scoreboard 0-999 dans une liste.
       Techniquement avoir tous les scores possibles *)
    let get_levels () =
        get_score_data "./asset/scores.txt"
        |> List.map (fun arr -> if Array.length arr > 0 then arr.(0).level else -1) 
        |> List.filter (fun level -> level <> -1) 
        |> List.sort_uniq compare 
        |> List.rev (* pour commencer le scorebord au niveau 1 *)

end
