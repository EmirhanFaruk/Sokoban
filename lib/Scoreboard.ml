module Scoreboard =
struct
    open Player

    (* Type de score *)
    type score =
    {
        name: string;
        level: int;
        mutable moves: int;
    }

    (* Sauvegarder le score dans le fichier *)
    let save_score (stat : Player.stat) level =
        (* Ouvrir une chaine de sortie pour écrire dans le fichier *)
        let ochnl =
        open_out_gen [Open_append; Open_creat] 0o666 "./assert/scores.txt" in
        (* Preparation de string à écrire *)
        let res =
        stat.name ^ ";" ^
        (string_of_int level) ^ ";" ^
        (string_of_int stat.moves) ^ "\n" in

        (* Ecriture et ferméture de fichier *)
        output_string ochnl res;
        close_out ochnl


    (* Lire le fichier donne *)
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


    (* Transformer liste de string dans une liste de score
       en le triant par le niveau donne.
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




    (* Avoir le data de score dans une score array list(liste de array de score) *)
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



    (* Transformer score en string.
       Ajouter espace autour d'eux pour ameliorer affichage. *)
    let score_to_str score max_name max_move extra_padding =
        (* Calcul des espaces extras par rapport a la longueur du nom *)
        let name_rest =
        if max_name > (String.length score.name)
        then
            max_name - (String.length score.name)
        else
            0
        in

        (* Calcul des espaces extras par rapport a la longueur du déplacement en string *)
        let move_rest =
        if max_move > String.length (string_of_int score.moves)
        then
            max_move - String.length (string_of_int score.moves)
        else
            0
        in

        (* Mettre ces espaces calcules entre le nom et le déplacement *)
        let name = score.name ^ (String.make name_rest ' ') in
        let score = (String.make move_rest ' ') ^ (string_of_int score.moves) in

        (* Renvoyer le string avec une espace de longueur donné extra entre eux *)
        name ^ (String.make extra_padding ' ') ^ score


    (* Avoir les premiers 10 elements d'un array *)
    let get_10_els arr =
        let len = Array.length arr in
        (* Produire l'array à renvoyer par rapport au longueur d'array donné *)
        let res = Array.make len (arr.(0)) in
        (* Mettre les élements dans l'array à renvoyer *)
        for i = 0 to len - 1 do
        	Array.set res i (arr.(i))
        done;
        res

    (* Mettre top 10 d'un niveau dans une liste de string *)
    let get_level_scoreboard level =
        (* Avoir le data de score dans une liste d'array de score *)
        let all_scores = get_score_data "./assert/scores.txt" in
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


                (* Préparation des strings de score avec les espaces calculées *)
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

    (* Mettre le resultat de get_level_scoreboard 0-999 dans une liste.
       Techniquement avoir toutes les scores possibles *)
    let get_levels () =
        let res = ref [] in
        for i = 0 to 999 do
            if List.length (get_level_scoreboard i) > 0 then
                res := i :: !res
        done;
        !res


end