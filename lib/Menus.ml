(*----------------------------------------------------------- MENU PRINCIPAL -------------------------------------------------------------------*)
(* Affiche le menu et retourne le choix de l'utilisateur *)
let showMenu () = 
  print_endline "";
  print_endline "    -------------------------------";
  print_endline "    |        S O K O B A N        |";
  print_endline "    -------------------------------";
  print_endline "";

  print_endline "     1. Commencer le jeu";
  print_endline "     2. Règles";
  print_endline "     3. Quitter";
  print_endline "";

  print_string "Choisissez une option : ";
  flush stdout;  (* S'assurer que l'affichage est fait avant la lecture de l'entrée *)

  read_line ()  (* Lit l'entrée du joueur pour traitement dans handleChoice *)


(*---------------------------------------------------------------- REGLES ----------------------------------------------------------------------*)

let showRules () = 
  print_endline "";
  print_endline "    -------------------------------";
  print_endline "    |          R U L E S          |";
  print_endline "    -------------------------------";
  print_endline "";

  print_endline "***     Principe du jeu     ***";
  print_endline "Sokoban est un jeu de puzzle où le joueur pousse des caisses vers des emplacements cibles dans un labyrinthe. Les règles :";
  print_endline "";

  print_endline "      - Placez chaque caisse sur une cible pour gagner.";
  print_endline "      - Vous pouvez pousser les caisses mais pas les tirer.";
  print_endline "      - Attention aux murs et aux coins qui peuvent bloquer les caisses.";
  print_endline "";

  print_endline "Le but : planifier les déplacements pour ne rien bloquer en chemin !";
  print_endline "";
  flush stdout  (* S'assurer que l'affichage est fait *)


(*----------------------------------------------------------- NIVEAUX ----------------------------------------------------------------------*)

let showLevel level =
  print_endline "";
  Printf.printf "    -------------------------------\n";
  Printf.printf "    |           NIVEAU %d          |\n" level;
  Printf.printf "    -------------------------------\n"


(*------------------------------------------------------ FONCTION PRINCIPALE ----------------------------------------------------------------*)

(* Fonction principale qui affiche le menu et traite le choix du joueur *)
let mainMenu () =
  let rec loop () =
    let choice = showMenu () in  (* Afficher le menu et récupérer le choix *)
    
    (* Gérer chaque option en fonction du choix de l'utilisateur *)
    match choice with
    | "1" -> showLevel 1; loop ()  (* Affiche le niveau 1 pour démarrer *)
    | "2" -> showRules (); loop () (* Affiche les règles *)
    | "3" -> print_endline "Au revoir!"; exit 0  (* Quitte le programme *)
    | _ -> print_endline "Choix invalide. Veuillez réessayer."; loop ()  (* Re-demande un choix *)

  in
  loop ()  (* Lance la boucle pour afficher le menu en continu jusqu'à ce que l'utilisateur choisisse de quitter *)
