(*----------------------------------------------------------- MENU PRINCIPAL -------------------------------------------------------------------*)
(* Affiche le menu et retourne le choix de l'utilisateur *)
let showMenu () = 
  print_string "\x1b[1m";
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
  print_string "\x1b[0m";
  flush stdout;  (* S'assurer que l'affichage est fait avant la lecture de l'entrée *)
  read_line ()  (* Lit l'entrée du joueur *)

(*---------------------------------------------------------------- REGLES ----------------------------------------------------------------------*)

(* Affiche les règles du jeu *)
let showRules () = 
  print_string "\x1b[1m";
  print_endline "";
  print_endline "    -------------------------------";
  print_endline "    |          R U L E S          |";
  print_endline "    -------------------------------";
  print_endline "";
  print_endline "***     Principe du jeu     ***";
  print_endline "Sokoban est un jeu de puzzle où le joueur pousse des caisses vers des emplacements cibles dans un labyrinthe.";
  print_endline "";
  print_endline "      - Placez chaque caisse sur une cible pour gagner.";
  print_endline "      - Vous pouvez pousser les caisses mais pas les tirer.";
  print_endline "      - Attention aux murs et aux coins qui peuvent bloquer les caisses.";
  print_endline "";
  print_endline "Le but : planifier les déplacements pour ne rien bloquer en chemin !";
  print_endline "";
  flush stdout;  (* S'assurer que l'affichage est fait *)
  print_string "\x1b[0m"

(*------------------------------------------------------ FONCTION PRINCIPALE ----------------------------------------------------------------*)

(* Fonction principale qui affiche le menu et traite le choix du joueur *)
let mainMenu () =
  let rec loop () =
    let choice = showMenu () in
    (* Gére chaque option en fonction du choix de l'utilisateur *)
    match choice with
    | "1" -> Play.play () (* Démarre le jeu *)
    | "2" -> showRules (); loop ()     (* Affiche les règles *)
    | "3" -> print_string "\x1b[1m"; print_endline "Au revoir!"; print_string "\x1b[0m";  exit 0  (* Quitte le programme *)
    | _ -> print_string "\x1b[1m";print_endline "Choix invalide. Veuillez réessayer.";print_string "\x1b[0m";   loop ()  (* Redemande un choix *)
  in
  loop ()  (* Lance la boucle pour afficher le menu en continu jusqu'à ce que l'utilisateur choisisse de quitter *)
