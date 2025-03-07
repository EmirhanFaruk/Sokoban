module Menu =
struct
  open ScoreboardView
  open Play
  open Canonique


  (*--------------------------------------------------------------- TERMINALE ---------------------------------------------------------------------*)

  (* Fonction pour nettoyer le terminal *)
  let clear_terminal () =
    let command =
    if Sys.os_type = "Unix" then "clear"
    else "cls" in
    ignore (Unix.system command)


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
    print_endline "     3. Scoreboard";
    print_endline "     4. Quitter";
    print_endline "";

    print_string "Choisissez une option : ";
    print_string "\x1b[0m";
    flush stdout;  (* S'assurer que l'affichage est fait avant la lecture de l'entrée *)
    input_char stdin (* Lit l'entrée du terminal *)

  (*---------------------------------------------------------------- REGLES ----------------------------------------------------------------------*)

  (* Affiche les règles du jeu *)
  let showRules () = 
    print_string "\x1b[1m";
    print_endline "";
    print_endline "    -------------------------------";
    print_endline "    |         R È G L E S         |";
    print_endline "    -------------------------------";
    print_endline "";
    print_endline "    ***     Principe du jeu     ***";
    print_endline "";
    print_endline "Sokoban est un jeu de puzzle où le joueur pousse des caisses vers des emplacements cibles dans un labyrinthe.";
    print_endline "";
    print_endline "      - Placez chaque caisse sur une cible pour gagner.";
    print_endline "      - Vous pouvez pousser les caisses mais pas les tirer.";
    print_endline "      - Attention aux murs et aux coins qui peuvent bloquer les caisses.";
    print_endline "";
    print_endline "Le but : planifier les déplacements pour ne rien bloquer en chemin !";
    print_endline "";

    print_endline "    ***     Déplacements possibles     ***";
    print_endline "";
    print_endline "Nous pouvons nous déplacer sur 4 directions grace aux flèches directionnelles :";
    print_endline "";
    print_endline "   Unix :";
    print_endline "      - Haut (↑) : vers le haut du labyrinthe.";
    print_endline "      - Bas (↓) : vers le bas du labyrinthe.";
    print_endline "      - Gauche (←) : vers la gauche du labyrinthe.";
    print_endline "      - Droite (→) : vers la droite du labyrinthe.";
    print_endline "";
    print_endline "   Windows :";
    print_endline "      - Haut (z/w) : vers le haut du labyrinthe.";
    print_endline "      - Bas (s) : vers le bas du labyrinthe.";
    print_endline "      - Gauche (q/a) : vers la gauche du labyrinthe.";
    print_endline "      - Droite (d) : vers la droite du labyrinthe.";
    print_endline "";
    print_endline "/!\\ Le déplacement ne peut pas se faire sur les caisses ou les murs. /!\\";
    print_endline "";

    print_endline "    ***     Pousser les caisses     ***";
    print_endline "";
    print_endline "Le joueur peut pousser les caisses seulement s'il n'y a pas d'obstacle derriere la caisse tel que un mur ou une autre caisse.";
    print_endline "      - Si vous êtes bloqué appuyez vous pouvez recommencer le niveau (R)";
    print_endline "      - Vous pouvez quitter le jeu (X)";
    print_endline "";

    print_endline "  ------[      Amusez vous bien !       ]------";
    print_endline "";

    flush stdout;  (* S'assurer que l'affichage est fait *)
    print_string "\x1b[0m";
    if Sys.os_type <> "Unix" then (let _ = read_line () in ())
    else (print_string "Appuyez sur Entrer pour retourner au menu..."; 
    let _ = read_line () in (); clear_terminal ())
  

  (*------------------------------------------------------ FONCTION PRINCIPALE ----------------------------------------------------------------*)
  
  (* Fonction principale qui affiche le menu et traite le choix du joueur *)
  let mainMenu () =
    Canonique.makeNoCanonique ();
    clear_terminal ();
    let rec loop () =
      let choice = showMenu () in
      (* Gére chaque option en fonction du choix de l'utilisateur *)
      match choice with
      | '1' ->clear_terminal (); Play.play (); clear_terminal (); loop () (* Démarre le jeu *)
      | '2' -> clear_terminal (); showRules (); loop ()     (* Affiche les règles *)
      | '3' -> ScoreboardView.scoreboard_menu (); loop() (* Affiche le scoreboard *)
      | '4' -> Canonique.makeCanonique (); print_string "\x1b[1m"; print_endline "Au revoir!"; print_string "\x1b[0m";  exit 0  (* Quitte le programme *)
      | _ ->clear_terminal (); print_string "\x1b[1m";print_endline "Choix invalide. Veuillez réessayer.";print_string "\x1b[0m";   loop ()  (* Redemande un choix *)
    in
    loop ()  (* Lance la boucle pour afficher le menu en continu jusqu'à ce que l'utilisateur choisisse de quitter *)

end