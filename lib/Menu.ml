(* lib/Menu.ml *)

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
  flush stdout;  (* Assurez-vous que l'invite s'affiche avant de lire l'entrée *)

  read_line ()  (* Lis ce que le joueur a entré dans le terminale pour ensuite être traité dans handleChoice*)



(* Fonction pour traiter le choix de l'utilisateur, qui retourne un booléen qui indique si on redemande ou non au joueur ce qu'il choisit *)
let handleChoice (choice : string) : bool =
  match choice with
  | "1" -> 
      print_endline "Merci d'avoir choisi de commencer le jeu !"; 
      false  (* Quitte le menu et lance le jeu *)
  | "2" -> 
      print_endline "Affichage des règles..."; 
      false  (* Quitte le menu et met sur la page des règles *)
  | "3" -> 
      print_endline "";
      print_endline "----------Merci d'avoir joué ! À bientôt.----------";
      false  (* Arrete le menu et le programme *)
  | _ -> 
      print_endline "";
      print_endline "----------Choix invalide. Veuillez réessayer.----------";
      true  (* Continuer le menu *)



(* Fonction principale qui affiche le menu et traite le choix du joueur*)
let mainMenu =
  let rec loop () =

    let choice = showMenu () in  (* Afficher le menu et récupérer le choix *)
    if handleChoice choice then  (* Traiter le choix et vérifier si l'utilisateur veut continuer *)
      loop ()  (* Continuer à redemander le choix *)
    else
      ()  (* Sortir de la boucle si l'utilisateur choisit de quitter *)

  in
  loop ()  (* Démarrer la boucle *)


