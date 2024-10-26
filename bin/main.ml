(* main.ml *)

(* Modules *)
module GameState = GameState
module GameView = GameView

let () = 
  let filename = "./assert/levels.txt" in  (* Remplacez avec le chemin de votre fichier de carte *)
  let niveau = 1 in  (* Numéro du niveau à charger *)
  match GameState.loadMap filename niveau with
  | Some map -> GameView.getMap map  (* Appel de la fonction pour afficher la carte *)
  | None -> Printf.eprintf "Erreur : Impossible de charger le niveau %d depuis %s\n" niveau filename
