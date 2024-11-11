module UndoRedo =
struct
  open GameState
  open Tile
  open Player

  (* Type représentant une sauvegarde avec les coordonnées et les tuiles avant et après déplacement *)
  type save = { x : int; y : int; before : Tile.tile; after : Tile.tile }

  (* Type représentant les piles de undo et redo représenté par une liste de liste de sauvegarde*)
  type stacks = {
    mutable undoStack : save list list;
    mutable redoStack : save list list;
  }

  (* Fonction pour initialiser les piles *)
  let initializeStacks () = {undoStack = []; redoStack = []}

  (* Fonction pour reinitialiser les piles *)
  let resetStacks stacks =
    stacks.undoStack <- [];
    stacks.redoStack <- []
  
  (* Fonction pour créer une sauvegarde *)  
  let makeSave x y before after =
    {x; y; before; after}
  
  (* Fonction qui ajoute une liste de sauvegarde à la pile undo *)
  let addSavesToStack stacks save_list =
    if List.length save_list > 0 then
    (stacks.undoStack <- save_list :: stacks.undoStack;
    stacks.redoStack <- [])
  
  (* Fonction qui met à jour la map selon les sauvegardes *)
  let update map (player : Player.player) (liste : save list) isBefore =
    (* Pour chaque sauvegarde de la liste on met à jour map *)
    List.iter (fun s -> 
      (* isBefore est un boolean pour savoir si on utilise la fonction pour undo ou redo*)
      let tile = 
        if isBefore then s.before
        else s.after in
      GameState.modifyList map s.x s.y tile; (* mise à jour de la map *)
      (* mise à jour du joueur *)
      if tile = Tile.Player then
        (if isBefore then Player.stat_down player.stat
        else Player.stat_upt player.stat;
        Player.updatePlayerPos player (s.x, s.y))
      ) liste

  (* Fonction pour retourner en arrière *)
  let undo map (player : Player.player) (stacks : stacks) =
    (* On met à jour la map avec la premiere liste de sauvegarde de la pile*)
    match stacks.undoStack with
    | h :: t -> update map player h true;
      (* mise à jour des stacks *)
      let new_redo = h :: stacks.redoStack in
      stacks.undoStack <- t;
      stacks.redoStack <- new_redo
    | _ -> ()
  
  (* Fonction pour revenir en avant *)
  let redo map (player : Player.player) (stacks : stacks) =
    match stacks.redoStack with
    | h :: t -> update map player h false;
      
      let new_undo = h :: stacks.undoStack in
      stacks.undoStack <- new_undo;
      stacks.redoStack <- t
    | _ -> ()
end