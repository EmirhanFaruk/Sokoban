open GameState
open Tile
open Player

module UndoRedo =
struct

  type save = { x : int; y : int; before : Tile.tile; after : Tile.tile }

  type stacks = {
    mutable undoStack : save list list;
    mutable redoStack : save list list;
  }

  let initializeStacks () = {undoStack = []; redoStack = []}

  let resetStacks stacks =
    stacks.undoStack <- [];
    stacks.redoStack <- []

  let makeSave x y before after =
    {x; y; before; after}

  let addSavesToStack stacks save_list =
    if List.length save_list > 0 then
    (stacks.undoStack <- save_list :: stacks.undoStack;
    stacks.redoStack <- [])

  let update map player stat (liste : save list) isBefore =
    List.iter (fun s -> 
      let tile = 
        if isBefore then s.before
        else s.after in
      GameState.modifyList map s.x s.y tile;
      if tile = Tile.Player then
        (if isBefore then Player.stat_down stat
        else Player.stat_upt stat;
        Player.updatePlayer player (s.x, s.y))
      ) liste

  (* Fonction pour retourner en arrière *)
  let undo map player (stacks : stacks) stat=
    match stacks.undoStack with
    | h:: t -> update map player stat h true;
      
      let new_redo = h :: stacks.redoStack in
      stacks.undoStack <- t;
      stacks.redoStack <- new_redo
    | _ -> ()
  
  (* Fonction pour revenir en avant *)
  let redo map player (stacks : stacks) stat=
    match stacks.redoStack with
    | h :: t -> update map player stat h false;
      
      let new_undo = h :: stacks.undoStack in
      stacks.undoStack <- new_undo;
      stacks.redoStack <- t
    | _ -> ()
end