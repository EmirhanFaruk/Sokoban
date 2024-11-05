module Canonique =
struct
  open Unix

  (* Met le terminal en mode non canonique *)
  let makeNoCanonique () = 
    let para = tcgetattr stdin in 
    let newPara = { para with 
      c_icanon = false; (* Désactiver le mode canonique pour réagir immédiatement *)
      c_echo = false;   (* Désactiver l'écho pour ne pas afficher les caractères *)
    } in
    tcsetattr stdin TCSANOW newPara  (* Appliquer les nouveaux paramètres *)

  (* Remet le terminal en mode canonique *)
  let makeCanonique () =
    let para = tcgetattr stdin in 
    let newPara = { para with 
      c_icanon = true;  (* Activer le mode canonique *)
      c_echo = true;    (* Activer l'écho pour afficher les caractères *)
    } in
    tcsetattr stdin TCSANOW newPara  (* Appliquer les nouveaux paramètres *)
end
