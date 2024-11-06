module Canonique =
struct
  open Unix

  (* Met le terminal en mode non canonique, le terminal réagit directement *)
  let makeNoCanonique () =
    if Sys.os_type = "Unix" then
        let para = tcgetattr stdin in
        let newPara = { para with
        (* Désactiver le mode canonique pour réagir immédiatement *)
          c_icanon = false;
        (* Désactiver l'écho pour ne pas afficher les caractères *)
          c_echo = false;
        } in
        (* Appliquer les nouveaux paramètres *)
        tcsetattr stdin TCSANOW newPara
    else ()

  (* Remet le terminal en mode canonique, il faut appuyer sur entrée a chaque fois*)
  let makeCanonique () =
    if Sys.os_type = "Unix" then
        let para = tcgetattr stdin in
        let newPara = { para with
        (* Activer le mode canonique *)
          c_icanon = true;
         (* Activer l'écho pour afficher les caractères *)
          c_echo = true;
        } in
        tcsetattr stdin TCSANOW newPara  (* Appliquer les nouveaux paramètres *)
    else ()
end
