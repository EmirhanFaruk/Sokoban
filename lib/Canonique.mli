(* Module pour gérer leq modes canonique et non canonique du terminal *)
module Canonique = sig

  (* Permet de mettre le terminal en mode non canonique, ce qui permet de lire chaque entrée du terminale sans avoir besoin de confirmer avec Entrer *)
  val makeNoCanonique : unit -> unit

  (* Permet de remettre le terminal en mode canonique, ce qui permet d'avoir un terminale de base ou il faut appuyer sur Entrer pour validerf chaque requette *)
  val makeCanonique : unit -> unit

end