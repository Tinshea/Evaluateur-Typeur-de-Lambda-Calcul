(* syntax.ml *)
(* Ce fichier contient la définition des types simples et les fonctions associées pour générer les équations de typage à partir des termes. *)

(* Environnement de typage *)

(* Représentation des types simples *)
type ptype = 
  | Var of string                (* Variable de type *)
  | Arr of ptype * ptype         (* Fonction T -> T *)
  | Nat                          (* Type des entiers *)

let rec print_type (t : ptype) : string =
  match t with
  Var x -> x
  | Arr (t1 , t2) -> "(" ˆ ( print_type t1) ˆ" -> "ˆ ( print_type t2) ˆ")"  