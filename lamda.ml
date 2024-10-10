(* lambda.ml *)
(* Ce fichier contient la définition des termes du λ-calcul (l'AST), ainsi que certaines fonctions utilitaires comme l'impression de termes. *)

(* Représentation des termes du lambda-calcul *)
type pterm = 
  | Var of string               (* Variable *)
  | App of pterm * pterm        (* Application *)
  | Abs of string * pterm       (* Abstraction *)

(* Pretty-printer des termes *)
let rec print_term (t : pterm) : string = 
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"
