(* syntax.ml *)
(* Ce fichier contient la définition des types simples et les fonctions associées pour générer les équations de typage à partir des termes. *)

(* Représentation des types simples *)
type ptype = 
  | Tvar of string                (* Variable de type *)
  | Arr of ptype * ptype         (* Fonction T -> T *)
  | N                            (* Type des entiers *)
  | Listtype of ptype                (* Type des listes *)
  | Forall of string * ptype     (* Type polymorphe *)
  | Nat        
  | TUnit                         (* Type unit *)
  | RefType of ptype                (* Type référence *)

let rec print_type (t : ptype) : string =
  match t with
  Tvar x -> x
  | Arr (t1 , t2) -> "(" ^ ( print_type t1) ^ " -> " ^ ( print_type t2) ^ ")"  
  | N -> "Int"
  | Listtype t -> "(" ^ (print_type t) ^ " list)"
  | Forall (x, t) -> "(forall " ^ x ^ ". " ^ (print_type t) ^ ")"
  | Nat -> "Nat"
  | TUnit -> "Unit"
  | RefType t -> "(" ^ (print_type t) ^ " ref)"