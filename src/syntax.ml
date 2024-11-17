(* syntax.ml *)
(* Ce fichier contient la définition des types simples et les fonctions associées pour générer les équations de typage à partir des termes. *)

(* Représentation des types simples *)
type ptype = 
  | Tvar of string                
  | Arr of ptype * ptype        
  | N                            
  | Listtype of ptype                
  | Forall of string * ptype    
  | Nat        
  | TUnit                       
  | RefType of ptype                
  | ExnType

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
  | ExnType -> "Exn"
  