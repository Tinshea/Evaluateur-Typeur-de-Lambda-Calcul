(* lambda.ml *)
(* Ce fichier contient la définition des termes du λ-calcul (l'AST), ainsi que certaines fonctions utilitaires comme l'impression de termes. *)

(* Représentation des termes du lambda-calcul *)
type pterm = 
  | Var of string               (* Variable *)
  | App of pterm * pterm        (* Application *)
  | Abs of string * pterm       (* Abstraction *)
  | Int of int
  | Add of pterm * pterm
  | Sub of pterm * pterm
  | List of pterm list
  | Head of pterm
  | Tail of pterm
  | Cons of pterm * pterm
  | IfZero of pterm * pterm * pterm
  | IfEmpty of pterm * pterm * pterm
  | Fix of pterm
  | Let of string * pterm * pterm
  | Unit
  | Ref of pterm
  | Deref of pterm
  | Assign of pterm * pterm
  | Address of int  

(* Pretty-printer des termes *)
let rec print_term (t : pterm) : string = 
  match t with
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^ " " ^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ (print_term t) ^ ")"
  | Int n -> string_of_int n
  | Add (t1, t2) -> "(" ^ (print_term t1) ^ " + " ^ (print_term t2) ^ ")"
  | Sub (t1, t2) -> "(" ^ (print_term t1) ^ " - " ^ (print_term t2) ^ ")"
  | List l -> "[" ^ String.concat "; " (List.map print_term l) ^ "]"
  | Head l -> "head " ^ (print_term l)
  | Tail l -> "tail " ^ (print_term l)
  | Cons (h, t) -> "cons " ^ (print_term h) ^ " " ^ (print_term t)
  | IfZero (c, t, e) -> "ifzero " ^ (print_term c) ^ " then " ^ (print_term t) ^ " else " ^ (print_term e)
  | IfEmpty (c, t, e) -> "ifempty " ^ (print_term c) ^ " then " ^ (print_term t) ^ " else " ^ (print_term e)
  | Fix t -> "fix " ^ (print_term t)
  | Let (x, t1, t2) -> "let " ^ x ^ " = " ^ (print_term t1) ^ " in " ^ (print_term t2)
  | Unit -> "()"
  | Ref t -> "ref " ^ (print_term t)
  | Deref t -> "!" ^ (print_term t)
  | Assign (t1, t2) -> (print_term t1) ^ " := " ^ (print_term t2)
  | Address n -> "address " ^ (string_of_int n)
  