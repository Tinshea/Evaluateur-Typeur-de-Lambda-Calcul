(* types.ml *)
(* Ce fichier contient la définition des types simples et les fonctions associées pour générer les équations de typage à partir des termes. *)

open Lambda

(* Représentation des types simples *)
type ptype = 
  | Var of string                (* Variable de type *)
  | Arr of ptype * ptype         (* Fonction T -> T *)
  | Nat                          (* Type des entiers *)

(* Environnement de typage *)
type env = (string * ptype) list

(* Générateur de noms de variables de type *)
let compteur_var_t = ref 0
let nouvelle_var_t () = 
  compteur_var_t := !compteur_var_t + 1;
  "T" ^ string_of_int !compteur_var_t

(* Génération des équations de typage *)
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : (ptype * ptype) list = 
  match te with
  | Var v -> [(List.assoc v e, ty)]
  | Abs (x, t) -> 
      let ta = Var (nouvelle_var_t ()) in
      let tr = Var (nouvelle_var_t ()) in
      (ty, Arr (ta, tr)) :: genere_equa t tr ((x, ta) :: e)
  | App (t1, t2) -> 
      let ta = Var (nouvelle_var_t ()) in
      genere_equa t1 (Arr (ta, ty)) e @ genere_equa t2 ta e
