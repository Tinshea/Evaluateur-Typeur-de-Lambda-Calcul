(* unify.ml *)
(* Ce fichier contient l'algorithme d'unification qui résout les équations générées par le typeur. *)
open Syntax

(* Substitution dans les types *)
let rec subst_type (var : string) (t_sub : ptype) (t : ptype) : ptype =
  match t with
  | Tvar v when v = var -> t_sub
  | Arr (t1, t2) -> Arr (subst_type var t_sub t1, subst_type var t_sub t2)
  | _ -> t

(* Unification des équations de typage
let rec unify (equations : (ptype * ptype) list) : (ptype * ptype) list =
  match equations with
  | [] -> []
  | (Var v, t) :: eqs when t <> Var v -> 
      let eqs' = List.map (fun (t1, t2) -> (subst_type v t t1, subst_type v t2)) eqs in
      unify eqs'
  | (Arr (t1, t2), Arr (t3, t4)) :: eqs ->
      unify ((t1, t3) :: (t2, t4) :: eqs)
  | (Var v, Var v') :: eqs when v = v' -> unify eqs
  | _ -> failwith "Unification failed" *)
