(* eval.ml *)
(* Ce fichier contient les fonctions d’évaluation des termes en suivant la stratégie Call-by-Value, ainsi que la substitution et la réduction. *)

(* Substitution dans les termes du lambda-calcul *)
let rec substitution (x : string) (n : pterm) (t : pterm) : pterm = 
  match t with
  | Var v when v = x -> n
  | Var v -> Var v
  | Abs (v, t1) when v = x -> Abs (v, t1)
  | Abs (v, t1) -> Abs (v, substitution x n t1)
  | App (t1, t2) -> App (substitution x n t1, substitution x n t2)

(* Une étape de réduction Call-by-Value *)
let rec eval_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None
  | Abs (_, _) -> None
  | App (Abs (x, t1), t2) -> Some (substitution x t2 t1)
  | App (t1, t2) -> 
      (match eval_step t1 with
       | Some t1' -> Some (App (t1', t2))
       | None -> 
           match eval_step t2 with
           | Some t2' -> Some (App (t1, t2'))
           | None -> None)

(* Évaluation complète par normalisation *)
let rec eval (t : pterm) : pterm =
  match eval_step t with
  | Some t' -> eval t'
  | None -> t
