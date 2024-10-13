(* eval.ml *)
(* Ce fichier contient les fonctions d’évaluation des termes en suivant la stratégie Call-by-Value, ainsi que la substitution et la réduction. *)

open Lambda
exception Timeout
(* Fonction pour obtenir les variables libres d'un terme *)
let rec fv t =
  match t with
  | Var x -> [x]  (* Si c'est une variable, elle est libre *)
  | Abs (x, l) -> List.filter (fun e -> e <> x) (fv l)  (* Exclure la variable liée *)
  | App (t1, t2) -> fv t1 @ fv t2  (* Concaténer les variables libres des deux termes *)

(* Compteur pour générer des variables uniques *)
let compteur_var = ref 0

(* Générer une nouvelle variable unique *)
let nouvelle_var () : string = 
  compteur_var := !compteur_var + 1;
  "x" ^ string_of_int !compteur_var  (* Noms générés comme x1, x2, etc. *)

(* Fonction de renommage *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x  (* Ne pas renommer les variables libres *)
  | Abs (x, body) -> 
      let new_var = nouvelle_var () in  (* Générer une nouvelle variable *)
      let new_body = alphaconv (substitute body x new_var) in  (* Renommer dans le corps *)
      Abs (new_var, new_body)  (* Créer une nouvelle abstraction *)
  | App (t1, t2) -> 
      let new_t1 = alphaconv t1 in  (* Alpha-conversion du premier terme *)
      let new_t2 = alphaconv t2 in  (* Alpha-conversion du deuxième terme *)
      App (new_t1, new_t2)  (* Application des nouveaux termes *)
      
(* Remplacement des variables dans le corps *)
and substitute (body : pterm) (old_var : string) (new_var : string) : pterm =
  match body with
  | Var x when x = old_var -> Var new_var  (* Remplacer l'ancienne variable par la nouvelle *)
  | Var x -> Var x  (* Garder les autres variables *)
  | Abs (x, t) when x = old_var -> Abs (x, t)  (* Ne pas remplacer si c'est la variable liée *)
  | Abs (x, t) -> 
      let new_t = substitute t old_var new_var in  (* Remplacer dans le corps *)
      Abs (x, new_t)  (* Garder l'abstraction *)
  | App (t1, t2) -> 
      let new_t1 = substitute t1 old_var new_var in  (* Remplacer dans le premier terme *)
      let new_t2 = substitute t2 old_var new_var in  (* Remplacer dans le deuxième terme *)
      App (new_t1, new_t2)  (* Retourner l'application des nouveaux termes *)


(* Substitution dans les termes du lambda-calcul *)
let rec substitution (x : string) (n: pterm) (t : pterm) : pterm =
  match t with
  | Var s -> if s = x then n else t  (* Remplacer la variable x par n *)
  | Abs (s, t1) ->
      if s = x then
        Abs (s, t1)  (* Ne pas remplacer si s est x *)
      else if List.mem s (fv n) then  (* Si n contient s, renommer pour éviter la capture *)
        let new_var = nouvelle_var () in
        Abs (new_var, substitution x n (substitution s (Var new_var) t1))  (* Renommer s dans t1 *)
      else
        Abs (s, substitution x n t1)  (* Appliquer la substitution au corps de l'abstraction *)
  | App (t1, t2) ->
      App (substitution x n t1, substitution x n t2)  (* Appliquer la substitution aux deux sous-termines *)

(* Une étape de réduction Call-by-Value *)
let rec ltr_ctb_step (t : pterm) : pterm option =
  match t with
  | Var _ -> None
  | Abs (_, _) -> None
  | App (Abs (x, t1), t2) -> Some (substitution x t2 t1)
  | App (t1, t2) -> 
      (match ltr_ctb_step t1 with
       | Some t1' -> Some (App (t1', t2))
       | None -> 
           match ltr_ctb_step t2 with
           | Some t2' -> Some (App (t1, t2'))
           | None -> None)

(* Évaluation complète par normalisation avec gestion de timeout *)
let ltr_cbv_norm (t : pterm) : pterm =
  let timeout = 1.0 in  (* Définir le timeout fixe à 1 seconde *)
  let start_time = Unix.gettimeofday () in

  let rec eval_with_timeout t =
    (* Vérifier si le temps d'exécution a dépassé le timeout *)
    let current_time = Unix.gettimeofday () in
    if current_time -. start_time > timeout then
      raise Timeout
    else
      match ltr_ctb_step t with
      | Some t' -> eval_with_timeout t'
      | None -> t  (* Terminaison sans évaluation possible *)
  in

  (* Lancer l'évaluation *)
  try
    eval_with_timeout t
  with
  | Timeout -> failwith "Évaluation échouée: timeout atteint."