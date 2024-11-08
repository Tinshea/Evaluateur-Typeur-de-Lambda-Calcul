(* eval.ml *)
(* Ce fichier contient les fonctions d’évaluation des termes en suivant la stratégie Call-by-Value, ainsi que la substitution et la réduction. *)

open Lambda
exception Timeout

(* Fonction pour obtenir les variables libres d'un terme *)
let rec fv t =
  match t with
  | Var x -> [x]  (* Si c'est une variable, elle est libre *)
  | Abs (x, l) -> List.filter (fun e -> e <> x) (fv l)  (* Exclure la variable liée *)
  | App (t1, t2) -> fv t1 @ fv t2  
  | Int _ -> [] 
  | Add (t1, t2) -> fv t1 @ fv t2  
  | Sub (t1, t2) -> fv t1 @ fv t2 
  | List ts -> List.flatten (List.map fv ts) 
  | Head t -> fv t 
  | Tail t -> fv t
  | Cons (t1, t2) -> fv t1 @ fv t2 
  | IfZero (t1, t2, t3) -> fv t1 @ fv t2 @ fv t3 
  | IfEmpty (t1, t2, t3) -> fv t1 @ fv t2 @ fv t3 
  | Fix t -> fv t 
  | Let (x, t1, t2) -> fv t1 @ List.filter (fun e -> e <> x) (fv t2) 

(* Compteur pour générer des variables uniques *)
let compteur_var = ref 0

(* Générer une nouvelle variable unique *)
let nouvelle_var () : string = 
  compteur_var := !compteur_var + 1;
  "x" ^ string_of_int !compteur_var  

(* Fonction de renommage *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x  (* Ne pas renommer les variables libres *)
  | Abs (x, body) -> 
      let new_var = nouvelle_var () in  
      let new_body = alphaconv (substitute body x new_var) in  (* Renommer dans le corps avec new_var*)
      Abs (new_var, new_body) 
  | App (t1, t2) -> 
      let new_t1 = alphaconv t1 in  
      let new_t2 = alphaconv t2 in  
      App (new_t1, new_t2) 
  | Int n -> Int n
  | Add (t1, t2) -> Add (alphaconv t1, alphaconv t2)
  | Sub (t1, t2) -> Sub (alphaconv t1, alphaconv t2)
  | List ts -> List (List.map alphaconv ts)
  | Head t -> Head (alphaconv t)
  | Tail t -> Tail (alphaconv t)
  | Cons (t1, t2) -> Cons (alphaconv t1, alphaconv t2)
  | IfZero (t1, t2, t3) -> IfZero (alphaconv t1, alphaconv t2, alphaconv t3)
  | IfEmpty (t1, t2, t3) -> IfEmpty (alphaconv t1, alphaconv t2, alphaconv t3)
  | Fix t -> Fix (alphaconv t)
  | Let (x, t1, t2) -> Let (x, alphaconv t1, alphaconv t2)
      
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
      let new_t1 = substitute t1 old_var new_var in  
      let new_t2 = substitute t2 old_var new_var in  
      App (new_t1, new_t2)  
  | Int n -> Int n
  | Add (t1, t2) -> Add (substitute t1 old_var new_var, substitute t2 old_var new_var)
  | Sub (t1, t2) -> Sub (substitute t1 old_var new_var, substitute t2 old_var new_var)
  | List ts -> List (List.map (fun t -> substitute t old_var new_var) ts)
  | Head t -> Head (substitute t old_var new_var)
  | Tail t -> Tail (substitute t old_var new_var)
  | Cons (t1, t2) -> Cons (substitute t1 old_var new_var, substitute t2 old_var new_var)
  | IfZero (t1, t2, t3) -> IfZero (substitute t1 old_var new_var, substitute t2 old_var new_var, substitute t3 old_var new_var)
  | IfEmpty (t1, t2, t3) -> IfEmpty (substitute t1 old_var new_var, substitute t2 old_var new_var, substitute t3 old_var new_var)
  | Fix t -> Fix (substitute t old_var new_var)
  | Let (x, t1, t2) -> Let (x, substitute t1 old_var new_var, substitute t2 old_var new_var)


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
  | Int _ -> t
  | Add (t1, t2) -> Add (substitution x n t1, substitution x n t2)
  | Sub (t1, t2) -> Sub (substitution x n t1, substitution x n t2)
  | List ts -> List (List.map (fun t -> substitution x n t) ts)
  | Head t -> Head (substitution x n t)
  | Tail t -> Tail (substitution x n t)
  | Cons (t1, t2) -> Cons (substitution x n t1, substitution x n t2)
  | IfZero (t1, t2, t3) -> IfZero (substitution x n t1, substitution x n t2, substitution x n t3)
  | IfEmpty (t1, t2, t3) -> IfEmpty (substitution x n t1, substitution x n t2, substitution x n t3)
  | Fix t -> Fix (substitution x n t)
  | Let (y, t1, t2) -> 
      if y = x then
        Let (y, substitution x n t1, t2)  (* Ne pas remplacer si y est x *)
      else if List.mem y (fv n) then  (* Si n contient y, renommer pour éviter la capture *)
        let new_var = nouvelle_var () in
        Let (new_var, substitution x n t1, substitution y (Var new_var) t2)  (* Renommer y dans t2 *)
      else
        Let (y, substitution x n t1, substitution x n t2)  (* Appliquer la substitution au corps de la liaison *)

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
  | Int _ -> None
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2)) (* Addition d'entiers *)
  | Add (t1, t2) -> 
      (match ltr_ctb_step t1 with
       | Some t1' -> Some (Add (t1', t2))
       | None -> 
           match ltr_ctb_step t2 with
           | Some t2' -> Some (Add (t1, t2'))
           | None -> None) 
  | Sub (Int n1, Int n2) -> Some (Int (n1 - n2)) (* Soustraction d'entiers *)
  | Sub (t1, t2) -> 
      (match ltr_ctb_step t1 with
       | Some t1' -> Some (Sub (t1', t2))
       | None -> 
           match ltr_ctb_step t2 with
           | Some t2' -> Some (Sub (t1, t2'))
           | None -> None)
  | List [] -> None
  | List (t :: ts) -> 
    (match ltr_ctb_step t with
     | Some t' -> Some (List (t' :: ts))
     | None -> 
         match ltr_ctb_step (List ts) with
         | Some (List ts') -> Some (List (t :: ts'))
         | _ -> None)
  | Head (List []) -> None
  | Head (List (t :: _)) -> Some t (* Récupérer la tête de la liste *)
  | Head t -> 
      (match ltr_ctb_step t with
       | Some t' -> Some (Head t')
       | None -> None)
  | Tail (List []) -> None
  | Tail (List (_ :: ts)) -> Some (List ts) (* Récupérer la queue de la liste *)
  | Tail t -> 
      (match ltr_ctb_step t with
       | Some t' -> Some (Tail t')
       | None -> None)
  | Cons (t1, List ts) -> Some (List (t1 :: ts)) (* Ajouter un élément à la liste *)
  | Cons (t1, t2) -> 
      (match ltr_ctb_step t1 with
       | Some t1' -> Some (Cons (t1', t2))
       | None -> 
           match ltr_ctb_step t2 with
           | Some t2' -> Some (Cons (t1, t2'))
           | None -> None)
  | IfZero (Int 0, t2, _) -> Some t2 (* Si la condition est 0 *)
  | IfZero (Int _, _, t3) -> Some t3 (* Si la condition est différente de 0 *)
  | IfZero (t1, t2, t3) -> 
      (match ltr_ctb_step t1 with
       | Some t1' -> Some (IfZero (t1', t2, t3))
       | None -> None)
  | IfEmpty (List [], t2, _) -> Some t2 (* Si la liste est vide *)
  | IfEmpty (List _, _, t3) -> Some t3 (* Si la liste n'est pas vide *)
  | IfEmpty (t1, t2, t3) -> 
      (match ltr_ctb_step t1 with
       | Some t1' -> Some (IfEmpty (t1', t2, t3))
       | None -> None)
  | Fix t ->
      (match ltr_ctb_step t with
       | Some t' -> Some (Fix t')
       | None -> None)
  | Let (x, t1, t2) -> Some (substitution x t1 t2) (* Remplacer la variable liée *)

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