(* eval.ml *)
(* Ce fichier contient les fonctions d’évaluation des termes en suivant la stratégie Call-by-Value, ainsi que la substitution et la réduction. *)

(* Module d'évaluation - Implémentation de la stratégie d'évaluation CBV *)

open Lambda

(* Exceptions et types *)
exception Timeout
type memory = (int * pterm) list

(* certaine logique comme l'alpha-conversion l'evalutation etc viennent de Christophe Deleuze : Lambda Calculus Evaluator in OCaml*)


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
  | Unit -> []
  | Ref t -> fv t
  | Deref t -> fv t
  | Assign (t1, t2) -> fv t1 @ fv t2
  | Address _ -> []
  | Exn -> []
  | Raise e -> fv e
  | TryWith (e1, e2) -> fv e1 @ fv e2

(* Compteur pour générer des variables uniques *)
let compteur_var = ref 0

(* Générer une nouvelle variable unique *)
let nouvelle_var () : string = 
  compteur_var := !compteur_var + 1;
  "x" ^ string_of_int !compteur_var  

(* memory state *)
let mem_counter = ref 0
let new_mem () : int = 
  mem_counter := !mem_counter + 1;
  !mem_counter

let rec verify = function
  | Int _ | Unit | Abs _ | Address _ -> true
  | List l -> List.for_all verify l
  | _ -> false

(* Fonction de renommage *)
let rec alphaconv (t : pterm) : pterm =
  match t with
  | Var x -> Var x  
  | Abs (x, body) -> 
      let new_var = nouvelle_var () in  
      let new_body = alphaconv (substitute body x new_var) in  
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
  | Unit -> Unit
  | Ref t -> Ref (alphaconv t)
  | Deref t -> Deref (alphaconv t)
  | Assign (t1, t2) -> Assign (alphaconv t1, alphaconv t2)
  | Address n -> Address n
  | Exn -> Exn
  | Raise e -> Raise (alphaconv e)
  | TryWith (e1, e2) -> TryWith (alphaconv e1, alphaconv e2)
      
(* Remplacement des variables dans le corps *)
and substitute (body : pterm) (old_var : string) (new_var : string) : pterm =
  match body with
  | Var x when x = old_var -> Var new_var  
  | Var x -> Var x  
  | Abs (x, t) when x = old_var -> Abs (x, t)  
  | Abs (x, t) -> 
      let new_t = substitute t old_var new_var in  
      Abs (x, new_t) 
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
  | Unit -> Unit
  | Ref t -> Ref (substitute t old_var new_var)
  | Deref t -> Deref (substitute t old_var new_var)
  | Assign (t1, t2) -> Assign (substitute t1 old_var new_var, substitute t2 old_var new_var)
  | Address n -> Address n
  | Exn -> Exn
  | Raise e -> Raise (substitute e old_var new_var)
  | TryWith (e1, e2) -> TryWith (substitute e1 old_var new_var, substitute e2 old_var new_var)


(* Substitution dans les termes du lambda-calcul *)
let rec substitution (x : string) (n: pterm) (t : pterm) : pterm =
  match t with
  | Var s -> if s = x then n else t 
  | Abs (s, t1) ->
      if s = x then
        Abs (s, t1)  
      else if List.mem s (fv n) then  
        let new_var = nouvelle_var () in
        Abs (new_var, substitution x n (substitution s (Var new_var) t1)) 
      else
        Abs (s, substitution x n t1)  
  | App (t1, t2) ->
      App (substitution x n t1, substitution x n t2)  
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
        Let (y, substitution x n t1, t2) 
      else if List.mem y (fv n) then  
        let new_var = nouvelle_var () in
        Let (new_var, substitution x n t1, substitution y (Var new_var) t2)  
      else
        Let (y, substitution x n t1, substitution x n t2)  
  | Unit -> t
  | Ref t -> Ref (substitution x n t)
  | Deref t -> Deref (substitution x n t)
  | Assign (t1, t2) -> Assign (substitution x n t1, substitution x n t2)
  | Address _ -> t
  | Exn -> t 
  | Raise e -> Raise (substitution x n e)
  | TryWith (e1, e2) -> TryWith (substitution x n e1, substitution x n e2)
  

(* Une étape de réduction Call-by-Value *)
let rec ltr_cbv_step (t : pterm) (mem : memory) : (pterm * memory) option =
  match t with
  | Var _ | Abs _ | Int _ | Address _ | Unit -> None
  | App (Abs (x, t1), t2) when verify t2 -> 
      Some (substitution x t2 t1, mem)
  | App (t1, t2) -> 
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (App (t1', t2), mem')
       | None when verify t1 -> 
           (match ltr_cbv_step t2 mem with
            | Some (t2', mem') -> Some (App (t1, t2'), mem')
            | None -> None)
       | None -> None)
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2), mem)
  | Add (t1, t2) -> 
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (Add (t1', t2), mem')
       | None when verify t1 -> 
           (match ltr_cbv_step t2 mem with
            | Some (t2', mem') -> Some (Add (t1, t2'), mem')
            | None -> None)
       | None -> None)
  | Sub (Int n1, Int n2) -> Some (Int (n1 - n2), mem)
  | Sub (t1, t2) -> 
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (Sub (t1', t2), mem')
       | None when verify t1 -> 
           (match ltr_cbv_step t2 mem with
            | Some (t2', mem') -> Some (Sub (t1, t2'), mem')
            | None -> None)
       | None -> None)
  | List [] -> None
  | List (t :: ts) -> 
      (match ltr_cbv_step t mem with
       | Some (t', mem') -> Some (List (t' :: ts), mem')
       | None when verify t -> 
           (match ltr_cbv_step (List ts) mem with
            | Some (List ts', mem') -> Some (List (t :: ts'), mem')
            | _ -> None)
       | None -> None)
  | Head (List []) -> None
  | Head (List (t :: _)) when verify t -> Some (t, mem)
  | Head t -> 
      (match ltr_cbv_step t mem with
       | Some (t', mem') -> Some (Head t', mem')
       | None -> None)
  | Tail (List []) -> None
  | Tail (List (_ :: ts)) -> Some (List ts, mem)
  | Tail t -> 
      (match ltr_cbv_step t mem with
       | Some (t', mem') -> Some (Tail t', mem')
       | None -> None)
  | Cons (t1, List ts) when verify t1 -> Some (List (t1 :: ts), mem)
  | Cons (t1, t2) -> 
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (Cons (t1', t2), mem')
       | None when verify t1 -> 
           (match ltr_cbv_step t2 mem with
            | Some (t2', mem') -> Some (Cons (t1, t2'), mem')
            | None -> None)
       | None -> None)
  | IfZero (Int n, t2, t3) -> Some ((if n = 0 then t2 else t3), mem)
  | IfZero (t1, t2, t3) -> 
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (IfZero (t1', t2, t3), mem')
       | None -> None)
  | IfEmpty (List [], t2, _) -> Some (t2, mem)
  | IfEmpty (List (_::_), _, t3) -> Some (t3, mem)
  | IfEmpty (t1, t2, t3) -> 
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (IfEmpty (t1', t2, t3), mem')
       | None -> None)
  | Fix (Abs (x, t)) -> Some (substitution x (Fix (Abs (x, t))) t, mem)
  | Fix t ->
      (match ltr_cbv_step t mem with
       | Some (t', mem') -> Some (Fix t', mem')
       | None -> None)
  | Let (x, t1, t2) -> 
      (match ltr_cbv_step t1 mem with
       | Some (v1, mem') when verify v1 -> Some (substitution x v1 t2, mem')
       | Some (t1', mem') -> Some (Let (x, t1', t2), mem')
       | None -> None)
  | Ref v when verify v ->
      let addr = new_mem () in
      Some (Address addr, (addr, v) :: mem)
  | Ref t ->
      (match ltr_cbv_step t mem with
       | Some (v, mem') when verify v -> 
           let addr = new_mem () in
           Some (Address addr, (addr, v) :: mem')
       | Some (t', mem') -> Some (Ref t', mem')
       | None -> None)
  | Assign (Address a, v) when verify v ->
      Some (Unit, (a, v) :: List.remove_assoc a mem)
  | Assign (t1, t2) ->
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (Assign (t1', t2), mem')
       | None when verify t1 ->
           (match ltr_cbv_step t2 mem with
            | Some (t2', mem') -> Some (Assign (t1, t2'), mem')
            | None -> None)
       | None -> None)
  | Deref (Address a) -> 
      (try Some (List.assoc a mem, mem)
       with Not_found -> failwith ("Invalid address: " ^ string_of_int a))
  | Deref t ->
      (match ltr_cbv_step t mem with
       | Some (t', mem') -> Some (Deref t', mem')
       | None -> None)
  | Raise e ->
      (match ltr_cbv_step e mem with
       | Some (e', mem') -> Some (Raise e', mem')
       | None -> None)
  | TryWith (Exn, t2) -> Some (t2, mem)
  | TryWith (t1, t2) ->
      (match ltr_cbv_step t1 mem with
       | Some (t1', mem') -> Some (TryWith (t1', t2), mem')
       | None -> None)
  | Exn -> None

(* Évaluation complète par normalisation avec gestion de timeout *)
let ltr_cbv_norm (t : pterm) : pterm * memory =
  let timeout = 1.0 in
  let start_time = Unix.gettimeofday () in
  let initial_memory = [] in
  let step_counter = ref 0 in 

  let rec eval_with_timeout t mem =
    incr step_counter;
    if !step_counter > 1000 then 
      raise (Failure "Too many reduction steps")
    else if Unix.gettimeofday () -. start_time > timeout then
      raise Timeout
    else (
      match t with
      | Unit -> (Unit, mem)
      | _ -> 
          match ltr_cbv_step t mem with
          | Some (t', mem') when t' = t -> (t, mem') 
          | Some (t', mem') -> eval_with_timeout t' mem'
          | None -> (t, mem)
    )
  in

  try
    eval_with_timeout t initial_memory
  with
  | Timeout -> failwith "Évaluation échouée: timeout atteint."
  | Failure msg -> failwith ("Évaluation échouée: " ^ msg)