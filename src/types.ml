(* Module de typage *)

open Syntax
open Lambda

(* Types de base *)
type env = (string * ptype) list
type equa = (ptype * ptype) list

(* Générateur de noms de variables de type *)
let compteur_var_t = ref 0

let nouvelle_var_t () = 
  compteur_var_t := !compteur_var_t + 1;
  "T" ^ string_of_int !compteur_var_t

(* Fonction de recherche dans l'environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
  | [] -> failwith ("Variable " ^ v ^ " non trouvée dans l'environnement")
  | (x, t) :: rest -> if x = v then t else cherche_type v rest


(* Vérifie si un terme est non-expansif *)
let rec is_nonexpansive (term : pterm) : bool =
  match term with

  | Var _ -> true                
  | Int _ -> true               
  | Unit -> true                
  | Address _ -> true          
  | Abs _ -> true             
  | List [] -> true     
  | Exn -> true     

  | Add (t1, t2) | Sub (t1, t2) | Cons (t1, t2) -> 
      is_nonexpansive t1 && is_nonexpansive t2
  | List terms -> 
      List.for_all is_nonexpansive terms
  | IfZero (t1, t2, t3) | IfEmpty (t1, t2, t3) ->
      is_nonexpansive t1 && is_nonexpansive t2 && is_nonexpansive t3

  | App _ | Ref _ | Deref _ | Assign _ | Fix _ | Head _ | Tail _ -> false
  | Raise _ | TryWith _ -> false

  | Let (_, e1, _) -> is_nonexpansive e1

(* Fonction de substitution d'une variable de type par un type dans un autre type *)
let rec substitue_type (v : string) (sub : ptype) (t : ptype) : ptype =
  match t with
  | Tvar x -> if x = v then sub else t
  | Arr (t1, t2) -> Arr (substitue_type v sub t1, substitue_type v sub t2)
  | N  -> t
  | Listtype t1 -> Listtype (substitue_type v sub t1)
  | Forall (x, t1) -> if x = v then t else Forall (x, substitue_type v sub t1)
  | Nat -> t
  | TUnit -> t
  | RefType t1 -> RefType (substitue_type v sub t1)
  | ExnType -> t

(* Fonction de substitution d'une variable de type par un type dans un système d'équations *)
let substitue_equa (v : string) (sub : ptype) (eqs : equa) : equa =
  List.map (fun (t1, t2) -> (substitue_type v sub t1, substitue_type v sub t2)) eqs

let rec occur_check (v : string) (t : ptype) : bool =
  match t with
  | Tvar x -> x = v
  | Arr (t1, t2) -> occur_check v t1 || occur_check v t2
  | N -> false
  | Listtype t -> occur_check v t
  | Forall (x, t) -> x <> v && occur_check v t
  | Nat -> false
  | TUnit -> false
  | RefType t -> occur_check v t
  | ExnType -> false


(* Fonction de renommage des variables de type liées (barendregtisation) *)
let rec barendregtisation (t : ptype) (env : (string * string) list) : ptype =
  match t with
  | Tvar x -> (try Tvar (List.assoc x env) with Not_found -> Tvar x)
  | Arr (t1, t2) -> Arr (barendregtisation t1 env, barendregtisation t2 env)
  | Listtype t -> Listtype (barendregtisation t env)
  | Forall (x, t) ->
      let new_var = "X" ^ string_of_int (List.length env) in
      Forall (new_var, barendregtisation t ((x, new_var) :: env))
  | Nat -> Nat
  | N -> N
  | TUnit -> TUnit
  | RefType t -> RefType (barendregtisation t env)
  | ExnType -> ExnType


(* Fonction pour ouvrir un type universel *)
let rec ouvrir (t : ptype) : ptype =
  match t with
  | Forall (x, t) -> 
      let new_var = Tvar (nouvelle_var_t ()) in
      substitue_type x new_var (ouvrir t)
  | _ -> t

(* Fonction d'unification d'une étape *)
let rec unifie (eqs : equa) (substitutions_acc : (string * ptype) list) : (equa * (string * ptype) list) =
  match eqs with
  | [] -> ([], substitutions_acc)
  | (t1, t2) :: rest ->
      if t1 = t2 then
        unifie rest substitutions_acc
      else
        match (t1, t2) with
        | (Tvar x, t) | (t, Tvar x) ->
            if occur_check x t then
              failwith ("Unification échoue : occur check échoue pour " ^ x)
            else
              let new_eqs = substitue_equa x t rest in
              let new_substitutions = (x, t) :: substitutions_acc in
              unifie new_eqs new_substitutions
        | (Arr (t1a, t1r), Arr (t2a, t2r)) ->
            unifie ((t1a, t2a) :: (t1r, t2r) :: rest) substitutions_acc
        | (Listtype t1, Listtype t2) ->
            unifie ((t1, t2) :: rest) substitutions_acc
        | (Forall (_, t1), t2) ->
            let t1' = barendregtisation t1 [] in
            unifie ((ouvrir t1', t2) :: rest) substitutions_acc
        | (t1, Forall (_, t2)) ->
            let t2' = barendregtisation t2 [] in
            unifie ((t1, ouvrir t2') :: rest) substitutions_acc
        | (RefType t1, RefType t2) ->
            unifie ((t1, t2) :: rest) substitutions_acc
        | (TUnit, TUnit) ->
            unifie rest substitutions_acc
        | _ -> failwith "Unification échoue : types incompatibles"

(* Génération des équations de typage *)
let rec genere_equa t ty e =
  match t with
  | Var x -> 
      (try 
         let t' = List.assoc x e in 
         [(t', ty)]
       with Not_found -> [])
 | Abs (x, t1) -> 
    let t1' = Tvar (nouvelle_var_t ()) in
    let t2' = Tvar (nouvelle_var_t ()) in
    let inner_eqs = genere_equa t1 t2' ((x, t1') :: e) in
    inner_eqs @ [(Arr (t1', t2'), ty)]
  | App (t1, t2) -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 (Arr (t', ty)) e @ genere_equa t2 t' e
  | Int _ -> [(ty, N)]
  | Add (t1, t2) -> 
      genere_equa t1 N e @ genere_equa t2 N e @ [(ty, N)]
  | Sub (t1, t2) -> 
      genere_equa t1 N e @ genere_equa t2 N e @ [(ty, N)]
  | List ts -> 
      let t = Tvar (nouvelle_var_t ()) in
      List.flatten (List.map (fun x -> genere_equa x t e) ts) @ [(Listtype t, ty)]
  | Head t -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t (Listtype t') e @ [(ty, t')]
  | Tail t -> 
      genere_equa t (Listtype ty) e
  | Cons (t1, t2) -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 t' e @ genere_equa t2 (Listtype t') e @ [(ty, Listtype t')]
  | IfZero (t1, t2, t3) -> 
      genere_equa t1 N e @ genere_equa t2 ty e @ genere_equa t3 ty e
  | IfEmpty (t1, t2, t3) -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 (Listtype t') e @ genere_equa t2 ty e @ genere_equa t3 ty e
  | Fix t -> 
      let t1 = Tvar (nouvelle_var_t ()) in
      genere_equa t (Arr (t1, ty)) e @ [(t1, ty)]
  | Let (x, e1, e2) ->
      let ty_e1 = infer_type e1 e in
      let gen_t0 = generalise ty_e1 e in
      let env2 = (x, gen_t0) :: e in
      genere_equa e2 ty env2
  | Unit -> [(ty, TUnit)]
  | Ref t -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t t' e @ [(ty, RefType t')]
  | Deref t ->
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t (RefType t') e @ [(ty, t')]
  | Assign (t1, t2) ->
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 (RefType t') e @ genere_equa t2 t' e @ [(ty, TUnit)]
  | Address _ -> [(ty, N)]
  | Raise t -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t t' e @ [(ty, ExnType)]
  | TryWith (t1, t2) ->
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 t' e @ genere_equa t2 ty e @ [(ExnType, ty)]
  | Exn -> [(ty, ExnType)]

and resout_systeme (eqs : equa) (timeout : float) : (equa * (string * ptype) list) option =
  let start_time = Unix.gettimeofday () in
  let rec loop eqs substitutions =
    if Unix.gettimeofday () -. start_time > timeout then
      None
    else
      try 
        match eqs with
        | [] -> Some ([], substitutions)  
        | _ -> 
            let (remaining_eqs, new_subst) = unifie eqs substitutions in
            if remaining_eqs = eqs then
              Some (remaining_eqs, new_subst)  
            else
              loop remaining_eqs new_subst  
      with Failure _ -> None
  in
  loop eqs []

(* Modifie infer_type pour utiliser la généralisation faible *)
and generalise (t : ptype) (e : env) : ptype =
  let env_vars = List.map fst e in
  let libres = variables_libres t in
  let libres_hors_env = List.filter (fun x -> not (List.mem x env_vars)) libres in
  let result = List.fold_right (fun x acc -> Forall (x, acc)) libres_hors_env t in
  result


(* Modifie la généralisation pour n'autoriser que les termes non-expansifs *)
and generalise_weak (ty : ptype) (env : env) (term : pterm) : ptype =
  if is_nonexpansive term then
    generalise ty env
  else
    let msg = "Cannot generalize type of expansive term: " ^ print_term term in
    failwith msg

and infer_type (te : pterm) (env : env) : ptype =
  match te with
  | Let (x, e1, e2) ->
      let ty_e1 = infer_type e1 env in
      let gen_t0 = generalise_weak ty_e1 env e1 in 
      let env2 = (x, gen_t0) :: env in
      infer_type e2 env2
  | _ -> 
      let ty_cible = Tvar (nouvelle_var_t ()) in
      let equa = genere_equa te ty_cible env in
      match resout_systeme equa 1.0 with
      | Some (_, substitutions) -> 
          List.fold_left 
            (fun t (v, sub) -> substitue_type v sub t) 
            ty_cible 
            substitutions
      | None -> failwith "Type inference failed"

(* Fonction pour généraliser un type à partir d'un environnement *)
and variables_libres (t : ptype) : string list =
  let rec collect = function
    | Tvar x -> [x]
    | Arr (t1, t2) -> collect t1 @ collect t2
    | Listtype t -> collect t
    | Forall (x, t) -> 
        let inner_vars = collect t in
        List.filter (fun y -> y <> x) inner_vars
    | Nat -> []
    | N -> []
    | TUnit -> []
    | RefType t -> collect t
    | ExnType -> []
  in
  List.sort_uniq String.compare (collect t)








