(* types.ml *)
(* Ce fichier contient la définition des types simples et les fonctions associées pour générer les équations de typage à partir des termes. *)

open Syntax
open Lambda

(* Environnement de typage *)
type env = (string * ptype) list

(* Type pour les équations de typage *)
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
      genere_equa t1 t2' ((x, t1') :: e) @ [(Arr (t1', t2'), ty)]
  | App (t1, t2) -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 (Arr (t', ty)) e @ genere_equa t2 t' e
  | Int _ -> [(N, ty)]
  | Add (t1, t2) -> 
      genere_equa t1 N e @ genere_equa t2 N e @ [(ty, Arr (N, Arr (N, N)))]
  | Sub (t1, t2) -> 
      genere_equa t1 N e @ genere_equa t2 N e @ [(ty, Arr (N, Arr (N, N)))]
  | List ts -> 
      let t = Tvar (nouvelle_var_t ()) in
      List.flatten (List.map (fun x -> genere_equa x t e) ts) @ [(Listtype t, ty)]
  | Head t -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t (Listtype t') e @ [(ty, Arr (Listtype t', t'))]
  | Tail t -> 
      genere_equa t (Listtype ty) e
  | Cons (t1, t2) -> 
      genere_equa t1 ty e @ genere_equa t2 (Listtype ty) e
  | IfZero (t1, t2, t3) -> 
      genere_equa t1 N e @ genere_equa t2 ty e @ genere_equa t3 ty e
  | IfEmpty (t1, t2, t3) -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 (Listtype t') e @ genere_equa t2 ty e @ genere_equa t3 ty e
  | Fix t -> 
      let t1 = Tvar (nouvelle_var_t ()) in
      genere_equa t (Arr (t1, ty)) e @ [(t1, ty)]
  | Let (x, t1, t2) -> 
      let t = Tvar (nouvelle_var_t ()) in
      let t1_type = typage t1 e in
      genere_equa t1 t e @ genere_equa t2 ty ((x, t1_type) :: e)

and typage t e =
  match t with
  | Var x -> List.assoc x e
  | Abs (x, t1) -> 
      let t1' = Tvar (nouvelle_var_t ()) in
      let t2' = typage t1 ((x, t1') :: e) in
      Arr (t1', t2')
  | App (t1, t2) -> 
      let t1_type = typage t1 e in
      let t2_type = typage t2 e in
      (match t1_type with
       | Arr (t1', t2') when t1' = t2_type -> t2'
       | _ -> failwith "Type mismatch")
  | Int _ -> N
  | Add (t1, t2) -> 
      if typage t1 e = N && typage t2 e = N then N
      else failwith "Type mismatch"
  | Sub (t1, t2) -> 
      if typage t1 e = N && typage t2 e = N then N
      else failwith "Type mismatch"
  | List ts -> 
      let t = Tvar (nouvelle_var_t ()) in
      List.iter (fun x -> if typage x e <> t then failwith "Type mismatch") ts;
      Listtype t
  | Head t -> 
      (match typage t e with
       | Listtype t' -> t'
       | _ -> failwith "Type mismatch")
  | Tail t -> 
      (match typage t e with
       | Listtype t' -> Listtype t'
       | _ -> failwith "Type mismatch")
  | Cons (t1, t2) -> 
      let t1_type = typage t1 e in
      let t2_type = typage t2 e in
      if t2_type = Listtype t1_type then Listtype t1_type
      else failwith "Type mismatch"
  | IfZero (t1, t2, t3) -> 
      if typage t1 e = N then
        let t2_type = typage t2 e in
        let t3_type = typage t3 e in
        if t2_type = t3_type then t2_type
        else failwith "Type mismatch"
      else failwith "Type mismatch"
  | IfEmpty (t1, t2, t3) -> 
      (match typage t1 e with
       | Listtype _ -> 
           let t2_type = typage t2 e in
           let t3_type = typage t3 e in
           if t2_type = t3_type then t2_type
           else failwith "Type mismatch"
       | _ -> failwith "Type mismatch")
  | Fix t -> 
      (match typage t e with
       | Arr (t1, t2) when t1 = t2 -> t1
       | _ -> failwith "Type mismatch")
  | Let (x, t1, t2) -> 
      let t1_type = typage t1 e in
      typage t2 ((x, t1_type) :: e)
  

      
(* Fonction d'occur check *)
let rec occur_check (v : string) (t : ptype) : bool =
  match t with
  | Tvar x -> x = v
  | Arr (t1, t2) -> occur_check v t1 || occur_check v t2
  | N -> false
  | Listtype t -> occur_check v t
  | Forall (x, t) -> x <> v && occur_check v t
  | Nat -> false


(* Fonction de substitution d'une variable de type par un type dans un autre type *)
let rec substitue_type (v : string) (sub : ptype) (t : ptype) : ptype =
  match t with
  | Tvar x -> if x = v then sub else t
  | Arr (t1, t2) -> Arr (substitue_type v sub t1, substitue_type v sub t2)
  | N  -> t
  | Listtype t1 -> Listtype (substitue_type v sub t1)
  | Forall (x, t1) -> if x = v then t else Forall (x, substitue_type v sub t1)
  | Nat -> t

(* Fonction de substitution d'une variable de type par un type dans un système d'équations *)
let substitue_equa (v : string) (sub : ptype) (eqs : equa) : equa =
  List.map (fun (t1, t2) -> (substitue_type v sub t1, substitue_type v sub t2)) eqs

(* Fonction pour obtenir les variables libres d'un type *)
let rec variables_libres (t : ptype) : string list =
  match t with
  | Tvar x -> [x]
  | Arr (t1, t2) -> variables_libres t1 @ variables_libres t2
  | Listtype t -> variables_libres t
  | Forall (x, t) -> List.filter (fun y -> y <> x) (variables_libres t)
  | Nat -> []
  | N -> []

(* Fonction pour généraliser un type à partir d'un environnement *)
let generalise (t : ptype) (e : env) : ptype =
  let env_vars = List.map fst e in
  let libres = variables_libres t in (*Utiliser la fonction variables_libres pour obtenir la liste des variables libres dans le type t.*)
  let libres_hors_env = List.filter (fun x -> not (List.mem x env_vars)) libres in (*Filtrer les variables libres qui ne sont pas dans l'environnement.*)
  List.fold_right (fun x acc -> Forall (x, acc)) libres_hors_env t (*Utiliser List.fold_right pour construire le type polymorphe.*)

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

(* Fonction pour ouvrir un type universel *)
let rec ouvrir (t : ptype) : ptype =
  match t with
  | Forall (x, t) -> 
      let new_var = Tvar (nouvelle_var_t ()) in
      substitue_type x new_var (ouvrir t)
  | _ -> t

(* Fonction d'unification d'une étape *)
let rec unifie (eqs : equa) : equa =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest ->
      if t1 = t2 then
        unifie rest
      else
        match (t1, t2) with
        | (Tvar x, t) | (t, Tvar x) ->
            if occur_check x t then
              failwith ("Unification échoue : occur check échoue pour " ^ x)
            else
              unifie (substitue_equa x t rest)
        | (Arr (t1a, t1r), Arr (t2a, t2r)) ->
            unifie ((t1a, t2a) :: (t1r, t2r) :: rest)
        | (Listtype t1, Listtype t2) ->
            unifie ((t1, t2) :: rest)
        | (Forall (_, t1), t2) ->
            let t1' = barendregtisation t1 [] in
            unifie ((ouvrir t1', t2) :: rest)
        | (t1, Forall (_, t2)) ->
            let t2' = barendregtisation t2 [] in
            unifie ((t1, ouvrir t2') :: rest)
        | _ -> failwith "Unification échoue : types incompatibles"

(* Fonction qui résout un système d'équations avec un timeout *)
let resout_systeme (eqs : equa) (timeout : float) : equa option =
  let start_time = Unix.gettimeofday () in
    let loop eqs =
      if Unix.gettimeofday () -. start_time > timeout then
        None
      else
        try Some (unifie eqs)
        with Failure _ -> None
    in
    loop eqs

(* Fonction qui infère le type d'un terme ou indique la non-typabilité *)
let infere_type (te : pterm) (timeout : float) : ptype option =
  let ty = Tvar (nouvelle_var_t ()) in
  let eqs = genere_equa te ty [] in
    match resout_systeme eqs timeout with
    | Some _ -> Some ty
    | None -> None