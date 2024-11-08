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
  | Int _ -> [(Nat, ty)]
  | Add (t1, t2) -> 
      genere_equa t1 Nat e @ genere_equa t2 Nat e @ [(Nat, ty)]
  | Sub (t1, t2) -> 
      genere_equa t1 Nat e @ genere_equa t2 Nat e @ [(Nat, ty)]
  | List ts -> 
      let t = Tvar (nouvelle_var_t ()) in
      List.flatten (List.map (fun x -> genere_equa x t e) ts) @ [(Listtype t, ty)]
  | Head t -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t (Listtype t') e @ [(t', ty)]
  | Tail t -> 
      genere_equa t (Listtype ty) e
  | Cons (t1, t2) -> 
      genere_equa t1 ty e @ genere_equa t2 (Listtype ty) e
  | IfZero (t1, t2, t3) -> 
      genere_equa t1 Nat e @ genere_equa t2 ty e @ genere_equa t3 ty e
  | IfEmpty (t1, t2, t3) -> 
      let t' = Tvar (nouvelle_var_t ()) in
      genere_equa t1 (Listtype t') e @ genere_equa t2 ty e @ genere_equa t3 ty e
  | Fix t -> 
      let t1 = Tvar (nouvelle_var_t ()) in
      genere_equa t (Arr (t1, ty)) e @ [(t1, ty)]
  | Let (x, t1, t2) -> 
      let t = Tvar (nouvelle_var_t ()) in
      genere_equa t1 t e @ genere_equa t2 ty ((x, t) :: e)
  

      
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