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
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa = 
  match te with
  | Var v -> [(cherche_type v e, ty)]
  | App (t1, t2) -> 
    let ta = Var (nouvelle_var_t ()) in
    genere_equa t1 (Arr (ta, ty)) e @ genere_equa t2 ta e
  | Abs (x, t) -> 
      let ta = Var (nouvelle_var_t ()) in
      let tr = Var (nouvelle_var_t ()) in
      (ty, Arr (ta, tr)) :: genere_equa t tr ((x, ta) :: e)

      (* Fonction d'occur check *)
let rec occur_check (v : string) (t : ptype) : bool =
  match t with
  | Var x -> x = v
  | Arr (t1, t2) -> occur_check v t1 || occur_check v t2
  | Nat -> false


  (* Fonction de substitution d'une variable de type par un type dans un autre type *)
  let rec substitue_type (v : string) (sub : ptype) (t : ptype) : ptype =
    match t with
    | Var x -> if x = v then sub else t
    | Arr (t1, t2) -> Arr (substitue_type v sub t1, substitue_type v sub t2)
    | Nat -> Nat

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
          | (Var x, t) | (t, Var x) ->
            if occur_check x t then
              failwith ("Unification échoue : occur check échoue pour " ^ x)
            else
              unifie (substitue_equa x t rest)
          | (Arr (t1a, t1r), Arr (t2a, t2r)) ->
            unifie ((t1a, t2a) :: (t1r, t2r) :: rest)
          | _ -> failwith "Unification échoue : types incompatibles"

          open Unix

          (* Fonction qui résout un système d'équations avec un timeout *)
          let resout_systeme (eqs : equa) (timeout : float) : equa option =
            let start_time = Unix.gettimeofday () in
            let rec loop eqs =
              if Unix.gettimeofday () -. start_time > timeout then
                None
              else
                try Some (unifie eqs)
                with Failure _ -> None
            in
            loop eqs

          (* Fonction qui infère le type d'un terme ou indique la non-typabilité *)
          let infere_type (te : pterm) (timeout : float) : ptype option =
            let ty = Var (nouvelle_var_t ()) in
            let eqs = genere_equa te ty [] in
            match resout_systeme eqs timeout with
            | Some _ -> Some ty
            | None -> None