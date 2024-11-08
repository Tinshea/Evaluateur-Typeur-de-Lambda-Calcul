(* test_types.ml *)
(* Ce fichier rassemble tout le projet. Il contient le programme principal pour tester l'évaluateur et le typeur. *)

open Types
open Syntax
open Lambda

(* Fonction pour afficher les équations de typage *)
let print_equa (eqs : equa) : string =
  String.concat "\n" (List.map (fun (t1, t2) -> (print_type t1) ^ " = " ^ (print_type t2)) eqs)

(* Tests *)
let () =
  let open Printf in

  (* Test 1: Variable *)
  let env = [("x", Tvar "T1")] in
  let term = Var "x" in
  let ty = Tvar "T2" in
  let eqs = genere_equa term ty env in
  printf "Test 1: Variable\n";
  printf "Term: x\n";
  printf "Type: %s\n" (print_type ty);
  printf "Equations:\n%s\n\n" (print_equa eqs);

  (* Test 2: Application *)
  let env = [("f", Arr (Tvar "T1", Tvar "T2")); ("x", Tvar "T1")] in
  let term = App (Var "f", Var "x") in
  let ty = Tvar "T3" in
  let eqs = genere_equa term ty env in
  printf "Test 2: Application\n";
  printf "Term: f x\n";
  printf "Type: %s\n" (print_type ty);
  printf "Equations:\n%s\n\n" (print_equa eqs);

  (* Test 3: Abstraction *)
  let env = [] in
  let term = Abs ("x", Var "x") in
  let ty = Tvar "T1" in
  let eqs = genere_equa term ty env in
  printf "Test 3: Abstraction\n";
  printf "Term: \\x. x\n";
  printf "Type: %s\n" (print_type ty);
  printf "Equations:\n%s\n\n" (print_equa eqs);

  (* Test 4: Complex term *)
  let env = [("f", Arr (Tvar "T1", Tvar "T2")); ("x", Tvar "T1")] in
  let term = Abs ("y", App (Var "f", Var "x")) in
  let ty = Tvar "T3" in
  let eqs = genere_equa term ty env in
  printf "Test 4: Complex term\n";
  printf "Term: \\y. f x\n";
  printf "Type: %s\n" (print_type ty);
  printf "Equations:\n%s\n\n" (print_equa eqs);

  (* Test 5: Unification *)
  let eqs = [(Tvar "T1", Tvar "T2"); (Tvar "T2", Nat)] in
  let unified_eqs = unifie eqs in
  printf "Test 5: Unification\n";
  printf "Equations:\n%s\n" (print_equa eqs);
  printf "Unified Equations:\n%s\n\n" (print_equa unified_eqs);

  (* Test 6: Type inference *)
  let term = Abs ("x", App (Var "x", Var "x")) in
  let inferred_type = infere_type term 1.0 in
  printf "Test 6: Type inference\n";
  printf "Term: \\x. x x\n";
  (match inferred_type with
  | Some ty -> printf "Inferred Type: %s\n\n" (print_type ty)
  | None -> printf "Inferred Type: None\n\n")
