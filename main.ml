(* main.ml *)
(* Ce fichier rassemble tout le projet. Il contient le programme principal pour tester l'évaluateur et le typeur. *)

open Lambda
open Eval
open Types
open Unify

let env = [("x", Var "T1"); ("y", Var "T2")]

let term = App (Abs ("x", Var "x"), Var "y") (* Exemple: (λx.x) y *)

let () =
  (* Évaluer le terme *)
  let result = eval term in
  print_endline ("Résultat de l'évaluation: " ^ print_term result);

  (* Inférer le type du terme *)
  let ty = Var (nouvelle_var_t ()) in
  let equas = genere_equa term ty env in
  match unify equas with
  | [] -> print_endline "Le type a été inféré avec succès."
  | _ -> print_endline "Erreur dans l'inférence de type."
