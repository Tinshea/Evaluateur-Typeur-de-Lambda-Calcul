(* test_eval.ml *)
(* Ce fichier rassemble tout le projet. Il contient le programme principal pour tester l'évaluateur et le typeur. *)

open Lambda  
open Eval   

(* Définition des combinateurs et des encodages *)
let i = Abs ("x", Var "x")
let delta = Abs ("x", App (Var "x", Var "x"))
let omega = App (delta, delta)
let s = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let k = Abs ("x", Abs ("y", Var "x"))
let skk = App (App (s, k), k)
let sii = App (App (s, i), i)

(* Church encodings for numbers *)
let zero = Abs ("f", Abs ("x", Var "x"))
let one = Abs ("f", Abs ("x", App (Var "f", Var "x")))
let two = Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x"))))
let three = Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))

(* Arithmetic operations *)
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))
let add = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
let mul = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))

(* Tests pour la normalisation avec les nouveaux termes *)
let test_normalization_with_examples () =
  let examples = [
    ("I", i);
    ("δ", delta);
    ("S", s);
    ("K", k);
    ("S K K", skk);
    ("S I I", sii);
    ("0", zero);
    ("1", one);
    ("2", two);
    ("3", three);
    ("succ 0", App (succ, zero));
    ("succ 1", App (succ, one));
    ("succ 2", App (succ, two));
    ("1 + 1", App (App (add, one), one));
    ("2 * 2", App (App (mul, two), two));
    ("I applied to 1", App (i, one));
    ("K applied to 1 and 2", App (App (k, one), two));
    ("S applied to K and I", App (App (s, k), i));
    
    ("Ω", omega);
  ] in
  List.iter (fun (name, term) ->
    print_endline ("Test " ^ name ^ ": " ^ print_term term);
    let result = ltr_cbv_norm term in
    print_endline ("Result: " ^ print_term result)
  ) examples

(* Fonction principale pour exécuter les tests *)
let () =
  test_normalization_with_examples ();
  print_endline "Fin des tests."
