open Lambda
open Eval

let zero = Int 0
let one = Int 1
let two = Int 2
let three = Int 3

(* Fonction d'identité polymorphe *)
let id = Abs ("x", Var "x")

(* Fonction de composition polymorphe *)
let compose = Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x")))))

let factorial = 
  Fix (Abs ("f", Abs ("n", IfZero (Var "n", Int 1, 
    App (App (Var "mul", Var "n"), App (Var "f", App (Var "pred", Var "n")))))))

(* Tests pour la normalisation avec les nouveaux termes *)
let test_normalization_with_examples () =
  let examples = [
    ("0", zero);
    ("1", one);
    ("2", two);
    ("3", three);
    ("1 + 1", Add (one, one));
    ("1 + 2", Add (one, two));
    ("2 + 1", Add (two, one));
    ("2 + 2", Add (two, two));
    ("1 - 1", Sub (one, one));
    ("2 - 1", Sub (two, one));
    ("2 - 2", Sub (two, two));
    ("[1; 2; 3]", List [one; two; three]);
    ("head [1; 2; 3]", Head (List [one; two; three]));
    ("tail [1; 2; 3]", Tail (List [one; two; three]));
    ("cons 1 [2; 3]", Cons (one, List [two; three]));
    ("ifzero 0 then 1 else 2", IfZero (zero, one, two));
    ("ifzero 1 then 1 else 2", IfZero (one, one, two));
    ("ifzero 2 then 1 else 2", IfZero (two, one, two));
    ("ifempty [] then 1 else 2", IfEmpty (List [], one, two));
    ("ifempty [1; 2] then 1 else 2", IfEmpty (List [one; two], one, two));
    ("fix id", Fix id);
    ("id 1", App (id, one));
    ("compose id id 1", App (App (compose, id), App (id, one)));
    ("factorial 0", App (factorial, zero));
    ("factorial 1", App (factorial, one));
    ("factorial 2", App (factorial, two));
    ("factorial 3", App (factorial, three));
  ] in
  List.iter (fun (name, term) ->
    print_endline ("Test " ^ name ^ ": " ^ print_term term);
    let result = ltr_cbv_norm term in
    print_endline ("Result: " ^ print_term result)
  ) examples

let () =
  test_normalization_with_examples ()

(* Tests pour la normalisation avec les nouveaux termes *)
let test_normalization_with_examples () =
  let examples = [
    ("0", zero);
    ("1", one);
    ("2", two);
    ("3", three);
    ("1 + 1", Add (one, one));
    ("1 + 2", Add (one, two));
    ("2 + 1", Add (two, one));
    ("2 + 2", Add (two, two));
    ("1 - 1", Sub (one, one));
    ("2 - 1", Sub (two, one));
    ("2 - 2", Sub (two, two));
    ("[1; 2; 3]", List [one; two; three]);
    ("head [1; 2; 3]", Head (List [one; two; three]));
    ("tail [1; 2; 3]", Tail (List [one; two; three]));
    ("cons 1 [2; 3]", Cons (one, List [two; three]));
    ("ifzero 0 then 1 else 2", IfZero (zero, one, two));
    ("ifzero 1 then 1 else 2", IfZero (one, one, two));
    ("ifzero 2 then 1 else 2", IfZero (two, one, two));
    ("ifempty [] then 1 else 2", IfEmpty (List [], one, two));
    ("ifempty [1; 2] then 1 else 2", IfEmpty (List [one; two], one, two));
    ("fix id", Fix id);
    ("id 1", App (id, one));
    ("compose id id 1", App (App (compose, id), App (id, one)));

  ] in
  List.iter (fun (name, term) ->
    print_endline ("Test " ^ name ^ ": " ^ print_term term);
    let result = ltr_cbv_norm term in
    print_endline ("Result: " ^ print_term result)
  ) examples

let () =
  test_normalization_with_examples ()