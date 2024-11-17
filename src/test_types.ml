open Types
open Syntax
open Lambda

(* Test for cherche_type *)
let test_cherche_type () =
  let env = [("x", Tvar "T1"); ("y", Arr (Tvar "T2", Tvar "T3"))] in
  assert (cherche_type "x" env = Tvar "T1");
  assert (cherche_type "y" env = Arr (Tvar "T2", Tvar "T3"));
  try
    let _ = cherche_type "z" env in
    assert false
  with Failure _ -> assert true

(* Test for substitue_type *)
let test_substitue_type () =
  let t = Arr (Tvar "T1", Tvar "T2") in
  let sub = Tvar "T3" in
  let result = substitue_type "T1" sub t in
  assert (result = Arr (Tvar "T3", Tvar "T2"))

(* Test for occur_check *)
let test_occur_check () =
  let t = Arr (Tvar "T1", Tvar "T2") in
  assert (occur_check "T1" t = true);
  assert (occur_check "T3" t = false)

(* Test for barendregtisation *)
let test_barendregtisation () =
  let t = Forall ("X", Arr (Tvar "X", Tvar "Y")) in
  let result = barendregtisation t [] in
  match result with
  | Forall (new_var, Arr (Tvar x, Tvar y)) ->
      assert (new_var <> "X");
      assert (x = new_var);
      assert (y = "Y")
  | _ -> assert false

(* Test for ouvrir *)
let test_ouvrir () =
  let t = Forall ("X", Arr (Tvar "X", Tvar "Y")) in
  let result = ouvrir t in
  match result with
  | Arr (Tvar new_var, Tvar "Y") -> assert (new_var <> "X")
  | _ -> assert false

(* Test for unifie *)
(* Test for unifie *)
let test_unifie () =
  let eqs = [(Tvar "T1", Tvar "T2"); 
             (Arr (Tvar "T1", Tvar "T3"), Arr (Tvar "T2", Tvar "T4"))] in
  
  (* Call unifie with empty substitutions *)
  let (result, subst) = unifie eqs [] in
  
  
  (* Verify result and substitutions *)
  assert (result = []);
  assert (List.exists (fun (v,t) -> v = "T1" && t = Tvar "T2") subst);
  assert (List.exists (fun (v,t) -> v = "T3" && t = Tvar "T4") subst)

(* Test for genere_equa *)
let test_genere_equa () =
  let t = Abs ("x", Var "x") in
  let ty = Tvar "T1" in
  let env = [] in
  let result = genere_equa t ty env in
  assert (result = [(Tvar "T2", Tvar "T3"); (Arr (Tvar "T2", Tvar "T3"), Tvar "T1")])

(* Test for infer_type *)
let test_infer_type () =
  let t = Abs ("x", Var "x") in
  let env = [] in
  let result = infer_type t env in

  match result with
  | Arr (Tvar x, Tvar y) -> 
      assert (String.equal x y);  (* Identity function should have same input and output type *)
  | _ -> 
      Printf.printf "Unexpected type: %s\n" (print_type result);
      assert false
  
(* Test for polymorphic identity function *)
let test_polymorphic_identity () =
  let t = Let("id", Abs ("x", Var "x"), Var "id") in
  let env = [] in
  let result = infer_type t env in
  Printf.printf "Polymorphic Identity Result: %s\n" (print_type result);
  match result with
  | Forall (_, Arr (Tvar x, Tvar y)) -> assert (x = y)
  | other -> 
      Printf.printf "Expected forall T.(T -> T), got %s\n" (print_type other);
      assert false

(* Test for polymorphic constant function *)
let test_polymorphic_constant () =
  let t = Let("const", 
              Abs ("x", Abs ("y", Var "x")),
              Var "const") in
  let env = [] in
  let result = infer_type t env in
  
  Printf.printf "Polymorphic Constant Result: %s\n" (print_type result);
  
  match result with
  | Forall (_, Forall(_, Arr (Tvar a, Arr (Tvar b, Tvar c)))) -> 
      Printf.printf "Found type structure: %s -> (%s -> %s)\n" 
        (print_type (Tvar a))
        (print_type (Tvar b))
        (print_type (Tvar c));
      assert (a = c); (* First argument type equals return type *)
      assert (b <> a) (* Second argument type is different *)
  | other -> 
      Printf.printf "Expected forall a.forall b.(a -> b -> a), got %s\n" 
        (print_type other);
      assert false
(* Test for factorial function *)
let test_factorial () =
  let t = Fix (Abs ("f", Abs ("n",
    IfZero (Var "n",
      Int 1,
      Add (Var "n", App (Var "f", Sub (Var "n", Int 1)))
    )
  ))) in
  let env = [] in
  let result = infer_type t env in
  Printf.printf "Factorial Result: %s\n" (print_type result);
  match result with
  | Arr (N, N) -> ()
  | _ -> assert false

(* Test for unit and reference types *)
(* Test for unit and reference types with more granular cases *)
let test_unit_and_ref () =
  (* Test unit type *)
  let unit_term = Unit in
  let unit_type = infer_type unit_term [] in
  assert (unit_type = TUnit);

  (* Test basic reference creation *)
  let ref_term = Ref (Int 42) in
  let ref_type = infer_type ref_term [] in
  (match ref_type with
   | RefType N -> ()
   | _ -> 
       Printf.printf "Expected RefType N, got %s\n" (print_type ref_type);
       assert false);

  (* Test basic reference dereferencing *)
  let deref_term = Deref (Ref (Int 42)) in
  let deref_type = infer_type deref_term [] in
  assert (deref_type = N);

  (* Test reference assignment *)
  let assign_term = Assign (Ref (Int 42), Int 43) in
  let assign_type = infer_type assign_term [] in
  assert (assign_type = TUnit);

  (* Test reference to function *)
  let fun_ref_term = Ref (Abs ("x", Var "x")) in
  let fun_ref_type = infer_type fun_ref_term [] in
  match fun_ref_type with
  | RefType (Arr (Tvar x, Tvar y)) -> assert (x = y)
  | _ -> 
      Printf.printf "Expected RefType (T -> T), got %s\n" (print_type fun_ref_type);

  (* Test dereferencing function reference *)
  let deref_fun_term = Deref (Ref (Abs ("x", Var "x"))) in
  let deref_fun_type = infer_type deref_fun_term [] in
  match deref_fun_type with
  | Arr (Tvar x, Tvar y) -> assert (x = y)
  | _ -> 
      Printf.printf "Expected T -> T, got %s\n" (print_type deref_fun_type);

  (* Test nested references *)
  let nested_ref = Ref (Ref (Int 42)) in
  let nested_type = infer_type nested_ref [] in
  match nested_type with
  | RefType (RefType N) -> ()
  | _ -> 
      Printf.printf "Expected RefType (RefType N), got %s\n" (print_type nested_type);
      assert false
(* Test functions for type system *)
let test_weak_polymorphism () =
  (* Test non-expansive term - should be generalized *)
  let id_let = Let ("id", Abs ("x", Var "x"), Var "id") in
  let id_type = infer_type id_let [] in
  (match id_type with
   | Forall (_, Arr (Tvar x, Tvar y)) when x = y -> ()
   | _ -> 
       Printf.printf "Expected forall T. T->T, got %s\n" (print_type id_type);
       failwith "Non-expansive term not properly generalized");

  (* Test expansive term - should not be generalized *)
  let ref_let = Let ("r", 
                    Ref (Abs ("x", Var "x")),
                    Deref (Var "r")) in
  let ref_type = infer_type ref_let [] in
  (match ref_type with
   | Arr (Tvar x, Tvar y) when x = y -> ()
   | _ -> 
       Printf.printf "Expected T->T (not generalized), got %s\n" (print_type ref_type);
       failwith "Expansive term incorrectly generalized");

  print_endline "Weak polymorphism tests passed!"

let test_error_cases () =
  (* Test type mismatch *)
  let bad_term = App (Int 42, Int 43) in
  (try
    let _ = infer_type bad_term [] in
    assert false
  with Failure _ -> ());

  (* Test invalid reference access *)
  let bad_ref = Deref (Int 42) in
  (try
    let _ = infer_type bad_ref [] in
    assert false
  with Failure _ -> ());

  print_endline "Error case tests passed!"

  
(* Run all tests *)
let () =

  test_cherche_type ();
  test_substitue_type ();
  test_occur_check ();
  test_barendregtisation ();
  test_ouvrir ();
  test_unifie ();
  test_genere_equa ();
  test_polymorphic_identity ();
  test_polymorphic_constant ();
  test_factorial ();
  test_infer_type ();
  test_unit_and_ref ();
  test_weak_polymorphism ();
  test_error_cases ();
  print_endline "All tests passed!";




