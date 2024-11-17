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

  (* Print debug info *)
  Printf.printf "Raw type: %s\n" (print_type result);

  match result with
  | Arr (Tvar x, Tvar y) -> 
      Printf.printf "Found arrow type %s -> %s\n" x y;
      assert (x = y)
  | _ -> assert false
  
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

  (* Test simple function application typing *)
let test_simple_application () =
  (* Create environment with function f: B1 -> B2 and argument x: B1 *)
  let env = [("f", Arr(Tvar "B1", Tvar "B2")); 
             ("x", Tvar "B1")] in
  
  (* Build application term f x *)
  let term = App(Var "f", Var "x") in
  
  (* Infer type *)
  let result = infer_type term env in
  
  Printf.printf "Application type result: %s\n" (print_type result);
  
  (* Should infer B2 as the result type *)
  match result with
  | Tvar "B2" -> ()  (* Success *)
  | other -> 
      Printf.printf "Expected B2, got %s\n" (print_type other);
      assert false

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
  test_simple_application ();
  print_endline "All tests passed!"