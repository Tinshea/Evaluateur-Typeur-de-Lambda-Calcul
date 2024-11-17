open Lambda
open Eval

(* Basic values *)
let unit_val = Unit
let zero = Int 0
let one = Int 1
let two = Int 2

let test_imperative_features () =
  let examples = [
    (* Basic unit and reference tests *)
    ("unit value", unit_val);
    ("create ref", Ref(one));
    ("deref ref", Let("x", Ref(one), Deref(Var "x")));
    
    (* Assignment tests *)
    ("simple assignment", 
     Let("x", Ref(one),
         Let("_", Assign(Var "x", two),
             Deref(Var "x"))));
    
    (* Multiple references *)
    ("multiple refs",
     Let("x", Ref(one),
         Let("y", Ref(two),
             Add(Deref(Var "x"), Deref(Var "y")))));
    
    (* Sequential operations *)
    ("sequence operations",
     Let("x", Ref(zero),
         Let("_", Assign(Var "x", one),
             Let("_", Assign(Var "x", two),
                 Deref(Var "x")))));
    
    (* Combining with other features *)
    ("ref with arithmetic",
     Let("x", Ref(one),
         Let("y", Ref(two),
             Let("_", Assign(Var "x", Add(Deref(Var "x"), Deref(Var "y"))),
                 Deref(Var "x")))));
  ] in
  
  List.iter (fun (name, term) ->
    print_endline ("\nTest: " ^ name);
    print_endline ("Expression: " ^ print_term term);
    let (result, memory) = ltr_cbv_norm term in
    print_endline ("Result: " ^ print_term result);
    print_endline "Memory state:";
    List.iter (fun (addr, value) -> 
      Printf.printf "  Address %d -> %s\n" addr (print_term value)
    ) memory;
    print_endline "-------------------";
  ) examples

  let test_exceptions () =
    (* Test 1: Basic exception raising *)
    let test1 = ltr_cbv_step (Raise Exn) [] in
    assert (match test1 with
      | None -> true  
      | _ -> false);
  
    print_endline "test passed!"

let () =
  print_endline "\n=== Testing Imperative Features ===";
  test_imperative_features ();
  print_endline "\n=== Testing Exception Features ===";
  test_exceptions ()

