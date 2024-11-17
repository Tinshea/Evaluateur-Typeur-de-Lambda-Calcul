open Lambda

let parse_string s =
  let lexbuf = Lexing.from_string s in
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.LexError msg -> failwith ("Lexing error: " ^ msg)
  | Parser.Error -> failwith "Parsing error"

let test_parser () =
  let examples = [
    "x", Var "x";
    "(lambda x. x)", Abs("x", Var "x");
    "let x = 1 in x + 2", 
      Let("x", Int 1, Add(Var "x", Int 2));
    "ref 42", Ref(Int 42);
    "!x", Deref(Var "x");
    "x := 42", Assign(Var "x", Int 42);
  ] in

  List.iter (fun (input, expected) ->
    let result = parse_string input in
    if result <> expected then
      Printf.printf "Error parsing '%s':\nExpected: %s\nGot: %s\n"
        input 
        (print_term expected) 
        (print_term result);
    assert (result = expected)
  ) examples;
  print_endline "All parser tests passed!"

let test_advanced_parser () =
  let tests = [

    "42", Int 42;
    "x", Var "x";
    "(lambda x. x)", Abs("x", Var "x");
    

    "let x = 42 in x", Let("x", Int 42, Var "x");
    
    "ref 42", Ref(Int 42);
    "!x", Deref(Var "x");
    "x := 42", Assign(Var "x", Int 42);
    

    "1 + 2", Add(Int 1, Int 2);
    "3 - 4", Sub(Int 3, Int 4);
    
    "[]", List [];
    "[1, 2, 3]", List [Int 1; Int 2; Int 3];
    
    "let x = ref 42 in !x + 1",
      Let("x", Ref(Int 42), Add(Deref(Var "x"), Int 1));
  ] in

  List.iteri (fun i (input, expected) ->
    Printf.printf "\nTest %d: %s\n" (i + 1) input;
    try
      let result = parse_string input in
      if result <> expected then
        Printf.printf "ÉCHEC:\n  Attendu: %s\n  Obtenu: %s\n"
          (print_term expected)
          (print_term result)
      else
        Printf.printf "OK\n"
    with e ->
      Printf.printf "ERREUR: %s\n" (Printexc.to_string e)
  ) tests

let interactive_test () =
  print_endline "Enter expressions to parse (Ctrl+D to quit):";
  try
    while true do
      print_string "> ";
      flush stdout;
      let line = input_line stdin in
      try
        let result = parse_string line in
        Printf.printf "Parsed as: %s\n" (print_term result)
      with Failure msg ->
        Printf.printf "Error: %s\n" msg
    done
  with End_of_file ->
    print_endline "\nGoodbye!"

let () = 
  print_endline "\n=== Tests du parser ===";
  test_parser();
  test_advanced_parser ();
  print_endline "\n=== Mode interactif ===";
  interactive_test ()