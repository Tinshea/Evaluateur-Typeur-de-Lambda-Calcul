{
open Parser
exception LexError of string
}

let whitespace = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*

rule token = parse
  | whitespace+  { token lexbuf }
  | "lambda"     { LAMBDA }
  | "fun"        { LAMBDA }
  | "let"        { LET }
  | "in"         { IN }
  | "fix"        { FIX }
  | "ref"        { REF }
  | "!"          { DEREF }
  | ":="         { ASSIGN }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "["          { LBRACK }
  | "]"          { RBRACK }
  | "->"         { ARROW }
  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { STAR }
  | "="          { EQUALS }
  | ","          { COMMA }
  | "."          { DOT }
  | digit+ as n  { INT(int_of_string n) }
  | identifier as id { IDENT(id) }
  | eof          { EOF }
  | _ as c       { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }