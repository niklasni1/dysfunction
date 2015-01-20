open Lexing
open Lexer
open Printf

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf " %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
      | SyntaxError msg ->
          printf "%a: %s\n" print_position lexbuf msg;
          None
      | Parser.Error ->
          printf "%a: syntax error\n" print_position lexbuf;
          exit (-1)

let main () =
  let lexbuf = Lexing.from_channel stdin in
  begin match parse_with_error lexbuf with
  | None -> ()
  | Some exps ->
    let x = Interpreter.interpExps exps Interpreter.empty in
      (match x with 
        | Integer x -> printf "returned %d\n" x
        | Boolean b-> printf "returned %B\n" b
        | Function _ -> printf "returned a function\n")
  end

let _ =
  main ()
