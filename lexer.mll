{
open Lexing
open Parser

exception SyntaxError of string

}

let int = ['0'-'9']+
let id = ['a'-'z']+
let white = [' ' '\t' '\n' '\r']

rule read =
  parse
  | white { read lexbuf }
  | int { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "print" { PRINT }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | '(' { OPAREN }
  | ')' { CPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIV }
  | '*' { TIMES }
  | "=" { EQUALS }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

