{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
                         pos_lnum = pos.pos_lnum + 1
    } 
}

let int = ['0'-'9']+
let id = ['a'-'z']+
let white = [' ' '\t' ]+
let newline = '\n' | '\r' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "call" { CALL }
  | "print" { PRINT }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | '(' { OPAREN }
  | ')' { CPAREN }
  | '{' { OBRACK }
  | '}' { CBRACK }
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIV }
  | '*' { TIMES }
  | '=' { EQUALS }
  | ';' { SEMICOLON }
  | '.' { STOP }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

