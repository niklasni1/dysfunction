open Printf

type id = string

type binop  = Plus | Minus | Times | Div


type exp = 
         IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | LetExp of id * exp * exp
         | IfExp of exp * exp * exp
         | PrintExp of exp

module Id =
  struct
    type t = id
    let compare = compare
  end

module Env = Map.Make (Id)

let empty = Env.empty

let rec interpExps exps env = 
  match exps with
  | e::[] -> interpExp e env
  | e::es -> interpExp e env; interpExps es env

 and interpExp exp env =
  match exp with
  | IdExp id -> (try Env.find id env with
                 | Not_found -> printf "unbound identifier: %s\n" id;
                   exit (-1)
                )
  | NumExp n -> n
  | OpExp (e1, op, e2) -> (match op with
                           | Plus -> (interpExp e1 env) + (interpExp e2 env)
                           | Minus -> (interpExp e1 env) - (interpExp e2 env)
                           | Times -> (interpExp e1 env) * (interpExp e2 env)
                           | Div -> (interpExp e1 env) / (interpExp e2 env))
  | LetExp (id,value,body) -> interpExp body (Env.add id (interpExp value env) env)
  | IfExp (c,e1,e2) -> (match (interpExp c env) with 
                        | 1 -> interpExp e1 env 
                        | 0 -> interpExp e2 env)
  | PrintExp e -> let x = (interpExp e env) in print_int x; print_newline (); x
