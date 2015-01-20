open Printf

type id = string

type binop  = Plus | Minus | Times | Div | Equals

type exp = 
         IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | LetExp of id * exp * exp
         | IfExp of exp * exp * exp
         | PrintExp of exp
         | FunExp of id * exp
         | CallExp of id * exp
         | SeqExp of exp * exp

type value = 
  Integer of int
  | Boolean of bool
  | Function of exp * id


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
  | NumExp n -> Integer n
  | OpExp (e1, op, e2) -> let aV = (interpExp e1 env) in 
      let bV = (interpExp e2 env) in (match (aV,bV) with 
      | (Integer a, Integer b) -> (match op with 
            | Plus -> Integer (a + b)
            | Minus -> Integer (a - b)
            | Times -> Integer (a * b)
            | Div -> Integer (a / b)
            | Equals -> if a = b then Boolean true else Boolean false)
      | _ -> exit(-1))

  | LetExp (id,v,body) -> interpExp body (Env.add id (interpExp v env) env)
  | IfExp (c,e1,e2) -> (match (interpExp c env) with 
                        | Boolean true -> interpExp e1 env 
                        | Boolean false -> interpExp e2 env
                        | Integer 0 -> interpExp e2 env
                        | Integer _ -> interpExp e1 env)
  | PrintExp e -> (match (interpExp e env) with 
                  | Integer x -> print_int x; print_newline (); Integer x
                  | Boolean b-> printf "%B\n" b; Boolean b 
                  | Function (f,b) -> print_string "Function"; print_newline (); Function (f, b))
  | FunExp (p,body) -> Function(body, p)
  | CallExp (func,binding) -> (match (Env.find func env) with
                        | Function (f,p) -> interpExp f (Env.add p (interpExp binding env) env))
  | SeqExp (e1,e2) -> interpExp e1 env; interpExp e2 env 
