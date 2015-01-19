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
         | FunExp of id * id * exp * exp
         | CallExp of id * exp
         | SeqExp of exp * exp

type func = Function of exp * id


module Id =
  struct
    type t = id
    let compare = compare
  end

module Env = Map.Make (Id)
module Fenv = Map.Make (Id)

let empty = Env.empty
let fempty = Fenv.empty

let rec interpExps exps venv fenv = 
  match exps with
  | e::[] -> interpExp e venv fenv
  | e::es -> interpExp e venv fenv; interpExps es venv fenv

 and interpExp exp venv fenv =
  match exp with
  | IdExp id -> (try Env.find id venv with
                 | Not_found -> printf "unbound identifier: %s\n" id;
                   exit (-1)
                )
  | NumExp n -> n
  | OpExp (e1, op, e2) -> (match op with
                           | Plus -> (interpExp e1 venv fenv) + (interpExp e2 venv fenv)
                           | Minus -> (interpExp e1 venv fenv) - (interpExp e2 venv fenv)
                           | Times -> (interpExp e1 venv fenv) * (interpExp e2 venv fenv)
                           | Div -> (interpExp e1 venv fenv) / (interpExp e2 venv fenv)
                           | Equals -> (let a = interpExp e1 venv fenv in
                                         let b = interpExp e2 venv fenv in
                                          if a = b then 1 else 0))
  | LetExp (id,value,body) -> interpExp body (Env.add id (interpExp value venv fenv) venv) fenv
  | IfExp (c,e1,e2) -> (match (interpExp c venv fenv) with 
                        | 1 -> interpExp e1 venv fenv 
                        | 0 -> interpExp e2 venv fenv)
  | PrintExp e -> let x = (interpExp e venv fenv) in print_int x; print_newline (); x
  | FunExp (id,p,e,body) -> interpExp body venv (Env.add id (Function(e, p)) fenv)
  | CallExp (func,binding) -> (match (Fenv.find func fenv) with
                        | Function (f,p) -> interpExp f (Env.add p (interpExp binding venv fenv) venv) fenv)
  | SeqExp (e1,e2) -> interpExp e1 venv fenv; interpExp e2 venv fenv 
