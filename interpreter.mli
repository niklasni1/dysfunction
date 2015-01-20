type id = string

module Env : Map.S with type key = id

type binop = Plus | Minus | Times | Div | Equals

and exp =
    IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | LetExp of id * exp * exp 
  | IfExp of exp * exp * exp 
  | PrintExp of exp
  | FunExp of id * exp
  | CallExp of id * exp
  | SeqExp of exp * exp

and value = 
  Integer of int 
  | Boolean of bool
  | Function of exp * id * value Env.t

val empty : value Env.t

val interpExps : exp list -> value Env.t -> value
