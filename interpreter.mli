type id = string

type binop = Plus | Minus | Times | Div

and exp =
    IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | LetExp of id * exp * exp 
  | IfExp of exp * exp * exp 
  | PrintExp of exp

module Env : Map.S with type key = id

val empty : int Env.t

val interpExps : exp list -> int Env.t -> int
