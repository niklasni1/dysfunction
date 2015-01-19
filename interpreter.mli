type id = string

type binop = Plus | Minus | Times | Div | Equals

and exp =
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

module Env : Map.S with type key = id

val empty : int Env.t
val fempty : func Env.t

val interpExps : exp list -> int Env.t -> func Env.t -> int
