%{
  module I = Interpreter
%}

%token <int> NUM
%token <string> ID
%token PRINT
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token OPAREN
%token CPAREN
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token EQUALS
%token EOF

(* 'Interpreter' can't be abbreviated as our module definition above will *)
(* not show up in the generated mli file. *)
%start <Interpreter.exp list option> prog

%type <I.exp> expr
%type <I.exp list> exps

%%

prog:
  | e = exps; EOF { Some e }
  | EOF { None };

exps:
  | { [] }
  | e = expr; es = exps { e::es }

expr:
  | n = NUM
    { I.NumExp n }
  | i = ID
    { I.IdExp i }
  | PRINT; e = expr
    { I.PrintExp e }
  | LET; i = ID; EQUALS; e1 = expr; IN; e2 = expr;
    { I.LetExp (i, e1, e2) }
  | OPAREN; IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr; CPAREN
    { I.IfExp (c,e1,e2) }
  | OPAREN; e1 = expr; PLUS; e2 = expr; CPAREN
    { I.OpExp (e1, I.Plus, e2) }
  | OPAREN; e1 = expr; MINUS; e2 = expr; CPAREN
    { I.OpExp (e1, I.Minus, e2) }
  | OPAREN; e1 = expr; TIMES; e2 = expr; CPAREN
    { I.OpExp (e1, I.Times, e2) }
  | OPAREN; e1 = expr; DIV; e2 = expr; CPAREN
    { I.OpExp (e1, I.Div, e2) }; 
