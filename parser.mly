%{
  module I = Interpreter
%}

%token <int> NUM
%token <string> ID
%token PRINT
%token CALL
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token OPAREN
%token CPAREN
%token OBRACK
%token CBRACK
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token EQUALS
%token SEMICOLON
%token STOP
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
  | e1 = expr; SEMICOLON e2 = expr
    { I.SeqExp (e1,e2) }
  | n = NUM
    { I.NumExp n }
  | i = ID
    { I.IdExp i }
  | PRINT; e = expr
    { I.PrintExp e }
  | LET; i = ID; EQUALS; e1 = expr; IN; e2 = expr;
    { I.LetExp (i, e1, e2) }
  | LET; i = ID; EQUALS; p = ID; STOP; OBRACK; e1 = expr; CBRACK; IN; e2 = expr;
    { I.FunExp (i, p, e1, e2) }
  | CALL; i = ID; b = expr;
    { I.CallExp (i, b) }
  | OPAREN; IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr; CPAREN
    { I.IfExp (c,e1,e2) }
  | OPAREN; e1 = expr; PLUS; e2 = expr; CPAREN
    { I.OpExp (e1, I.Plus, e2) }
  | OPAREN; e1 = expr; EQUALS; e2 = expr; CPAREN
    { I.OpExp (e1, I.Equals, e2) }
  | OPAREN; e1 = expr; MINUS; e2 = expr; CPAREN
    { I.OpExp (e1, I.Minus, e2) }
  | OPAREN; e1 = expr; TIMES; e2 = expr; CPAREN
    { I.OpExp (e1, I.Times, e2) }
  | OPAREN; e1 = expr; DIV; e2 = expr; CPAREN
    { I.OpExp (e1, I.Div, e2) }; 
