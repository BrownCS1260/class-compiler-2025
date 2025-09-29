open S_exp

type prim1 = Add1 | Sub1 | ZeroP | NumP | Not

let prim1_of_string (s : string) : prim1 option =
  match s with
  | "add1" ->
      Some Add1
  | "sub1" ->
      Some Sub1
  | "zero?" ->
      Some ZeroP
  | "num?" ->
      Some NumP
  | "not" ->
      Some Not
  | _ ->
      None

type prim2 = Plus | Minus | Eq | Lt

let prim2_of_string = function
  | "+" ->
      Some Plus
  | "-" ->
      Some Minus
  | "=" ->
      Some Eq
  | "<" ->
      Some Lt
  | _ ->
      None

exception BadSExpression of s_exp

type expr =
  | Num of int
  | Bool of bool
  | Var of string
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

let rec expr_of_s_exp (e : s_exp) : expr =
  match e with
  | Num n ->
      Num n
  | Sym "true" ->
      Bool true
  | Sym "false" ->
      Bool false
  | Sym var ->
      Var var
  | Lst [Sym f; e1] -> (
    match prim1_of_string f with
    | Some p1 ->
        Prim1 (p1, expr_of_s_exp e1)
    | None ->
        raise (BadSExpression e) )
  | Lst [Sym f; e1; e2] when Option.is_some (prim2_of_string f) ->
      Prim2
        ( Option.get (prim2_of_string f)
        , expr_of_s_exp e1
        , expr_of_s_exp e2 )
  | Lst [Sym "if"; e1; e2; e3] ->
      If (expr_of_s_exp e1, expr_of_s_exp e2, expr_of_s_exp e3)
  | Lst [Sym "let"; Lst [Lst [Sym s; e]]; body] ->
      Let (s, expr_of_s_exp e, expr_of_s_exp body)
  | _ ->
      raise (BadSExpression e)
