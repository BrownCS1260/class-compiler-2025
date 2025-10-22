open S_exp

type prim0 = ReadNum | Newline

let prim0_of_string (s : string) : prim0 option =
  match s with
  | "read-num" ->
      Some ReadNum
  | "newline" ->
      Some Newline
  | _ ->
      None

type prim1 = Add1 | Sub1 | ZeroP | NumP | Not | Left | Right | Print

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
  | "left" ->
      Some Left
  | "right" ->
      Some Right
  | "print" ->
      Some Print
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
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Pair of expr * expr
  | Do of expr list

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
  | Lst [Sym f] when Option.is_some (prim0_of_string f) ->
      Prim0 (Option.get (prim0_of_string f))
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
  | Lst [Sym "pair"; e1; e2] ->
      Pair (expr_of_s_exp e1, expr_of_s_exp e2)
  | Lst (Sym "do" :: args) ->
      Do (List.map expr_of_s_exp args)
  | _ ->
      raise (BadSExpression e)

let rec fv (bound : string list) (exp : expr) =
  match exp with
  | Var s when not (List.mem s bound) ->
      [s]
  | Var _ ->
      []
  | Let (v, e, body) ->
      fv bound e @ fv (v :: bound) body
  | If (te, the, ee) ->
      fv bound te @ fv bound the @ fv bound ee
  | Prim0 _ ->
      []
  | Prim1 (_, e) ->
      fv bound e
  | Prim2 (_, e1, e2) ->
      fv bound e1 @ fv bound e2
  | Pair (e1, e2) ->
      fv bound e1 @ fv bound e2
  | Num _ ->
      []
  | Bool _ ->
      []
  | Do exps ->
      List.fold_left (fun l e -> fv bound e @ l) [] exps

let has_free_vars (exp : expr) : bool = fv [] exp <> []
