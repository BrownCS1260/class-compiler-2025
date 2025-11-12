type token = PLUS | TIMES | LPAREN | RPAREN | NUM of int

type expr = Num of int | Plus of expr * expr | Times of expr * expr

exception ParseError

let rec parse_expr (toks : token list) =
  let t, toks = parse_term toks in
  match toks with
  | PLUS :: toks ->
      let e, toks = parse_expr toks in
      (Plus (t, e), toks)
  | _ ->
      (t, toks)

and parse_term (toks : token list) =
  let f, toks = parse_factor toks in
  match toks with
  | TIMES :: toks ->
      let e, toks = parse_term toks in
      (Times (f, e), toks)
  | _ ->
      (f, toks)

and parse_factor (toks : token list) =
  match toks with
  | NUM n :: toks ->
      (Num n, toks)
  | LPAREN :: toks -> (
      let e, toks = parse_expr toks in
      match toks with
      | RPAREN :: toks ->
          (e, toks)
      | _ ->
          raise ParseError )
  | _ ->
      raise ParseError
