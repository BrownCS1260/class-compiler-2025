open S_exp
open Asm
open Ast
open Util

exception BadExpression of expr

let num_shift = 2

let num_mask = 0b11

let num_tag = 0b00

let bool_shift = 7

let bool_mask = 0b1111111

let bool_tag = 0b0011111

let heap_mask = 0b111

let pair_tag = 0b010

let operand_of_bool (b : bool) : operand =
  Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let operand_of_num (x : int) : operand =
  Imm ((x lsl num_shift) lor num_tag)

let zf_to_bool : directive list =
  [ Mov (Reg Rax, Imm 0)
  ; Setz (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag) ]

let lf_to_bool : directive list =
  [ Mov (Reg Rax, Imm 0)
  ; Setl (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag) ]

(* overwrites R8   *)
let ensure_num (op : operand) : directive list =
  [ Mov (Reg R8, op)
  ; And (Reg R8, Imm num_mask)
  ; Cmp (Reg R8, Imm num_tag)
  ; Jnz "error" ]

(* overwrites R8   *)
let ensure_bool (op : operand) : directive list =
  [ Mov (Reg R8, op)
  ; And (Reg R8, Imm bool_mask)
  ; Cmp (Reg R8, Imm bool_tag)
  ; Jnz "error" ]

let ensure_pair (op : operand) : directive list =
  [ Mov (Reg R8, op)
  ; And (Reg R8, Imm heap_mask)
  ; Cmp (Reg R8, Imm pair_tag)
  ; Jnz "error" ]

let stack_address (stack_index : int) =
  MemOffset (Reg Rsp, Imm stack_index)

(* stack_index: the smallest negative amount to add to rsp to find an empty location on the stack *)
let rec compile_exp (tab : int symtab) (stack_index : int) (exp : expr)
    : directive list =
  match exp with
  | Bool b ->
      [Mov (Reg Rax, operand_of_bool b)]
  | Num n ->
      [Mov (Reg Rax, operand_of_num n)]
  | Prim1 (Add1, arg) ->
      compile_exp tab stack_index arg
      @ ensure_num (Reg Rax)
      @ [Add (Reg Rax, operand_of_num 1)]
  | Prim1 (Sub1, arg) ->
      compile_exp tab stack_index arg
      @ ensure_num (Reg Rax)
      @ [Sub (Reg Rax, operand_of_num 1)]
  | Prim1 (Not, arg) ->
      compile_exp tab stack_index arg
      @ [Cmp (Reg Rax, operand_of_bool false)]
      @ zf_to_bool
  | Prim1 (ZeroP, arg) ->
      compile_exp tab stack_index arg
      @ [Cmp (Reg Rax, operand_of_num 0)]
      @ zf_to_bool
  | Prim1 (NumP, arg) ->
      compile_exp tab stack_index arg
      @ [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)]
      @ zf_to_bool
  | Prim1 (Left, arg) ->
      compile_exp tab stack_index arg
      @ ensure_pair (Reg Rax)
      @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag)))]
  | Prim1 (Right, arg) ->
      compile_exp tab stack_index arg
      @ ensure_pair (Reg Rax)
      @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8)))]
  | Prim2 (Plus, e1, e2) ->
      compile_exp tab stack_index e1
      @ ensure_num (Reg Rax)
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ ensure_num (Reg Rax)
      @ [Add (Reg Rax, stack_address stack_index)]
  | Prim2 (Minus, e1, e2) ->
      compile_exp tab stack_index e1
      @ ensure_num (Reg Rax)
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, Reg Rax)
        ; Mov (Reg Rax, stack_address stack_index) ]
      @ [Sub (Reg Rax, Reg R8)]
  | Prim2 (Eq, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, stack_address stack_index)
        ; Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | Prim2 (Lt, e1, e2) ->
      compile_exp tab stack_index e1
      @ ensure_num (Reg Rax)
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ ensure_num (Reg Rax)
      @ [ Mov (Reg R8, stack_address stack_index)
        ; Cmp (Reg R8, Reg Rax) ]
      @ lf_to_bool
  | If (test_exp, then_exp, else_exp) ->
      let else_label = Util.gensym "else" in
      let continue_label = Util.gensym "continue" in
      compile_exp tab stack_index test_exp
      @ [Cmp (Reg Rax, operand_of_bool false); Jz else_label]
      @ compile_exp tab stack_index then_exp
      @ [Jmp continue_label] @ [Label else_label]
      @ compile_exp tab stack_index else_exp
      @ [Label continue_label]
  | Var s when Symtab.mem s tab ->
      [Mov (Reg Rax, stack_address (Symtab.find s tab))]
  | Var _ ->
      raise (BadExpression exp)
  | Let (s, e, body) ->
      compile_exp tab stack_index e
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp
          (Symtab.add s stack_index tab)
          (stack_index - 8) body
  | Pair (e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, stack_address stack_index)
        ; Mov (MemOffset (Reg Rdi, Imm 0), Reg R8)
        ; Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax)
        ; Mov (Reg Rax, Reg Rdi)
        ; Or (Reg Rax, Imm pair_tag)
        ; Add (Reg Rdi, Imm 16) ]

let compile (program : expr) : string =
  [Global "entry"; Extern "error"; Label "entry"]
  @ compile_exp Symtab.empty (-8) program
  @ [Ret]
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : expr) : unit =
  let file = open_out "program.s" in
  output_string file (compile program) ;
  close_out file

let compile_and_run (program : string) : string =
  let out = parse program |> expr_of_s_exp in
  if has_free_vars out then raise (BadExpression out) ;
  compile_to_file out ;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
  ignore
    (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
  let inp = Unix.open_process_in "./program" in
  let r = input_line inp in
  close_in inp ; r
