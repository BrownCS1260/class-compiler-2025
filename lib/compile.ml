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
      @ [Add (Reg Rax, Imm (1 lsl num_shift))]
  | Prim1 (Sub1, arg) ->
      compile_exp tab stack_index arg
      @ [Sub (Reg Rax, Imm (1 lsl num_shift))]
  | Prim1 (Not, arg) ->
      compile_exp tab stack_index arg
      @ [Cmp (Reg Rax, Imm ((0 lsl bool_shift) lor bool_tag))]
      @ zf_to_bool
  | Prim1 (ZeroP, arg) ->
      compile_exp tab stack_index arg
      @ [Cmp (Reg Rax, operand_of_num 0)]
      @ zf_to_bool
  | Prim1 (NumP, arg) ->
      compile_exp tab stack_index arg
      @ [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)]
      @ zf_to_bool
  | Prim2 (Plus, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [Add (Reg Rax, MemOffset (Reg Rsp, Imm stack_index))]
  | Prim2 (Minus, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, Reg Rax)
        ; Mov (Reg Rax, MemOffset (Reg Rsp, Imm stack_index)) ]
      @ [Sub (Reg Rax, Reg R8)]
  | Prim2 (Eq, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index))
        ; Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | Prim2 (Lt, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index))
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

let compile (program : expr) : string =
  [Global "entry"; Label "entry"]
  @ compile_exp Symtab.empty (-8) program
  @ [Ret]
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : expr) : unit =
  let file = open_out "program.s" in
  output_string file (compile program) ;
  close_out file

let compile_and_run (program : string) : string =
  parse program |> expr_of_s_exp |> compile_to_file ;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
  ignore
    (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
  let inp = Unix.open_process_in "./program" in
  let r = input_line inp in
  close_in inp ; r
