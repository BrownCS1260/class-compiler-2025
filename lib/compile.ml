open S_exp
open Asm

exception BadExpression of s_exp

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

(* stack_index: the smallest negative amount to add to rsp to find an empty location on the stack *)
let rec compile_exp (program : s_exp) (stack_index : int) :
    directive list =
  match program with
  | Num n ->
      [Mov (Reg Rax, Imm (n lsl num_shift))]
  | Sym "true" ->
      [Mov (Reg Rax, Imm ((1 lsl bool_shift) lor bool_tag))]
  | Sym "false" ->
      [Mov (Reg Rax, Imm ((0 lsl bool_shift) lor bool_tag))]
  | Lst [Sym "not"; arg] ->
      compile_exp arg stack_index
      @ [Cmp (Reg Rax, operand_of_bool false)]
      @ zf_to_bool
  | Lst [Sym "zero?"; arg] ->
      compile_exp arg stack_index
      @ [Cmp (Reg Rax, operand_of_num 0)]
      @ zf_to_bool
  | Lst [Sym "num?"; arg] ->
      compile_exp arg stack_index
      @ [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)]
      @ zf_to_bool
  | Lst [Sym "add1"; exp] ->
      compile_exp exp stack_index
      @ [Add (Reg Rax, Imm (1 lsl num_shift))]
  | Lst [Sym "sub1"; exp] ->
      compile_exp exp stack_index
      @ [Sub (Reg Rax, Imm (1 lsl num_shift))]
  | Lst [Sym "if"; test_exp; then_exp; else_exp] ->
      let else_label = Util.gensym "else" in
      let continue_label = Util.gensym "continue" in
      compile_exp test_exp stack_index
      @ [Cmp (Reg Rax, operand_of_bool false); Jz else_label]
      @ compile_exp then_exp stack_index
      @ [Jmp continue_label] @ [Label else_label]
      @ compile_exp else_exp stack_index
      @ [Label continue_label]
  | Lst [Sym "+"; e1; e2] ->
      compile_exp e1 stack_index
      @ [Mov (MemOffset (Reg Rsp, Imm stack_index), Reg Rax)]
      @ compile_exp e2 (stack_index - 8)
      @ [Mov (Reg R8, MemOffset (Reg Rsp, Imm stack_index))]
      @ [Add (Reg Rax, Reg R8)]
  | _ ->
      raise (BadExpression program)

let compile (program : s_exp) : string =
  [Global "entry"; Label "entry"] @ compile_exp program (-8) @ [Ret]
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : s_exp) : unit =
  let file = open_out "program.s" in
  output_string file (compile program) ;
  close_out file

let compile_and_run (program : string) : string =
  compile_to_file (parse program) ;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
  ignore
    (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
  let inp = Unix.open_process_in "./program" in
  let r = input_line inp in
  close_in inp ; r
