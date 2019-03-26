(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 * We write
 *
 * BINOP  operand1 <- operand2,operand3
 * MOV    operand1 <- operand2
 *
 *)

type reg = EAX | T0 | T1 | T3 | T4 | T5 | T6 | T7 | T9 | T10 | T11 | T12 | STACK

type if_in_stack = T | F


type operand =
  | IMM of Int32.t
  | REG of reg
  | TEMP of Temp.t

type operation = ADD | SUB| MUL | MOD | DIV

type instr =
  | BINOP of operation * operand * operand * operand
  | MOV of operand * operand
  | DIRECTIVE of string
  | COMMENT of string


(* functions that format assembly output *)

let format_reg = function
  | EAX -> "%eax"
  | T0 -> "%ebx"
  | T1 -> "%ecx"
  | T3 -> "%esi"
  | T4 -> "%edi"
  | T5 -> "%r8d"
  | T6 -> "%r9d"
  | T7 -> "%r10d"
  | T9 -> "%r12d"
  | T10 -> "%r13d"
  | T11 -> "%r14d"
  | T12 -> "%r15d"
  | STACK -> "STACK"

let format_binop = function
  | ADD -> "addl"
  | SUB -> "subl"
  | DIV -> "idivl"
  | MUL -> "imull"
  | _ -> "none"



let find_reg = function 
  | "0" -> T0
  | "1" -> T1
  | "2" -> T3
  | "3" -> T4
  | "4" -> T5
  | "5" -> T6
  | "6" -> T7
  | "7" -> T9
  | "8" -> T10
  | "9" -> T11
  | "10" -> T12
  | _ -> STACK

let format_operand = function
  | IMM n  -> "$" ^ Int32.to_string n
  | TEMP t -> 
    (match find_reg (Temp.name t) with
    | STACK -> 
      (string_of_int (
        ((int_of_string (Temp.name t)) - 10) * (-16)
          )) ^ "(%rbp)"
    | _ -> format_reg (find_reg (Temp.name t))
    )

  (* format_reg (find_reg (Temp.name t))  *)
  | REG r  -> format_reg r

let test_if_in_stack = function
  | IMM n -> F
  | TEMP t ->
    (match find_reg (Temp.name t) with
      | STACK -> T
      | _ -> F
  )
  | REG r -> F



(* 
let temp_reg_transformation t = function
  let tNum = Temp.name t
  in let _, regnum = List.nth colors tNum in
  find_reg regnum
in let tReg = find_reg tNum
in format_reg tReg *)


(* 

let format_temp t =
  let _, regnum = List.nth colors t
in format_reg regnum
 *)

let rec format = function
      
      
      
     (* "\t" ^ format_operand d
      ^ " <-- " ^ format_operand s1
      ^ " " ^ format_binop oper
      ^ " " ^ format_operand s2 ^ "\n"*)
  | MOV (d, s) ->
    (match (test_if_in_stack d, test_if_in_stack s) with
      | T, T -> "\tmovl "^ format_operand s ^ ", " ^ "%r11d"  ^ "\n"
                        ^ "\tmovl " ^ "%r11d" ^ ", " ^ format_operand d ^ "\n"
(*       | _, STACK -> "\t dest is stack\n"
      | STACK, _ -> "\t source is stack\n" *)
      | _, _ -> "\t" ^ "movl " ^ format_operand s
                ^ ", " ^ format_operand d ^ "\n")



  | DIRECTIVE str ->
      "\t" ^ str ^ "\n"
  | COMMENT str ->
      "\t" ^ "/* " ^ str ^ "*/\n"
  | BINOP (oper, d, s1, s2) ->
    match oper with
      | DIV -> 
        let i1 = MOV (REG EAX, s1) in
        let i2 = MOV (d, REG EAX) in
        (format i1)
        ^
        "\t" ^ "cltd\n" ^
        "\t" ^ "idivl " ^ format_operand s2 ^ "\n" ^
        format i2
      | MOD -> 
        let j1 = MOV (REG EAX, s1) in
        (* let j2 = MOV (d, REG EAX) in *)
        format j1 ^
        "\t" ^ "cltd\n" ^
        "\t" ^ "idivl " ^ format_operand s2 ^ "\n" ^
        "\tmovl %edx, " ^ format_operand d ^ "\n"
      | _ -> 
        (* ( match (format_operand s1, format_operand s2) with
          | ("STACK","STACK") ->

          | (_, "STACK") ->
          | ("STACK", _) ->
          | (_, _) -> *)

          ( match test_if_in_stack s1 with
            | T -> let k1 = MOV (d, s1) in
                    "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
                    "\t" ^ format_binop oper
                    ^ " " ^ format_operand s2
                    ^ ", %r11d\n"
                    ^ "\tmovl %r11d, " ^ format_operand d ^ "\n"
            | F ->let k2 = MOV (d, s1) in
                  "\t" ^ format_binop oper
                  ^ " " ^ format_operand s2
                  ^ ", " ^ format_operand s1 ^ "\n"
                  ^ format k2
          )
(*         ^ "\t" ^ "movl " ^ format_operand s1
        ^ ", " ^ format_operand d ^ "\n" *)
