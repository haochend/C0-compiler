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

type reg = EAX | T0 | T3 | T4 | T5 | T6 | T7 | T9 | T10 | T11 | T12 | STACK

type if_in_stack = T | F

(* type check = GREATER | GREATEROREQ | LESS | LESSOREQ | EQUAL | NOTEQE *)

type operand =
  | IMM of Int32.t
  | BOOL of bool
  | REG of reg
  | TEMP of Temp.t

type label =
  | LABEL of Label.t
(* type binop = ADD | SUB| MUL | MOD | DIV *)

type binop = ADD | SUB | MUL | DIV | MOD | LES | LEQ | GRE | GEQ | IEQ | NEQ | LAND | LOR | BAND | BOR | BXOR | SLEFT | SRIGHT

(* type asnop = EQL | PEQ | MINEQ | MULEQ | DIVEQ | MODEQ | ANDEQ | XOREQ | OREQ | RSHEQ | LSHEQ *)

type unop = LNOT | BNOT | NEGA

(* type postop = PLUSPLUS | MINUSMINUS *)

type instr =
  | BINOP of binop * operand * operand * operand
  (* | IF of label * label * label *)
  | UNOP of unop * operand * operand
  (* | QUESTION of operand * operand *)
  | CP of operand * label * label
  | GOTO of label
  | WRITELABEL of label
  | MOV of operand * operand
  | DIRECTIVE of string
  | COMMENT of string
  | RETURN


(* functions that format assembly output *)

let format_reg = function
  | EAX -> "%eax"
  | T0 -> "%ebx"
  (* | T1 -> "%ecx" *)
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

(* LES | LEQ | GRE
   | GEQ | IEQ | NEQ |
   *)


let format_binop = function
  | ADD -> "addl"
  | SUB -> "subl"
  | DIV -> "idivl"
  | MUL -> "imull"
  | LAND -> "andl"
  | LOR -> "orl"
  | BAND -> "andl"
  | BOR -> "orl"
  | BXOR -> "xorl"
  | SLEFT -> "sall"
  | SRIGHT -> "sarl"
  | _ -> "none"



let find_reg = function
  | "0" -> T0
  (* | "1" -> T1 *)
  | "1" -> T3
  | "2" -> T4
  | "3" -> T5
  | "4" -> T6
  | "5" -> T7
  | "6" -> T9
  | "7" -> T10
  | "8" -> T11
  | "9" -> T12
  | _ -> STACK

let format_label = function
  | LABEL l -> "label" ^ Label.name l

let format_operand = function
  | IMM n  -> "$" ^ Int32.to_string n
  | TEMP t ->
    (match find_reg (Temp.name t) with
    | STACK ->
      (string_of_int (
        ((int_of_string (Temp.name t)) - 9) * (-16)
          )) ^ "(%rbp)"
    | _ -> format_reg (find_reg (Temp.name t))
    )
  | BOOL b ->
    begin
      match string_of_bool b with
      | "true" -> "$1"
      | "false" ->"$0"
    end

  (* format_reg (find_reg (Temp.name t))  *)
  | REG r  -> format_reg r

let test_if_in_stack = function
  | IMM n -> F
  | BOOL b -> F
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
  | RETURN ->
    "\tret\n"
  | WRITELABEL l ->
    format_label l ^ ":\n"
  | GOTO l ->
    "\t" ^ "jmp " ^ format_label l ^ "\n"
  | UNOP (oper, d, s) ->
    begin
      match oper with
      | BNOT ->
        begin
          match test_if_in_stack s with
          | T ->
            "\tmovl " ^ format_operand s ^ ", " ^ "%r11d"  ^ "\n" ^
            "\t" ^ "not %r11d\n"
            ^ "\tmovl %r11d, " ^ format_operand d ^ "\n"
          | F ->let k2 = MOV (d, s) in
            "\t" ^ "not "
            ^ " " ^ format_operand s ^ "\n"
            ^ format k2
        end
      | LNOT ->
        begin
          match test_if_in_stack s with
          | T ->
            "\tmovl " ^ format_operand s ^ ", " ^ "%r11d"  ^ "\n" ^
            "\t" ^ "xorl $1, %r11d\n"
            ^ "\tmovl %r11d, " ^ format_operand d ^ "\n"
          | F ->let k2 = MOV (d, s) in
            "\t" ^ "xorl $1," ^ format_operand s ^ "\n"
            ^ format k2
        end
      (* match oper with
      | NEGA ->
        let u1 = BINOP (SUB ,d, Int32.zero, s) in
        format u1
      | _ -> *)
      | _ -> assert false
        (* (match test_if_in_stack s with
          | T ->
            "\tmovl " ^ format_operand s ^ ", " ^ "%r11d"  ^ "\n" ^
            "\t" ^ "not %r11d\n"
            ^ "\tmovl %r11d, " ^ format_operand d ^ "\n"
          | F ->let k2 = MOV (d, s) in
            "\t" ^ "not "
            ^ " " ^ format_operand s ^ "\n"
            ^ format k2
        ) *)
    end
  | CP (s, l1, l2) ->
    (
      match test_if_in_stack s with
      | T ->
        "\tmovl "^ format_operand s ^ ", " ^ "%r11d"  ^ "\n" ^
        "\t" ^ "cmpl $0, %r11d\n"
        ^ "\tjne " ^ format_label l1^ "\n" ^
        "jmp " ^ format_label l2 ^ "\n"
      | F ->
        "\t" ^ "cmpl "
        ^ "$0, " ^ format_operand s ^ "\n"
        ^ "\tjne " ^ format_label l1^ "\n" ^
        "jmp " ^ format_label l2 ^ "\n"
    )



  (* LES | LEQ | GRE
     | GEQ | IEQ | NEQ |
  *)
  | DIRECTIVE str ->
      "\t" ^ str ^ "\n"
  | COMMENT str ->
      "\t" ^ "/* " ^ str ^ "*/\n"
  | BINOP (oper, d, s1, s2) ->
    let lpass = LABEL (Label.create())
    and lerror = LABEL (Label.create())
    and l3 = LABEL (Label.create())
    (* and l4 = LABEL (Label.create()) *)
    in
    match oper with
    | SLEFT ->
      begin
      match test_if_in_stack s1 with
      | T ->
      "\tmovl " ^ format_operand s2 ^ ", %ecx\n" ^
      "\tcmpl " ^ "$32,%ecx\n" ^
      "\tjl " ^ format_label lpass ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label lerror ^ ":\n" ^
      "\tmovl $1, %eax\n" ^
      "\tmovl $0, %ecx\n" ^
      "\tcltd\n" ^
      "\tidivl %ecx\n" ^
      "\tmovl %eax, %eax\n" ^
      "\tret\n" ^
      format_label lpass ^ ":\n" ^
      "\tcmpl " ^ "$0, %ecx\n" ^
      "\tjge " ^ format_label l3 ^ "\n" ^
        "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label l3 ^ ":\n" ^
      "\tsall %cl, " ^ format_operand s1 ^ "\n" ^
      "\tmovl " ^ format_operand s1 ^ ", %r11d\n" ^
      "\tmovl "  ^ "%r11d, " ^ format_operand d ^ "\n"
      | F ->
      "\tmovl " ^ format_operand s2 ^ ", %ecx\n" ^
      "\tcmpl " ^ "$32,%ecx\n" ^
      "\tjl " ^ format_label lpass ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label lerror ^ ":\n" ^
      "\tmovl $1, %eax\n" ^
      "\tmovl $0, %ecx\n" ^
      "\tcltd\n" ^
      "\tidivl %ecx\n" ^
      "\tmovl %eax, %eax\n" ^
      "\tret\n" ^
      format_label lpass ^ ":\n" ^
      "\tcmpl " ^ "$0, %ecx\n" ^
      "\tjge " ^ format_label l3 ^ "\n" ^
        "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label l3 ^ ":\n" ^
      "\tsall %cl, " ^ format_operand s1 ^ "\n" ^
      "\tmovl " ^ format_operand s1 ^ ", " ^ format_operand d ^ "\n"
    end
    | SRIGHT ->
      begin
    match test_if_in_stack s1 with
    | T ->
    "\tmovl " ^ format_operand s2 ^ ", %ecx\n" ^
    "\tcmpl " ^ "$32,%ecx\n" ^
    "\tjl " ^ format_label lpass ^ "\n" ^
    "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label lerror ^ ":\n" ^
    "\tmovl $1, %eax\n" ^
    "\tmovl $0, %ecx\n" ^
    "\tcltd\n" ^
    "\tidivl %ecx\n" ^
    "\tmovl %eax, %eax\n" ^
    "\tret\n" ^
    format_label lpass ^ ":\n" ^
    "\tcmpl " ^ "$0, %ecx\n" ^
    "\tjge " ^ format_label l3 ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label l3 ^ ":\n" ^
    "\tsarl %cl, " ^ format_operand s1 ^ "\n" ^
    "\tmovl " ^ format_operand s1 ^ ", %r11d\n" ^
    "\tmovl "  ^ "%r11d, " ^ format_operand d ^ "\n"
    | F ->
    "\tmovl " ^ format_operand s2 ^ ", %ecx\n" ^
    "\tcmpl " ^ "$32,%ecx\n" ^
    "\tjl " ^ format_label lpass ^ "\n" ^
    "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label lerror ^ ":\n" ^
    "\tmovl $1, %eax\n" ^
    "\tmovl $0, %ecx\n" ^
    "\tcltd\n" ^
    "\tidivl %ecx\n" ^
    "\tmovl %eax, %eax\n" ^
    "\tret\n" ^
    format_label lpass ^ ":\n" ^
    "\tcmpl " ^ "$0, %ecx\n" ^
    "\tjge " ^ format_label l3 ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label l3 ^ ":\n" ^
    "\tsarl %cl, " ^ format_operand s1 ^ "\n" ^
    "\tmovl " ^ format_operand s1 ^ ", " ^ format_operand d ^ "\n"
  end
    | LES ->
      let l1 = LABEL (Label.create())
      and l2 = LABEL (Label.create())
      and l3 = LABEL (Label.create())in
      begin
        match test_if_in_stack s1 with
        | T -> "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
        "\t" ^ "cmpl " ^ format_operand s2 ^ ", %r11d" ^ "\n" ^
        "\t" ^ "jl " ^ format_label l1 ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
        format_label l1 ^ ":\n" ^
        "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l2 ^ ":\n" ^
        "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l3 ^ ":\n"
        | F -> "\t" ^ "cmpl " ^ format_operand s2 ^ ", " ^
        format_operand s1 ^ "\n" ^
        "\t" ^ "jl " ^ format_label l1 ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
        format_label l1 ^ ":\n" ^
        "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l2 ^ ":\n" ^
        "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l3 ^ ":\n"
      end
    | LEQ ->
      let l1 = LABEL (Label.create())
      and l2 = LABEL (Label.create())
      and l3 = LABEL (Label.create())in
      begin
        match test_if_in_stack s1 with
        | T -> "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
               "\t" ^ "cmpl " ^ format_operand s2 ^ ", %r11d" ^ "\n" ^
               "\t" ^ "jle " ^ format_label l1 ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
               format_label l1 ^ ":\n" ^
               "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l2 ^ ":\n" ^
               "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l3 ^ ":\n"
        | F -> "\t" ^ "cmpl " ^ format_operand s2 ^ ", " ^
               format_operand s1 ^ "\n" ^
               "\t" ^ "jle " ^ format_label l1 ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
               format_label l1 ^ ":\n" ^
               "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l2 ^ ":\n" ^
               "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l3 ^ ":\n"
      end

      | GRE ->
        let l1 = LABEL (Label.create())
        and l2 = LABEL (Label.create())
        and l3 = LABEL (Label.create())in
        begin
          match test_if_in_stack s1 with
          | T -> "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
                 "\t" ^ "cmpl " ^ format_operand s2 ^ ", %r11d" ^ "\n" ^
                 "\t" ^ "jg " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
          | F -> "\t" ^ "cmpl " ^ format_operand s2 ^ ", " ^
                 format_operand s1 ^ "\n" ^
                 "\t" ^ "jg " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
        end

      | GEQ ->
        let l1 = LABEL (Label.create())
        and l2 = LABEL (Label.create())
        and l3 = LABEL (Label.create())in
        begin
          match test_if_in_stack s1 with
          | T -> "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
                 "\t" ^ "cmpl " ^ format_operand s2 ^ ", %r11d" ^ "\n" ^
                 "\t" ^ "jge " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
          | F -> "\t" ^ "cmpl " ^ format_operand s2 ^ ", " ^
                 format_operand s1 ^ "\n" ^
                 "\t" ^ "jge " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
        end
        | IEQ ->
          let l1 = LABEL (Label.create())
          and l2 = LABEL (Label.create())
          and l3 = LABEL (Label.create())in
          begin
            match test_if_in_stack s1 with
            | T -> "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
                   "\t" ^ "cmpl " ^ format_operand s2 ^ ", %r11d" ^ "\n" ^
                   "\t" ^ "je " ^ format_label l1 ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                   format_label l1 ^ ":\n" ^
                   "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l2 ^ ":\n" ^
                   "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l3 ^ ":\n"
            | F -> "\t" ^ "cmpl " ^ format_operand s2 ^ ", " ^
                   format_operand s1 ^ "\n" ^
                   "\t" ^ "je " ^ format_label l1 ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                   format_label l1 ^ ":\n" ^
                   "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l2 ^ ":\n" ^
                   "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l3 ^ ":\n"
          end
          | NEQ ->
            let l1 = LABEL (Label.create())
            and l2 = LABEL (Label.create())
            and l3 = LABEL (Label.create())in
            begin
              match test_if_in_stack s1 with
              | T -> "\tmovl "^ format_operand s1 ^ ", " ^ "%r11d"  ^ "\n" ^
                     "\t" ^ "cmpl " ^ format_operand s2 ^ ", %r11d" ^ "\n" ^
                     "\t" ^ "jne " ^ format_label l1 ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                     format_label l1 ^ ":\n" ^
                     "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l2 ^ ":\n" ^
                     "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l3 ^ ":\n"
              | F -> "\t" ^ "cmpl " ^ format_operand s2 ^ ", " ^
                     format_operand s1 ^ "\n" ^
                     "\t" ^ "jne " ^ format_label l1 ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                     format_label l1 ^ ":\n" ^
                     "\t" ^ "movl " ^ "$1, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l2 ^ ":\n" ^
                     "\t" ^ "movl " ^ "$0, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l3 ^ ":\n"
            end

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
            | T ->
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
