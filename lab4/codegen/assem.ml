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

type reg = EDX | RSP | EAX | ECX | EBX | ESI | EDI | R8D | R9D | R12D | R13D | R14D | R15D | STACK

type if_in_stack = T | F

(* type check = GREATER | GREATEROREQ | LESS | LESSOREQ | EQUAL | NOTEQE *)

type operand =
  | IMM of Int32.t
  | BOOL of bool
  | REG of reg
  | TEMP of Temp.t
  | ARGUEMENT of Arguement.t
  | BEGINARG of Arguement.t
  | OFFSET of Int32.t

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
  | MOVARGBEFORECALL of operand * operand
  | DIRECTIVE of string
  | COMMENT of string
  | RETURN
  | FUNCSTART of string
  | MAINSTART of string
  | INTERNALCALL of string
  | EXTERNALCALL of string * int
  | FUNCTIONABORT
  | ARGOFFSET of operand * int
  | ARGONSET of operand * int
  | STACKOFFSET of operand * int
  | STACKONSET of operand * int
  | PASS of operand * operand
  | PUSH of operand
  | POP of operand
  | PASSARG of operand * operand * operand
  | LEA of operand * int * label
  | DOTDEREF of operand * operand * label
  | MALLOC of operand * int
  | ALLOCARRY of operand * int * operand * label
  | ARRDEREF of operand * operand * operand * int * label * label * label
  | ARRDONOTHING of operand * operand * operand * int * label * label * label
  | NULL of operand
  | MOVETOADDR of operand * operand * label
  | MOVETOADDRFOURBYTE of operand * operand * label
  | GETCONTENT of operand * operand
  | GETCONTENTFOURBYTE of operand * operand
  | ARRADDRCOMP of operand * operand * operand * int * label * label * label
  | CALLRAISE
(* functions that format assembly output *)

let format_reg = function
  | EAX -> "%rax"
  | ECX -> "%rcx"
  | EDX -> "%rdx"
  | RSP -> "%rsp"
  | EBX -> "%rbx"
  | ESI -> "%rsi"
  | EDI -> "%rdi"
  | R8D -> "%r8"
  | R9D -> "%r9"
  (* | R10D -> "%r10d" *)
  | R12D -> "%r12"
  | R13D -> "%r13"
  | R14D -> "%r14"
  | R15D -> "%r15"
  | STACK -> "STACK"

  let get_short_reg = function
          | "%rax" -> "%eax"
          | "%rcx" -> "%ecx"
          | "%rdx" -> "%edx"
          | "%rbx" -> "%ebx"
          | "%rsi" -> "%esi"
          | "%rdi" -> "%edi"
          | "%r8" -> "%r8d"
          | "%r9"  -> "%r9d"
          (* | "%r10d" -> "%r10" *)
          | "%r12" -> "%r12d"
          | "%r13" -> "%r13d"
          | "%r14" -> "%r14d"
          | "%r15" -> "%r15d"
          | t -> t

let get_full_reg = function
  | "%eax" -> "%rax"
    | "%ecx" -> "%rcx"
    | "%edx" -> "%rdx"
    | "%ebx" -> "%rbx"
    | "%esi" -> "%rsi"
    | "%edi" -> "%rdi"
    | "%r8d" -> "%r8"
    | "%r9d"  -> "%r9"
    (* | "%r10d" -> "%r10" *)
    | "%r12d" -> "%r12"
    | "%r13d" -> "%r13"
    | "%r14d" -> "%r14"
    | "%r15d" -> "%r15"
        | "%rax" -> "%rax"
        | "%rcx" -> "%rcx"
        | "%rdx" -> "%rdx"
        | "%rbx" -> "%rbx"
        | "%rsi" -> "%rsi"
        | "%rdi" -> "%rdi"
        | "%r8" -> "%r8"
        | "%r9"  -> "%r9"
        (* | "%r10d" -> "%r10" *)
        | "%r12" -> "%r12"
        | "%r13" -> "%r13"
        | "%r14" -> "%r14"
        | "%r15" -> "%r15"
        | t -> t
(* LES | LEQ | GRE
   | GEQ | IEQ | NEQ |
   *)


let format_binop = function
  | ADD -> "addq"
  | SUB -> "subq"
  | DIV -> "idivq"
  | MUL -> "imulq"
  | LAND -> "andq"
  | LOR -> "orq"
  | BAND -> "andq"
  | BOR -> "orq"
  | BXOR -> "xorq"
  | SLEFT -> "salq"
  | SRIGHT -> "sarl"
  | _ -> "none"



let find_reg = function
  (*
  | "0" -> EDI
  | "1" -> ESI
  | "2" -> EDX
  | "3" -> ECX
  | "4" -> R8D
  | "5" -> R9D
  | "6" -> R10D *)
  | "0" -> EBX
  | "1" -> R12D
  | "2" -> R13D
  | "3" -> R14D
  | "4" -> R15D
  | _ -> STACK

let find_arg = function
  | "0" -> EDI
  | "1" -> ESI
  | "2" -> EDX
  | "3" -> ECX
  | "4" -> R8D
  | "5" -> R9D
  | _ -> STACK

let format_label = function
  | LABEL l -> "label" ^ Label.name l

let format_operand = function
  | IMM n  -> "$" ^ Int32.to_string n
  | TEMP t ->
    (match find_reg (Temp.name t) with
    | STACK ->
      (string_of_int (
        (-(int_of_string (Temp.name t) - 4)*8 ) - 48
          )) ^ "(%rbp)"
    | _ -> format_reg (find_reg (Temp.name t))
    )
  | ARGUEMENT t ->
    (match find_arg (Arguement.name t) with
     | STACK ->
       (string_of_int (
           ((int_of_string (Arguement.name t)) - 5) * (-8)
         )) ^ "(%rsp)"
     | _ -> format_reg (find_arg (Arguement.name t))
    )
  | BOOL b ->
    begin
      match string_of_bool b with
      | "true" -> "$1"
      | "false" ->"$0"
    end
  | OFFSET o ->
    begin
      (string_of_int (((Int32.to_int o)) * (16)))
      (* match (Int32.to_int o) with
      | 0 -> ""
      | 1 -> ""
      | 2 -> ""
      | 3 -> ""
      | 4 -> ""
      | 5 -> ""
      | _ -> (string_of_int (((Int32.to_int o) - 5) * (16))) *)
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
  | ARGUEMENT a ->
    (match find_arg (Arguement.name a) with
     | STACK -> T
     | _ -> F
    )
  | BEGINARG a ->
    (match find_arg (Arguement.name a) with
     | STACK -> T
     | _ -> F
    )
  | REG r -> F
  | OFFSET o -> F
let format_begin_arg= function
  | IMM cnt,BEGINARG a ->
  begin
    (match find_arg (Arguement.name a) with
     | STACK ->
       (string_of_int (
           ((Int32.to_int cnt)-(int_of_string (Arguement.name a))+1) * (8)
         )) ^ "(%rbp)"
     | _ -> format_reg (find_arg (Arguement.name a))
    )
  end
  | _,_ -> assert false
  (* | ARGUEMENT t ->
    (match find_arg (Arguement.name t) with
     | STACK ->
       (string_of_int (
           ((int_of_string (Arguement.name t)) - 5) * (-16)
         )) ^ "(%rsp)"
     | _ -> format_reg (find_arg (Arguement.name t))
    ) *)

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
  | GETCONTENT (s,d) ->
    begin
      match test_if_in_stack s with
      | T ->
        "\tmovq " ^ format_operand s ^ ", %r11\n" ^
        "\tmovq (%r11), %r11\n" ^
        "\tmovq %r11, " ^ get_full_reg (format_operand d) ^ "\n"
      | F ->
        "\tmovq (" ^ get_full_reg (format_operand s) ^ "), %r11\n" ^
        "\tmovq %r11, "^ get_full_reg (format_operand d) ^ "\n"
    end
  | GETCONTENTFOURBYTE (s,d) ->
    (* begin
      match test_if_in_stack s with
      | T -> *)
        "\tmovq " ^ format_operand s ^ ", %r11\n" ^
        "\tmovl (%r11), %r10d\n" ^
        "\tmovl %r10d, " ^ get_short_reg (get_full_reg (format_operand d)) ^ "\n"
      (* | F ->
        "\tmovq (" ^ get_full_reg (format_operand s) ^ "), %r11\n" ^
        "\tmovq %r11, "^ get_full_reg (format_operand d) ^ "\n" *)
    (* end *)
  | NULL d ->
    begin
      "\tmovq $0, " ^ format_operand d ^ "\n"
    end
  | LEA (t, offset, l) ->
    begin
      let call_raise = EXTERNALCALL ("raise",0) in
      match offset with
      | 0 -> ""
      | _ ->
        begin
          (* match test_if_in_stack t with
          | T -> *)
            "\tmovq %rcx, %rdi\n" ^
            "\tmovq $31, %rcx\n" ^
            "\tmovq $1, %r10\n" ^
            "\tsall %cl, %r10d\n" ^
            "\tmovq $" ^ (string_of_int offset) ^ ", " ^ "%r11"  ^ "\n" ^
              "\tcmpq %r10, %r11\n" ^
            "jl " ^ format_label l ^ "\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
              format_label l ^ ":\n" ^
              "\tmovq "^ format_operand t ^ ", " ^ "%r11"  ^ "\n"
               ^ "\tleaq " ^ string_of_int offset ^ "(%r11), %r11\n"
            ^ "\tmovq %r11, " ^ format_operand t ^ "\n" ^
            "\tmovq %rdi, %rcx\n"
          (* | F ->
            "\tleaq " ^ string_of_int offset ^ "("^format_operand t^"), " ^ format_operand t ^ "\n" *)


        end


    end
  | CALLRAISE ->
    let call_raise = EXTERNALCALL ("raise",0) in
    "\tmovq $12,%rdi\n" ^
    "\tsubq $16, %rsp\n" ^
    format call_raise ^
    "\taddq $16, %rsp\n"
  | DOTDEREF (d,s,l1) ->
    let call_raise = EXTERNALCALL ("raise",0) in
    begin
      match test_if_in_stack s with
      | T ->
        begin
          match test_if_in_stack d with
          | T ->
            "\tmovq " ^ format_operand s ^ ", %r11\n" ^
            "\tcmpl $0, %r11d\n" ^
            "\tjne " ^ format_label l1 ^ "\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
          "\tmovq (%r11), %r10\n" ^
          "\tmovq %r10, " ^ format_operand d ^ "\n"
          | F ->
          "\tmovq " ^ format_operand s ^ ", %r11\n" ^
          "\tcmpl $0, %r11d\n" ^
          "\tjne " ^ format_label l1 ^"\n" ^
          "\tmovq $12,%rdi\n" ^
          "\tsubq $16, %rsp\n" ^
          format call_raise ^
          "\taddq $16, %rsp\n" ^
          format_label l1 ^ ":\n" ^
          "\tmovq (%r11), " ^ get_full_reg (format_operand d) ^ "\n"
        end
      | F ->
        begin
          match test_if_in_stack d with
          | T ->

            "\tcmpl $0, "^ get_short_reg (get_full_reg (format_operand s)) ^"\n" ^
            "\tjne " ^ format_label l1 ^"\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tmovq (" ^ get_full_reg (format_operand s) ^ "), %r11\n" ^
            "\tmovq %r11, " ^ format_operand d ^ "\n"
          | F ->
          "\tcmpl $0, "^ get_short_reg (get_full_reg (format_operand s)) ^"\n" ^
          "\tjne " ^ format_label l1 ^"\n" ^
          "\tmovq $12,%rdi\n" ^
          "\tsubq $16, %rsp\n" ^
          format call_raise ^
          "\taddq $16, %rsp\n" ^
          format_label l1 ^ ":\n" ^
            "\tmovq (" ^ get_full_reg (format_operand s) ^ "), " ^ get_full_reg (format_operand d) ^ "\n"
        end


    end
  | MALLOC (d,siz) ->
    begin
      let external_malloc = EXTERNALCALL ("malloc",1) in
      match test_if_in_stack d with
      | T ->
        "\tpushq %rdi\n" ^
        "\tmovq $" ^ string_of_int siz ^ ", %rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        format external_malloc ^
        "\taddq $16, %rsp\n" ^
        "\tpopq %rdi\n" ^
      "\tmovq %rax, " ^ format_operand d ^ "\n"
      | F ->
        "\tpushq %rdi\n" ^
        "\tmovq $" ^ string_of_int siz ^ ", %rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        format external_malloc ^
        "\taddq $16, %rsp\n" ^
        "\tpopq %rdi\n" ^
      "\tmovq %rax, " ^ get_full_reg (format_operand d) ^ "\n"
    end
  | ALLOCARRY (d,element_size, s,l) ->
    begin
      let external_malloc = EXTERNALCALL ("malloc",1) in
      let call_raise = EXTERNALCALL ("raise",1) in
      "\tmovq " ^ format_operand s ^ ", %r11\n" ^
      "\tcmpl $0, %r11d\n" ^
      "\tjge " ^ format_label l ^ "\n" ^
      "\tmovq $12,%rdi\n" ^
      "\tsubq $16, %rsp\n" ^
      format call_raise ^
      format_label l^ ":\n" ^
      "\timulq $" ^ string_of_int (element_size) ^ ", %r11\n" ^
      "\taddq $8, %r11\n" ^
      "\tpushq %rdi\n" ^
      "\tmovq %r11, %rdi\n" ^
      "\tsubq $16, %rsp\n" ^
      format external_malloc ^
      "\taddq $16, %rsp\n" ^
      "\tpopq %rdi\n" ^
      "\tmovq " ^ format_operand s ^ ", %r11\n" ^
      "\tmovq %r11,(%rax)\n" ^
      "\taddq $8, %rax\n" ^
      "\tmovq %rax, " ^ format_operand d ^ "\n"
    end
  | ARRADDRCOMP (s,base,mult,arr_size_for_each_element,l1,l2,l3) ->
    let call_raise = EXTERNALCALL ("raise",1) in
    begin
        "\tmovq " ^ format_operand base ^ ", %r11\n" ^
        "\tmovq $0, %r10\n" ^
        "\tcmpq %r11, %r10\n" ^
        "\tjle " ^ format_label l3 ^"\n" ^
        "\tjmp " ^ format_label l2 ^"\n" ^
        format_label l3 ^ ":\n" ^
        "\tmovq -8(%r11), %r10\n" ^
        "\tmovq " ^ format_operand mult ^ ", %rdi\n" ^
        "\tcmpl %edi, %r10d\n" ^
        "\tjle " ^ format_label l2 ^ "\n" ^
        "\tjmp " ^ format_label l1 ^ "\n" ^
        format_label l2 ^ ":\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        format call_raise ^
        "\taddq $16, %rsp\n" ^
        format_label l1 ^ ":\n" ^
        "\tcmpl $0, %edi\n" ^
        "\tjl " ^ format_label l2 ^ "\n" ^
        "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
        (* "\taddq $8,%rdi\n" ^ *)
        "\taddl %edi,%r11d\n" ^
        (* "\tmovq " ^ format_operand s ^ ", %r10\n" ^ *)
        "\tmovq %r11, " ^ format_operand s ^"\n"
    end

  | ARRDONOTHING (d,s,index_exp,arr_size_for_each_element,l1,l2,l3) ->
    let call_raise = EXTERNALCALL ("raise",1) in
    begin
      match test_if_in_stack d with
      | T ->
        begin
          match test_if_in_stack s with
          | T ->
            "\tmovq " ^ format_operand s ^ ", %r11\n" ^

            "\tmovq $0, %r10\n" ^
            "\tcmpq %r11, %r10\n" ^
            "\tjle " ^ format_label l3 ^"\n" ^
            "\tjmp " ^ format_label l2 ^"\n" ^

            format_label l3 ^ ":\n" ^
            "\tmovq -8(%r11), %r10\n" ^
            "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
            "\tcmpl %edi, %r10d\n" ^
            "\tjle " ^ format_label l2 ^ "\n" ^
            "\tjmp " ^ format_label l1 ^ "\n" ^
            format_label l2 ^ ":\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tcmpl $0, %edi\n" ^
            "\tjl " ^ format_label l2 ^ "\n" ^
            (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
            "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
            (* "\taddq $8,%rdi\n" ^ *)
            "\taddl %edi,%r11d\n" ^
            "\tmovq %r11, " ^ get_full_reg (format_operand d) ^ "\n"
          | F ->
            "\tmovq " ^ get_full_reg (format_operand s) ^ ", %r11\n" ^

            "\tmovq $0, %r10\n" ^
            "\tcmpq %r11, %r10\n" ^
            "\tjle " ^ format_label l3 ^"\n" ^
            "\tjmp " ^ format_label l2 ^"\n" ^
            format_label l3 ^ ":\n" ^

            "\tmovq -8(%r11), %r10\n" ^
            "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
            "\tcmpl %edi, %r10d\n" ^
            "\tjle " ^ format_label l2 ^ "\n" ^
            "\tjmp " ^ format_label l1 ^ "\n" ^
            format_label l2 ^ ":\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tcmpl $0, %edi\n" ^
            "\tjl " ^ format_label l2 ^ "\n" ^
            (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
            "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
            (* "\taddq $8,%rdi\n" ^ *)
            "\taddl %edi,%r11d\n" ^
            "\tmovq %r11, " ^ get_full_reg (format_operand d) ^ "\n"
        end
      | F ->
        begin
          match test_if_in_stack s with
          | T ->
            "\tmovq " ^ format_operand s ^ ", %r11\n" ^

            "\tmovq $0, %r10\n" ^
            "\tcmpq %r11, %r10\n" ^
            "\tjle " ^ format_label l3 ^"\n" ^
            "\tjmp " ^ format_label l2 ^"\n" ^
            format_label l3 ^ ":\n" ^

            "\tmovq -8(%r11), %r10\n" ^
            "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
            "\tcmpl %edi, %r10d\n" ^
            "\tjle " ^ format_label l2 ^ "\n" ^
            "\tjmp " ^ format_label l1 ^ "\n" ^
            format_label l2 ^ ":\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tcmpl $0, %edi\n" ^
            "\tjl " ^ format_label l2 ^ "\n" ^
            (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
            "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
            (* "\taddq $8,%rdi\n" ^ *)
            "\taddl %edi,%r11d\n" ^
            "\tmovq %r11, " ^ get_full_reg (format_operand d) ^ "\n"
          | F ->
            "\tmovq " ^ get_full_reg (format_operand s) ^ ", %r11\n" ^

            "\tmovq $0, %r10\n" ^
            "\tcmpq %rdi, %r10\n" ^
            "\tjle " ^ format_label l3 ^"\n" ^
            "\tjmp " ^ format_label l2 ^"\n" ^
            format_label l3 ^ ":\n" ^
            "\tmovq -8(%r11), %r10\n" ^
            "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
            "\tcmpl %edi, %r10d\n" ^
            "\tjle " ^ format_label l2 ^ "\n" ^
            "\tjmp " ^ format_label l1 ^ "\n" ^
            format_label l2 ^ ":\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            format call_raise ^
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tcmpl $0, %edi\n" ^
            "\tjl " ^ format_label l2 ^ "\n" ^
            (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
            "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
            (* "\taddq $8,%rdi\n" ^ *)
            "\taddl %edi,%r11d\n" ^
            "\tmovq %r11, " ^ get_full_reg (format_operand d) ^ "\n"
        end

    end







  | ARRDEREF (d,s,index_exp,arr_size_for_each_element,l1,l2,l3) ->
    let call_raise = EXTERNALCALL ("raise",1) in
    begin
      begin
        match arr_size_for_each_element with
        | 4 ->
          begin
            match test_if_in_stack d with
            | T ->
              begin
                match test_if_in_stack s with
                | T ->
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^
                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^
                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^

                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^


                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovl (%r11), %r10d\n" ^
                  "\tmovq %r10, " ^ get_full_reg (format_operand d) ^ "\n"
                | F ->
                  "\tmovq " ^ get_full_reg (format_operand s) ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^

                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovl (%r11), %r10d\n" ^
                  "\tmovq %r10, " ^ get_full_reg (format_operand d) ^ "\n"
              end
            | F ->
              begin
                match test_if_in_stack s with
                | T ->
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^

                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovl (%r11), " ^ get_short_reg (get_full_reg (format_operand d)) ^ "\n"
                | F ->
                  "\tmovq " ^ get_full_reg (format_operand s) ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^
                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovl (%r11), " ^ get_short_reg (get_full_reg (format_operand d)) ^ "\n"
              end

          end
        | _ ->
          begin
            match test_if_in_stack d with
            | T ->
              begin
                match test_if_in_stack s with
                | T ->
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^

                  format_label l3 ^ ":\n" ^
                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovq (%r11), %r10\n" ^
                  "\tmovq %r10, " ^ get_full_reg (format_operand d) ^ "\n"
                | F ->
                  "\tmovq " ^ get_full_reg (format_operand s) ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^

                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovq (%r11), %r10\n" ^
                  "\tmovq %r10, " ^ get_full_reg (format_operand d) ^ "\n"
              end
            | F ->
              begin
                match test_if_in_stack s with
                | T ->
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^

                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovq (%r11), " ^ (get_full_reg (format_operand d)) ^ "\n"
                | F ->
                  "\tmovq " ^ get_full_reg (format_operand s) ^ ", %r11\n" ^

                  "\tmovq $0, %r10\n" ^
                  "\tcmpq %r11, %r10\n" ^
                  "\tjne " ^ format_label l3 ^"\n" ^
                  "\tjmp " ^ format_label l2 ^"\n" ^
                  format_label l3 ^ ":\n" ^
                  "\tmovq -8(%r11), %r10\n" ^
                  "\tmovq " ^ format_operand index_exp ^ ", %rdi\n" ^
                  "\tcmpl %edi, %r10d\n" ^
                  "\tjle " ^ format_label l2 ^ "\n" ^
                  "\tjmp " ^ format_label l1 ^ "\n" ^
                  format_label l2 ^ ":\n" ^
                  "\tmovq $12,%rdi\n" ^
                  "\tsubq $16, %rsp\n" ^
                  format call_raise ^
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpq $0, %rdi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\taddl %edi,%r11d\n" ^
                  "\tmovq (%r11), " ^ (get_full_reg (format_operand d)) ^ "\n"
              end

          end
      end




    end

  | MOVETOADDRFOURBYTE (d,s,l) ->
    let call_raise = EXTERNALCALL ("raise",0) in
    begin
      match test_if_in_stack s with
      | T ->
        "\tmovq " ^ format_operand d ^ ", %r11\n" ^
        "\tmovq " ^ format_operand s ^ ", %r10\n" ^

        "\tcmpl $0, %r11d\n" ^
        "\tjne " ^ format_label l ^ "\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        format call_raise ^
        "\taddq $16, %rsp\n" ^
        format_label l ^ ":\n" ^
        "\tmovl %r10d, (%r11)\n"
      | F ->
        "\tmovq " ^ format_operand d ^ ", %r11\n" ^
        "\tcmpl $0, %r11d\n" ^
        "\tjne " ^ format_label l ^ "\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        format call_raise ^
        "\taddq $16, %rsp\n" ^
        format_label l ^ ":\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg ( format_operand s)) ^ ", (%r11)\n"
    end


  | MOVETOADDR (d,s,l) ->

    let call_raise = EXTERNALCALL ("raise",0) in
        begin
          match test_if_in_stack s with
          | T ->
          "\tmovq " ^ format_operand d ^ ", %r11\n" ^
          "\tmovq " ^ format_operand s ^ ", %r10\n" ^

          "\tcmpl $0, %r11d\n" ^
          "\tjne " ^ format_label l ^ "\n" ^
          "\tmovq $12,%rdi\n" ^
          "\tsubq $16, %rsp\n" ^
          format call_raise ^
          "\taddq $16, %rsp\n" ^
          format_label l ^ ":\n" ^
            "\tmovq %r10, (%r11)\n"
          | F ->
            "\tmovq " ^ format_operand d ^ ", %r11\n" ^
                      "\tcmpl $0, %r11d\n" ^
                      "\tjne " ^ format_label l ^ "\n" ^
                      "\tmovq $12,%rdi\n" ^
                      "\tsubq $16, %rsp\n" ^
                      format call_raise ^
                      "\taddq $16, %rsp\n" ^
                      format_label l ^ ":\n" ^
          "\tmovq " ^ get_full_reg ( format_operand s) ^ ", (%r11)\n"
        end

  | MOV (d, s) ->
    (match (test_if_in_stack d, test_if_in_stack s) with
      | T, T -> "\tmovq "^ format_operand s ^ ", " ^ "%r11"  ^ "\n"
                        ^ "\tmov " ^ "%r11" ^ ", " ^ format_operand d ^ "\n"
      | _, _ -> "\t" ^ "movq " ^ format_operand s
                ^ ", " ^ format_operand d ^ "\n")

  | MOVARGBEFORECALL (d, s) ->
    (match (test_if_in_stack d, test_if_in_stack s) with
     | T, T -> "\tmovq "^ format_operand s ^ ", " ^ "%r11"  ^ "\n"
               ^ "\tmov " ^ "%r11" ^ ", " ^ format_operand d ^ "\n"
     | _, _ -> "\t" ^ "movq " ^ format_operand s
               ^ ", " ^ format_operand d ^ "\n")
  | PASS (d,a) ->
    (match (test_if_in_stack d, test_if_in_stack a) with
     | T, T -> "\tmovq "^ format_operand a ^ ", " ^ "%r11"  ^ "\n"
               ^ "\tmovq " ^ "%r11" ^ ", " ^ format_operand d ^ "\n"
     | _, _ -> "\t" ^ "movq " ^ format_operand a
               ^ ", " ^ format_operand d ^ "\n")
  | PASSARG (t,a,cnt) ->
    begin
      match (test_if_in_stack t,test_if_in_stack a) with
      | T, T ->
        begin
          "\tmovq "^ format_begin_arg (cnt,a) ^ ", " ^ "%r11"  ^ "\n"
          ^ "\tmovq " ^ "%r11" ^ ", " ^ format_operand t ^ "\n"
        end
      | _,_ ->
        begin
          "\t" ^ "movq " ^ format_begin_arg (cnt,a)
          ^ ", " ^ format_operand t ^ "\n"
        end
    end
  | RETURN ->
    "\tpopq %rbp\n" ^
    "\tret\n"
  | ARGOFFSET (d,offset) ->
    begin
      (* "\tsubq $" ^ format_argoffset offset^", "^ format_operand d ^ "\n" *)
      match (offset) with
      (*
      | 0 -> "\tsubq $8, "^ format_operand d ^ "\n"
      | 1 -> "\tsubq $8, "^ format_operand d ^ "\n"
      | 2 -> "\tsubq $8, "^ format_operand d ^ "\n"
      | 3 -> "\tsubq $8, "^ format_operand d ^ "\n"
      | 4 -> "\tsubq $8, "^ format_operand d ^ "\n"
      | 5 -> "\tsubq $8, "^ format_operand d ^ "\n"
      | _ -> "\tsubq $" ^ (string_of_int ((offset - 6) * (8))) ^ ", "^ format_operand d ^ "\n" *)
      | 0 -> "\tsubq $16, "^ format_operand d ^ "\n"
      | 1 -> "\tsubq $16, "^ format_operand d ^ "\n"
      | 2 -> "\tsubq $16, "^ format_operand d ^ "\n"
      | 3 -> "\tsubq $16, "^ format_operand d ^ "\n"
      | 4 -> "\tsubq $16, "^ format_operand d ^ "\n"
      | 5 -> "\tsubq $16, "^ format_operand d ^ "\n"
      | _ -> "\tsubq $" ^ (string_of_int ((offset - 6) * (8))) ^ ", "^ format_operand d ^ "\n"
(*
      | 0 -> "\tsubq $24, "^ format_operand d ^ "\n"
      | 1 -> "\tsubq $24, "^ format_operand d ^ "\n"
      | 2 -> "\tsubq $24, "^ format_operand d ^ "\n"
      | 3 -> "\tsubq $24, "^ format_operand d ^ "\n"
      | 4 -> "\tsubq $24, "^ format_operand d ^ "\n"
      | 5 -> "\tsubq $24, "^ format_operand d ^ "\n"
      | _ -> "\tsubq $" ^ (string_of_int ((offset - 5) * (16) + 8)) ^ ", "^ format_operand d ^ "\n" *)
    end

  | ARGONSET (d,offset) ->
    begin
      (* "\taddq $" ^ format_argoffset offset^", "^ format_operand d ^ "\n" *)
      match (offset) with

      (*
      | 0 -> "\taddq $8, "^ format_operand d ^ "\n"
      | 1 -> "\taddq $8, "^ format_operand d ^ "\n"
      | 2 -> "\taddq $8, "^ format_operand d ^ "\n"
      | 3 -> "\taddq $8, "^ format_operand d ^ "\n"
      | 4 -> "\taddq $8, "^ format_operand d ^ "\n"
      | 5 -> "\taddq $8, "^ format_operand d ^ "\n"
      | _ -> "\taddq $" ^ (string_of_int ((offset - 6) * (8))) ^ ", "^ format_operand d ^ "\n" *)
      | 0 -> "\taddq $16, "^ format_operand d ^ "\n"
      | 1 -> "\taddq $16, "^ format_operand d ^ "\n"
      | 2 -> "\taddq $16, "^ format_operand d ^ "\n"
      | 3 -> "\taddq $16, "^ format_operand d ^ "\n"
      | 4 -> "\taddq $16, "^ format_operand d ^ "\n"
      | 5 -> "\taddq $16, "^ format_operand d ^ "\n"
      | _ -> "\taddq $" ^ (string_of_int ((offset - 6) * (8))) ^ ", "^ format_operand d ^ "\n"
      (* | 0 -> "\taddq $24, "^ format_operand d ^ "\n"
      | 1 -> "\taddq $24, "^ format_operand d ^ "\n"
      | 2 -> "\taddq $24, "^ format_operand d ^ "\n"
      | 3 -> "\taddq $24, "^ format_operand d ^ "\n"
      | 4 -> "\taddq $24, "^ format_operand d ^ "\n"
      | 5 -> "\taddq $24, "^ format_operand d ^ "\n"
      | _ -> "\taddq $" ^ (string_of_int ((offset - 5) * (16) +8)) ^ ", "^ format_operand d ^ "\n" *)
    end
  | STACKOFFSET (d,offset) ->
    begin
      match ( offset) with
      | 0 -> ""
      | 1 -> ""
      | 2 -> ""
      | 3 -> ""
      | 4 -> ""
      | 5 -> ""
      (* | 5 -> ""
      | 6 -> ""
      | 7 -> ""
      | 8 -> ""
      | 9 -> ""
      | 10 -> "" *)
      | _ -> "\tsubq $" ^ (string_of_int ((offset*8))) ^ ", "^ format_operand d ^ "\n"

      (* "\tsubq $" ^ format_stackoffset offset^", "^ format_operand d ^ "\n" *)
      (* match (Int32.to_int offset) with
         | 0 -> "\tsubq $16, "^ format_operand d ^ "\n"
         | 1 -> "\tsubq $16, "^ format_operand d ^ "\n"
         | 2 -> "\tsubq $16, "^ format_operand d ^ "\n"
         | 3 -> "\tsubq $16, "^ format_operand d ^ "\n"
         | 4 -> "\tsubq $16, "^ format_operand d ^ "\n"
         | 5 -> "\tsubq $16, "^ format_operand d ^ "\n"
         | _ -> "\tsubq $" ^ (string_of_int (((Int32.to_int offset) - 5) * (16))) ^ ", "^ format_operand d ^ "\n" *)
    end

  | STACKONSET (d,offset) ->
    begin
      match (offset) with
    | 0 -> ""
    | 1 -> ""
    | 2 -> ""
    | 3 -> ""
    | 4 -> ""
    | 5 -> ""
    (* | 5 -> ""
    | 6 -> ""
    | 7 -> ""
    | 8 -> ""
    | 9 -> ""
    | 10 -> "" *)
    | _ -> "\taddq $" ^ (string_of_int ((offset*8))) ^ ", "^ format_operand d ^ "\n"

      (* "\taddq $" ^ format_stackoffset offset^", "^ format_operand d ^ "\n" *)
      (* match (Int32.to_int offset) with
         | 0 -> "\taddq $16, "^ format_operand d ^ "\n"
         | 1 -> "\taddq $16, "^ format_operand d ^ "\n"
         | 2 -> "\taddq $16, "^ format_operand d ^ "\n"
         | 3 -> "\taddq $16, "^ format_operand d ^ "\n"
         | 4 -> "\taddq $16, "^ format_operand d ^ "\n"
         | 5 -> "\taddq $16, "^ format_operand d ^ "\n"
         | _ -> "\taddq $" ^ (string_of_int (((Int32.to_int offset) - 5) * (16))) ^ ", "^ format_operand d ^ "\n" *)
    end

  (* | ARGUEMENT t ->
    (match find_arg (Arguement.name t) with
     | STACK ->
       (string_of_int (
           ((int_of_string (Arguement.name t)) - 5) * (-16)
         )) ^ "(%rsp)"
     | _ -> format_reg (find_arg (Arguement.name t))
    ) *)
  | WRITELABEL l ->
    format_label l ^ ":\n"
  | FUNCSTART id ->
    "\t.globl\t_c0_" ^ id ^"\n" ^
    "_c0_" ^ id ^ ":\n" ^
    "\tpushq %rbp\n" ^
    "\tmovq %rsp, %rbp\n"
    (* begin
      match id with
      | "abort" ->
      (* "\t.globl\t__c0_" ^ id ^"\n" ^ *)
      "_c0_" ^ id ^ ":\n" ^
      "\tpushq %rbp\n" ^
      "\tmovq %rsp, %rbp\n"
      | _ ->
      "\t.globl\t" ^ id ^"\n" ^
      id ^ ":\n" ^
      "\tpushq %rbp\n" ^
      "\tmovq %rsp, %rbp\n"
    end *)
  | MAINSTART id ->
    "\t.globl\t" ^ id ^"\n" ^
    "" ^ id ^ ":\n" ^
    "\tpushq %rbp\n" ^
    "\tmovq %rsp, %rbp\n"
  | INTERNALCALL id ->
    "\tcallq _c0_" ^ id ^ "\n"
    (* begin
      match id with
      | "abort" -> "\tcallq _c0_" ^ id ^ "\n"
      | _ -> "\tcallq " ^ id ^ "\n"
    end *)

  | EXTERNALCALL (id,cnt) ->
    "\tcallq " ^ id ^ "\n"


    (* let needs = LABEL (Label.create()) in
    let dont = LABEL (Label.create()) in
    let done_correction = LABEL (Label.create()) in
    let call_func =
    "\tcallq _" ^ id ^ "\n" in

    let set_mod =
    "\tmovq %rsp, %rax\n" ^
    "\tmovq $8,   %r10\n" ^
    "\t" ^ "cltd\n" ^
    "\t" ^ "idivq %r10\n" in
let correct_mod =
  "\tcmpq $0, %rdx\n" ^
  "\tje "^format_label needs^"\n" ^
  "\tjmp "^format_label dont^"\n" in
let offset =
  begin
  match (cnt) with
  | 0 -> "\tsubq $12, %rsp\n"
  | 1 -> "\tsubq $12, %rsp\n"
  | 2 -> "\tsubq $12, %rsp\n"
  | 3 -> "\tsubq $12, %rsp\n"
  | 4 -> "\tsubq $12, %rsp\n"
  | 5 -> "\tsubq $12, %rsp\n"
  | _ -> "\tsubq $" ^ (string_of_int ((cnt - 5) * (4))) ^ ", %rsp\n"
end in
let onset =
  begin
  match (cnt) with
  | 0 -> "\taddq $12, %rsp\n"
  | 1 -> "\taddq $12, %rsp\n"
  | 2 -> "\taddq $12, %rsp\n"
  | 3 -> "\taddq $12, %rsp\n"
  | 4 -> "\taddq $12, %rsp\n"
  | 5 -> "\taddq $12, %rsp\n"
  | _ -> "\taddq $" ^ (string_of_int ((cnt - 5) * (4)+16)) ^ ", %rsp\n"
end in
set_mod ^ correct_mod ^
format_label needs ^ ":\n" ^
"\tsubq $4, %rsp\n" ^
offset ^ call_func ^ onset ^ "\taddq $4, %rsp\n" ^ "\tjmp " ^ format_label done_correction ^ "\n" ^
format_label dont ^ ":\n" ^ offset ^ call_func ^ onset ^ format_label done_correction ^ ":\n" *)



  | FUNCTIONABORT ->
    "\tsubq $16,%rsp\n" ^
    "\tcallq abort\n" ^
    "\taddq $16,%rsp\n" ^
    "\tpopq %rbp\n" ^
    "\tret\n"
  | GOTO l ->
    "\t" ^ "jmp " ^ format_label l ^ "\n"
  | PUSH a ->
    begin
      match test_if_in_stack a with
      | T -> ""
      | F -> "\tpushq " ^ get_full_reg (format_operand a) ^ "\n"
    end
  | POP a ->
    begin
      match test_if_in_stack a with
      | T -> ""
      | F -> "\tpopq " ^ get_full_reg (format_operand a) ^ "\n"
    end
  | UNOP (oper, d, s) ->
    begin
      match oper with
      | BNOT ->
        begin
          match test_if_in_stack s with
          | T ->
            "\tmovq " ^ format_operand s ^ ", " ^ "%r11"  ^ "\n" ^
            "\t" ^ "not %r11\n"
            ^ "\tmovq %r11, " ^ format_operand d ^ "\n"
          | F ->let k2 = MOV (d, s) in
            "\t" ^ "not "
            ^ " " ^ format_operand s ^ "\n"
            ^ format k2
        end
      | LNOT ->
        begin
          match test_if_in_stack s with
          | T ->
            "\tmovq " ^ format_operand s ^ ", " ^ "%r11"  ^ "\n" ^
            "\t" ^ "xorq $1, %r11\n"
            ^ "\tmovq %r11, " ^ format_operand d ^ "\n"
          | F ->let k2 = MOV (d, s) in
            "\t" ^ "xorq $1," ^ format_operand s ^ "\n"
            ^ format k2
        end
      | _ -> assert false
    end
  | CP (s, l1, l2) ->
    (
      match test_if_in_stack s with
      | T ->
        "\tmovq "^ format_operand s ^ ", " ^ "%r11"  ^ "\n" ^
        "\t" ^ "cmpl $0, %r11d\n"
        ^ "\tjne " ^ format_label l1^ "\n" ^
        "jmp " ^ format_label l2 ^ "\n"
      | F ->
        "\t" ^ "cmpl "
        ^ "$0, " ^ get_short_reg (get_full_reg (format_operand s)) ^ "\n"
        ^ "\tjne " ^ format_label l1^ "\n" ^
        "jmp " ^ format_label l2 ^ "\n"
    )

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
      "\tmovq %rcx, %r10\n" ^
      "\tmovq " ^ format_operand s2 ^ ", %rcx\n" ^
      "\tcmpl " ^ "$32,%ecx\n" ^
      "\tjl " ^ format_label lpass ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label lerror ^ ":\n" ^
      "\tmovq $1, %rax\n" ^
      "\tmovq $0, %rcx\n" ^
      "\tcltd\n" ^
      "\tidivl %ecx\n" ^
      "\tmovq %rax, %rax\n" ^
      "\tret\n" ^
      format_label lpass ^ ":\n" ^
      "\tcmpl " ^ "$0, %ecx\n" ^
      "\tjge " ^ format_label l3 ^ "\n" ^
        "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label l3 ^ ":\n" ^
      "\tsall %cl, " ^ format_operand s1 ^ "\n" ^
      "\tmovq " ^ format_operand s1 ^ ", %r11\n" ^
      "\tmovq %r10, %rcx\n" ^
      "\tmovq "  ^ "%r11, " ^ format_operand d ^ "\n"
      | F ->
      "\tmovq %rcx, %r10\n" ^
      "\tmovq " ^ format_operand s1 ^ ", %r11\n" ^
      "\tmovq " ^ format_operand s2 ^ ", %rcx\n" ^
      "\tcmpl " ^ "$32,%ecx\n" ^
      "\tjl " ^ format_label lpass ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label lerror ^ ":\n" ^
      "\tmovq $1, %rax\n" ^
      "\tmovq $0, %rcx\n" ^
      "\tcltd\n" ^
      "\tidivl %ecx\n" ^
      "\tmovq %rax, %rax\n" ^
      "\tret\n" ^
      format_label lpass ^ ":\n" ^
      "\tcmpl " ^ "$0, %ecx\n" ^
      "\tjge " ^ format_label l3 ^ "\n" ^
        "\tjmp " ^ format_label lerror ^ "\n" ^
      format_label l3 ^ ":\n" ^
      "\tsall %cl, %r11d\n" ^
      "\tmovq %r10, %rcx\n" ^
      "\tmovl %r11d, " ^ get_short_reg (get_full_reg (format_operand d)) ^ "\n"
    end
    | SRIGHT ->
      begin
    match test_if_in_stack s1 with
    | T ->
    "\tmovq %rcx, %r10\n" ^
    "\tmovq " ^ format_operand s2 ^ ", %rcx\n" ^
    "\tcmpl " ^ "$32,%ecx\n" ^
    "\tjl " ^ format_label lpass ^ "\n" ^
    "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label lerror ^ ":\n" ^
    "\tmovq $1, %rax\n" ^
    "\tmovq $0, %rcx\n" ^
    "\tcltd\n" ^
    "\tidivl %ecx\n" ^
    "\tmovq %rax, %rax\n" ^
    "\tret\n" ^
    format_label lpass ^ ":\n" ^
    "\tcmpl " ^ "$0, %ecx\n" ^
    "\tjge " ^ format_label l3 ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label l3 ^ ":\n" ^
    "\tsarl %cl, " ^ format_operand s1 ^ "\n" ^
    "\tmovl " ^ format_operand s1 ^ ", %r11d\n" ^
    "\tmovq %r10, %rcx\n" ^
    "\tmovl "  ^ "%r11d, " ^ get_short_reg (get_full_reg (format_operand d)) ^ "\n"
    | F ->
    "\tmovq %rcx, %r10\n" ^
    "\tmovq " ^ format_operand s1 ^ ", %r11\n" ^
    "\tmovq " ^ format_operand s2 ^ ", %rcx\n" ^
    "\tcmpl " ^ "$32,%ecx\n" ^
    "\tjl " ^ format_label lpass ^ "\n" ^
    "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label lerror ^ ":\n" ^
    "\tmovq $1, %rax\n" ^
    "\tmovq $0, %rcx\n" ^
    "\tcltd\n" ^
    "\tidivl %ecx\n" ^
    "\tmovq %rax, %rax\n" ^
    "\tret\n" ^
    format_label lpass ^ ":\n" ^
    "\tcmpl " ^ "$0, %ecx\n" ^
    "\tjge " ^ format_label l3 ^ "\n" ^
      "\tjmp " ^ format_label lerror ^ "\n" ^
    format_label l3 ^ ":\n" ^
    "\tsarl %cl, %r11d\n" ^
    "\tmovq %r10, %rcx\n" ^
    "\tmovl %r11d, " ^ get_short_reg ( get_full_reg (format_operand d)) ^ "\n"
  end
    | LES ->
      let l1 = LABEL (Label.create())
      and l2 = LABEL (Label.create())
      and l3 = LABEL (Label.create())in
      begin
        match test_if_in_stack s1 with
        | T -> "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
        "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", %r11d" ^ "\n" ^
        "\t" ^ "jl " ^ format_label l1 ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
        format_label l1 ^ ":\n" ^
        "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l2 ^ ":\n" ^
        "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l3 ^ ":\n"
        | F -> "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", " ^
               get_short_reg (get_full_reg (format_operand s1)) ^ "\n" ^
        "\t" ^ "jl " ^ format_label l1 ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
        format_label l1 ^ ":\n" ^
        "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l2 ^ ":\n" ^
        "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
        "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
        format_label l3 ^ ":\n"
      end
    | LEQ ->
      let l1 = LABEL (Label.create())
      and l2 = LABEL (Label.create())
      and l3 = LABEL (Label.create())in
      begin
        match test_if_in_stack s1 with
        | T -> "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
               "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", %r11d" ^ "\n" ^
               "\t" ^ "jle " ^ format_label l1 ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
               format_label l1 ^ ":\n" ^
               "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l2 ^ ":\n" ^
               "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l3 ^ ":\n"
        | F -> "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", " ^
               get_short_reg (get_full_reg (format_operand s1)) ^ "\n" ^
               "\t" ^ "jle " ^ format_label l1 ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
               format_label l1 ^ ":\n" ^
               "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l2 ^ ":\n" ^
               "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
               "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
               format_label l3 ^ ":\n"
      end

      | GRE ->
        let l1 = LABEL (Label.create())
        and l2 = LABEL (Label.create())
        and l3 = LABEL (Label.create())in
        begin
          match test_if_in_stack s1 with
          | T -> "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
                 "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", %r11d" ^ "\n" ^
                 "\t" ^ "jg " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
          | F -> "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", " ^
                 get_short_reg (get_full_reg (format_operand s1)) ^ "\n" ^
                 "\t" ^ "jg " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
        end

      | GEQ ->
        let l1 = LABEL (Label.create())
        and l2 = LABEL (Label.create())
        and l3 = LABEL (Label.create())in
        begin
          match test_if_in_stack s1 with
          | T -> "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
                 "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", %r11d" ^ "\n" ^
                 "\t" ^ "jge " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
          | F -> "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", " ^
                 get_short_reg (get_full_reg (format_operand s1)) ^ "\n" ^
                 "\t" ^ "jge " ^ format_label l1 ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                 format_label l1 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l2 ^ ":\n" ^
                 "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                 "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                 format_label l3 ^ ":\n"
        end
        | IEQ ->
          let l1 = LABEL (Label.create())
          and l2 = LABEL (Label.create())
          and l3 = LABEL (Label.create())in
          begin
            match test_if_in_stack s1 with
            | T -> "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
                   "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", %r11d" ^ "\n" ^
                   "\t" ^ "je " ^ format_label l1 ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                   format_label l1 ^ ":\n" ^
                   "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l2 ^ ":\n" ^
                   "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l3 ^ ":\n"
            | F -> "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", " ^
                   get_short_reg (get_full_reg (format_operand s1)) ^ "\n" ^
                   "\t" ^ "je " ^ format_label l1 ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                   format_label l1 ^ ":\n" ^
                   "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l2 ^ ":\n" ^
                   "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                   "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                   format_label l3 ^ ":\n"
          end
          | NEQ ->
            let l1 = LABEL (Label.create())
            and l2 = LABEL (Label.create())
            and l3 = LABEL (Label.create())in
            begin
              match test_if_in_stack s1 with
              | T -> "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
                     "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", %r11d" ^ "\n" ^
                     "\t" ^ "jne " ^ format_label l1 ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                     format_label l1 ^ ":\n" ^
                     "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l2 ^ ":\n" ^
                     "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l3 ^ ":\n"
              | F -> "\t" ^ "cmpl " ^ get_short_reg (get_full_reg (format_operand s2)) ^ ", " ^
                     get_short_reg (get_full_reg (format_operand s1)) ^ "\n" ^
                     "\t" ^ "jne " ^ format_label l1 ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l2 ^ "\n" ^
                     format_label l1 ^ ":\n" ^
                     "\t" ^ "movq " ^ "$1, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l2 ^ ":\n" ^
                     "\t" ^ "movq " ^ "$0, " ^ format_operand d ^ "\n" ^
                     "\t" ^ "jmp " ^ format_label l3 ^ "\n" ^
                     format_label l3 ^ ":\n"
            end

      | DIV ->
        let i1 = MOV (REG EAX, s1) in
        let i2 = MOV (d, REG EAX) in
        (format i1)
        ^
        "\tmovq %rdx, %r11\n" ^
        "\tmovq " ^ get_full_reg (format_operand s2) ^ ", %r10\n" ^
        "\t" ^ "cltd\n" ^
        "\t" ^ "idivl %r10d\n" ^
        "\tmovq %r11, %rdx\n" ^
        format i2
      | MOD ->
        (* let j1 = MOV (REG EAX, s1) in
        (* let j2 = MOV (d, REG EAX) in *)
        format j1 ^
        "\tmovl "^ format_operand s2 ^ ", %r11d"^"\n"^
        "\tpushq %rbp\n" ^
        "\tmovl %edx, %ebp\n" ^
        "\t" ^ "cltd\n" ^
        "\t" ^ "idivl %r11d\n" ^
        "\tmovl %edx, " ^ format_operand d ^ "\n" ^
        "\tmovl %ebp, %edx\n" ^
           "\tpopq %rbp\n" *)
        begin
          match format_operand d with
          | "%edx" ->
            let j1 = MOV (REG EAX, s1) in
          (* let j2 = MOV (d, REG EAX) in *)
            format j1 ^
            "\tmovq " ^ get_full_reg (format_operand s2) ^ ", %r10\n" ^
          "\t" ^ "cltd\n" ^
          "\t" ^ "idivl %r10d\n" ^
          "\tmovq %rdx, " ^ format_operand d ^ "\n"
          | _ ->
            let j1 = MOV (REG EAX, s1) in
          (* let j2 = MOV (d, REG EAX) in *)
          format j1 ^
          "\tmovq %rdx, %r11\n" ^
          "\tmovq " ^ get_full_reg (format_operand s2) ^ ", %r10\n" ^
          "\t" ^ "cltd\n" ^
          "\t" ^ "idivl %r10d\n" ^
          "\tmovq %rdx, " ^ format_operand d ^ "\n" ^
          "\tmovq %r11, %rdx\n"
        end

      | _ ->
          ( match test_if_in_stack s1 with
            | T ->
                    "\tmovq "^ format_operand s1 ^ ", " ^ "%r11"  ^ "\n" ^
                    "\t" ^ format_binop oper
                    ^ " " ^ format_operand s2
                    ^ ", %r11\n"
                    ^ "\tmovq %r11, " ^ format_operand d ^ "\n"
            | F ->let k2 = MOV (d, s1) in
                  "\t" ^ format_binop oper
                  ^ " " ^ format_operand s2
                  ^ ", " ^ format_operand s1 ^ "\n"
                  ^ format k2
          )
