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
 *| QUESTION (d,s1,s2) ->
      (* begin
         match test_if_in_stack s1 with
         | F -> *)
      "\tcmpl $0, %r11d\n" ^
      "\tmovq " ^ (get_full_reg (format_operand s1)) ^ ", %r10\n" ^
      "\tmovq " ^ get_full_reg (format_operand s2) ^ ", %r11\n" ^
      "\tcmovel %r11d, %r10d\n" ^
      "\tmovq %r10, " ^ get_full_reg (format_operand d) ^ "\n"
*)

type reg = EDX | RSP | EAX | ECX | EBX | ESI | EDI | R8D | R9D | R11D | R12D | R13D | R14D | R15D | STACK

type if_in_stack = T | F

(* type check = GREATER | GREATEROREQ | LESS | LESSOREQ | EQUAL | NOTEQE *)

type operand =
  | NULLP
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
  | BINOP of binop * operand * operand * operand * Ast.typedefine
  | CLEANBINOP of binop * operand * operand * operand * Ast.typedefine
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
  | RETURN of operand * string
  | VOIDRETURN
  | FUNCSTART of string * string * string
  | MAINSTART of string
  | INTERNALCALL of operand * string * string * string
  | EXTERNALCALL of operand * string * string * string
  | FUNCTIONCALL of string
  | FUNCTIONABORT
  | ARGOFFSET of operand * int
  | ARGONSET of operand * int
  | STACKOFFSET of operand * int
  | STACKONSET of operand * int
  | PASS of operand * operand
  | PUSH of operand
  | POP of operand
  | PASSARG of operand * operand * operand
  | LEA of operand * int * label * label
  | MOVEEXPTOADDR of operand * operand * operand * label * label
  | DOTDEREF of operand * operand * label
  | MALLOC of operand * int * string
  | INLINEFUNCTIONCALL of string * operand * operand * operand * operand * operand * operand * operand
  | ALLOCARRY of operand * int * operand * label
  | ARRDEREF of operand * operand * operand * int * label * label * label
  | ARRDEREFSHORTCUT of operand * operand * operand * int
  | ARRDONOTHING of operand * operand * operand * int * label * label * label
  | ARRDONOTHINGSHORTCUT of operand * operand * operand
  | NULL of operand
  | MOVETOADDR of operand * operand * string
  | MOVETOADDRFOURBYTE of operand * operand * label
  | GETCONTENT of operand * operand
  | GETCONTENTFOURBYTE of operand * operand
  | ARRADDRCOMP of operand * operand * operand * int * label * label * label
  | ARRADDRCOMPSHORT of operand * operand * operand
  | CALLRAISE
  |  QUESTION of operand * operand * operand * operand
  | NOP
  | JUSTWRITE of string
  | JMP of string * int
  | ALLOC of string * string
  | STORE of string * operand * string
  | VAR of string * operand * string
  | PHI of operand * operand * label * operand * label * Ast.typedefine
  | STOREARG of string * int * string
  | FUNCEND of string
  | LOAD of operand * operand * string
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
  | R11D -> "%r11"
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
  | "%r11" -> "%r11d"
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
  | "%r11d" -> "%r11"
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
  | "%r11" -> "%r11"
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
  | ADD -> "add nsw"
  | SUB -> "sub nsw"
  | DIV -> "sdiv"
  | MUL -> "mul"
  | MOD -> "srem"
  | LAND -> "and"
  | LOR -> "or"
  | BAND -> "and"
  | BOR -> "or"
  | BXOR -> "xor"
  | SLEFT -> "shl nsw"
  | SRIGHT -> "ashr"
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
  | NULLP -> "null"
  | IMM n  -> Int32.to_string n
  | TEMP t ->
    (
    "%t" ^ (Temp.name t)
    )
  | BOOL b ->
    begin
      match string_of_bool b with
      | "true" -> "1"
      | "false" ->"0"
    end
  | _ ->
    "have a problem here"

let format_type = function
  | Ast.BOOL -> "i1"
  | _ -> "i32"


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

  | MALLOC (d,siz,typedef) ->
    begin
      let t = TEMP (Temp.createeight()) in
      "\t" ^ format_operand t ^ " = call i8* @malloc(i64 " ^ (string_of_int siz) ^ ")\n" ^
      "\t" ^ format_operand d ^ " = bitcast i8* " ^ format_operand t ^ " to " ^ typedef ^ "*\n"
    end
  | FUNCEND typedef ->
    begin
      match typedef with
      | "void" ->"\tret void\n}\n"
      | "i1" ->"\tret i1 0\n}\n"
      | "i32" -> "\tret i32 0\n}\n"
      | _ ->
      "\tret " ^ typedef ^ " null\n}\n"
    end
  | STOREARG (id,counter,typedef) ->
    begin
      "\tstore " ^ typedef ^ " %" ^ (string_of_int counter) ^ ", " ^ typedef ^ "* %" ^ id ^ ", align 4\n"
    end
  | PHI (d,t1,l1,t2,l2,typedef) ->
    "\t" ^ format_operand d ^ " = phi " ^ format_type typedef ^ " [" ^ format_operand t1 ^ ", %" ^ format_label l1^ "], ["^ format_operand t2 ^ ", %" ^ format_label l2^ "]\n"
  | LOAD (t,d,typedef) ->
  "\t" ^ format_operand d ^ " = load " ^ typedef ^ ", " ^ typedef ^"* " ^ format_operand t ^",align 8\n"
  | VAR (id, d,typedef) ->
    begin
      match typedef with
      | "i1" ->
        "\t" ^ format_operand d ^ " = load i1, i1* %" ^ id ^",align 4\n"
      | "i32" ->
          "\t" ^ format_operand d ^ " = load i32, i32* %" ^ id ^",align 4\n"
      | _ ->
      "\t" ^ format_operand d ^ " = load " ^ typedef ^ ", " ^ typedef ^"* %" ^ id ^",align 8\n"
    end
  | STORE (id, t,typedef) ->
    begin
      match typedef with
      | "i1" ->
        "\tstore i1 " ^ format_operand t ^ ", i1* %" ^ id ^ ", align 4\n"
      | "i32" ->
        "\tstore i32 " ^ format_operand t ^ ", i32* %" ^ id ^ ", align 4\n"
      | _ ->
      "\tstore " ^ typedef ^ " " ^ format_operand t ^ ", " ^ typedef ^ "* %" ^ id ^ ", align 8\n"
    end
  | ALLOC (id,typedef) ->
    begin
      match typedef with
      | "i32" ->
        "\t%" ^ id ^ " = alloca i32, align 4\n"
      | "i1" ->
        "\t%" ^ id ^ " = alloca i1, align 4\n"
      | _ ->
        "\t%" ^ id ^ " = alloca " ^ typedef ^ ", align 8\n"
    end
  | JUSTWRITE s ->
    s
  | NOP -> ""
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
  | LEA (t, offset, l1,l2) ->
    begin
      (* let call_raise = EXTERNALCALL ("raise",0) in *)
      match offset with
      | 0 ->
        "\tmovq $31, %rcx\n" ^
        "\tmovq $1, %r10\n" ^
        "\tsall %cl, %r10d\n" ^
        "\tmovq $" ^ (string_of_int offset) ^ ", " ^ "%r11"  ^ "\n" ^
        "\tcmpq %r10, %r11\n" ^
        "jl " ^ format_label l1 ^ "\n" ^
        format_label l2 ^ ":\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        (* format call_raise ^ *)
        "\taddq $16, %rsp\n" ^
        format_label l1 ^ ":\n" ^
        "\tmovq "^ format_operand t ^ ", " ^ "%r11"  ^ "\n" ^
        "\tcmpq $0, %r11\n" ^
        "\tje " ^ format_label l2 ^ "\n"
      | _ ->
        begin
          (* match test_if_in_stack t with
             | T -> *)
          "\tmovq $31, %rcx\n" ^
          "\tmovq $1, %r10\n" ^
          "\tsall %cl, %r10d\n" ^
          "\tmovq $" ^ (string_of_int offset) ^ ", " ^ "%r11"  ^ "\n" ^
          "\tcmpq %r10, %r11\n" ^
          "jl " ^ format_label l1 ^ "\n" ^
          format_label l2 ^ ":\n" ^
          "\tmovq $12,%rdi\n" ^
          "\tsubq $16, %rsp\n" ^
          (* format call_raise ^ *)
          "\taddq $16, %rsp\n" ^
          format_label l1 ^ ":\n" ^
          "\tmovq "^ format_operand t ^ ", " ^ "%r11"  ^ "\n" ^
          "\tcmpq $0, %r11\n" ^
          "\tje " ^ format_label l2 ^ "\n"
          ^ "\tleaq " ^ string_of_int offset ^ "(%r11), %r11\n"
          ^ "\tmovq %r11, " ^ format_operand t ^ "\n"
          (* | F ->
             "\tleaq " ^ string_of_int offset ^ "("^format_operand t^"), " ^ format_operand t ^ "\n" *)


        end


    end
  | MOVEEXPTOADDR (base, offset, to_be_moved, l1,l2) ->
    begin
      (* let call_raise = EXTERNALCALL ("raise",0) in *)
      match offset with
      | IMM c ->
        "\tmovq " ^ format_operand base ^ ", %r11\n" ^
        "\tcmpq $0, %r11\n" ^
        "\tjne " ^ format_label l1 ^ "\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        (* format call_raise ^ *)
        "\taddq $16, %rsp\n" ^
        format_label l1 ^ ":\n" ^
        "\taddq " ^ format_operand offset ^ ", %r11\n" ^
        "\tmovq " ^ format_operand to_be_moved ^ ", %r10\n" ^
        "\tmovq %r10, (%r11)\n"
      | _ ->
        "\tmovq " ^ format_operand base ^ ", %r11\n" ^
        "\tcmpq $0, %r11\n" ^
        "\tjne " ^ format_label l1 ^ "\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        (* format call_raise ^ *)
        "\taddq $16, %rsp\n" ^
        format_label l1 ^ ":\n" ^
        "\taddq " ^ format_operand offset ^ ", %r11\n" ^
        "\tmovq " ^ format_operand to_be_moved ^ ", %r10\n" ^
        "\tmovq %r10, (%r11)\n"
    end
  | CALLRAISE ->
    "\tcall i32 @raise(i32 8)\n"

  | DOTDEREF (d,s,l1) ->
    (* let call_raise = EXTERNALCALL ("raise",0) in *)
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
            (* format call_raise ^ *)
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
            (* format call_raise ^ *)
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
            (* format call_raise ^ *)
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tmovq (" ^ get_full_reg (format_operand s) ^ "), %r11\n" ^
            "\tmovq %r11, " ^ format_operand d ^ "\n"
          | F ->
            "\tcmpl $0, "^ get_short_reg (get_full_reg (format_operand s)) ^"\n" ^
            "\tjne " ^ format_label l1 ^"\n" ^
            "\tmovq $12,%rdi\n" ^
            "\tsubq $16, %rsp\n" ^
            (* format call_raise ^ *)
            "\taddq $16, %rsp\n" ^
            format_label l1 ^ ":\n" ^
            "\tmovq (" ^ get_full_reg (format_operand s) ^ "), " ^ get_full_reg (format_operand d) ^ "\n"
        end


    end
  | ALLOCARRY (d,element_size, s,l) ->
    begin
      (* let external_malloc = EXTERNALCALL ("malloc",1) in
      let call_raise = EXTERNALCALL ("raise",1) in *)
      "\tmovq " ^ format_operand s ^ ", %r11\n" ^
      "\tcmpl $0, %r11d\n" ^
      "\tjge " ^ format_label l ^ "\n" ^
      "\tmovq $12,%rdi\n" ^
      "\tsubq $16, %rsp\n" ^
      (* format call_raise ^ *)
      format_label l^ ":\n" ^
      "\timulq $" ^ string_of_int (element_size) ^ ", %r11\n" ^
      "\taddq $8, %r11\n" ^
      (* "\tpushq %rdi\n" ^ *)
      "\tmovq %r11, %rdi\n" ^
      (* "\tsubq $16, %rsp\n" ^ *)
      (* format external_malloc ^ *)
      (* "\taddq $16, %rsp\n" ^ *)
      (* "\tpopq %rdi\n" ^ *)
      "\tmovq " ^ format_operand s ^ ", %r11\n" ^
      "\tmovq %r11,(%rax)\n" ^
      "\taddq $8, %rax\n" ^
      "\tmovq %rax, " ^ format_operand d ^ "\n"
    end
  | INLINEFUNCTIONCALL (func_id, d, t1,t2,t3,t4,t5,t6)->
    begin
      match func_id with
      | "leftrotate" ->
        "\tmovl $4294967295, %eax\n" ^
        "\tmovl $32, %ecx\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg (format_operand t1)) ^ ", %esi\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg (format_operand t2)) ^ ", %edi\n" ^
        "\tmovl %ecx, %r11d\n" ^
        "\tmovl %edi, %ecx\n" ^
        "\tshll %cl, %esi\n" ^
        "\tmovl %esi, " ^ get_short_reg (get_full_reg (format_operand t3)) ^ "\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg (format_operand t1)) ^ ", %esi\n" ^
        "\tmovl %r11d, %edi\n" ^
        "\tsubl " ^ get_short_reg (get_full_reg (format_operand t2)) ^ ", %edi\n" ^
        "\tmovl %edi, %ecx\n" ^
        "\tsarl %cl, %esi\n" ^
        "\tmovl %esi, " ^ get_short_reg (get_full_reg (format_operand t4)) ^ "\n" ^
        "\tmovl " ^ format_operand t2 ^ ", %ecx\n" ^
        "\tshll %cl, %eax\n" ^
        "\txorl $-1, %eax\n" ^
        "\tmovl %eax, %r11d\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg (format_operand t3)) ^ ", %eax\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg (format_operand t4)) ^ ", %esi\n" ^
        "\tandl %r11d, %esi\n" ^
        "\torl %esi, %eax\n" ^
        "\tmovl %eax, " ^ get_short_reg (get_full_reg (format_operand d)) ^ "\n"
    end
  | ARRADDRCOMP (s,base,mult,arr_size_for_each_element,l1,l2,l3) ->
    (* let call_raise = EXTERNALCALL ("raise",1) in *)
    begin
      "\tmovq " ^ format_operand base ^ ", %r11\n" ^
      "\tmovq $0, %r10\n" ^
      "\tcmpq %r11, %r10\n" ^
      "\tjne " ^ format_label l3 ^"\n" ^
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
      (* format call_raise ^ *)
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
    (* let call_raise = EXTERNALCALL ("raise",1) in *)
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
            (* format call_raise ^ *)
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
            (* format call_raise ^ *)
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
            (* format call_raise ^ *)
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
            (* format call_raise ^ *)
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
    (* let call_raise = EXTERNALCALL ("raise",1) in *)
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^

                  "\tcmpl $0, %edi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^


                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^
                  "\taddq %rdi,%r11\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^
                  "\taddq %rdi,%r11\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^
                  "\taddq %rdi,%r11\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
                  "\tjl " ^ format_label l2 ^ "\n" ^

                  (* "\tlea $" ^ string_of_int (8+index*arr_size_for_each_element) ^ ", %r11\n" ^ *)
                  "\timulq $" ^ string_of_int arr_size_for_each_element ^ ", %rdi\n" ^
                  (* "\taddq $8,%rdi\n" ^ *)
                  "\tmovq " ^ format_operand s ^ ", %r11\n" ^
                  "\taddq %rdi,%r11\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
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
                  (* format call_raise ^ *)
                  "\taddq $16, %rsp\n" ^
                  format_label l1 ^ ":\n" ^
                  "\tcmpl $0, %edi\n" ^
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
    (* let call_raise = EXTERNALCALL ("raise",0) in *)
    begin
      match test_if_in_stack s with
      | T ->
        "\tmovq " ^ format_operand d ^ ", %r11\n" ^
        "\tmovq " ^ format_operand s ^ ", %r10\n" ^

        "\tcmpl $0, %r11d\n" ^
        "\tjne " ^ format_label l ^ "\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        (* format call_raise ^ *)
        "\taddq $16, %rsp\n" ^
        format_label l ^ ":\n" ^
        "\tmovl %r10d, (%r11)\n"
      | F ->
        "\tmovq " ^ format_operand d ^ ", %r11\n" ^
        "\tcmpl $0, %r11d\n" ^
        "\tjne " ^ format_label l ^ "\n" ^
        "\tmovq $12,%rdi\n" ^
        "\tsubq $16, %rsp\n" ^
        (* format call_raise ^ *)
        "\taddq $16, %rsp\n" ^
        format_label l ^ ":\n" ^
        "\tmovl " ^ get_short_reg (get_full_reg ( format_operand s)) ^ ", (%r11)\n"
    end


  | MOVETOADDR (d,s,typedef) ->

    (* let call_raise = EXTERNALCALL ("raise",0) in *)
    begin
      "\tstore " ^ typedef ^ " " ^ format_operand s ^ ", " ^ typedef ^ "* " ^ format_operand d ^ ", align 8\n"
    end

  | MOV (d, s) ->
    (
      if (get_full_reg (format_operand d)) = (get_full_reg (format_operand s)) then ""
      else
        begin
          match (test_if_in_stack d, test_if_in_stack s) with
          | T, T -> "\tmovq "^ format_operand s ^ ", " ^ "%r11"  ^ "\n"
                    ^ "\tmovq " ^ "%r11" ^ ", " ^ format_operand d ^ "\n"
          | _, _ -> "\t" ^ "movq " ^ format_operand s
                    ^ ", " ^ format_operand d ^ "\n"

        end)

  | MOVARGBEFORECALL (d, s) ->
    (match (test_if_in_stack d, test_if_in_stack s) with
     | T, T -> "\tmovq "^ format_operand s ^ ", " ^ "%r11"  ^ "\n"
               ^ "\tmovq " ^ "%r11" ^ ", " ^ format_operand d ^ "\n"
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
  | RETURN (t,typedef)->
    "\tret " ^ typedef ^ " " ^ format_operand t ^ "\n"
  | VOIDRETURN ->

    let test = LABEL (Label.create()) in
    "\tret void\n" ^
    format_label test ^ ":\n"
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
      let stack_offset =

        begin
          match ( offset) with
          | 0 -> ""
          | 1 -> ""
          | 2 -> ""
          | 3 -> ""
          | 4 -> ""
          | 5 -> ""
          | _ -> "\tsubq $" ^ (string_of_int ((offset*8))) ^ ", "^ format_operand d ^ "\n"

        end in
      let register_offset =
        begin
          match ( offset) with
          | 0 -> ""
          | 1 -> "\tpushq %rbx\n"
          | 2 -> "\tpushq %rbx\n\tpushq %r12\n"
          | 3 -> "\tpushq %rbx\n\tpushq %r12\n\tpushq %r13\n"
          | 4 -> "\tpushq %rbx\n\tpushq %r12\n\tpushq %r13\n\tpushq %r14\n"
          | _ -> "\tpushq %rbx\n\tpushq %r12\n\tpushq %r13\n\tpushq %r14\n\tpushq %r15\n"

        end in
      register_offset ^ stack_offset
    end

  | STACKONSET (d,offset) ->
    begin
      let stack_onset =
        begin
          match (offset) with
          | 0 -> ""
          | 1 -> ""
          | 2 -> ""
          | 3 -> ""
          | 4 -> ""
          | 5 -> ""
          | _ -> "\taddq $" ^ (string_of_int ((offset*8))) ^ ", "^ format_operand d ^ "\n"
        end in
      let register_onset =
        begin
          match offset with
          | 0 -> ""
          | 1 -> "\tpopq %rbx\n"
          | 2 -> "\tpopq %r12\n\tpopq %rbx\n"
          | 3 -> "\tpopq %r13\n\tpopq %r12\n\tpopq %rbx\n"
          | 4 -> "\tpopq %r14\n\tpopq %r13\n\tpopq %r12\n\tpopq %rbx\n"
          | _ -> "\tpopq %r15\n\tpopq %r14\n\tpopq %r13\n\tpopq %r12\n\tpopq %rbx\n"
        end in
      stack_onset ^ register_onset

    end

  | WRITELABEL l ->
    format_label l ^ ":\n"
  | FUNCSTART (id,arg,return_type)->
    "define " ^ return_type ^ " @_c0_" ^ id ^ "(" ^ arg ^ ") {\n"

  | MAINSTART id ->
    "define i32 @_c0_main() {\nentry:\n"
  | INTERNALCALL (d,id,return_type,call_arg) ->
    begin
      match return_type with
      | "void" ->"\tcall " ^ return_type ^ " @_c0_" ^ id ^ "(" ^ call_arg  ^")\n"
      | _ ->
      "\t" ^ format_operand d ^ " = call " ^ return_type ^ " @_c0_" ^ id ^ "(" ^ call_arg  ^")\n"
    end

  | EXTERNALCALL (d,id,return_type,call_arg) ->
    begin
      match return_type with
      | "void" ->
      "\tcall " ^ return_type ^ " @" ^ id ^ "(" ^ call_arg  ^")\n"
      | _ ->
      "\t" ^ format_operand d ^ " = call " ^ return_type ^ " @" ^ id ^ "(" ^ call_arg  ^")\n"
    end


  | FUNCTIONABORT ->
    "\tcall void @abort()\n"
  | GOTO l ->
    "\tbr label %" ^ format_label l ^ "\n"
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
          "\t" ^ format_operand d ^ " = xor i32 " ^ format_operand s ^ ", -1\n"
        end
      | LNOT ->
        begin
         "\t" ^ format_operand d ^ " = xor i1 " ^ format_operand s ^ ", 1\n"
        end
      | _ -> assert false
    end
  | CP (s, l1, l2) ->
    (
      "\tbr i1 " ^ format_operand s ^ ", label %" ^ format_label l1 ^ ", label %" ^ format_label l2 ^ "\n"
    )

  | DIRECTIVE str ->
    "\t" ^ str ^ "\n"
  | COMMENT str ->
    "\t" ^ "/* " ^ str ^ "*/\n"

  | CLEANBINOP (oper, d, s1, s2,typedef) ->
    let embed_type =
      begin
        match typedef with
        | Ast.BOOL -> "i1"
        | _ -> "i32"
      end in
    "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"

  | BINOP (oper, d, s1, s2,typedef) ->
    let embed_type =
      begin
        match typedef with
        | Ast.BOOL -> "i1"
        | _ -> "i32"
      end in
    begin
      match oper with
      | LES ->
        "\t" ^ format_operand d ^ " = icmp slt " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | LEQ ->
        "\t" ^ format_operand d ^ " = icmp sle " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | GRE ->
        "\t" ^ format_operand d ^ " = icmp sgt " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | GEQ ->
        "\t" ^ format_operand d ^ " = icmp sge " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | IEQ ->
        "\t" ^ format_operand d ^ " = icmp eq " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | NEQ ->
        "\t" ^ format_operand d ^ " = icmp ne " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | LOR ->
        "\t" ^ format_operand d ^ " = or i1 " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | LAND ->
        "\t" ^ format_operand d ^ " = and i1 " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | DIV ->
        let t1 = TEMP (Temp.createeight()) in
        let t2 = TEMP (Temp.createeight()) in
        let t3 = TEMP (Temp.createeight()) in
        let raise1 = LABEL (Label.create()) in
        let raise2 = LABEL (Label.create()) in
        let raise3 = LABEL (Label.create()) in
        let dont1 = LABEL (Label.create()) in
        let dont = LABEL (Label.create()) in
         "\t" ^ format_operand t1 ^ " = icmp eq i32 " ^ format_operand s2 ^ ", 0\n" ^
         "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
         "\t" ^ format_label raise1 ^ ":\n" ^
         "\tcall i32 @raise(i32 8)\n" ^
         "\tbr label %" ^ format_label dont1 ^ "\n" ^
         "\t" ^ format_label dont1 ^ ":\n" ^
         "\t" ^ format_operand t2 ^ " = icmp eq i32 " ^ format_operand s2 ^ ", -1\n" ^
         "\tbr i1 " ^ format_operand t2 ^ ", label %" ^ format_label raise2 ^ ", label %" ^ format_label dont ^ "\n" ^
         "\t" ^ format_label raise2 ^ ":\n" ^
         "\t" ^ format_operand t3 ^ " = icmp eq i32 " ^ format_operand s1 ^ ", -2147483648\n" ^
         "\tbr i1 " ^ format_operand t3 ^ ", label %" ^ format_label raise3 ^ ", label %" ^ format_label dont ^ "\n" ^
         "\t" ^ format_label raise3 ^ ":\n" ^
         "\tcall i32 @raise(i32 8)\n" ^
         "\tbr label %" ^ format_label dont ^ "\n" ^
         "\t" ^ format_label dont ^ ":\n" ^
         "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | MOD ->
      let t1 = TEMP (Temp.createeight()) in
      let t2 = TEMP (Temp.createeight()) in
      let t3 = TEMP (Temp.createeight()) in
      let raise1 = LABEL (Label.create()) in
      let raise2 = LABEL (Label.create()) in
      let raise3 = LABEL (Label.create()) in
      let dont1 = LABEL (Label.create()) in
      let dont = LABEL (Label.create()) in
       "\t" ^ format_operand t1 ^ " = icmp eq i32 " ^ format_operand s2 ^ ", 0\n" ^
       "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
       "\t" ^ format_label raise1 ^ ":\n" ^
       "\tcall i32 @raise(i32 8)\n" ^
       "\tbr label %" ^ format_label dont1 ^ "\n" ^
       "\t" ^ format_label dont1 ^ ":\n" ^
       "\t" ^ format_operand t2 ^ " = icmp eq i32 " ^ format_operand s2 ^ ", -1\n" ^
       "\tbr i1 " ^ format_operand t2 ^ ", label %" ^ format_label raise2 ^ ", label %" ^ format_label dont ^ "\n" ^
       "\t" ^ format_label raise2 ^ ":\n" ^
       "\t" ^ format_operand t3 ^ " = icmp eq i32 " ^ format_operand s1 ^ ", -2147483648\n" ^
       "\tbr i1 " ^ format_operand t3 ^ ", label %" ^ format_label raise3 ^ ", label %" ^ format_label dont ^ "\n" ^
       "\t" ^ format_label raise3 ^ ":\n" ^
       "\tcall i32 @raise(i32 8)\n" ^
       "\tbr label %" ^ format_label dont ^ "\n" ^
       "\t" ^ format_label dont ^ ":\n" ^
       "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
      | SLEFT ->
        let t1 =TEMP (Temp.createeight()) in
        let t2 =TEMP (Temp.createeight()) in
        let raise = LABEL (Label.create()) in
        let dont1 = LABEL (Label.create()) in
        let dont2 = LABEL (Label.create()) in
        "\t" ^ format_operand t1 ^ " = icmp slt i32 " ^ format_operand s2 ^ ", 0\n" ^
        "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise ^ ", label %" ^ format_label dont1 ^ "\n" ^
        "\t" ^ format_label raise ^ ":\n" ^
        "\tcall i32 @raise(i32 8)\n" ^
        "\tbr label %" ^ format_label dont1 ^ "\n" ^
        "\t" ^ format_label dont1 ^ ":\n" ^
        "\t" ^ format_operand t2 ^ " = icmp sgt i32 " ^ format_operand s2 ^ ", 31\n" ^
        "\tbr i1 " ^ format_operand t2 ^ ", label %" ^ format_label raise ^ ", label %" ^ format_label dont2 ^ "\n" ^
        "\t" ^ format_label dont2 ^ ":\n" ^
        "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"

      | SRIGHT ->
        let t1 =TEMP (Temp.createeight()) in
        let t2 =TEMP (Temp.createeight()) in
        let raise = LABEL (Label.create()) in
        let dont1 = LABEL (Label.create()) in
        let dont2 = LABEL (Label.create()) in
        "\t" ^ format_operand t1 ^ " = icmp slt i32 " ^ format_operand s2 ^ ", 0\n" ^
        "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise ^ ", label %" ^ format_label dont1 ^ "\n" ^
        "\t" ^ format_label raise ^ ":\n" ^
        "\tcall i32 @raise(i32 8)\n" ^
        "\tbr label %" ^ format_label dont1 ^ "\n" ^
        "\t" ^ format_label dont1 ^ ":\n" ^
        "\t" ^ format_operand t2 ^ " = icmp sgt i32 " ^ format_operand s2 ^ ", 31\n" ^
        "\tbr i1 " ^ format_operand t2 ^ ", label %" ^ format_label raise ^ ", label %" ^ format_label dont2 ^ "\n" ^
        "\t" ^ format_label dont2 ^ ":\n" ^
        "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"

      | _ ->
        (
          "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        )

    end
