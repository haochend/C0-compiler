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

open Int64

type reg = EDX | RSP | EAX | ECX | EBX | ESI | EDI | R8D | R9D | R11D | R12D | R13D | R14D | R15D | STACK

type if_in_stack = T | F

(* type check = GREATER | GREATEROREQ | LESS | LESSOREQ | EQUAL | NOTEQE *)

type operand =
  | NULLP
  | FIMM of Base.Float.t
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
  | BINOP of binop * operand * operand * operand * string
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
  | ALLOCARRY of operand * int * operand * string
  | ARRDEREF of operand * operand * operand * string
  | ARRDEREFSHORTCUT of operand * operand * operand * int
  | ARRDONOTHING of operand * operand * operand  * string
  | ARRDONOTHINGSHORTCUT of operand * operand * operand
  | NULL of operand
  | MOVETOADDR of operand * operand * string
  | MOVETOADDRCHECK of operand * operand * string
  (* | MOVETOADDRCHECK of operand * operand * string *)
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
  | STORESPECIAL of string * operand * string
  | VAR of string * operand * string
  | PHI of operand * operand * label * operand * label * Ast.typedefine
  | STOREARG of string * int * string
  | FUNCEND of string
  | LOAD of operand * operand * string
  | LOADCHECK of operand * operand * string
  (* | LOADCHECK of operand * operand * string *)
  | STRUCTDEFN of string * string
  | RARROW of string * operand * operand * int * string
  | RARROWWITHOUTLOAD of string * operand * operand * int * string
  | RARROWWITHOUTCHECK of string * operand * operand * int * string
  | RARROWWITHOUTCHECKANDLOAD of string * operand * operand * int * string
  | DOTASSIGN of string * operand * int * operand * string
  | DOTASSIGNCHECK of string * operand * int * operand * string
  | ASSIGNARR of operand * operand * operand * string
  | GETSTRUADDR of operand * string * operand * int * string

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

let format_binop_float = function
  | ADD -> "fadd"
  | SUB -> "fsub"
  | DIV -> "fdiv"
  | MUL -> "fmul"
  | MOD -> "frem"
  | _ -> assert false

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
  | FIMM f ->
    begin
      let iof = (Int64.logand (Int64.bits_of_float f) (Int64.of_string "0xFFFFFFFFE0000000")) in
      let hoi = Printf.sprintf "%LX" iof in
      "0x" ^ hoi
    end
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
  | Ast.FLOAT -> "float"
  | _ -> "i32"


let test_if_in_stack = function
  | IMM n -> F
  | FIMM f -> F
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
(* let format_begin_arg= function
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
  | _,_ -> assert false *)
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
  | GETSTRUADDR (dest, sid, base, offset, typedef) ->

    let temp1 =TEMP (Temp.createeight()) in
    let raise1 = LABEL (Label.create()) in
    let dont1 = LABEL (Label.create()) in
    "\t" ^ format_operand temp1 ^ " = icmp eq %struct." ^ sid ^ "* " ^ format_operand base ^ ", null\n" ^
    "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label raise1 ^ ":\n" ^
    "\tcall i32 @raise(i32 12)\n" ^
    "\tbr label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label dont1 ^ ":\n" ^



    "\t" ^ format_operand dest ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand base ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n"


  | DOTASSIGNCHECK (sid,base,offset,d,typedef) ->
    let t = TEMP (Temp.createeight()) in

    let temp1 =TEMP (Temp.createeight()) in
    let raise1 = LABEL (Label.create()) in
    let dont1 = LABEL (Label.create()) in
    "\t" ^ format_operand temp1 ^ " = icmp eq %struct." ^ sid ^ "* " ^ format_operand base ^ ", null\n" ^
    "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label raise1 ^ ":\n" ^
    "\tcall i32 @raise(i32 12)\n" ^
    "\tbr label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label dont1 ^ ":\n" ^
    "\t" ^ format_operand d ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand base ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n"
  (* "\tstore " ^ typedef ^ " " ^ format_operand to_be_moved ^ ", " ^ typedef ^ "* " ^ format_operand t  ^ ", align 8\n" *)


  | DOTASSIGN (sid,base,offset,to_be_moved,typedef) ->
    let t = TEMP (Temp.createeight()) in

    (* let temp1 =TEMP (Temp.createeight()) in
       let raise1 = LABEL (Label.create()) in
       let dont1 = LABEL (Label.create()) in
       "\t" ^ format_operand temp1 ^ " = icmp eq %struct." ^ sid ^ "* " ^ format_operand base ^ ", null\n" ^
       "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
       "\t" ^ format_label raise1 ^ ":\n" ^
       "\tcall i32 @raise(i32 12)\n" ^
       "\tbr label %" ^ format_label dont1 ^ "\n" ^
       "\t" ^ format_label dont1 ^ ":\n" ^ *)
    "\t" ^ format_operand t ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand base ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n" ^
    "\tstore " ^ typedef ^ " " ^ format_operand to_be_moved ^ ", " ^ typedef ^ "* " ^ format_operand t  ^ ", align 8\n"
  | ASSIGNARR (base,index_exp,d,typedef) ->
    let t1 = TEMP (Temp.createeight()) in
    let t2 = TEMP (Temp.createeight()) in

    let t3 = TEMP (Temp.createeight()) in
    let t4 = TEMP (Temp.createeight()) in
    let t5 = TEMP (Temp.createeight()) in
    let t6 = TEMP (Temp.createeight()) in
    let t7 = TEMP (Temp.createeight()) in

    let temp1 =TEMP (Temp.createeight()) in
    let raise1 = LABEL (Label.create()) in
    let dont1 = LABEL (Label.create()) in
    let dont2 = LABEL (Label.create()) in
    let dont3 = LABEL (Label.create()) in
    "\t" ^ format_operand temp1 ^ " = icmp eq " ^ typedef ^ "* " ^ format_operand base ^ ", null\n" ^
    "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label raise1 ^ ":\n" ^
    "\tcall i32 @raise(i32 12)\n" ^
    "\tbr label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label dont1 ^ ":\n" ^


    "\t" ^ format_operand t3 ^ " = bitcast " ^ typedef ^ "* " ^ format_operand base ^ " to i32*\n" ^
    "\t" ^ format_operand t4 ^ " = getelementptr inbounds i32, i32* " ^ format_operand t3 ^ ", i32 -1\n" ^
    "\t" ^ format_operand t5 ^ " = load i32, i32* " ^ format_operand t4 ^ "\n" ^
    "\t" ^ format_operand t6 ^ " = icmp sge i32 " ^ format_operand index_exp ^ ", " ^ format_operand t5 ^ "\n" ^
    "\tbr i1 " ^ format_operand t6 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont2 ^ "\n" ^
    "\t" ^ format_label dont2 ^ ":\n" ^



    "\t" ^ format_operand t7 ^ " = icmp slt i32 " ^ format_operand index_exp ^ ", 0\n" ^
    "\tbr i1 " ^ format_operand t7 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont3 ^ "\n" ^
    "\t" ^ format_label dont3 ^ ":\n" ^

    "\t" ^ format_operand t1 ^ " = sext i32 " ^ format_operand index_exp ^ " to i64\n" ^
    "\t" ^ format_operand d ^ " = getelementptr inbounds " ^ typedef ^ ", " ^ typedef ^ "* " ^ format_operand base ^ ", i64 " ^ format_operand t1 ^ "\n"

  (* "\tstore " ^ typedef ^ " " ^ format_operand to_be_moved ^ ", " ^ typedef ^ "* " ^ format_operand t2 ^ ", align 8\n" *)

  | ARRDONOTHING (d,base,index_exp,typedef) ->
    begin
      let t1 = TEMP (Temp.createeight()) in
      let t2 = TEMP (Temp.createeight()) in
      let t3 = TEMP (Temp.createeight()) in
      let t4 = TEMP (Temp.createeight()) in
      let t5 = TEMP (Temp.createeight()) in
      let t7 = TEMP (Temp.createeight()) in
      let temp1 =TEMP (Temp.createeight()) in
      let raise1 = LABEL (Label.create()) in
      let dont1 = LABEL (Label.create()) in
      let dont2 = LABEL (Label.create()) in
      let dont3 = LABEL (Label.create()) in
      "\t" ^ format_operand temp1 ^ " = icmp eq " ^ typedef ^ "* " ^ format_operand base ^ ", null\n" ^
      "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label raise1 ^ ":\n" ^
      "\tcall i32 @raise(i32 12)\n" ^
      "\tbr label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label dont1 ^ ":\n" ^

      "\t" ^ format_operand t2 ^ " = bitcast " ^ typedef ^ "* " ^ format_operand base ^ " to i32*\n" ^
      "\t" ^ format_operand t3 ^ " = getelementptr inbounds i32, i32* " ^ format_operand t2 ^ ", i32 -1\n" ^
      "\t" ^ format_operand t4 ^ " = load i32, i32* " ^ format_operand t3 ^ "\n" ^
      "\t" ^ format_operand t5 ^ " = icmp sge i32 " ^ format_operand index_exp ^ ", " ^ format_operand t4 ^ "\n" ^
      "\tbr i1 " ^ format_operand t5 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont2 ^ "\n" ^
      "\t" ^ format_label dont2 ^ ":\n" ^

      "\t" ^ format_operand t7 ^ " = icmp slt i32 " ^ format_operand index_exp ^ ", 0\n" ^
      "\tbr i1 " ^ format_operand t7 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont3 ^ "\n" ^
      "\t" ^ format_label dont3 ^ ":\n" ^



      "\t" ^ format_operand t1 ^ " = sext i32 " ^ format_operand index_exp ^ " to i64\n" ^
      "\t" ^ format_operand d ^ " = getelementptr inbounds " ^ typedef ^ ", " ^ typedef ^ "* " ^ format_operand base ^ ", i64 " ^ format_operand t1 ^ "\n"
    end


  | ARRDEREF (d,base,index_exp,typedef) ->
    begin
      let t1 = TEMP (Temp.createeight()) in
      let t2 = TEMP (Temp.createeight()) in


      let t3 = TEMP (Temp.createeight()) in
      let t4 = TEMP (Temp.createeight()) in
      let t5 = TEMP (Temp.createeight()) in
      let t6 = TEMP (Temp.createeight()) in
      let t7 = TEMP (Temp.createeight()) in
      let temp1 =TEMP (Temp.createeight()) in
      let raise1 = LABEL (Label.create()) in
      let dont1 = LABEL (Label.create()) in
      let dont2 = LABEL (Label.create()) in
      let dont3 = LABEL (Label.create()) in
      "\t" ^ format_operand temp1 ^ " = icmp eq " ^ typedef ^ "* " ^ format_operand base ^ ", null\n" ^
      "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label raise1 ^ ":\n" ^
      "\tcall i32 @raise(i32 12)\n" ^
      "\tbr label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label dont1 ^ ":\n" ^



      "\t" ^ format_operand t3 ^ " = bitcast " ^ typedef ^ "* " ^ format_operand base ^ " to i32*\n" ^
      "\t" ^ format_operand t4 ^ " = getelementptr inbounds i32, i32* " ^ format_operand t3 ^ ", i32 -1\n" ^
      "\t" ^ format_operand t5 ^ " = load i32, i32* " ^ format_operand t4 ^ "\n" ^
      "\t" ^ format_operand t6 ^ " = icmp sge i32 " ^ format_operand index_exp ^ ", " ^ format_operand t5 ^ "\n" ^
      "\tbr i1 " ^ format_operand t6 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont2 ^ "\n" ^
      "\t" ^ format_label dont2 ^ ":\n" ^


      "\t" ^ format_operand t7 ^ " = icmp slt i32 " ^ format_operand index_exp ^ ", 0\n" ^
      "\tbr i1 " ^ format_operand t7 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont3 ^ "\n" ^
      "\t" ^ format_label dont3 ^ ":\n" ^

      "\t" ^ format_operand t1 ^ " = sext i32 " ^ format_operand index_exp ^ " to i64\n" ^
      "\t" ^ format_operand t2 ^ " = getelementptr inbounds " ^ typedef ^ ", " ^ typedef ^ "* " ^ format_operand base ^ ", i64 " ^ format_operand t1 ^
      "\t" ^ format_operand d ^ " = load " ^ typedef ^ ", " ^ typedef ^ "* " ^ format_operand t2 ^ ", align 8\n"
    end
  | ALLOCARRY (d,element_size, s,typedef) ->
    begin
      let t1 = TEMP (Temp.createeight()) in
      let t2 = TEMP (Temp.createeight()) in
      let t3 = TEMP (Temp.createeight()) in
      let t4 = TEMP (Temp.createeight()) in
      let t5 = TEMP (Temp.createeight()) in


      let temp1 =TEMP (Temp.createeight()) in
      let raise1 = LABEL (Label.create()) in
      let dont1 = LABEL (Label.create()) in
      "\t" ^ format_operand temp1 ^ " = icmp slt i32"  ^ " " ^ format_operand s ^ ", 0\n" ^
      "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label raise1 ^ ":\n" ^
      "\tcall i32 @raise(i32 12)\n" ^
      "\tbr label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label dont1 ^ ":\n" ^


      "\t" ^ format_operand t1 ^ " = sext i32 " ^ format_operand s ^ " to i64\n" ^
      "\t" ^ format_operand t2 ^ " = mul i64 " ^ string_of_int element_size ^ ", " ^ format_operand t1 ^ "\n" ^
      "\t" ^ format_operand t3 ^ " = call i8* @malloc(i64 " ^ format_operand t2 ^ ")\n" ^


      "\t" ^ format_operand t4 ^ " = bitcast i8* " ^ format_operand t3 ^ " to i32*\n" ^
      "\t" ^ format_operand t5 ^ " = getelementptr inbounds i32, i32* " ^ format_operand t4 ^ ", i32 -1\n" ^
      "\tstore i32 " ^ format_operand s ^ ", i32* " ^ format_operand t5 ^ "\n" ^

      "\t" ^ format_operand d ^ " = bitcast i8* " ^ format_operand t3 ^ " to " ^ typedef ^ "*\n"
    end
  | RARROW (sid,dest,src,offset,typedef) ->
    let t = TEMP (Temp.createeight()) in

    let temp1 =TEMP (Temp.createeight()) in
    let raise1 = LABEL (Label.create()) in
    let dont1 = LABEL (Label.create()) in
    "\t" ^ format_operand temp1 ^ " = icmp eq %struct." ^ sid ^ "* " ^ format_operand src ^ ", null\n" ^
    "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label raise1 ^ ":\n" ^
    "\tcall i32 @raise(i32 12)\n" ^
    "\tbr label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label dont1 ^ ":\n" ^
    "\t" ^ format_operand t ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand src ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n" ^
    "\t" ^ format_operand dest ^ " = load " ^ typedef ^ ", " ^ typedef ^ "* " ^ format_operand t ^ ", align 8\n"


  | RARROWWITHOUTLOAD (sid,dest,src,offset,typedef) ->
    let t = TEMP (Temp.createeight()) in

    let temp1 =TEMP (Temp.createeight()) in
    let raise1 = LABEL (Label.create()) in
    let dont1 = LABEL (Label.create()) in
    "\t" ^ format_operand temp1 ^ " = icmp eq %struct." ^ sid ^ "* " ^ format_operand src ^ ", null\n" ^
    "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label raise1 ^ ":\n" ^
    "\tcall i32 @raise(i32 12)\n" ^
    "\tbr label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label dont1 ^ ":\n" ^
    "\t" ^ format_operand dest ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand src ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n"



  | RARROWWITHOUTCHECKANDLOAD (sid,dest,src,offset,typedef) ->


      (* let temp1 =TEMP (Temp.createeight()) in
      let raise1 = LABEL (Label.create()) in
      let dont1 = LABEL (Label.create()) in
      "\t" ^ format_operand temp1 ^ " = icmp eq %struct." ^ sid ^ "* " ^ format_operand src ^ ", null\n" ^
      "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label raise1 ^ ":\n" ^
      "\tcall i32 @raise(i32 12)\n" ^
      "\tbr label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label dont1 ^ ":\n" ^ *)

    "\t" ^ format_operand dest ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand src ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n"



  | RARROWWITHOUTCHECK (sid,dest,src,offset,typedef) ->
    let t = TEMP (Temp.createeight()) in

    "\t" ^ format_operand t ^ " = getelementptr inbounds %struct." ^ sid ^ ", %struct." ^ sid ^ "* " ^ format_operand src ^ ", i32 0, i32 " ^ string_of_int offset ^ "\n" ^
    "\t" ^ format_operand dest ^ " = load " ^ typedef ^ ", " ^ typedef ^ "* " ^ format_operand t ^ ", align 8\n"



  | STRUCTDEFN (id, stru_str) ->
    "%struct." ^ id ^ " = type {" ^ stru_str ^ "}\n"
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
      | "float" -> "ret float 0.0\n}\n"
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
  | LOADCHECK (t,d,typedef) ->

    let temp1 =TEMP (Temp.createeight()) in
    let raise1 = LABEL (Label.create()) in
    let dont1 = LABEL (Label.create()) in
    "\t" ^ format_operand temp1 ^ " = icmp eq " ^ typedef ^ "* " ^ format_operand t ^ ", null\n" ^
    "\tbr i1 " ^ format_operand temp1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label raise1 ^ ":\n" ^
    "\tcall i32 @raise(i32 12)\n" ^
    "\tbr label %" ^ format_label dont1 ^ "\n" ^
    "\t" ^ format_label dont1 ^ ":\n" ^


    "\t" ^ format_operand d ^ " = load " ^ typedef ^ ", " ^ typedef ^"* " ^ format_operand t ^",align 8\n"
  | VAR (id, d,typedef) ->
    begin
      match typedef with
      | "i1" ->
        "\t" ^ format_operand d ^ " = load i1, i1* %" ^ id ^",align 4\n"
      | "i32" ->
        "\t" ^ format_operand d ^ " = load i32, i32* %" ^ id ^",align 4\n"
      | "float" ->
      "\t" ^ format_operand d ^ " = load float, float* %" ^ id ^",align 4\n"
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
      | "float" ->
        "\tstore float " ^ format_operand t ^ ", float* %" ^ id ^ ", align 4\n"
      | _ ->
        "\tstore " ^ typedef ^ " " ^ format_operand t ^ ", " ^ typedef ^ "* %" ^ id ^ ", align 8\n"
    end
  | STORESPECIAL (id, t,typedef) ->
    begin

      let temp = TEMP (Temp.createeight()) in
      "\t" ^ format_operand temp ^ " = bitcast i32* " ^ format_operand t ^ " to " ^ typedef ^ "\n" ^
      "\tstore " ^ typedef ^ " " ^ format_operand temp ^ ", " ^ typedef ^ "* %" ^ id ^ ", align 8\n"
    end
  | ALLOC (id,typedef) ->
    begin
      match typedef with
      | "i32" ->
        "\t%" ^ id ^ " = alloca i32, align 4\n"
      | "i1" ->
        "\t%" ^ id ^ " = alloca i1, align 4\n"
      | "float" ->
        "\t%" ^ id ^ " = alloca float, align 4\n"
      | _ ->
        "\t%" ^ id ^ " = alloca " ^ typedef ^ ", align 8\n"
    end
  | JUSTWRITE s ->
    s
  | NOP -> ""
  | CALLRAISE ->
    "\tcall i32 @raise(i32 8)\n"


  | MOVETOADDR (d,s,typedef) ->

    (* let call_raise = EXTERNALCALL ("raise",0) in *)
    begin


      "\tstore " ^ typedef ^ " " ^ format_operand s ^ ", " ^ typedef ^ "* " ^ format_operand d ^ ", align 8\n"
    end


  | MOVETOADDRCHECK (d,s,typedef) ->

    (* let call_raise = EXTERNALCALL ("raise",0) in *)
    begin

      let t1 =TEMP (Temp.createeight()) in
      let raise1 = LABEL (Label.create()) in
      let dont1 = LABEL (Label.create()) in
      "\t" ^ format_operand t1 ^ " = icmp eq " ^ typedef ^ "* " ^ format_operand d ^ ", null\n" ^
      "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label raise1 ^ ":\n" ^
      "\tcall i32 @raise(i32 12)\n" ^
      "\tbr label %" ^ format_label dont1 ^ "\n" ^
      "\t" ^ format_label dont1 ^ ":\n" ^



      "\tstore " ^ typedef ^ " " ^ format_operand s ^ ", " ^ typedef ^ "* " ^ format_operand d ^ ", align 8\n"
    end




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
  (* | PASSARG (t,a,cnt) ->
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
    end *)
  | RETURN (t,typedef)->
    "\tret " ^ typedef ^ " " ^ format_operand t ^ "\n"
  | VOIDRETURN ->

    let test = LABEL (Label.create()) in
    "\tret void\n" ^
    format_label test ^ ":\n"



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

  | BINOP (oper, d, s1, s2,embed_type) ->
    begin
      match oper with
      | LES ->
        begin
          match embed_type with
          | "float" -> "\t" ^ format_operand d ^ " = fcmp olt " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
          | _ -> "\t" ^ format_operand d ^ " = icmp slt " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        end
      | LEQ ->
        begin
          match embed_type with
          | "float" -> "\t" ^ format_operand d ^ " = fcmp ole " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
          | _ -> "\t" ^ format_operand d ^ " = icmp sle " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        end
      | GRE ->
        begin
          match embed_type with
          | "float" -> "\t" ^ format_operand d ^ " = fcmp ogt " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
          | _ -> "\t" ^ format_operand d ^ " = icmp sgt " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        end
      | GEQ ->
        begin
          match embed_type with
          | "float" -> "\t" ^ format_operand d ^ " = fcmp oge " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
          | _ -> "\t" ^ format_operand d ^ " = icmp sge " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        end
      | IEQ ->
        begin
          match embed_type with
          | "float" -> "\t" ^ format_operand d ^ " = fcmp oeq " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
          | _ -> "\t" ^ format_operand d ^ " = icmp eq " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        end
      | NEQ ->
        begin
          match embed_type with
          | "float" -> "\t" ^ format_operand d ^ " = fcmp one " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
          | _ -> "\t" ^ format_operand d ^ " = icmp ne " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"
        end
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
        (match embed_type with
         | "float" -> ("\t" ^ format_operand t1 ^ " = fcmp oeq float " ^ format_operand s2 ^ ", 0.0\n" ^
                       "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
                       "\t" ^ format_label raise1 ^ ":\n" ^
                       "\tcall i32 @raise(i32 8)\n" ^
                       "\tbr label %" ^ format_label dont1 ^ "\n" ^
                       "\t" ^ format_label dont1 ^ ":\n" ^
                       "\t" ^ format_operand d ^ " = " ^ format_binop_float oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n")
         | _ -> ("\t" ^ format_operand t1 ^ " = icmp eq i32 " ^ format_operand s2 ^ ", 0\n" ^
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
                 "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"))

      | MOD ->
        let t1 = TEMP (Temp.createeight()) in
        let t2 = TEMP (Temp.createeight()) in
        let t3 = TEMP (Temp.createeight()) in
        let raise1 = LABEL (Label.create()) in
        let raise2 = LABEL (Label.create()) in
        let raise3 = LABEL (Label.create()) in
        let dont1 = LABEL (Label.create()) in
        let dont = LABEL (Label.create()) in
        (match embed_type with
         | "float" -> ("\t" ^ format_operand t1 ^ " = fcmp oeq float " ^ format_operand s2 ^ ", 0.0\n" ^
                       "\tbr i1 " ^ format_operand t1 ^ ", label %" ^ format_label raise1 ^ ", label %" ^ format_label dont1 ^ "\n" ^
                       "\t" ^ format_label raise1 ^ ":\n" ^
                       "\tcall i32 @raise(i32 8)\n" ^
                       "\tbr label %" ^ format_label dont1 ^ "\n" ^
                       "\t" ^ format_label dont1 ^ ":\n" ^
                       "\t" ^ format_operand d ^ " = " ^ format_binop_float oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n")
         | _ -> ("\t" ^ format_operand t1 ^ " = icmp eq i32 " ^ format_operand s2 ^ ", 0\n" ^
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
         "\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"))

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
        (match embed_type with
         | "float" -> ("\t" ^ format_operand d ^ " = " ^ format_binop_float oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n")
         | _ -> ("\t" ^ format_operand d ^ " = " ^ format_binop oper ^ " " ^ embed_type ^ " " ^ format_operand s1 ^ ", " ^ format_operand s2 ^ "\n"))


    end
