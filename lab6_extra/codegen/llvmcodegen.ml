(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *
 * Implements a "convenient munch" algorithm
*)

open Core

module T = Llvmtree
module AS = Llvmreadable


let as_binop = function
  (* | T.ADD -> AS.ADD
     | T.SUB -> AS.SUB
     | T.MUL -> AS.MUL
     | T.DIV -> AS.DIV

     | T.MOD -> AS.MOD *)
  | T.ADD -> AS.ADD
  | T.SUB -> AS.SUB
  | T.MUL -> AS.MUL
  | T.DIV -> AS.DIV
  | T.MOD -> AS.MOD
  | T.LES -> AS.LES
  | T.LEQ -> AS.LEQ
  | T.GRE -> AS.GRE
  | T.GEQ -> AS.GEQ
  | T.IEQ -> AS.IEQ
  | T.NEQ -> AS.NEQ
  | T.LAND -> AS.LAND
  | T.LOR -> AS.LOR
  | T.BAND -> AS.BAND
  | T.BXOR -> AS.BXOR
  | T.BOR -> AS.BOR
  | T.SLEFT -> AS.SLEFT
  | T.SRIGHT -> AS.SRIGHT
let as_unop = function
  | T.LNOT -> AS.LNOT
  | T.BNOT -> AS.BNOT
  | T.NEGA -> AS.NEGA
(* munch_exp : AS.operand -> T.exp -> AS.instr list *)
(* munch_exp d e
 * generates instructions to achieve d <- e
 * d must be TEMP(t) or REG(r)
*)
let rec llvm_exp = function

  | T.RARROWWITHOUTCHECKANDLOAD (sid,exp, offset,typedef) ->
    let (pre,t) = llvm_exp exp in
    let d = AS.TEMP (Temp.createeight()) in
    (pre@[AS.RARROWWITHOUTCHECKANDLOAD (sid,d,t,offset,typedef)],d)
  | T.RARROWWITHOUTCHECK (sid,exp, offset,typedef) ->
    let (pre,t) = llvm_exp exp in
    let d = AS.TEMP (Temp.createeight()) in
    (pre@[AS.RARROWWITHOUTCHECK (sid,d,t,offset,typedef)],d)

  | T.RARROWWITHOUTLOAD (sid,exp, offset,typedef) ->
    let (pre,t) = llvm_exp exp in
    let d = AS.TEMP (Temp.createeight()) in
    (pre@[AS.RARROWWITHOUTLOAD (sid,d,t,offset,typedef)],d)
  | T.RARROW (sid,exp, offset,typedef) ->
    let (pre,t) = llvm_exp exp in
    let d = AS.TEMP (Temp.createeight()) in
    (pre@[AS.RARROW (sid,d,t,offset,typedef)],d)
  | T.NULL ->
    ([],AS.NULLP)
  | T.VAR (id,typedefine) ->
    let t= Temp.createeight() in
    ([AS.VAR (id,AS.TEMP t,typedefine)],AS.TEMP t)
  | T.CONST n ->
    let t= Temp.createeight() in
    ([],AS.IMM n)
  | T.FCONST f ->
    let t= Temp.createeight() in
    ([],AS.FIMM f)
  | T.BOOL b ->
    ([], AS.BOOL b)
  | T.TEMP t ->
    ([], AS.TEMP t)
  | T.BINOP (binop,e1,e2,t1,t2,typedef) ->
    let t = AS.TEMP (Temp.createeight()) in
    let bino= llvm_binop (t) (binop, e1,e2,typedef) in
    (bino,t)
  | T.UNOP (unop,e,t1) ->
    let t = AS.TEMP (Temp.createeight()) in
    let exp = llvm_unop t (unop,e) in
    (exp,t)
  | T.QUESTION (e1, e2, e3,t1,t2,t3,typedef) ->
    let l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create())
    and l3 = AS.LABEL (Label.create()) in
    let temp = (Temp.createeight()) in
    let temp_name = "t" ^ (Temp.name temp) in
    let (pre1,t1) = llvm_exp e1
    and (pre2,t2) = llvm_exp e2
    and (pre3,t3) = llvm_exp e3 in
    let d = AS.TEMP (Temp.createeight()) in
    (
      [AS.ALLOC (temp_name, typedef)]
      @ pre1
      @ [AS.CP (t1, l1, l2)]
      @ [AS.WRITELABEL (l1)]
      @ pre2
      @ [AS.STORE (temp_name,t2,typedef)]
      @ [AS.GOTO (l3)]
      @ [AS.WRITELABEL (l2)]
      @ pre3
      @ [AS.STORE (temp_name,t3,typedef)]
      @ [AS.GOTO (l3)]
      @ [AS.WRITELABEL (l3)]
      @ [AS.VAR(temp_name, d,typedef)],d)
  | T.INTERNALCALL (curr_id, exps,arg_type_list,return_type) ->
    begin
      let (prev1,call_arg,_) = List.fold2_exn ~f:(fun (prev_instrs,c_a,flag) e each_arg_type->
          begin
            (* let a = AS.ARGUEMENT (Arguement.create()) in *)
            let (pre,t) = llvm_exp e in
            let prev1 = prev_instrs@pre in
            if flag = 1 then
              (prev1,c_a ^ each_arg_type ^ " " ^ AS.format_operand t,0)
            else
              (prev1,c_a ^ ", " ^ each_arg_type ^ " " ^ AS.format_operand t,0)

          end
        ) ~init:([],"",1) exps arg_type_list in
      let d = AS.TEMP (Temp.createeight()) in
      (prev1
       @[AS.INTERNALCALL (d,curr_id,return_type,call_arg)],d)
    end
  | T.EXTERNALCALL (curr_id, exps) ->
    begin
      let (prev1,call_arg,_) = List.fold_left ~f:(fun (prev_instrs,c_a,flag) e->
          begin
            (* let a = AS.ARGUEMENT (Arguement.create()) in *)
            let (pre,t) = llvm_exp e in
            let prev1 = prev_instrs@pre in
            begin
              match curr_id with
              | "strlen" ->
                if flag = 1 then
                  (prev1,c_a ^ "i32* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", i32* " ^ AS.format_operand t,0)
              | "puts" ->
                if flag = 1 then
                  (prev1,c_a ^ "i32* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", i32* " ^ AS.format_operand t,0)
              | "dadd" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "dsub" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "dmul" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "ddiv" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "dless" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "dtoi" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "print_dub" ->
                if flag = 1 then
                  (prev1,c_a ^ "%struct.dub* " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", %struct.dub* " ^ AS.format_operand t,0)
              | "print_float" ->
                if flag = 1 then
                  (prev1,c_a ^ "float " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", float " ^ AS.format_operand t,0)
              | "fti" ->
                if flag = 1 then
                  (prev1,c_a ^ "float " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", float " ^ AS.format_operand t,0)
              | _ ->
                if flag = 1 then
                  (prev1,c_a ^ "i32 " ^ AS.format_operand t,0)
                else
                  (prev1,c_a ^ ", i32 " ^ AS.format_operand t,0)
            end

          end
        ) ~init:([],"",1) exps in
      let d = AS.TEMP (Temp.createeight()) in
      let return_type =
        begin
          match curr_id with
          | "fless" -> "i1"
          | "print_fpt" -> "void"
          | "print_int" -> "void"
          | "print_hex" -> "void"
          | "print_dub" -> "void"
          | "print_float" -> "void"
          | "fti" -> "i32"
          | "srand" -> "void"
          | "puts" -> "void"
          | "dadd" -> "%struct.dub*"
          | "dsub" -> "%struct.dub*"
          | "dmul" -> "%struct.dub*"
          | "ddiv" -> "%struct.dub*"
          | "dless" -> "i1"
          | "itod" -> "%struct.dub*"
          | _ -> "i32"


        end in
      let d = AS.TEMP (Temp.createeight()) in
      (prev1
       @[AS.EXTERNALCALL (d,curr_id,return_type,call_arg)],d)
    end
  | T.MALLOC (siz,typedef) ->
    let d = AS.TEMP (Temp.createeight()) in
    ([AS.MALLOC (d,siz,typedef)],d)

  | T.DEREF (exp,typedef)->
    let d = AS.TEMP (Temp.createeight()) in
    let (pre,t) = llvm_exp exp in
    (pre@[AS.LOADCHECK (t, d,typedef)],d)

  | T.ALLOCARRY (element_size, exp, typedef) ->
    let d = AS.TEMP (Temp.createeight()) in
    let (pre,t) = llvm_exp exp in
    (pre@[AS.ALLOCARRY (d,element_size,t,typedef)],d)

  | T.ARRDEREF (base,index,typedef) ->
    let d = AS.TEMP (Temp.createeight()) in
    let (pre_base,t_base) = llvm_exp base in
    let (pre_index,t_index) = llvm_exp index in
    (pre_base@pre_index@[AS.ARRDEREF (d,t_base, t_index, typedef)],d)
  | T.ARRDONOTHING (base,index,typedef) ->
    let d = AS.TEMP (Temp.createeight()) in
    let (pre_base,t_base) = llvm_exp base in
    let (pre_index,t_index) = llvm_exp index in
    (pre_base@pre_index@[AS.ARRDONOTHING (d,t_base, t_index, typedef)],d)


and munch_addr_asnop (binop, d_s,t_compu,typedef) =
  let operator = as_binop binop in
  begin
    match operator with
    | AS.LAND ->
      let l1 = AS.LABEL (Label.create())
      and l2 = AS.LABEL (Label.create())
      and l3 = AS.LABEL (Label.create()) in
      let t1 = AS.TEMP (Temp.createeight()) in
      let temp = (Temp.createeight()) in
      let temp_name = "t" ^ (Temp.name temp) in
      let temp = AS.TEMP temp in
      [AS.LOADCHECK (d_s,temp,typedef)]
      @ [AS.ALLOC (temp_name, "i1")]
      @ [AS.GOTO l1]
      @ [AS.WRITELABEL (l1)]
      @ [AS.MOVETOADDR (temp,t1,"i1")]
      @ [AS.CP (t1, l2, l3)]
      @ [AS.WRITELABEL (l2)]
      @ [AS.MOVETOADDR (temp,t_compu,"i1")]
      @ [AS.GOTO (l3)]
      @ [AS.WRITELABEL (l3)]
      @ [AS.MOVETOADDR (temp, d_s,"i1")]
    | AS.LOR ->
      let l1 = AS.LABEL (Label.create())
      and l2 = AS.LABEL (Label.create())
      and l3 = AS.LABEL (Label.create()) in
      let t1 = AS.TEMP (Temp.createeight()) in
      let temp = (Temp.createeight()) in
      let temp_name = "t" ^ (Temp.name temp) in
      let temp = AS.TEMP temp in
      [AS.LOADCHECK (d_s,temp,typedef)]
      @ [AS.ALLOC (temp_name, "i1")]
      @ [AS.GOTO l1]
      @ [AS.WRITELABEL (l1)]
      @ [AS.MOVETOADDR (temp,t1,"i1")]
      @ [AS.CP (t1, l3, l2)]
      @ [AS.WRITELABEL (l2)]
      @ [AS.MOVETOADDR (temp,t_compu,"i1")]
      @ [AS.GOTO l3]
      @ [AS.WRITELABEL (l3)]
      @ [AS.MOVETOADDR (temp, d_s,"i1")]
    | _ ->
      let temp = AS.TEMP (Temp.createeight()) in
      let temp2 = AS.TEMP (Temp.createeight()) in
      [AS.LOADCHECK (d_s,temp,typedef)]
      @ [AS.BINOP (operator, temp2, temp, t_compu,typedef)]
      @ [AS.MOVETOADDR (d_s,temp2,typedef)]
  end



and llvm_binop d (binop, e1, e2,typedef) =
  let operator = as_binop binop in
  begin
    match operator with
    | AS.LAND ->
      let l1 = AS.LABEL (Label.create())
      and l2 = AS.LABEL (Label.create())
      and l3 = AS.LABEL (Label.create()) in
      let (pre1,t1) = llvm_exp e1
      and (pre2,t2) = llvm_exp e2 in
      let temp = (Temp.createeight()) in
      let temp_name = "t" ^ (Temp.name temp) in
      [AS.ALLOC (temp_name, "i1")]
      @ [AS.GOTO l1]
      @ [AS.WRITELABEL (l1)]
      @ pre1
      @ [AS.STORE (temp_name,t1,"i1")]
      @ [AS.CP (t1, l2, l3)]
      @ [AS.WRITELABEL (l2)]
      @ pre2
      @ [AS.STORE (temp_name,t2,"i1")]
      @ [AS.GOTO (l3)]
      @ [AS.WRITELABEL (l3)]
      @ [AS.VAR(temp_name, d,"i1")]
    | AS.LOR ->
      let l1 = AS.LABEL (Label.create())
      and l2 = AS.LABEL (Label.create())
      and l3 = AS.LABEL (Label.create()) in
      let (pre1,t1) = llvm_exp e1
      and (pre2,t2) = llvm_exp e2 in
      let temp = (Temp.createeight()) in
      let temp_name = "t" ^ (Temp.name temp) in
      [AS.ALLOC (temp_name, "i1")]
      @ [AS.GOTO l1]
      @ [AS.WRITELABEL (l1)]
      @ pre1
      @ [AS.STORE (temp_name,t1,"i1")]
      @ [AS.CP (t1, l3, l2)]
      @ [AS.WRITELABEL (l2)]
      @ pre2
      @ [AS.STORE (temp_name,t2,"i1")]
      @ [AS.GOTO l3]
      @ [AS.WRITELABEL (l3)]
      @ [AS.VAR(temp_name, d,"i1")]

    | _ ->
      let (exp1,t1) = llvm_exp e1 in
      let (exp2,t2) = llvm_exp e2 in
      begin
        match t1,t2 with
        | AS.NULLP, AS.NULLP ->
          let temp = (Temp.createeight()) in
          let temp_name = "t" ^ (Temp.name temp) in
          [AS.ALLOC (temp_name, "i1")]
          @ [AS.STORE (temp_name,AS.IMM (Int32.one),"i1")]
          @ [AS.LOAD (AS.TEMP temp, d, "i1")]
        | AS.NULLP,_ ->
          (exp1@exp2@[AS.BINOP (operator, d, t2, t1,typedef)])

        | _, _ ->
          (exp1@exp2@[AS.BINOP (operator, d, t1, t2,typedef)])
      end
  end

and llvm_unop d (unop, e) =
  let operator = as_unop unop in
  (* and t = AS.TEMP t1 in *)
  let (exp,t) = llvm_exp e in
  exp@[AS.UNOP (operator, d, t)]


let rec munch_stm = function
  | T.OPAQUE s ->
    [AS.JUSTWRITE ("%struct." ^s ^ " = type opaque\n")]
  | T.ARRASNOP (oper,base,index,to_be_compute,typedef) ->
    let (pre_base,t_base) = llvm_exp base in
    let (pre_index,t_index) = llvm_exp index in
    let (pre_to,t_to) = llvm_exp to_be_compute in
    let d = AS.TEMP (Temp.createeight()) in
    pre_base@pre_index@[AS.ARRDONOTHING (d,t_base,t_index,typedef)]@pre_to
    @ munch_addr_asnop (oper,d,t_to,typedef)
  | T.DEREFASNOP (oper,pointer,to_be_compute,typedef) ->
    let (pre_poin,t_poin) = llvm_exp pointer in
    let (pre_compu,t_compu) = llvm_exp to_be_compute in
    pre_poin
    @ pre_compu
    @ munch_addr_asnop (oper,t_poin,t_compu,typedef)
  | T.DOTASNOP (oper,sid,base,offset,typedef,to_be_compute) ->
    let (pre_base,t_base) = llvm_exp base in
    let (pre_compu,t_compu) = llvm_exp to_be_compute in
    let d = AS.TEMP (Temp.createeight()) in
    pre_compu
    @ pre_base
    @ [AS.GETSTRUADDR (d,sid,t_base,offset,typedef)]
    @ munch_addr_asnop (oper,d,t_compu,typedef)

  | T.ASSIGNARR (base,index,to_be_moved,typedef) ->
    let (pre_base,t_base) = llvm_exp base in
    let (pre_index,t_index) = llvm_exp index in
    let (pre_to,t_to) = llvm_exp to_be_moved in
    let d = AS.TEMP (Temp.createeight()) in
    (pre_base@pre_index@[AS.ASSIGNARR (t_base, t_index, d ,typedef)]@pre_to@[AS.MOVETOADDR (d,t_to,typedef)])


  | T.DOTASSIGNCHECK (sid,base,offset,to_be_moved,typedef) ->
    let (pre1,t1) = llvm_exp base in
    let (pre2,t2) = llvm_exp to_be_moved in
    let d = AS.TEMP (Temp.createeight()) in
    pre1
    @[AS.DOTASSIGNCHECK (sid,t1,offset,d,typedef)]
    @pre2
    @[AS.MOVETOADDR (d,t2,typedef)]
  | T.DOTASSIGN (sid,base,offset,to_be_moved,typedef) ->
    let (pre1,t1) = llvm_exp base in
    let (pre2,t2) = llvm_exp to_be_moved in
    pre2
    @pre1
    @[AS.DOTASSIGN (sid,t1,offset,t2,typedef)]
  | T.STRUCTDEFN (id,stru_str) ->
    [AS.STRUCTDEFN (id,stru_str)]
  | T.STOREARG (arg_name,counter,typedef) ->
    begin
      [AS.STOREARG (arg_name,counter,typedef)]
    end
  | T.ASSERT (e, t1) ->
    let l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create()) in
    let (pre,t) = llvm_exp e in
    pre
    @ [AS.CP (t,l1,l2)]
    @ [AS.WRITELABEL l2]
    @ [AS.FUNCTIONABORT]
    @ [AS.GOTO l1]
    @ [AS.WRITELABEL l1]
  | T.WRITELABEL label1 ->
    let l1 = AS.LABEL label1 in
    [AS.WRITELABEL l1]
  | T.FINISHIF label1 ->
    let l1 = AS.LABEL label1 in
    [AS.GOTO l1]
    @[AS.WRITELABEL l1]
  | T.IF (e1, label1, label2,t1) ->
    let t= AS.TEMP t1
    and l1 = AS.LABEL label1
    and l2 = AS.LABEL label2 in
    let (pre,t) = llvm_exp e1 in
    pre@[AS.CP (t,l1,l2)]
  | T.GOTO label ->
    let l = AS.LABEL label in
    [AS.GOTO l]
  | T.FINISHWHILE (label1,label2) ->
    let l1 = AS.LABEL label1
    and l2 = AS.LABEL label2 in
    [AS.GOTO l1]
    @[AS.WRITELABEL l2]
  | T.MAINEND ->
    [AS.JUSTWRITE "\tret i32 0\n}\n"]
  | T.FUNCEND typedef->
    begin
      [AS.FUNCEND typedef]
    end
  | T.JUSTCOMPUTE (t,e) ->
    let (pre,t1) = llvm_exp e in
    pre
  | T.STORE (id, e,typedef) ->
    let (pre,t) = llvm_exp e in
    begin
      pre@[AS.STORE (id, t,typedef)]
    end
  | T.STORESPECIAL (id, e,typedef) ->
    let (pre,t) = llvm_exp e in
    begin
      pre@[AS.STORESPECIAL (id, t,typedef)]
    end
  | T.ALLOC (id,typedef) ->
    [AS.ALLOC (id,typedef)]
  | T.JUSTWRITE s ->
    [AS.JUSTWRITE s]


  | T.CREATEBASEADDR (s,d) ->
    [AS.MOV (AS.TEMP d, AS.TEMP s)]
  | T.MOVETOADDR (dest,to_be_moved, typedef) ->
    (* let t1 = AS.TEMP (Temp.createeight()) in
       let t2= AS.TEMP (Temp.createeight()) in *)
    let (pre1,t1) = llvm_exp dest in
    let (pre2,t2) = llvm_exp to_be_moved in
    pre1
    @ pre2
    @ [AS.MOVETOADDRCHECK (t1, t2,typedef)]
  | T.JUSTADDADDR (t,offset) ->
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    if offset < upper then
      [AS.LEA (AS.TEMP t,offset,l1,l2)]
    else
      [AS.CALLRAISE]

  | T.PASSARG (T.TEMP t1, T.BEGINARG a1) ->
    assert false
  | T.PASSARGTRUE (T.TEMP t1, T.BEGINARG a1, argcnt) ->
    let (Some cnt) = Int32.of_int argcnt in
    [AS.PASSARG (AS.TEMP t1, AS.BEGINARG a1, AS.IMM cnt)]
  | T.IMPRETURN (e, typedef) ->
    begin
      let (pre,t) = llvm_exp e in
      (* @ pop_callee *)
      pre@[AS.RETURN (t,typedef)]
    end


  | T.IMPVOIDRETURN cnt ->
    [AS.VOIDRETURN]
  | T.RETURN (e,typedef) ->
    assert false
  | T.VOIDRETURN ->
    assert false
  | T.NOP ->
    []
  | T.FUNCDEF (id, arg,return_type)->
    [AS.FUNCSTART (id,arg,return_type)]
  | T.MAINDEF (id, cnt)->
    [AS.MAINSTART id]
  | _ -> assert false


let rec codegen_func = function
  | [] -> []
  | stm::stms -> munch_stm stm @ codegen_func stms

let rec codegen = function
  | [[]] -> [[]]
  | func::funcs -> [codegen_func func] @ (codegen funcs)




(* let rec codegen = function
   | [] -> []
   | stm::stms -> munch_stm stm @ codegen stms *)
