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

  | T.NULL ->
    ([],AS.NULLP)
  | T.VAR (id,typedefine) ->
    let t= Temp.createeight() in
    ([AS.VAR (id,AS.TEMP t,typedefine)],AS.TEMP t)
  | T.CONST n ->
    let t= Temp.createeight() in
    ([],AS.IMM n)
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
            if flag = 1 then
              (prev1,c_a ^ "i32 " ^ AS.format_operand t,0)
            else
              (prev1,c_a ^ ", i32 " ^ AS.format_operand t,0)

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
          | "srand" -> "void"
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
    (pre@[AS.LOAD (t, d,typedef)],d)
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
            @ [AS.CP (t1, l3, l1)]
            @ [AS.WRITELABEL (l2)]
            @ pre2
            @ [AS.STORE (temp_name,t2,"i1")]
            @ [AS.GOTO l3]
            @ [AS.WRITELABEL (l3)]
            @ [AS.VAR(temp_name, d,"i1")]

        | _ ->
          let (exp1,t1) = llvm_exp e1 in
          let (exp2,t2) = llvm_exp e2 in
          (exp1@exp2@[AS.BINOP (operator, d, t1, t2,typedef)])
  end

and llvm_unop d (unop, e) =
  let operator = as_unop unop in
  (* and t = AS.TEMP t1 in *)
  let (exp,t) = llvm_exp e in
  exp@[AS.UNOP (operator, d, t)]




let rec munch_exp d = function
  | T.VAR (id,typedef)->
    [AS.VAR (id, d,typedef)]
  | T.NULL -> [AS.NULL d]
  | T.CONST n -> [AS.MOV (d, AS.IMM n)]
  | T.BOOL b -> [AS.MOV (d, AS.BOOL b)]
  | T.TEMP t -> [AS.MOV (d, AS.TEMP t)]
  | T.ARG a -> [AS.PASS (d, AS.ARGUEMENT a)]
  | T.BINOP (binop, e1, e2,t1,t2,typedef) ->
    (* munch_binop d (binop, e1, e2) *)
    assert false
  | T.UNOP (unop, e,t1) ->
    (* munch_unop d (unop, e,t1) *)
    assert false
  | T.QUESTION (e1, e2, e3,t1,t2,t3,typedef) ->
    (* munch_question d (e1, e2, e3,t1,t2) *)
    assert false
  (* | T.PASSARG (a, e) ->
     munch_exp (AS.ARGUEMENT a) e *)
  | T.INLINEFUNCTIONCALL (func_id ,exps,t1,t2,t3,t4,t5,t6)->
    begin
      match func_id with
      | "leftrotate" ->
        let [c;x] = exps in
        munch_exp (AS.TEMP t1) x
        @ munch_exp (AS.TEMP t2) c
        @ [AS.INLINEFUNCTIONCALL (func_id, d, AS.TEMP t1,AS.TEMP t2,AS.TEMP t3,AS.TEMP t4,AS.TEMP t5,AS.TEMP t6)]
      | _ -> assert false
    end
  | T.INTERNALCALL (curr_id, exps,arg_type_list,return_type) ->
    begin
      assert false
    end
  | T.EXTERNALCALL (id,exps) ->
    begin
      assert false
    end
  | T.NULL ->
    [AS.NULL d]
  | T.DOT (exp, offset,t) ->
    let t1 = AS.TEMP t in
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let l3 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    if offset < upper then
      (munch_exp t1 exp)
      @ [AS.LEA (t1,offset,l1,l2)]
      @ [AS.DOTDEREF (d,t1,l3)]
    else
      [AS.CALLRAISE]

  | T.ADDRPREP (prev_exp, offset, t) ->
    let t1 = AS.TEMP t in
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    if offset < upper then
      (munch_exp t1 prev_exp)
      @ [AS.LEA (t1, offset,l1,l2)]
      @ [AS.MOV (d, t1)]
    else
      [AS.CALLRAISE]

  | T.RARROW (exp, offset,t1) ->
    let t11 = AS.TEMP t1 in
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let l3 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    if offset < upper then
      (munch_exp t11 exp)
      @ [AS.LEA (t11,offset,l1,l2)]
      @ [AS.DOTDEREF (d,t11,l3)]
    else
      [AS.CALLRAISE]
  | T.MALLOC (siz,typedef) ->
    assert false
  | T.DEREF (exp,t)->
    assert false
  | T.DONOTHING (exp,t) ->
    let t1 = AS.TEMP t in
    (munch_exp t1 exp)
  | T.ALLOCARRY (element_size, exp, t) ->
    let t1 = AS.TEMP t in
    let l1 = AS.LABEL (Label.create()) in
    (munch_exp t1 exp)
    @ [AS.ALLOCARRY (d,element_size, t1,l1)]
  | T.ARRDEREF (exp,index_exp,arr_size_for_each_element,t1,t2) ->
    let t11 = AS.TEMP t1 in
    let t12 = AS.TEMP t2 in
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let l3 = AS.LABEL (Label.create()) in
    (munch_exp t11 exp)
    @ (munch_exp t12 index_exp)
    @ [AS.ARRDEREF (d,t11,t12,arr_size_for_each_element,l1,l2,l3)]
  | T.ARRDONOTHING (base,index,arr_size_for_each_element,t1,t2) ->
    let t11 = AS.TEMP t1 in
    let t12 = AS.TEMP t2 in
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let l3 = AS.LABEL (Label.create()) in
    (munch_exp t11 base)
    @ (munch_exp t12 index)
    @ [AS.ARRDONOTHING (d,t11,t12,arr_size_for_each_element,l1,l2,l3)]


and munch_addr_asnop d (binop, t3,t4) =
  let operator = as_binop binop
  and t1 = AS.TEMP t3
  and t2 = AS.TEMP t4 in
  begin
    match operator with
    | AS.LAND ->
      begin
        let l1 = AS.LABEL (Label.create())
        and l2 = AS.LABEL (Label.create())
        and l3 = AS.LABEL (Label.create())
        and l4 = AS.LABEL (Label.create()) in
        [AS.CP (t1, l1, l3)]
        @ [AS.WRITELABEL (l1)]
        @ [AS.CP (t2, l3, l2)]
        @ [AS.WRITELABEL (l2)]
        @ [AS.MOV (d, t2)]
        @ [AS.GOTO (l4)]
        @ [AS.WRITELABEL (l3)]
        @ [AS.MOV(d, t1)]
        @ [AS.WRITELABEL (l4)]
      end
    | AS.LOR ->
      begin
        let l1 = AS.LABEL (Label.create())
        and l2 = AS.LABEL (Label.create())
        and l3 = AS.LABEL (Label.create())
        and l4 = AS.LABEL (Label.create()) in
        [AS.CP (t1, l3, l1)]
        @ [AS.WRITELABEL (l1)]
        @ [AS.CP (t2, l2, l3)]
        @ [AS.WRITELABEL (l2)]
        @ [AS.MOV (d, t2)]
        @ [AS.GOTO (l4)]
        @ [AS.WRITELABEL (l3)]
        @ [AS.MOV(d, t1)]
        @ [AS.WRITELABEL (l4)]
      end
    | _ ->
      [AS.BINOP (operator, d, t1, t2,Ast.INT)]
  end

   (*
and munch_if (e, s1, s2) =
  let l1 = AS.LABEL (Label.create())
  and l2 = AS.LABEL (Label.create())
  and l3 = AS.LABEL (Label.create()) in

  munch_exp e
  @ AS.IF
  @ AS.LABEL (l1, munch_stm s1, l3)
  @ AS.LABEL l3 *)


(* munch_stm : T.stm -> AS.instr list *)
(* munch_stm stm generates code to execute stm *)
let rec munch_stm = function
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
  | T.ALLOC (id,typedef) ->
    [AS.ALLOC (id,typedef)]
  | T.JUSTWRITE s ->
    [AS.JUSTWRITE s]
  | T.ARRADDRCOMP (s,base_exp,mult_exp,esize,t1,t2) ->
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let l3 = AS.LABEL (Label.create()) in
    munch_exp (AS.TEMP t1) base_exp
    @ munch_exp (AS.TEMP t2) mult_exp
    @ [AS.ARRADDRCOMP (AS.TEMP s, AS.TEMP t1, AS.TEMP t2,esize,l1,l2,l3)]
  | T.ADDRASNOP (oper, s,exp,t,v_in_addr) ->
    let l1 = AS.LABEL (Label.create()) in
    munch_exp (AS.TEMP t) exp
    @ [AS.GETCONTENT (AS.TEMP s, AS.TEMP v_in_addr)]
    @ munch_addr_asnop (AS.TEMP v_in_addr) (oper,v_in_addr,t)
    @ [AS.MOVETOADDR (AS.TEMP s, AS.TEMP v_in_addr,"tobedone")]


  | T.ADDRASNOPFOURBYTE (oper, s,exp,t,v_in_addr) ->
    let l1 = AS.LABEL (Label.create()) in
    munch_exp (AS.TEMP t) exp
    @ [AS.GETCONTENTFOURBYTE (AS.TEMP s, AS.TEMP v_in_addr)]
    @ munch_addr_asnop (AS.TEMP v_in_addr) (oper,v_in_addr,t)
    @ [AS.MOVETOADDRFOURBYTE (AS.TEMP s, AS.TEMP v_in_addr,l1)]

  (* | T.ASSIGNARR (base, index, arr_size_for_each_element, to_be_moved, ) *)


  | T.MOVE (T.TEMP t1, e2) ->
    assert false
  | T.CREATEBASEADDR (s,d) ->
    [AS.MOV (AS.TEMP d, AS.TEMP s)]
  | T.MOVETOADDR (dest,to_be_moved, typedef) ->
    (* let t1 = AS.TEMP (Temp.createeight()) in
       let t2= AS.TEMP (Temp.createeight()) in *)
    let (pre1,t1) = llvm_exp dest in
    let (pre2,t2) = llvm_exp to_be_moved in
    pre2
    @ pre1
    @ [AS.MOVETOADDR (t1, t2,typedef)]
  | T.MOVETOADDRFOURBYTE (t,e, t1) ->
    let l1 = AS.LABEL (Label.create()) in
    munch_exp (AS.TEMP t1) e
    @ [AS.MOVETOADDRFOURBYTE (AS.TEMP t, AS.TEMP t1,l1)]
  | T.MOVETOADDRSHORTER (t,offset) ->
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    if offset < upper then
      [AS.GETCONTENT (AS.TEMP t, AS.TEMP t)]
      @ [AS.LEA (AS.TEMP t,offset,l1,l2)]
    else
      [AS.CALLRAISE]
  | T.JUSTADDADDR (t,offset) ->
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    if offset < upper then
      [AS.LEA (AS.TEMP t,offset,l1,l2)]
    else
      [AS.CALLRAISE]
  | T.MOVEEXPTOADDR (prev_exp, offset, to_be_moved_exp, t1,t2) ->
    let l1 = AS.LABEL (Label.create()) in
    let l2 = AS.LABEL (Label.create()) in
    let upper = Int.pow 2 31 in
    let Some offset_to = Int32.of_int offset in
    if offset < upper then
      begin
        match to_be_moved_exp with
        | T.CONST c ->
          munch_exp (AS.TEMP t1) prev_exp
          @ [AS.MOVEEXPTOADDR (AS.TEMP t1, AS.IMM offset_to, AS.IMM c,l1,l2)]
        | _ ->
          munch_exp (AS.TEMP t1) prev_exp
          @ munch_exp (AS.TEMP t2) to_be_moved_exp
          @ [AS.MOVEEXPTOADDR (AS.TEMP t1, AS.IMM offset_to, AS.TEMP t2,l1,l2)]
      end
    else
      [AS.CALLRAISE]

  | T.PASSARG (T.TEMP t1, T.BEGINARG a1) ->
    assert false
  | T.PASSARGTRUE (T.TEMP t1, T.BEGINARG a1, argcnt) ->
    let (Some cnt) = Int32.of_int argcnt in
    [AS.PASSARG (AS.TEMP t1, AS.BEGINARG a1, AS.IMM cnt)]
  (* begin
     match cnt with
     | 1 -> [AS.PUSH (AS.REG AS.EBX)]
     | 2 -> [AS.PUSH (AS.REG AS.EBX)]
           @ [AS.PUSH (AS.REG AS.R12D)]
     | 3 -> [AS.PUSH (AS.REG AS.EBX)]
           @ [AS.PUSH (AS.REG AS.R12D)]
           @ [AS.PUSH (AS.REG AS.R13D)]
     | 4 -> [AS.PUSH (AS.REG AS.EBX)]
           @ [AS.PUSH (AS.REG AS.R12D)]
           @ [AS.PUSH (AS.REG AS.R13D)]
           @ [AS.PUSH (AS.REG AS.R14D)]
     | _ -> [AS.PUSH (AS.REG AS.EBX)]
           @ [AS.PUSH (AS.REG AS.R12D)]
           @ [AS.PUSH (AS.REG AS.R13D)]
           @ [AS.PUSH (AS.REG AS.R14D)]
           @ [AS.PUSH (AS.REG AS.R15D)]
     end *)
  | T.IMPRETURN (e, typedef) ->
    (* let (Some offset) = (Int32.of_int cnt) in *)
    (* let pop_callee =
       begin
       match cnt with
       | 0 -> []
       | 1 -> [AS.POP (AS.REG AS.EBX)]
       | 2 -> [AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       | 3 -> [AS.POP (AS.REG AS.R13D)]
             @[AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       | 4 -> [AS.POP (AS.REG AS.R14D)]
             @[AS.POP (AS.REG AS.R13D)]
             @[AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       | _ -> [AS.POP (AS.REG AS.R15D)]
             @[AS.POP (AS.REG AS.R14D)]
             @[AS.POP (AS.REG AS.R13D)]
             @[AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       end in *)
    begin
        let (pre,t) = llvm_exp e in
        (* @ pop_callee *)
        pre@[AS.RETURN (t,typedef)]
    end


  | T.IMPVOIDRETURN cnt ->
    (* let (Some offset) = (Int32.of_int cnt) in *)
    (* let pop_callee =
       begin
       match cnt with
       | 0 -> []
       | 1 -> [AS.POP (AS.REG AS.EBX)]
       | 2 -> [AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       | 3 -> [AS.POP (AS.REG AS.R13D)]
             @[AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       | 4 -> [AS.POP (AS.REG AS.R14D)]
             @[AS.POP (AS.REG AS.R13D)]
             @[AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       | _ -> [AS.POP (AS.REG AS.R15D)]
             @[AS.POP (AS.REG AS.R14D)]
             @[AS.POP (AS.REG AS.R13D)]
             @[AS.POP (AS.REG AS.R12D)]
             @[AS.POP (AS.REG AS.EBX)]
       end in *)
    (* @ pop_callee *)
    [AS.VOIDRETURN]
  | T.RETURN (e,typedef) ->
    assert false
  | T.VOIDRETURN ->
    assert false
  | T.NOP ->
    []
  | T.FUNCDEF (id, arg,return_type)->
    (* let save_callee =
       begin
       match cnt with
       | 0 -> []
       | 1 -> [AS.PUSH (AS.REG AS.EBX)]
       | 2 -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
       | 3 -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
             @ [AS.PUSH (AS.REG AS.R13D)]
       | 4 -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
             @ [AS.PUSH (AS.REG AS.R13D)]
             @ [AS.PUSH (AS.REG AS.R14D)]
       | _ -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
             @ [AS.PUSH (AS.REG AS.R13D)]
             @ [AS.PUSH (AS.REG AS.R14D)]
             @ [AS.PUSH (AS.REG AS.R15D)]
       end in *)
    (* let (Some offset) = (Int32.of_int cnt) in *)
    [AS.FUNCSTART (id,arg,return_type)]
  | T.MAINDEF (id, cnt)->
    (* let (Some offset) = (Int32.of_int cnt) in *)
    (* let save_callee =
       begin
       match cnt with
       | 0 -> []
       | 1 -> [AS.PUSH (AS.REG AS.EBX)]
       | 2 -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
       | 3 -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
             @ [AS.PUSH (AS.REG AS.R13D)]
       | 4 -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
             @ [AS.PUSH (AS.REG AS.R13D)]
             @ [AS.PUSH (AS.REG AS.R14D)]
       | _ -> [AS.PUSH (AS.REG AS.EBX)]
             @ [AS.PUSH (AS.REG AS.R12D)]
             @ [AS.PUSH (AS.REG AS.R13D)]
             @ [AS.PUSH (AS.REG AS.R14D)]
             @ [AS.PUSH (AS.REG AS.R15D)]
       end in *)
    (* let (Some offset) = (Int32.of_int cnt) in *)
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
