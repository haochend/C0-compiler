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

module T = Tree
module AS = Assem

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
let rec munch_exp d = function
  | T.NULL -> [AS.NULL d]
  | T.CONST n -> [AS.MOV (d, AS.IMM n)]
  | T.BOOL b -> [AS.MOV (d, AS.BOOL b)]
  | T.TEMP t -> [AS.MOV (d, AS.TEMP t)]
  | T.ARG a -> [AS.PASS (d, AS.ARGUEMENT a)]
  | T.BINOP (binop, e1, e2,t1,t2) ->
      munch_binop d (binop, e1, e2,t1,t2)
  | T.UNOP (unop, e,t1) ->
    munch_unop d (unop, e,t1)
  | T.QUESTION (e1, e2, e3,t1,t2,t3) ->
    munch_question d (e1, e2, e3,t1,t2)
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
  | T.INTERNALCALL (id,exps, temps, args,offset) ->
    begin
  let (prev1) = List.fold2_exn ~f:(fun (prev_instrs) e tem->
      begin
        (* let a = AS.ARGUEMENT (Arguement.create()) in *)
        let t = AS.TEMP tem in
        ((munch_exp t e)
         @ prev_instrs
         (* @ [AS.PUSH a]
         @ [AS.MOV (a,t)] *)
        (* , [AS.POP a]
          @after_instr *)
        )
      end) ~init:([]) exps temps in
  let tem_rev = List.rev(temps) in
  let (prev2,after) = List.fold2_exn ~f:(fun (prev_instrs, after_instrs) tem arg ->
      begin
        let t = AS.TEMP tem in
        let a = AS.ARGUEMENT arg in
        (prev_instrs
         (* @[AS.PUSH a] *)
         @[AS.MOVARGBEFORECALL (a,t)],
         (* [AS.POP a] *)
         after_instrs
        )
      end
    ) ~init:([],[]) tem_rev args in
      (* begin
        match Arguement.access() with
        | 0 -> 1
        | 1 ->  1
        | 2 -> 1
        | 3 -> 1
        | 4 -> 1
        | 5 -> 1
        | _ -> (Arguement.access()-5)
      end *)

    (* Arguement.reset(); *)
  prev1
  @prev2
  @[AS.ARGOFFSET (AS.REG AS.RSP,  offset)]
  @[AS.INTERNALCALL (id)]
  @[AS.ARGONSET (AS.REG AS.RSP, offset)]
  @after
  @[AS.MOV (d, AS.REG AS.EAX)]
  end
  | T.EXTERNALCALL (id,exps, temps, args,offset) ->
    begin
      let (prev1) = List.fold2_exn ~f:(fun (prev_instrs) e tem->
          begin
            (* let a = AS.ARGUEMENT (Arguement.create()) in *)
            let t = AS.TEMP tem in
            ((munch_exp t e)
             @ prev_instrs
             (* @ [AS.PUSH a]
             @ [AS.MOV (a,t)] *)
            (* , [AS.POP a]
              @after_instr *)
            )
          end) ~init:([]) exps temps in
      let tem_rev = List.rev(temps) in
      let (prev2,after) = List.fold2_exn ~f:(fun (prev_instrs, after_instrs) tem arg ->
          begin
            let t = AS.TEMP tem in
            let a = AS.ARGUEMENT arg in
            (prev_instrs
             (* @[AS.PUSH a] *)
             @[AS.MOVARGBEFORECALL (a,t)],
             (* [AS.POP a] *)
             after_instrs
            )
          end
        ) ~init:([],[]) tem_rev args in
  (* let offset = Arguement.access() *)
      (* let ( offset) = *)
        (* begin
          match Arguement.access() with
          | 0 -> 1
          | 1 ->  1
          | 2 -> 1
          | 3 -> 1
          | 4 -> 1
          | 5 -> 1
          | _ -> (Arguement.access()-5)
        end *)

      (* Arguement.reset(); *)
      prev1
      @prev2
      @[AS.ARGOFFSET (AS.REG AS.RSP,  offset)]
      @[AS.EXTERNALCALL (id,offset)]
      @[AS.ARGONSET (AS.REG AS.RSP, offset)]
      @after
      @[AS.MOV (d, AS.REG AS.EAX)]
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
  | T.MALLOC siz ->
    [AS.MALLOC (d,siz)]
  | T.DEREF (exp,t)->
    let t1 = AS.TEMP t in
    let l1 = AS.LABEL (Label.create()) in
    (munch_exp t1 exp)
    @ [AS.DOTDEREF (d,t1,l1)]
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

  (* | T.QUESTION (e1, e2, e3) ->
      munch_question d (e1, e2, e3) *)
(* munch_binop : AS.operand -> T.binop * T.exp * T.exp -> AS.instr list *)
(* munch_binop d (binop, e1, e2)
 * generates instruction to achieve d <- e1 binop e2
 * d must be TEMP(t) or REG(r)
 *)
and munch_binop d (binop, e1, e2, t3, t4) =
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
          munch_exp t1 e1
          @ [AS.CP (t1, l1, l3)]
          @ [AS.WRITELABEL (l1)]
          @ munch_exp t2 e2
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
          munch_exp t1 e1
          @ [AS.CP (t1, l3, l1)]
          @ [AS.WRITELABEL (l1)]
          @ munch_exp t2 e2
          @ [AS.CP (t2, l2, l3)]
          @ [AS.WRITELABEL (l2)]
          @ [AS.MOV (d, t2)]
          @ [AS.GOTO (l4)]
          @ [AS.WRITELABEL (l3)]
          @ [AS.MOV(d, t1)]
          @ [AS.WRITELABEL (l4)]
        end
      | _ ->
        munch_exp t1 e1
        @ munch_exp t2 e2
        @ [AS.BINOP (operator, d, t1, t2)]
    end

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
      [AS.BINOP (operator, d, t1, t2)]
  end


and munch_unop d (unop, e,t1) =
  let operator = as_unop unop
  and t = AS.TEMP t1 in
  munch_exp t e
  @ [AS.UNOP (operator, d, t)]
and munch_question d (e1, e2, e3,t3,t4) =
  let l1 = AS.LABEL (Label.create())
  and l2 = AS.LABEL (Label.create())
  and l3 = AS.LABEL (Label.create())
  and t1 = AS.TEMP t3
  and t2 = AS.TEMP t4 in
  munch_exp t1 e1
  @ [AS.CP (t1, l1, l2)]
  @ [AS.WRITELABEL ( l1)]
  @ munch_exp t2 e2
  @ [AS.GOTO (l3)]
  @ [AS.WRITELABEL (l2)]
  @ munch_exp t2 e3
  @ [AS.GOTO (l3)]
  @ [AS.WRITELABEL (l3)]
  @ [AS.MOV(d, t2)]
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
    @ [AS.MOVETOADDR (AS.TEMP s, AS.TEMP v_in_addr,l1)]


  | T.ADDRASNOPFOURBYTE (oper, s,exp,t,v_in_addr) ->
    let l1 = AS.LABEL (Label.create()) in
    munch_exp (AS.TEMP t) exp
    @ [AS.GETCONTENTFOURBYTE (AS.TEMP s, AS.TEMP v_in_addr)]
    @ munch_addr_asnop (AS.TEMP v_in_addr) (oper,v_in_addr,t)
    @ [AS.MOVETOADDRFOURBYTE (AS.TEMP s, AS.TEMP v_in_addr,l1)]

  (* | T.ASSIGNARR (base, index, arr_size_for_each_element, to_be_moved, ) *)


  | T.MOVE (T.TEMP t1, e2) ->
    munch_exp (AS.TEMP t1) e2
  | T.CREATEBASEADDR (s,d) ->
    [AS.MOV (AS.TEMP d, AS.TEMP s)]
  | T.MOVETOADDR (t,e, t1) ->
    let l1 = AS.LABEL (Label.create()) in
    munch_exp (AS.TEMP t1) e
    @ [AS.MOVETOADDR (AS.TEMP t, AS.TEMP t1,l1)]
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
  | T.IMPRETURN (e, cnt) ->
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
    munch_exp (AS.REG AS.EAX) e
    @[AS.STACKONSET (AS.REG AS.RSP, cnt)]
    (* @ pop_callee *)
    @ [AS.RETURN]
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
    [AS.STACKONSET (AS.REG AS.RSP, cnt)]
    (* @ pop_callee *)
    @[AS.RETURN]
  | T.RETURN e ->
      (* return e is implemented as %eax <- e *)
    munch_exp (AS.REG AS.EAX) e
    @[AS.POP (AS.REG AS.R15D)]
    @[AS.POP (AS.REG AS.R14D)]
    @[AS.POP (AS.REG AS.R13D)]
    @[AS.POP (AS.REG AS.R12D)]
    @[AS.POP (AS.REG AS.EBX)]
    @[AS.STACKONSET (AS.REG AS.RSP, 100)]
    @ [AS.RETURN]
  | T.VOIDRETURN ->
    [AS.POP (AS.REG AS.R15D)]
    @[AS.POP (AS.REG AS.R14D)]
    @[AS.POP (AS.REG AS.R13D)]
    @[AS.POP (AS.REG AS.R12D)]
    @[AS.POP (AS.REG AS.EBX)]
    @[AS.STACKONSET (AS.REG AS.RSP, 100)]
    @[AS.RETURN]
  | T.ASSERT (e, t1) ->
    let t= AS.TEMP t1
    and l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create()) in
    munch_exp t e
    @ [AS.CP (t,l1,l2)]
    @ [AS.WRITELABEL l2]
    @ [AS.FUNCTIONABORT]
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
    munch_exp t e1
    @ [AS.CP (t,l1,l2)]
    (* munch_exp t e1
    @ [AS.CP (t,l1, l2)]
    @ [AS.WRITELABEL l1]
    @ List.fold_left ~f:(fun a b -> a @ (munch_stm b)) ~init:([]) s1
    @ [AS.GOTO l3]
    @ [AS.WRITELABEL l2]
    @ List.fold_left ~f:(fun a b -> a @ (munch_stm b)) ~init:([]) s2
    @ [AS.GOTO l3]
    @ [AS.WRITELABEL l3] *)
  (* | T.WHILE (e, s,t1) ->
    let t= AS.TEMP t1
    and l1 = AS.LABEL (Label.create())
    and l2 = AS.LABEL (Label.create())
    and l3 = AS.LABEL (Label.create()) in
    [AS.WRITELABEL l1]
    @ munch_exp t e
    @ [AS.CP (t,l2,l3)]
    @ [AS.WRITELABEL l2]
    @ List.fold_left ~f:(fun a b -> a @ (munch_stm b)) ~init:([]) s
    @ [AS.GOTO l1]
     @ [AS.WRITELABEL l3] *)
  | T.GOTO label ->
    let l = AS.LABEL label in
    [AS.GOTO l]
  | T.FINISHWHILE (label1,label2) ->
    let l1 = AS.LABEL label1
    and l2 = AS.LABEL label2 in
    [AS.GOTO l1]
    @[AS.WRITELABEL l2]
  | T.NOP ->
    []
  | T.FUNCDEF (id, cnt)->
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
      [AS.FUNCSTART id]
    (* @ save_callee *)
    @ [AS.STACKOFFSET (AS.REG AS.RSP, cnt)]
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
    (* @ save_callee *)
    @ [AS.STACKOFFSET (AS.REG AS.RSP, cnt)]
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
