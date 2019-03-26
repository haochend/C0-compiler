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
  | T.CONST n -> [AS.MOV (d, AS.IMM n)]
  | T.BOOL b -> [AS.MOV (d, AS.BOOL b)]
  | T.TEMP t -> [AS.MOV (d, AS.TEMP t)]
  | T.ARG a -> [AS.PASS (d, AS.ARGUEMENT a)]
  | T.BINOP (binop, e1, e2,t1,t2) ->
      munch_binop d (binop, e1, e2,t1,t2)
  | T.UNOP (unop, e,t1) ->
    munch_unop d (unop, e,t1)
  | T.QUESTION (e1, e2, e3,t1,t2) ->
    munch_question d (e1, e2, e3,t1,t2)
  (* | T.PASSARG (a, e) ->
    munch_exp (AS.ARGUEMENT a) e *)
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
         @[AS.PUSH a]
         @[AS.MOV (a,t)],
         [AS.POP a]
         @after_instrs
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
             @[AS.PUSH a]
             @[AS.MOV (a,t)],
             [AS.POP a]
             @after_instrs
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
      (* @[AS.ARGOFFSET (AS.REG AS.RSP,  offset)] *)
      @[AS.EXTERNALCALL (id,offset)] 
      (* @[AS.ARGONSET (AS.REG AS.RSP, offset)] *)
      @after
      @[AS.MOV (d, AS.REG AS.EAX)]
    end
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
  | T.MOVE (T.TEMP t1, e2) -> munch_exp (AS.TEMP t1) e2
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
    let pop_callee =
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
    end in
    munch_exp (AS.REG AS.EAX) e
    @[AS.STACKONSET (AS.REG AS.RSP, cnt)]
    @ pop_callee
    @ [AS.RETURN]
  | T.IMPVOIDRETURN cnt ->
    (* let (Some offset) = (Int32.of_int cnt) in *)
    let pop_callee =
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
    end in
    [AS.STACKONSET (AS.REG AS.RSP, cnt)]
    @ pop_callee
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
    let save_callee =
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
    end in
    (* let (Some offset) = (Int32.of_int cnt) in *)
      [AS.FUNCSTART id]
    @ save_callee
    @ [AS.STACKOFFSET (AS.REG AS.RSP, cnt)]
  | T.MAINDEF (id, cnt)->
    (* let (Some offset) = (Int32.of_int cnt) in *)
    let save_callee =
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
    end in
    (* let (Some offset) = (Int32.of_int cnt) in *)
      [AS.MAINSTART id]
    @ save_callee
    @ [AS.STACKOFFSET (AS.REG AS.RSP, cnt)]
  | _ -> assert false

let rec codegen = function
  | [] -> []
  | stm::stms -> munch_stm stm @ codegen stms
