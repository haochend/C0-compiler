(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Simple typechecker that checks two properties:
 *  (1) If a variable is initialized, it has previously been declared.
 *  (2) If a variable is used, it has previously been initialized.
 * This is sufficient for now, since only int types are available in L1.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with modern spec.
 * Modified: Matt Bryant <mbryant@andrew.cmu.edu> Fall 2015
 * Handles undefined variables in unreachable code, significant simplifications
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 *
 *)

open Core

module A = Ast
module S = Symbol.Map

type init_status = Decl | Init

(* tc_exp : init_status Symbol.Map.t -> Ast.exp -> Mark.ext option -> unit *)
let rec tc_exp env ast ext =
  match ast with
  | A.Var id ->
    begin
      match S.find env id with
      | None -> ErrorMsg.error ext
          ("undeclared variable `" ^ Symbol.name id ^ "'");
        raise ErrorMsg.Error
      | Some Decl -> ErrorMsg.error ext
          ("uninitialized variable `" ^ Symbol.name id ^ "'") ;
          raise ErrorMsg.Error
      | Some Init -> ()
    end
  | A.ConstExp c -> ()
  | A.OpExp (oper,es) ->
      (* Note: it is syntactically impossible in this language to
       * apply an operator to an incorrect number of arguments
       * so we only check each of the arguments
       *)
      List.iter es ~f:(fun e -> tc_exp env e ext)
  | A.Marked marked_exp ->
      tc_exp env (Mark.data marked_exp) (Mark.ext marked_exp)

(* tc_stms :
 *   init_status Symbol.Map.t -> Ast.program -> Mark.ext option -> bool -> bool
 *
 * tc_stms env ast ext ret
 *   env: environment under which to consider the ast, where:
 *     find env id = Some Init if id is declared and initialized
 *     find env id = Some Decl if id is declared but not initialized
 *     find env id = None      if id is not declared
 *
 *   ast: the sequence of statements to typecheck.
 *   ext: the mark of the current statement, if available.
 *   ret: whether a return statement has already been encountered.
 *
 *   Returns a boolean indicating whether a return statement was encountered in
 *   the sequence of statements.
 *)
let rec tc_stms env ast ext ret =
  match ast with
  | [] -> ret
  | A.Declare (A.NewVar id) :: stms ->
    begin
      match S.find env id with
      | Some _ ->
          ErrorMsg.error None ("redeclared variable `" ^ Symbol.name id);
          raise ErrorMsg.Error
      | None -> tc_stms (S.set env ~key:id ~data:Decl) stms ext ret
    end
    (* The following translation is sound:
     *
     * int x = expr;  ===>   int x;
     *                       x = expr;
     *
     * This is because the expression can't legally contain the identifier x.
     * NB: This property will no longer hold when function calls are introduced.
     *)
  | A.Declare (A.Init (id, e)) :: stms ->
      tc_stms env (A.Declare (A.NewVar id) :: A.Assign (id, e) :: stms) ext ret
  | A.Assign (id, e) :: stms ->
    begin
      tc_exp env e ext;
      match S.find env id with
      | None ->
          ErrorMsg.error ext ("undeclared variable `" ^ Symbol.name id ^ "'");
          raise ErrorMsg.Error
        (* just got initialized *)
      | Some Decl -> tc_stms (S.set env ~key:id ~data:Init) stms ext ret
        (* already initialized *)
      | Some Init -> tc_stms env stms ext ret
    end
  | A.Return e :: stms ->
      tc_exp env e ext;
      (* Define all variables declared before return *)
      let env = S.map env ~f:(fun _ -> Init) in
      tc_stms env stms ext true
  | A.Markeds marked_stm :: stms ->
      tc_stms env (Mark.data marked_stm :: stms) (Mark.ext marked_stm) ret

let typecheck prog =
  if not (tc_stms S.empty prog None false)
  then begin
    ErrorMsg.error None "main does not return\n";
    raise ErrorMsg.Error
  end
