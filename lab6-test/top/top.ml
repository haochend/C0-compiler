(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
*)

open Core

module S = Symbol.Map
(*module DataLayout = Llvm_target.DataLayout
*)let say = prerr_endline

exception EXIT


(* Command line arguments *)

type opt_lvl = Opt_None | Opt_Regalloc | Opt_Full
type output = X86_64 | AbstractAssem | LLVM

type cmdline =
  { verbose : bool;
    dump_parsing : bool;
    dump_ast : bool;
    dump_ir : bool;
    dump_assem : bool;
    typecheck_only : bool;
    safe_mode : bool;
    output : output;
    opt_lvl : opt_lvl;
    filename : string;
    header_filename : string option
  }

let read_cmdline () =
  let open Getopt in
  try
    (* Defaults, set like this because this GetOpt package is imperative  *)
    let verbose = ref false in
    let dump_parsing = ref false in
    let dump_ast = ref false in
    let dump_ir = ref false in
    let dump_assem = ref false in
    let typecheck_only = ref false in
    let safe_mode = ref true in
    let emit = ref AbstractAssem in
    let opt = ref Opt_None in
    let filename = ref None in
    let header_filename = ref None in



    (* Getopt arguments *)
    (* See https://github.com/mcandre/ocaml-getopt/blob/master/getopt.mli *)
    let opts =
      [ ('v',     "verbose",        set verbose true,        None);
        (noshort, "dump-parsing",   set dump_parsing true,   None);
        (noshort, "dump-ast",       set dump_ast true,       None);
        (noshort, "dump-ir",        set dump_ir true,        None);
        (noshort, "dump-assem",     set dump_assem true,     None);
        ('t',     "typecheck-only", set typecheck_only true, None);
        (noshort, "safe",           set safe_mode true,      None);
        (noshort, "unsafe",         set safe_mode false,      None);
        ('e',     "emit",           None,
         Some (function "abs" -> emit := AbstractAssem
                      | "x86-64" -> emit := X86_64
                      | "llvm" -> emit := LLVM
                      | x -> say ("Error: Unknown --emit arg: "^x); raise EXIT));
        ('O',     "opt",            None,
         Some (function "0" -> opt := Opt_None
                      | "1" -> opt := Opt_Regalloc
                      | "2" -> opt := Opt_Full
                      | x -> say ("Error: Unknown --opt arg: "^x)));
        ('l',     nolong,           None,
         Some (function f -> match !header_filename with
             | None -> header_filename := Some f
             | Some s -> say "Error: more than one header file"; raise EXIT))] in
    let file_opt f =
      match !filename with
      | None -> filename := Some f
      | Some s -> say "Error: more than one input file"; raise EXIT in

    let () = parse_cmdline opts file_opt in

    { verbose = !verbose;
      dump_parsing = !dump_parsing;
      dump_ast = !dump_ast;
      dump_ir = !dump_ir;
      dump_assem = !dump_assem;
      typecheck_only = !typecheck_only;
      safe_mode = !safe_mode;
      output = !emit;
      opt_lvl = !opt;
      header_filename = !header_filename;
      filename = match !filename with
        | Some s -> s
        | None -> say "Error: no input file provided"; raise EXIT}
  with Error s -> say s; raise EXIT
let say_if flag s =
  if flag then say (s ()) else ()

let main cmd =
  try
    let source = cmd.filename
    and header = cmd.header_filename in


    (* Parse *)
    say_if cmd.verbose (fun () -> "Parsing... " ^ source);
    if cmd.dump_parsing then ignore (Parsing.set_trace true);

    let h_ast = (match header with
        | Some h -> Some (Parse.parse h)
        | None -> None) in
    let ast = Parse.parse source in


    say_if cmd.verbose (fun () -> "Checking...");

    let envs = TypeChecker.checkheader h_ast in
    TypeChecker.typecheck ast envs;

    if cmd.typecheck_only then exit 0;


    match cmd.output with
    | X86_64 ->
      begin

        say_if cmd.verbose (fun () -> "Translating...");
        let (_,init_func_env,init_struct_env,init_struct_detail_env,init_struct_size_env,init_typedef_env) =
          Trans.translate_header h_ast S.empty S.empty S.empty S.empty S.empty in
        let (ir,func_env,struct_env,struct_detail_env,struct_size_env,_,temp_cnt_list,inline_env) =
          Trans.translate_gdecl (Some ast) init_func_env init_struct_env init_struct_detail_env init_struct_size_env init_typedef_env [] cmd.safe_mode S.empty false in




        (* say_if cmd.dump_ir (fun () -> Tree.Print.pp_program ir); *)

        say_if cmd.verbose (fun () -> "Codegen...");
        let assem = (match cmd.safe_mode with
            | true ->
              Codegen.codegen ir
            | _ ->
              Unsacodegen.codegen ir
          ) in

        (* Codegen *)

        say_if cmd.verbose (fun () -> "Generating new assem...");


        let assem = (match cmd.opt_lvl with
            | Opt_None -> assem
            | Opt_Regalloc -> Prog_optimize.prog_optimize temp_cnt_list assem cmd.safe_mode true
            | Opt_Full -> Prog_optimize.prog_optimize temp_cnt_list assem cmd.safe_mode false) in

        let unfold_assem = List.fold_left ~f:(fun l e -> l@e) ~init:([]) assem in


        (* Output: abstract 3-address assembly *)
        (* match cmd.output with
           | x86_64 -> *)
        let full =
          [Assem.DIRECTIVE(".file\t\"" ^ source ^ "\"")]
          (*
	  @ [Assem.DIRECTIVE(".globl  _c0_main\n")]
          @ [Assem.DIRECTIVE("_c0_main:\n")]
          @ [Assem.DIRECTIVE("movq $11, %rdi\n")]
             @ [Assem.DIRECTIVE("callq raise\n")]*)
          @ unfold_assem
        in

        let code = (match cmd.safe_mode with
            | true -> String.concat (List.map full ~f:Assem.format)
            | _ -> String.concat (List.map full ~f:Assem.unsafe_format)) in

        let pfname = source ^ ".s" in
        say_if cmd.verbose (fun () -> "Writing abstract assembly to "
                                      ^ pfname ^ " ...");
        Out_channel.with_file pfname
          ~f:(fun pfstream -> Out_channel.output_string pfstream code);

        (* Done *)
        exit 0

      end

    | LLVM ->
      begin


        let (func_decl,init_func_env,init_struct_env,init_struct_detail_env,init_struct_size_env,init_typedef_env) =
          Llvmtrans.translate_header h_ast S.empty S.empty S.empty S.empty S.empty in
        say_if cmd.verbose (fun () -> "Translating...");
        let (ir,func_env,struct_env,struct_detail_env,struct_size_env,_,temp_cnt_list,inline_env, dec_func_env,all_struct_name_list) =
          Llvmtrans.translate_gdecl (Some ast) init_func_env init_struct_env init_struct_detail_env init_struct_size_env init_typedef_env [] cmd.safe_mode S.empty init_func_env [] in
        (* let ir = func_decl@ir in *)
        let all_struct_name_list = all_struct_name_list@["dub"] in
        (* let struct_size_env = S.set struct_size_env ~key:(Symbol.symbol "dub") ~data:(0) in *)
        let (opaque_defn,_) = List.fold_left ~f:(fun (l,new_struct_env) e ->
            (* printf "struct %s\n" e; *)
            begin
              match S.find new_struct_env (Symbol.symbol e) with
              | Some _ -> (l,new_struct_env)
              | None ->
                (* printf "opaque struct %s\n" e; *)
                let n = S.set new_struct_env ~key:(Symbol.symbol e) ~data:(0) in
                (l@[Llvmtree.OPAQUE e],n)
            end
          ) ~init:([],struct_size_env) all_struct_name_list in


        (* say_if cmd.dump_ir (fun () -> Tree.Print.pp_program ir); *)
        let ir = [opaque_defn]@ir in
        say_if cmd.verbose (fun () -> "Codegen...");
        let assem = (
          (* match cmd.safe_mode with
            | true ->
              Codegen.codegen ir
            | _ ->
              Unsacodegen.codegen ir *)

                Llvmcodegen.codegen ir
          ) in

        let func_decl_assem = Llvmcodegen.codegen func_decl in
        (* Codegen *)

        say_if cmd.verbose (fun () -> "Generating new assem...");

        let assem = List.fold_left ~f:(fun l e ->
            let (allocas,new_e) = List.fold_left ~f:(fun (allo,ne_e) e_insd ->
                match e_insd with
                | Llvmreadable.ALLOC (name,typedef) ->
                  (* printf "alloca %s\n" name; *)
                  (allo@[e_insd],ne_e)
                | _ ->
                  (allo,ne_e@[e_insd])
              ) ~init:([],[]) e in
            let new_assem = List.fold_left ~f:(fun final_l e_insd ->
                match e_insd with
                | Llvmreadable.MAINSTART (name) ->
                  (* printf "reach funcstart\n"; *)
                  final_l@[e_insd]@allocas
                | Llvmreadable.FUNCSTART (name,arg,return_type) ->
                  (* printf "reach funcstart\n"; *)
                      final_l@[e_insd]@allocas
                  | _ ->final_l@[e_insd]
                ) ~init:([]) new_e in
            l@[new_assem]
          ) ~init:([]) assem in


(*
        let assem = (match cmd.opt_lvl with
            | Opt_None -> assem
            | Opt_Regalloc -> Prog_optimize.prog_optimize temp_cnt_list assem cmd.safe_mode true
            | Opt_Full -> Prog_optimize.prog_optimize temp_cnt_list assem cmd.safe_mode false) in *)

        let unfold_assem = List.fold_left ~f:(fun l e -> l@e) ~init:([]) assem in
        let unfold_func_decl_assem = List.fold_left ~f:(fun l e -> l@e) ~init:([]) func_decl_assem in


        (* Output: abstract 3-address assembly *)
        (* match cmd.output with
           | x86_64 -> *)
        let full =
          [Llvmreadable.DIRECTIVE("; ModuleID = \"" ^ source ^ "\"")]
          (* @[Llvmreadable.DIRECTIVE("\n%struct.dub = type opaque\n")] *)
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @raise(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @abort()\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @fadd(i32,i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @fsub(i32,i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @fmul(i32,i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @fdiv(i32,i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i1 @fless(i32,i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @itof(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @ftoi(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @print_fpt(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @print_int(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @print_hex(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @print_dub(%struct.dub*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @abs(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @rand()\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @srand(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i8* @malloc(i64)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @strlen(i32*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare void @puts(i32*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare %struct.dub* @dadd(%struct.dub*,%struct.dub*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare %struct.dub* @dsub(%struct.dub*,%struct.dub*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare %struct.dub* @dmul(%struct.dub*,%struct.dub*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare %struct.dub* @ddiv(%struct.dub*,%struct.dub*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i1 @dless(%struct.dub*,%struct.dub*)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare %struct.dub* @itod(i32)\n")]
          @[Llvmreadable.DIRECTIVE("\ndeclare i32 @dtoi(%struct.dub*)\n")]
          @ unfold_func_decl_assem
          @ unfold_assem in


          (* dub dadd(dub x, dub y);
          dub dsub(dub x, dub y);
          dub dmul(dub x, dub y);
          dub ddiv(dub x, dub y);
          bool dless(dub x, dub y);

          dub itod(int n);
          int dtoi(dub x); *)


        let code = (match cmd.safe_mode with
            | true -> String.concat (List.map full ~f:Llvmreadable.format)
            | _ -> String.concat (List.map full ~f:Llvmreadable.unsafe_format)) in

        let pfname = source ^ ".ll" in
        say_if cmd.verbose (fun () -> "Writing abstract assembly to "
                                      ^ pfname ^ " ...");
        Out_channel.with_file pfname
          ~f:(fun pfstream -> Out_channel.output_string pfstream code);

        let comm = "opt -S -mem2reg -tailcallelim " ^ pfname ^ " -o " ^ pfname in
        (* Done *)
        let result = (match cmd.opt_lvl with
            | Opt_None ->
              0
              (* Sys.command comm *)
          | Opt_Full -> Sys.command comm) in
        exit result

      end;
    | _ -> ();
    (* Output assembly *)
    say "x86_64 or LLVM not implemented yet"; raise EXIT

              (* Output assembly *)
      say "x86_64 and llvm not implemented yet"; raise EXIT
  with
    ErrorMsg.Error -> say "Compilation failed"; exit 1
  | EXIT -> exit 1
  | e -> Out_channel.output_string stderr (Exn.to_string e); exit 1


let () = main (read_cmdline ())
