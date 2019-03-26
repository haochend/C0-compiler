(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
*)

open Core

let say = prerr_endline

exception EXIT

(* Command line arguments *)

type opt_lvl = Opt_None
type output = X86_64 | AbstractAssem

type cmdline =
  { verbose : bool;
    dump_parsing : bool;
    dump_ast : bool;
    dump_ir : bool;
    dump_assem : bool;
    typecheck_only : bool;
    output : output;
    opt_lvl : opt_lvl;
    filename : string }

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
    let emit = ref AbstractAssem in
    let opt = ref Opt_None in
    let filename = ref None in

    (* Getopt arguments *)
    (* See https://github.com/mcandre/ocaml-getopt/blob/master/getopt.mli *)
    let opts =
      [ ('v',     "verbose",        set verbose true,        None);
        (noshort, "dump-parsing",   set dump_parsing true,   None);
        (noshort, "dump-ast",       set dump_ast true,       None);
        (noshort, "dump-ir",        set dump_ir true,        None);
        (noshort, "dump-assem",     set dump_assem true,     None);
        ('t',     "typecheck-only", set typecheck_only true, None);
        ('e',     "emit",           None,
         Some (function "abs" -> emit := AbstractAssem
                      | "x86-64" -> emit := X86_64
                      | x -> say ("Error: Unknown --emit arg: "^x); raise EXIT));
        ('O',     "opt",            None,
         Some (function "0" -> opt := Opt_None
                      | "1" -> say "Error: -O1 unimplemented (lab 2)"; raise EXIT
                      | "2" -> say "Error: -O2 unimplemented (lab 5)"; raise EXIT
                      | x -> say ("Error: Unknown --opt arg: "^x))) ] in
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
      output = !emit;
      opt_lvl = !opt;
      filename = match !filename with
        | Some s -> s
        | None -> say "Error: no input file provided"; raise EXIT }
  with Error s -> say s; raise EXIT
let say_if flag s =
  if flag then say (s ()) else ()

let main cmd =
  try
    let source = cmd.filename in

    (* Parse *)
    say_if cmd.verbose (fun () -> "Parsing... " ^ source);
    if cmd.dump_parsing then ignore (Parsing.set_trace true);

    let ast = Parse.parse source in
    (* say_if cmd.dump_ast (fun () -> Ast.Print.pp_program ast); *)

    (* Typecheck *)
    (* say_if cmd.verbose (fun () -> "Checking...");
       TypeChecker.typecheck ast;
       if cmd.typecheck_only then exit 0; *)

    (* Translate *)
    say_if cmd.verbose (fun () -> "Translating...");
    let ir = Trans.translate_gdecl ast in
    (* say_if cmd.dump_ir (fun () -> Tree.Print.pp_program ir); *)

    (* Codegen *)
    say_if cmd.verbose (fun () -> "Codegen...");
    let assem = Codegen.codegen ir in
    (* say_if cmd.dump_assem (fun () -> List.to_string ~f:Assem.format assem); *)

    (* let gra = Liveness.test assem; *)
    (*
    let () = List.iter ~f:(fun x ->
                              let i, neighbors = x in
                              let () = printf "%d " i in
                              let () = List.iter ~f:(printf "%d ") neighbors in
                              printf "\n") g in *)
    (* let () = List.iter ~f:(fun (x,y) -> printf "(%d, %d)\n" x y) coloring in *)

    say_if cmd.verbose (fun () -> "Generating new assem...");
    (*  coloring, liveness
        let g = Liveness.create_graph assem in
        let coloring = Regalloc.regalloc g in

        let new_assem = Apply.apply_coloring coloring assem in *)





    (* for i = 0 to (Liveness.find_len g)-1 do
        let Some (k, kneighbors) = List.nth g i in
        let () = printf "%d: " k in
        for j = 0 to (Liveness.find_len kneighbors)-1 do
            let Some n = List.nth kneighbors i in
            let () = printf "%d " n in
        done;
        let () = printf "\n"
       done *)
    (* let new_assem = Graphcoloring.allocation gra assem *)
    (*     let g = [(1,[2,3]), (2,[1,4])]
           in let test = Regalloc.regalloc g in *)

    (* Output: abstract 3-address assembly *)
    match cmd.output with
    | x86_64 ->
      let full =
        [Assem.DIRECTIVE(".file\t\"" ^ source ^ "\"")]
        (*@ [Assem.DIRECTIVE(".function\tmain()")]*)

        @ [Assem.DIRECTIVE(".globl\t_c0_main")]
        (* @ [Assem.DIRECTIVE(".type\t_c0_main, @function")] *)
        (* @ new_assem *)
        @ assem
        (* @ [Assem.DIRECTIVE("ret")] *)
        @ [Assem.DIRECTIVE ".ident\t\"15-411 L1 reference compiler\""] in

      let code = String.concat (List.map full ~f:Assem.format) in

      let pfname = source ^ ".s" in
      say_if cmd.verbose (fun () -> "Writing abstract assembly to "
                                    ^ pfname ^ " ...");
      Out_channel.with_file pfname
        ~f:(fun pfstream -> Out_channel.output_string pfstream code);

      (* Done *)
      exit 0
    | _ -> ();

      (* Output assembly *)
      say "x86_64 not implemented yet"; raise EXIT

  with
    ErrorMsg.Error -> say "Compilation failed"; exit 1
  | EXIT -> exit 1
  | e -> Out_channel.output_string stderr (Exn.to_string e); exit 1


let () = main (read_cmdline ())
