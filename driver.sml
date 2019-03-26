(* 15-411 autograder
 * Rob Simmons (rjsimmon@cs)
 *
 * Assumes that the current working directory is the one where the
 * compiler is built; in particular, tries to read from the following paths:
 *   ../tests*
 *   ../bench*
 *   ../runtime
 *
 * assumes that this file is located at
 *   ../grade/driver.sml
 *
 * and creates/manipulates the directory
 *   ../log
 *
 * with a record of what tests have run.
 *)

structure Grade (*:>
sig
   val gradeTests: string -> OS.Process.status
   val benchCompiler: string -> OS.Process.status
   val gradeCompiler: string -> OS.Process.status
   val selfTestCompiler: string -> OS.Process.status
end *) =
struct

(**************************************************************************)
(* Basic utilities and configuration                                      *)
(**************************************************************************)

local
open JSON
val INT' = INT o IntInf.fromInt

infix 5 //
fun dir // file = OS.Path.joinDirFile {dir = dir, file = file}

infix 5 **
fun base ** ext = OS.Path.joinBaseExt {base = base, ext = ext}
in

structure Config =
struct
   val MAX_TEST_LIMIT: int = 255 (* Database limit, don't change for F15 *)
   val MIN_VERSION: int = 522 (* Minimum correctly configured cc0 vers *)

   val debug: bool ref = ref false
   val TC_TIMEOUT: IntInf.int ref = ref 1
   val COMPILER_TIMEOUT: IntInf.int ref = ref 1
   val MAKE_TIMEOUT: IntInf.int ref = ref 1
   val GCC_TIMEOUT: IntInf.int ref = ref 1
   val RUN_TIMEOUT: IntInf.int ref = ref 1
   val MAX_FILENAME_BASE: int ref = ref 1
   val PARALLELISM: int ref = ref 1
   val quiet: IntInf.int ref = ref 0
   (* quiet 0: no suppression *)
   (* quiet 1+: suppress dull messages, output from PASS tests *)
   (* quiet 2+: suppress everything about PASS tests *)
   (* quiet 3+: suppress output from FAIL tests *)
   (* quiet 4+: suppress summaries of FAIL tests, output from make *)
   (* quiet 5+: suppress everything about FAIL tests *)
   (* quiet 6+: suppress all testing output (only show summary) *)
   (* quiet 7+: suppress detailed summary output (only show stats) *)
   val symlink = ref false
   val color = ref true
   val cc0 = ref "cc0"
   val make: string option option ref = ref (SOME NONE)
   val relax = ref false (* Relaxed validation of test cases *)
   val log = ref true (* Keep all log files *)
   val compiler_args: string list list ref = ref [[]] (* Extra arguments *)
   val autograder = ref false (* Print autograder output *)
   val filter_ext: string option ref = ref NONE (* Test filter *)

   datatype output = INTERMEDIATE | X86_64 | LLVM | EXE | ARM
   val output = ref X86_64

   (* What signals are allowed to count as process timeout? *)
   val time_sig: Posix.Signal.signal list ref = ref []

   (* What exit statuses are allowed to count as process timeout? *)
   val time_status: Word8.word list ref = ref []


end

structure Util =
struct
   fun toIntInf word =
      if word = 0wx80000000
         then ~2147483648
      else if Word32.> (word, 0wx80000000)
         then ~ (1 + Word32.toLargeInt (Word32.notb word))
      else Word32.toLargeInt word

   fun signalEq (signal1, signal2: Posix.Signal.signal) = signal1 = signal2
   fun signalIsAlarm signal = signalEq (signal, Posix.Signal.alrm)
   fun statusIsAlarm (Posix.Process.W_SIGNALED signal) = signalIsAlarm signal
     | statusIsAlarm _ = false

   fun head filename =
   let
      val stream = TextIO.openIn filename
      val line = valOf (TextIO.inputLine stream before TextIO.closeIn stream)
   in
      if String.sub (line, size line - 1) = #"\n"
         then String.extract (line, 0, SOME (size line - 1))
      else line
   end handle _ => ""

   fun printq (n, s) = if !Config.quiet <= n then print (s ^ "\n") else ()
   fun bold s = if !Config.color then ("\^[[1m" ^ s ^ "\^[[0m") else s
   fun dull s = if !Config.color then ("\^[[2m" ^ s ^ "\^[[0m") else s
   fun red s = if !Config.color then ("\^[[31m" ^ s ^ "\^[[0m") else s
   fun green s = if !Config.color then ("\^[[32m" ^ s ^ "\^[[0m") else s
   fun blue s = if !Config.color then ("\^[[34m" ^ s ^ "\^[[0m") else s

   fun mkdir dir =
    ( if not (OS.FileSys.access (dir, []))
         then OS.FileSys.mkDir dir
      else if not (OS.FileSys.isDir dir)
         then raise Fail ("Exists but is not a directory: `" ^ dir ^ "'")
      else () )
    handle exn => raise Fail ("Error making directory "
                              ^ dir ^ ": " ^ exnMessage exn)

   fun isDir s = OS.FileSys.isDir s handle _ => false
   fun isLink s = OS.FileSys.isLink s handle _ => false
   fun isRead s = OS.FileSys.access (s, [ OS.FileSys.A_READ ]) handle _ => false
   fun isExec s = OS.FileSys.access (s, [ OS.FileSys.A_EXEC ]) handle _ => false
   val rw = Posix.FileSys.S.flags [Posix.FileSys.S.irusr,
                                   Posix.FileSys.S.iwusr,
                                   Posix.FileSys.S.irgrp,
                                   Posix.FileSys.S.iwgrp]
   local
   structure S =
   RedBlackSetFn (struct type ord_key = string val compare = String.compare end)
   in
   fun readDirFiles dir =
   let
      val dirstream = OS.FileSys.openDir dir
      fun loop NONE set = set before OS.FileSys.closeDir dirstream
        | loop (SOME file) set =
             if isDir (dir // file) orelse (!Config.symlink = false andalso isLink (dir // file))
                then loop (OS.FileSys.readDir dirstream) set
             else loop (OS.FileSys.readDir dirstream) (S.add (set, file))
   in
      S.listItems (loop (OS.FileSys.readDir dirstream) S.empty)
   end
   end

   fun printFile s =
   let
      val stream = TextIO.openIn s
      fun loop stream =
         case TextIO.inputLine stream of
            NONE => TextIO.closeIn stream
          | SOME s => (print s; loop stream)
   in
    ( if isRead s then loop stream else () )
    handle exn => ( print ("Error encountered printing file "^s^"\n")
                  ; print (exnMessage exn^"\n")
                  ; TextIO.closeIn stream handle _ => () )
   end

   fun moveRead {old, new} =
      if isRead old
         then OS.FileSys.rename {old = old, new = new}
      else ()
end

structure Dict =
struct
   fun add [] x y = [ (x, [ y ]) ]
     | add ((x', ys) :: dict) x y =
          if x = x'
             then (x', y :: ys) :: dict
          else (x', ys) :: add dict x y
end

(**************************************************************************)
(* High-level representation of the result of test cases                  *)
(**************************************************************************)

structure Outcome =
struct

   (* The amount of information we have about the behavior of a test
    * case moves up as we run more tests.
    *
    *      INFLOOP  ABORT  MEMERROR  ARITH  RETURN  RUNFAIL
    *             \      \      |      /      /     /
    *              \      \     |     /      /     /
    * COMPILERFAIL  ---COMPILE---------------------
    *             \       |
    *              TYPECHECK  ERROR
    *                 |     /
    *               TIMEOUT
    *
    * *)

   datatype infloop = ALRM | SEGV | BUS (* Known infloop outcomes *)
   datatype outcome =
      TIMEOUT      (* Compiler timeout WITH or WITHOUT -t *)
    | ERROR        (* Compiler failure WITH or WITHOUT -t *)
    | TYPECHECK    (* Compiler success WITH -t, timeout otherwise *)
    | COMPILE      (* Compiler success *)
    | COMPILERFAIL of string (* Impossible for this to be correct *)
    | INFLOOP of infloop option (* Insufficient resources to finish *)
    | ABORT        (* Raises SIGABRT *)
    | ARITH        (* Raises SIGFPT *)
    | MEMERROR     (* Controlled signaling of a memory error (USR2) *)
    | RETURN of Word32.word (* Returns a 32-bit integer from main *)
    | RUNFAIL of string (* Impossible for this to be correct *)
   type t = outcome

   (* Try to read the first line of a file to get a test directive *)
   fun readTestDirective test: outcome option =
   let
      val file = TextIO.openIn test
      val line = TextIO.inputLine file before TextIO.closeIn file
   in
      case Option.map (String.tokens Char.isSpace) line of
         NONE => NONE
       | SOME [ "//test", "error" ] => SOME ERROR
       | SOME [ "//test", "typecheck" ] => SOME TYPECHECK
       | SOME [ "//test", "compile" ] => SOME COMPILE
       | SOME [ "//test", "infloop" ] => SOME (INFLOOP NONE)
       | SOME [ "//test", "abort" ] => SOME ABORT
       | SOME [ "//test", "memerror" ] => SOME MEMERROR
       | SOME [ "//test", "div-by-zero" ] => SOME ARITH
       | SOME [ "//test", "return", n ] =>
           (case IntInf.fromString n of
               NONE => NONE
             | SOME i => SOME (RETURN (Word32.fromLargeInt i)))
       | _ => NONE
   end handle _ => NONE

   fun toString outcome =
      case outcome of
         TIMEOUT => "timeout"
       | ERROR => "error"
       | TYPECHECK => "typecheck"
       | COMPILE => "compile"
       | COMPILERFAIL s => "compiler failure"
       | INFLOOP NONE => "timeout"
       | INFLOOP (SOME ALRM) => "timed out (sigalrm)"
       | INFLOOP (SOME SEGV) => "segfault"
       | INFLOOP (SOME BUS) => "raised sigbus"
       | ABORT => "abort"
       | ARITH => "div-by-zero"
       | MEMERROR => "memerror"
       | RETURN n => "return " ^ IntInf.toString (Util.toIntInf n)
       | RUNFAIL s => "runtime failure"

   fun matches {expected = expected, actual = actual} =
      case (expected, actual) of
         (INFLOOP _, INFLOOP _) => true
       | (COMPILERFAIL _, COMPILERFAIL _) => false (* Impossible? *)
       | _ => expected = actual

   fun report test logfile {expected, actual} =
   let
      val msg =
         case (expected, actual) of
            (_, TIMEOUT) => "compiler timed out while parsing and typechecking"
          | (ERROR, TYPECHECK) => "code parsed and checked successfully"
          | (ERROR, COMPILE) => "code compiled successfully"
          | (_, TYPECHECK) => "compiler timed out after typechecking"
          | (_, ERROR) => "code rejected as ill-formed"
          | (_, COMPILERFAIL s) => s
          | (_, INFLOOP NONE) => "program failed to terminate"
          | (_, INFLOOP (SOME ALRM)) => "program timed out (sigalrm)"
          | (_, INFLOOP (SOME SEGV)) =>  "program failed to terminate \
                            \due to memory error (possibly stack overflow?)"
          | (_, INFLOOP (SOME BUS)) =>  "program failed to terminate \
                            \due to memory error (probably stack overflow)"
          | (_, ABORT) => "program raised SIGABRT"
          | (_, ARITH) => "program encountered arithmetic error"
          | (_, MEMERROR) => "program signaled a safe memory error"
          | (_, RETURN n) => "return " ^ IntInf.toString (Util.toIntInf n)
          | (_, RUNFAIL s) => s
          | _ => "unexpected outcome (contact course staff?)"
   in
      if matches {actual = actual, expected = expected}
         then ( Util.printq (1, Util.green ("-- PASS: " ^ test ^ " --")) )
      else ( case logfile of
                NONE => ()
              | SOME file =>
                   if !Config.quiet <= 2
                      then (print "\n"; Util.printFile file)
                   else ()
           ; Util.printq (4, Util.red ("-- FAIL: " ^ test ^ " --"))
           ; Util.printq (3, Util.red ("   directive: " ^ Util.head test))
           ; Util.printq (3, Util.red ("     outcome: " ^ msg))  )
   end

end

structure Summary:>
sig
   (* Rather than keeping detailed records of all tests, we summarize
    * categories of tests and whether they did the right thing, wrong
    * thing, or timed out. A test that is supposed to compile and do
    * something will be in the category RET, LOOP, or RAISE if it
    * succeeds, but it can end up as a TC (typechecker) test if it
    * fails or times out, since that unit test is exposing an issue
    * with the typechecker. *)

   datatype cat = ERR | TC | COMP | RET | LOOP | RAISE
   datatype res = TIME | SUCC | FAIL

   (* Human readable summary, expected/got format *)
   val desc: cat * res -> string

   val catToString: cat -> string
   val catFromString: string -> cat
   val resToString: res -> string
   val resFromString: string -> res

   val summarize:
      {expected: Outcome.outcome, actual: Outcome.outcome} -> cat * res
   val toValue: cat * res -> value
   val fromValue: value -> cat * res
end =
struct
   datatype cat = ERR | TC | COMP | RET | LOOP | RAISE
   fun catToString ERR = "err"
     | catToString TC = "tc"
     | catToString COMP = "comp"
     | catToString RET = "ret"
     | catToString LOOP = "loop"
     | catToString RAISE = "raise"
   fun catFromString "err" = ERR
     | catFromString "tc" = TC
     | catFromString "comp" = COMP
     | catFromString "ret" = RET
     | catFromString "loop" = LOOP
     | catFromString "raise" = RAISE
     | catFromString s = raise Fail ("Outcome.catFromString " ^ s)

   datatype res = TIME | SUCC | FAIL
   fun resToString TIME = "time"
     | resToString SUCC = "succ"
     | resToString FAIL = "fail"
   fun resFromString "time" = TIME
     | resFromString "succ" = SUCC
     | resFromString "fail" = FAIL
     | resFromString s = raise Fail ("Outcome.resFromString " ^ s)

   fun catDesc ERR = "typechecker to report an error"
     | catDesc TC = "code to parse and typecheck"
     | catDesc COMP = "code to compile after typechecking"
     | catDesc RET = "successful execution"
     | catDesc LOOP = "nontermination or stack overflow"
     | catDesc RAISE = "a runtime exception"

   fun resDesc TIME = "timed out"
     | resDesc SUCC = "succeded"
     | resDesc FAIL = "failed"

   fun desc (cat, res) = "Expected " ^ catDesc cat ^ ", " ^ resDesc res ^ ":"

   local open Outcome
   in
   fun summarize {expected, actual} =
      case (expected, actual) of
         (ERROR, ERROR) =>            (ERR, SUCC)
       | (ERROR, TIMEOUT) =>          (ERR, TIME)
       | (ERROR, _) =>                (ERR, FAIL)

       | (TYPECHECK, TYPECHECK) =>    (TC, SUCC)
       | (_, ERROR) =>                (TC, FAIL)
       | (_, TIMEOUT) =>              (TC, TIME)

       | (COMPILE, COMPILE) =>        (COMP, SUCC)
       | (_, TYPECHECK) =>            (COMP, TIME)
       | (_, COMPILERFAIL _) =>       (COMP, FAIL)

       | (RETURN x, RETURN y) => if x = y
                                 then (RET, SUCC)
                                 else (RET, FAIL)
       | (RETURN _, INFLOOP _) =>     (RET, TIME)
       | (RETURN _, _) =>             (RET, FAIL)

       | (INFLOOP _, INFLOOP _) =>    (LOOP, SUCC)
       | (INFLOOP _, _) =>            (LOOP, FAIL)

       | (_, INFLOOP _) =>            (RAISE, TIME)
       | _ =>                    if matches {actual = actual,
                                             expected = expected}
                                 then (RAISE, SUCC)
                                 else (RAISE, FAIL)
   end

   fun toValue (cat, res) =
      ARRAY [ STRING (catToString cat), STRING (resToString res) ]
   fun fromValue value =
      case value of
         ARRAY [ STRING cat, STRING res ] =>
            (catFromString cat, resFromString res)
       | _ => raise Fail "Outcome.decode"
end

structure MapReduce =
struct

   datatype 'a workitem =
      VALUE of 'a * value
    | FAILURE of 'a
    | AWAIT of 'a * Posix.Process.pid * string

   (* Spawn a new process to do some work, return pid and pipe *)
   fun start (mapper: 'a -> value) (x: 'a) =
   let
      val temp = OS.FileSys.tmpName () (* XXX replace with a pipe? *)
      fun fork temp mapper x =
      let
         val value = mapper x
         val outstream = TextIO.openOut temp
         val string = JSONPrinter.print (outstream, value)
      in
       ( TextIO.closeOut outstream
       ; OS.Process.exit OS.Process.success )
      end handle exn =>
       ( OS.Process.exit OS.Process.failure )
   in
      case Posix.Process.fork () of
          NONE => fork temp mapper x
        | SOME pid => AWAIT (x, pid, temp)
   end

   (* Given a terminated process, find in worklist and replace with value *)
   fun join (pid, status) (AWAIT (x, pid', tmp) :: work) =
        ( if pid <> pid'
             then AWAIT (x, pid', tmp) :: join (pid, status) work
          else case status of
             Posix.Process.W_EXITED =>
              ( VALUE (x, JSONParser.parseFile tmp) :: work
                before OS.FileSys.remove tmp )
           | _ =>
              ( FAILURE x :: work
                before (OS.FileSys.remove tmp handle _ => ()) ) )
     | join res [] = [] (* Pid not found! (Unexpected, possible error) *)
     | join res (item :: work) = item :: join res work

   fun map mapper xs: ('a list * ('a * value) list) =
   let
      fun loop fail succeed [] [] = (rev fail, rev succeed)
        | loop fail succeed (FAILURE x :: worklist) xs =
             loop (x :: fail) succeed worklist xs
        | loop fail succeed (VALUE (x, v) :: worklist) xs =
             loop fail ((x, v) :: succeed) worklist xs
        | loop fail succeed (worklist: 'a workitem list) xs =
          let
             val res = Posix.Process.wait ()
             val (worklist, xs) =
                case xs of
                   [] => (join res worklist, [])
                 | x :: xs => (join res worklist @ [ start mapper x ], xs)
          in
             loop fail succeed worklist xs
          end

      val n = !Config.PARALLELISM
      val (worklist: 'a workitem list, xs: 'a list) =
         if length xs < n
            then (List.map (start mapper) xs, [])
         else (List.map (start mapper) (List.take (xs, n)), List.drop (xs, n))
   in
      loop [] [] worklist xs
   end
end



(**************************************************************************)
(* Statistics about a series of tests                                     *)
(**************************************************************************)

structure Stats:>
sig
   type 'a result =
      (string *
       (string * string) list *
       ((string * 'a * string) * (Summary.cat * Summary.res)) list) list

   val report: 'a result -> unit
end =
struct
   fun report1 (suite, badFiles, tests) =
   let
      val n = length badFiles + length tests
      val (succ, time) =
         List.foldr (fn ((_, value), (succ, time)) =>
                       (case value of
                           (_, Summary.SUCC) => (succ+1, time)
                         | (_, Summary.TIME) => (succ, time+1)
                         | _ => (succ, time)))
            (0, 0) tests
      fun frac m = Int.toString m ^ " / " ^ Int.toString n
   in
      Util.printq (7, (suite ^ ": passed " ^ frac succ
                       ^ (if time > 0 then (" timed out "^frac time) else "")))
   end

   type 'a result =
      (string *
       (string * string) list *
       ((string * 'a * string) * (Summary.cat * Summary.res)) list) list

   fun report (stats: 'a result)  =
   let
      val badFiles = List.concat (map #2 stats)
      val allTests = List.concat (map #3 stats)
   in
    ( if null badFiles then ()
      else ( Util.printq (6, "Ill-formed test files:")
           ; List.app (fn (s, f) => Util.printq (6, "   " ^ (s//f))) badFiles)

    ; List.app
         (fn ((_, Summary.SUCC), _) => ()
           | ((cat, res), tests) =>
               ( Util.printq (6, Summary.desc (cat, res))
               ; app (fn (suite, _, test) =>
                         Util.printq (6, "   " ^ (suite // test)))
                    tests))
         (List.foldr
            (fn ((name, value), dict) =>
                Dict.add dict value name)
            [] allTests)
    ; app report1 stats )
   end

end



(**************************************************************************)
(* Interpreter for the pseudo-assembly emitted by reference compiler      *)
(**************************************************************************)

structure Interpret:>
sig
   (* Interpreter for three-address code emitted by starter code compiler *)
   (* No final ret statement? Assumes "%eax" contains return value. *)
   val interpretFile: string -> Outcome.t
end =
struct
   datatype prem = VAL of Word32.word | TEMP of string

   structure E = RedBlackMapFn
   (struct
       type ord_key = string
       val compare = String.compare
    end)

   fun interpretFile filename =
   let
      val stream = TextIO.openIn filename

      fun read env tok =
         if #"$" = String.sub (tok, 0)
            then (case IntInf.fromString (String.extract (tok, 1, NONE)) of
                     NONE => raise Fail ("Bad literal: " ^ tok)
                   | SOME i => i)
         else (case E.find (env, tok) of
                  NONE => raise Fail ("No value stored for " ^ tok)
                | SOME i => Util.toIntInf i)

      fun store env dest value =
         E.insert (env, dest, Word32.fromLargeInt value)

      fun eval env NONE = Outcome.RETURN (Word32.fromLargeInt (read env "%eax"))
        | eval env (SOME s) =
          let
             val s' = String.map Char.toLower s
             val toks =
                String.tokens (fn #"," => true
                                | #"[" => true
                                | #"]" => true
                                | c => Char.isSpace c) s'
             val next = fn env => eval env (TextIO.inputLine stream)
          in
             case toks of
                [] => next env
              | ".file" :: _ => next env
              | ".globl" :: _ => next env
              | ".ident" :: _ => next env
              | ".function" :: _ => next env
              | [dest, "<--", src] =>
                   next (store env dest (read env src))
              | [dest, "<--", src1, "+", src2] =>
                   next (store env dest (read env src1 + read env src2))
              | [dest, "<--", src1, "-", src2] =>
                   next (store env dest (read env src1 - read env src2))
              | [dest, "<--", src1, "*", src2] =>
                   next (store env dest (read env src1 * read env src2))
              | [dest, "<--", src1, "/", src2] =>
                  (case (read env src1, read env src2) of
                      (_, 0) => Outcome.ARITH
                    | (~2147483648, ~1) => Outcome.ARITH
                    | (x, y) => next (store env dest (IntInf.quot (x, y))))
              | [dest, "<--", src1, "%", src2] =>
                  (case (read env src1, read env src2) of
                      (_, 0) => Outcome.ARITH
                    | (~2147483648, ~1) => Outcome.ARITH
                    | (x, y) => next (store env dest (IntInf.rem (x, y))))
              | ["ret", src] =>
                   Outcome.RETURN (Word32.fromLargeInt (read env src))
              | _ => raise Fail ("Couldn't read assembly line " ^ s)
       end
   in
      (eval E.empty (TextIO.inputLine stream)
       handle Fail s => Outcome.COMPILERFAIL s)
      before TextIO.closeIn stream
   end
end



(**************************************************************************)
(* Run forked process with timeout, redirecting standard input and output *)
(**************************************************************************)

structure Runner:>
sig
   (* Redirection:
    *
    * modifies the behavior of stdout/stderr to silence, redirect to
    * an existing file descriptor, or redirect to a log file (which
    * will be overwritten if it already exists) *)
   datatype redirection =
      UNCHANGED
    | SILENCE
    | REDIRECT of Posix.IO.file_desc

   val redirect:
      {msg: string,
       command: string,
       args: string list,
       secs: IntInf.int,
       out: redirection,
       err: redirection} -> Posix.Process.exit_status

   val print: redirection * string -> unit
   val close: redirection -> unit

   (* Does not perform any redirection *)
   val run:
      {command: string,
       args: string list,
       secs: IntInf.int} -> Posix.Process.exit_status

   (* Redirect standard {out, err} to logbase.{out, err} *)
   val log:
      {command: string,
       args: string list,
       secs: IntInf.int,
       logBase: string} -> Posix.Process.exit_status

end =
struct

   val devnull = Posix.FileSys.openf ("/dev/null", Posix.FileSys.O_WRONLY,
                                      Posix.FileSys.O.append)

   fun quietOut std false = ()
     | quietOut std true = Posix.IO.dup2 {old = devnull, new = std}

   (* Redirection:
    *
    * redirectOut (instructions) Posix.FileSys.std{out,err}
    * modifies the behavior of stdout/stderr to silence, redirect to
    * an existing file descriptor, or redirect to a log file (which
    * will be overwritten if it already exists) *)

   datatype redirection =
      UNCHANGED
    | SILENCE
    | REDIRECT of Posix.IO.file_desc

   fun close redirect =
      case redirect of
         REDIRECT desc => Posix.IO.close desc
       | _ => ()

   (* PERF: hilariously inefficient redirect case *)
   fun print (redirect, s) =
      case redirect of
         UNCHANGED => TextIO.print s
       | SILENCE => ()
       | REDIRECT desc =>
           (case Posix.Process.fork () of
               SOME pid =>
                  (ignore o Posix.Process.waitpid)
                     (Posix.Process.W_CHILD pid, [])
             | NONE =>
                  ( Posix.IO.dup2 {old = desc, new = Posix.FileSys.stdout}
                  ; TextIO.print s
                  ; OS.Process.exit OS.Process.success ))

   fun redirectOut UNCHANGED std = ()
     | redirectOut SILENCE std =
          Posix.IO.dup2 {old = devnull, new = std}
     | redirectOut (REDIRECT sink) std =
          Posix.IO.dup2 {old = sink, new = std}

   (* This is the main function, the others are helpers *)
   fun redirect {msg, command, args, secs, out, err} =
   let
      val () =
         if !Config.debug
            then TextIO.output (TextIO.stdErr, "** Forking to run " ^ command
                                               ^ " "
                                               ^ String.concatWith " " args
                                               ^ " **\n")
         else ()
      val (pid, status) =
         case Posix.Process.fork () of
            SOME pid => Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
          | NONE =>
             ( redirectOut out Posix.FileSys.stdout
             ; TextIO.print msg
             ; redirectOut err Posix.FileSys.stderr
             (* Become a process group leader *)
             ; Posix.ProcEnv.setpgid {pid = NONE, pgid = NONE}
             (* Set an alarm, prepare to die *)
             ; ignore (Posix.Process.alarm (Time.fromSeconds secs))
             ; Posix.Process.execp (command, command :: args) )
             handle _ => OS.Process.exit OS.Process.failure
   in
      (* Once we've gotten here, it's time for this job to go. Is
       * there a race condition here if another process has claimed
       * this (now terminated) pid in the brief interval? Should we
       * only run kill in the event of a timeout? *)
    ( Posix.Process.kill (Posix.Process.K_GROUP pid, Posix.Signal.kill)
      handle _ => ()
    ; status )
   end

   fun run {command, args, secs} =
      redirect {msg = "", command = command, args = args, secs = secs,
                out = UNCHANGED, err = UNCHANGED}

   fun redirectBoth {msg, command, args, secs, fd} =
      redirect {msg = msg, command = command, args = args, secs = secs,
                out = REDIRECT fd, err = REDIRECT fd}

   fun log {command, args, secs, logBase} =
   let
      val outFD = Posix.FileSys.creat (logBase ** SOME "out", Util.rw)
      val errFD = Posix.FileSys.creat (logBase ** SOME "err", Util.rw)
   in
      redirect {msg = "", command = command, args = args, secs = secs,
                out = REDIRECT outFD, err = REDIRECT errFD}
      before (Posix.IO.close outFD; Posix.IO.close errFD)
   end
end



(**************************************************************************)
(* Invoking the C0 compiler and running what comes out                    *)
(**************************************************************************)

structure Compiler:>
sig
   datatype lib =
      NOLIB
    | DEFAULT of string         (* DEFAULT "l3" versus DEFAULT "l4" *)
    | HEADER of string * string (* dir and base (without h0) *)
                                (* Example: HEADER ("../runtime", "15411-l3") *)

   (* Runs a C0 compiler with cc0-style options. *)
   val runCC0:
   string * lib * string list -> Outcome.t -> (Outcome.t * string option)

   (* Runs a C0 compiler with c0c-style options. *)
   type args = string * lib * string list list (* C0C Configuration options *)
   val runC0C: args -> Outcome.t -> (Outcome.t list * string option)

   (* benchmark (filename, optimize, safe) *)
   val benchmark: (string * int * bool) -> IntInf.int
end =
struct
   datatype lib =
      NOLIB
    | DEFAULT of string
    | HEADER of string * string

   type config = {safe: bool, opt: int option}

   type args = string * lib * string list list

   fun archive file =
   let
      val oldfile = file ** SOME "old"
   in
    ( if OS.FileSys.access (oldfile, [])
         then OS.FileSys.remove oldfile
      else ()
    ; if OS.FileSys.access (file, [])
         then OS.FileSys.rename {old = file, new = oldfile}
      else () )
   end

   fun remove file =
      if OS.FileSys.access (file, []) andalso not (!Config.log)
         then OS.FileSys.remove file
      else ()

   (* Munch on the path names and check the state of the world, get a
    * filename for outputs that is scoped to the name of the given
    * file but with 'log' prepended to the path components, remove old
    * test files with that name. *)
   fun prepareTestBase test =
   let
      val {dir = suiteDir, file = testFile} = OS.Path.splitDirFile test
      val {dir = parent, file = suite} = OS.Path.splitDirFile suiteDir
      val logDir = ".." // "log" // suite
      val () = if Util.isDir logDir
                   then ()
               else raise Fail ("Log directory " ^ logDir ^ " not prepared")

      val (logBase, ext) =
         case OS.Path.splitBaseExt testFile of
            {base, ext = SOME ext} => (base ^ "-" ^ ext, ext)
          | _ => raise Fail ("Test file has no extension `" ^ testFile)
      val testLog = logDir // logBase
   in
    ( archive testLog
    ; archive (testLog ** SOME "s")
    ; archive (testLog ** SOME "abs")
    ; archive (testLog ** SOME "ll")
    ; archive (testLog ** SOME "bc")
    ; archive (testLog ** SOME "exe")
    ; archive (testLog ** SOME "out")
    ; archive (testLog ** SOME "err")
    ; archive (testLog ** SOME "compile")
    ; testLog )
   end

   fun destroyTestBase testLog =
    ( remove testLog
    ; remove (testLog ** SOME "s")
    ; remove (testLog ** SOME "abs")
    ; remove (testLog ** SOME "ll")
    ; remove (testLog ** SOME "bc")
    ; remove (testLog ** SOME "exe")
    ; remove (testLog ** SOME "out")
    ; remove (testLog ** SOME "err")
    ; remove (testLog ** SOME "compile") )

   val studentCompiler = "bin" // "c0c"

   (* Invoke a C0C-style typechecker (should produce no output) *)
   (* Returns: TIMEOUT, ERROR, or TYPECHECK (success) *)
   fun runC0CTypechecker test testLog redirect args =
   let
      val args = "-t" :: args @ [ test ]
      val cmd = String.concatWith " " ("bin/c0c" :: args)
   in
      case Runner.redirect {msg = "-- Typechecker: " ^ cmd ^ " --\n",
                            command = studentCompiler,
                            args = args,
                            secs = !Config.TC_TIMEOUT,
                            out = redirect, err = redirect} of
         Posix.Process.W_EXITED => Outcome.TYPECHECK
       | Posix.Process.W_EXITSTATUS w =>
            if List.exists (fn w' => w = w') (!Config.time_status)
               then Outcome.TIMEOUT
            else Outcome.ERROR
       | Posix.Process.W_SIGNALED s =>
            if List.exists (fn s' => Util.signalEq (s, s')) (!Config.time_sig)
               then Outcome.TIMEOUT
            else Outcome.ERROR
       | Posix.Process.W_STOPPED _ => Outcome.ERROR
   end

   (* Invoke a C0C-style compiler (produces some file testLog.s) *)
   (* Presumes code passes typechecker *)
   (* Returns COMPILE (indicating successful compilation)
    *         TYPECHECK (indicating timeout)
    *         COMPILERFAIL (indicating other failure) *)
   fun runC0CCompiler test testLog redirect args =
   let
      val args = args @ [ test ]
      val cmd = String.concatWith " " ("bin/c0c" :: args)
   in
    ( case Runner.redirect {msg = "-- Compiler: " ^ cmd ^ " --\n",
                            command = studentCompiler,
                            args = args,
                            secs = !Config.COMPILER_TIMEOUT,
                            out = redirect, err = redirect} of
         Posix.Process.W_EXITED => () (* Expected, successful execution *)
       | Posix.Process.W_EXITSTATUS w =>
            if List.exists (fn w' => w = w') (!Config.time_status)
               then raise Overflow (* ===> TYPECHECK *)
            else raise Fail ("Compiler returned 0x" ^ Word8.toString w
                             ^ " after typechecking successfully")
       | Posix.Process.W_SIGNALED s =>
            if List.exists (fn s' => Util.signalEq (s, s')) (!Config.time_sig)
               then raise Overflow (* ===> TYPECHECK *)
            else raise Fail ("Compiler raised 0x"
                             ^ SysWord.toString (Posix.Signal.toWord s)
                             ^ " after typechecking successfully")
       | Posix.Process.W_STOPPED _ =>
            raise Fail ("Compiler stopped after typechecking successfully")

      (* Move any files that were plausibly created to log directory *)
    ; Util.moveRead {old = test ** SOME "s", new = testLog ** SOME "s"}
    ; Util.moveRead {old = test ** SOME "abs", new = testLog ** SOME "abs"}
    ; Util.moveRead {old = test ** SOME "ll", new = testLog ** SOME "ll"}
    ; Util.moveRead {old = test ** SOME "bc", new = testLog ** SOME "bc"}
    ; Util.moveRead {old = test ** SOME "exe", new = testLog ** SOME "exe"}

    ; Outcome.COMPILE )
   end handle Overflow => Outcome.TYPECHECK
            | Fail s => Outcome.COMPILERFAIL s

   fun runExe testLog args =
   let
      val exe = testLog ** SOME "exe"
      val out = testLog ** SOME "out"
   in
       case Runner.log {command = exe, args = args,
                        secs = !Config.RUN_TIMEOUT,
                        logBase = testLog} of
         Posix.Process.W_EXITED =>
         (let
             val ins = TextIO.openIn out
             val res =
                Option.mapPartial IntInf.fromString (TextIO.inputLine ins)
          in
            (case (res, TextIO.inputLine ins) of
                (SOME i, NONE) => Outcome.RETURN (Word32.fromLargeInt i)
              | _ => Outcome.RUNFAIL ("Error reading " ^ out))
            before TextIO.closeIn ins
          end handle _ => Outcome.RUNFAIL ("Error reading " ^ out))
       | Posix.Process.W_EXITSTATUS w => Outcome.RUNFAIL ("Bad return code")
       | Posix.Process.W_SIGNALED s =>
            if Util.signalEq (s, Posix.Signal.bus)
               then Outcome.INFLOOP (SOME Outcome.BUS)
            else if Util.signalEq (s, Posix.Signal.segv)
               then Outcome.INFLOOP (SOME Outcome.SEGV)
            else if Util.signalEq (s, Posix.Signal.alrm)
               then Outcome.INFLOOP (SOME Outcome.ALRM)
            else if List.exists (fn s' => Util.signalEq (s, s'))
                                (!Config.time_sig)
               then Outcome.INFLOOP NONE
            else if Util.signalEq (s, Posix.Signal.abrt) then Outcome.ABORT
            else if Util.signalEq (s, Posix.Signal.fpe) then Outcome.ARITH
            (* 12 is SIGUSR2 on Ubuntu, but we can't count on 12 being
             * SIGUSR2 everywhere. On OSX, it's a non-standard signal
             * SYS, so we cheat *)
            else if Util.signalEq (s, Posix.Signal.usr2) then Outcome.MEMERROR
            else if Util.signalEq (s, Posix.Signal.fromWord 0wxC)
               then Outcome.MEMERROR
            else Outcome.RUNFAIL ("Unexpected signal 0x"
                          ^ SysWord.toString (Posix.Signal.toWord s))
       | Posix.Process.W_STOPPED _ => Outcome.RUNFAIL "Process stopped"
   end

   fun link testLog runtime redirect =
   let
      val asm = testLog ** SOME "s"
      val exe = testLog ** SOME "exe"
   in
     (case Runner.redirect {msg = "-- Running linker on " ^ asm ^ " --\n",
                            command = "gcc",
                            args = ["-m64",
                                    runtime,
                                    asm, "-o", exe, "-lm"],
                            secs = !Config.GCC_TIMEOUT,
                            out = redirect, err = redirect} of
         Posix.Process.W_EXITED => Outcome.COMPILE
       | status =>
            if Util.statusIsAlarm status
               then Outcome.COMPILERFAIL "GCC assembler/linker timed out"
            else Outcome.COMPILERFAIL "GCC assembler/linker failed")
   end

   fun linkAndRun testLog runtime redirect =
     (case link testLog runtime redirect of
         Outcome.COMPILE =>
          ( Runner.print (redirect,
                          "-- Running executable "
                          ^ (testLog ** SOME "exe") ^ " --\n")
          ; runExe testLog [] )
       | outcome => outcome)

   fun sendArmFile testLog redirect =
   let
      val asm = testLog ** SOME "s"
   in
   (case Runner.redirect {msg = "--Sending " ^ asm ^ " to arm machine --\n",
                          command = "scp",
                          args = ["-oStrictHostKeyChecking=no",
                                 asm, "comp.pc.cc.cmu.edu:~/tests/"],
                          secs = !Config.RUN_TIMEOUT,
                          out = redirect, err = redirect} of
      Posix.Process.W_EXITED => Outcome.COMPILE
      | status =>
         if Util.statusIsAlarm status
         then Outcome.COMPILERFAIL "scp timed out"
         else Outcome.COMPILERFAIL "scp failed")
   end

   fun linkAndRunArm testLog redirect =
   let
      val asm = testLog ** SOME "s"
      val out = testLog ** SOME "out"
      val {dir = suiteDir, file = testFile} = OS.Path.splitDirFile asm
   in
   (case sendArmFile testLog redirect of
      Outcome.COMPILE =>
   ( Runner.print (redirect,"-- Running " ^ testFile ^ " on arm --\n") ;
        case Runner.log {command = "../harness/link",
                         args = [testFile],
                         secs = !Config.GCC_TIMEOUT,
                         logBase = testLog} of
         Posix.Process.W_EXITED =>
         (let
             val ins = TextIO.openIn out
             val res =
                Option.mapPartial IntInf.fromString (TextIO.inputLine ins )
          in
            (case (res, TextIO.inputLine ins) of
                (SOME i, NONE) => Outcome.RETURN (Word32.fromLargeInt i)
              | _ => Outcome.RUNFAIL ("Error reading " ^ out))
            before TextIO.closeIn ins
          end handle _ => Outcome.RUNFAIL ("Error reading " ^ out))
      | status =>
         if Util.statusIsAlarm status
               then Outcome.INFLOOP NONE
            else Outcome.COMPILERFAIL "Arm execution failed"))
   end

   fun llvm testLog extension arch redirect =
     (case Runner.redirect {msg = "-- Running llvm compiler on "
                                  ^ (testLog ** SOME extension) ^ " --\n",
                            command = "llc",
                            args = [ arch, "-O0",
                                     testLog ** SOME extension ],
                            secs = !Config.GCC_TIMEOUT,
                            out = redirect, err = redirect} of
         Posix.Process.W_EXITED => Outcome.COMPILE
       | status =>
            if Util.statusIsAlarm status
               then Outcome.COMPILERFAIL "LLVM compiler (llc) timed out"
            else Outcome.COMPILERFAIL "LLVM compiler (llc) failed")

   fun llvmAndRun testLog extension redirect =
   let
      val runtime =
         if Util.isRead "runllvm.c"
            then "runllvm.c"
         else (".." // "runtime" // "run411.c")
      val arch = "-march=x86-64"
   in case llvm testLog extension arch redirect of
          Outcome.COMPILE => linkAndRun testLog runtime redirect
        | outcome => outcome
   end

   fun llvmArmAndRun testLog extension redirect =
   let
      val arch = "-march=arm"
   in
      case llvm testLog extension arch redirect of
         Outcome.COMPILE => linkAndRunArm testLog redirect
       | outcome => outcome
   end

   fun tcOnly Outcome.TYPECHECK = true
     | tcOnly Outcome.ERROR = true
     | tcOnly _ = false

   (* Tests a compiler with C0C-style options that produces assembley *)
   fun runC0C (test, lib, configs) expected =
   let
      val testLog = prepareTestBase test
      val compileLog = testLog ** SOME "compile"
      val (redirect, log) =
         if !Config.quiet = 0
            then (Runner.UNCHANGED, NONE)
         else (Runner.REDIRECT (Posix.FileSys.creat (compileLog, Util.rw)),
               SOME compileLog)
      val lib_args = case lib of
                        NOLIB => []
                      | DEFAULT "l3" => [ "-l", "../runtime/15411-l3.h0" ]
                      | DEFAULT "l4" => [ "-l", "../runtime/15411-l4.h0" ]
                      | DEFAULT "l5" => [ "-l", "../runtime/15411-l5.h0" ]
                      | DEFAULT "l6" => [ "-l", "../runtime/15411-l5.h0" ]
                      | DEFAULT s => raise Fail ("No default for ." ^ s ^
                                                 " (runC0C)")
                      | HEADER (dir, file) => [ "-l",
                                                dir // (file ** SOME "h0") ]

      (* Run a single set of configurations *)
      fun run_config output config =
      let
         val output_generating_config =
            case output of
               Config.INTERMEDIATE => ["-eabs"] (* Starter code *)
             | Config.X86_64 => ["-ex86-64"] (* Default *)
             | Config.LLVM => [ "-ellvm" ] (* Lab 6, LLVM output *)
             | Config.ARM => [ "-ellvm" ] (* Lab 6, LLVM output *)
             | Config.EXE => [ "-eexe" ] (* Lab 6, LLVM output *)
         val all_config = lib_args @ output_generating_config @ config
         val outcome =
           (case runC0CCompiler test testLog redirect all_config of
               Outcome.COMPILE =>
                 (if expected = Outcome.COMPILE
                     then Outcome.COMPILE

                  (* Only at this point do we need to think about what sort of
                   * artifacts the compiler should have created
                   * (Note that runC0CCompiler should have already moved all
                   * these artifacts into the log directory) *)
                  else case output of
                     Config.INTERMEDIATE =>
                       (if Util.isRead (testLog ** SOME "abs")
                           then Interpret.interpretFile (testLog ** SOME "abs")
                        else Outcome.COMPILERFAIL ("Compiler ran but did not "
                                                   ^ "create " ^ test ^ ".abs"))

                   | Config.X86_64 =>
                       (if Util.isRead (testLog ** SOME "s")
                           then linkAndRun testLog "../runtime/run411.c" redirect
                        else Outcome.COMPILERFAIL ("Compiler ran but did not "
                                                   ^ "create " ^ test ^ ".s"))

                   | Config.LLVM =>
                       (if Util.isRead (testLog ** SOME "bc")
                           then llvmAndRun testLog "bc" redirect
                        else if Util.isRead (testLog ** SOME "ll")
                           then llvmAndRun testLog "ll" redirect
                        else Outcome.COMPILERFAIL ("Compiler ran but did not "
                                                   ^ "create " ^ test ^ ".ll "
                                                   ^ "or " ^ test ^ ".bc"))

                   | Config.ARM =>
                       (if Util.isRead (testLog ** SOME "bc")
                           then llvmArmAndRun testLog "bc" redirect
                        else if Util.isRead (testLog ** SOME "ll")
                           then llvmArmAndRun testLog "ll" redirect
                        else Outcome.COMPILERFAIL ("Compiler ran but did not "
                                                   ^ "create " ^ test ^ ".ll "
                                                   ^ "or " ^ test ^ ".bc"))

                   | Config.EXE =>
                       (if Util.isExec (testLog ** SOME "exe")
                           then runExe testLog []
                        else Outcome.COMPILERFAIL ("Compiler ran but did not "
                                                   ^ "create executable "
                                                   ^ test ^ ".exe")))
          | outcome => outcome)
      in
       ( Runner.print (redirect,
                       "-- Got result: " ^ Outcome.toString outcome ^ "\n")
       ; outcome )
      end

      (* Typecheck and, if successful, run all configurations *)
      val outcomes =
         case runC0CTypechecker test testLog redirect lib_args of
            Outcome.TYPECHECK =>
              (if (tcOnly expected)
                  then [ Outcome.TYPECHECK ]

               (* Because the LLVM compiler is supposed to remain a
                * working X86_64 backend, we hack in a configuration
                * change here. *)
               else if !Config.output = Config.LLVM
                  then map (run_config Config.LLVM) configs
                       @ map (run_config Config.X86_64) configs
               else map (run_config (!Config.output)) configs)
          | outcome => [ outcome ]
   in
    ( Runner.close redirect
    ; destroyTestBase testLog
    ; (outcomes, log) )
   end

   fun benchmark (test, optimize, safe) =
   let
      val testLog = prepareTestBase test
      val compileLog = testLog ** SOME "compile"
      val redirect =
         if !Config.quiet = 0 then Runner.UNCHANGED else Runner.SILENCE
      val opt_args =
         [ "-ellvm", "-O" ^ Int.toString optimize ]
         @ (if safe then [] else ["--unsafe"])
         @ (List.concat (!Config.compiler_args))
      val checksum =
         case Outcome.readTestDirective test of
            SOME (Outcome.RETURN v) => v
          | _ => raise Fail "Cannot read test directive from file"
   in
    ( case runC0CCompiler test testLog redirect opt_args of
         Outcome.COMPILE => llvmAndRun testLog "ll" redirect
       | _ => raise Fail "Compilation did not succeed"
    ; case link testLog "../runtime/bench.c" redirect of
         Outcome.COMPILE => ()
       | _ => raise Fail "Linking did not succeed"
    ; case runExe testLog ["-k"] of
         Outcome.RETURN v =>
            if checksum = v then ()
            else raise Fail ("Checksum incorrect: "
                             ^ IntInf.toString (Word32.toLargeInt checksum)
                             ^ " != " ^ IntInf.toString (Word32.toLargeInt v))
       | outcome => raise Fail ("Checksum computation failed: "
                                ^ Outcome.toString outcome)
    ; case runExe testLog [] of
         Outcome.RETURN cyc => Word32.toLargeInt cyc
       | outcome => raise Fail ("Cycle counting failed: "
                                ^ Outcome.toString outcome) )
   end

   (* Tests a compiler with CC0-style options that produces executables *)
   fun runCC0 (test, lib, config) expected =
   let
      val testLog = prepareTestBase test
      val exe = testLog ** SOME "exe"
      val compileLog = testLog ** SOME "compile"
      val (redirect, log) =
         if !Config.quiet = 0
            then (Runner.UNCHANGED, NONE)
         else (Runner.REDIRECT (Posix.FileSys.creat (compileLog, Util.rw)),
               SOME compileLog)

      val lib_args = case lib of
                        NOLIB => []
                      | DEFAULT ("l3") => [ "-lfpt", "-ralt" ]
                      | DEFAULT ("l4") => [ "-lfpt", "-ldub", "-ralt" ]
                      | DEFAULT ("l5" | "l6") => [ "-lfpt", "-ldub",
                                                   "-lstring",
                                                   "-lconio", "-lfile",
                                                   "-ralt" ]
                      | DEFAULT s => raise Fail ("No default for ." ^ s ^
                                                 " (runCC0)")
                      | HEADER (dir, file) => [ "-L"^dir, "-l"^file ]
      val tc_args =
         case expected of
            (Outcome.ERROR | Outcome.TYPECHECK) =>
            "--only-typecheck" :: config
          | _ => config
      val outcome =
         case Runner.redirect {msg = "-- Running compiler on " ^ test ^ " --\n",
                               command = !Config.cc0,
                               args = lib_args @ test :: "-o" :: exe :: tc_args,
                               secs = !Config.COMPILER_TIMEOUT,
                               out = redirect, err = redirect} of
             Posix.Process.W_EXITED =>
               (if expected = Outcome.ERROR
                   then Outcome.TYPECHECK
                else if expected = Outcome.TYPECHECK
                   then Outcome.TYPECHECK
                else if expected = Outcome.COMPILE
                   then Outcome.COMPILE
                else if OS.FileSys.access (exe, [ OS.FileSys.A_EXEC ])
                   then runExe testLog config
                else Outcome.COMPILERFAIL ("cc0 did not create " ^ exe))
           | res =>
                if Util.statusIsAlarm res
                   then Outcome.TIMEOUT
                else Outcome.ERROR
   in
    ( case redirect of
         Runner.REDIRECT desc => Posix.IO.close desc
       | _ => ()
    ; destroyTestBase testLog
    ; (outcome, log) )
   end
end


(**************************************************************************)
(* Scoring (lab dependent)                                                *)
(**************************************************************************)

structure Score =
struct
   fun failure () =
      if not (!Config.autograder)
         then ()
      else JSONPrinter.print (TextIO.stdOut, NULL)

   fun compiler stats =
      if not (!Config.autograder)
         then ()
      else let
         fun stat1 suite ((_, _, file), (cat, res)) =
         let
            val {base, ext} = OS.Path.splitBaseExt file
            val ext =
               case ext of
                  NONE => raise Fail ("Test with bad/no extension: " ^ file)
                | SOME ext => if size ext = 2
                                 then ext
                              else raise Fail ("File " ^ file ^ " has an "
                                               ^ " extension that is not 2"
                                               ^ " characters long")
         in
            OBJECT [ ("suite", STRING suite),
                     ("name", STRING base),
                     ("lang", STRING ext),
                     ("cat", STRING (Summary.catToString cat)),
                     ("res", STRING (Summary.resToString res) )
                   ]
         end

         fun statsuite (suite, bad, good) = map (stat1 suite) good
         val (passed, did_not_pass) =
            List.partition (fn (_, (_, res)) => res = Summary.SUCC)
               (List.concat (map #3 stats))
         val (timeout, failed) =
            List.partition (fn (_, (_, res)) => res = Summary.TIME) did_not_pass
      in
         JSONPrinter.print
             (TextIO.stdOut,
              OBJECT [ ("passed", INT (IntInf.fromInt (length passed))),
                       ("failed", INT (IntInf.fromInt (length failed))),
                       ("timeout", INT (IntInf.fromInt (length timeout))),
                       ("detail", ARRAY (List.concat (map statsuite stats)))
             ])
      end

   fun benchmarks stats =
      if not (!Config.autograder)
         then ()
      else let
            val v = List.concat (map (fn (bad, good) => bad @ good) stats)
            val success = List.all (fn (_, NULL) => false | _ => true) v
         in
            JSONPrinter.print'
               {strm = TextIO.stdOut, pretty = false}
               (OBJECT [ ("compiles", BOOL true),
                         ("success", BOOL success),
                         ("results", OBJECT v) ])
         end

   fun tests (stats: Compiler.lib Stats.result) =
   let
      val all_invalid_tests = List.concat (map #2 stats)
      val all_valid_tests = List.concat (map #3 stats)

      (* Valid tests are partitioned into good and bad *)
      val (all_good_tests, all_bad_tests) =
         List.partition
            (fn (_, (_, res)) => res = Summary.SUCC)
            all_valid_tests

      (* For good tests, we want individual statistics *)
      fun cat_size f =
      let val list_with_cat =
             List.filter (fn (_, (cat, _)) => f cat) all_good_tests
      in length list_with_cat
      end

   in
      OBJECT [
         ("invalid", INT' (length all_invalid_tests)),
         ("good", INT' (length all_good_tests)),
         ("bad", INT' (length all_bad_tests)),

         ("err", INT' (cat_size (fn cat => cat = Summary.ERR))),
         ("ret", INT' (cat_size (fn cat => cat = Summary.RET))),
         ("raise", INT' (cat_size (fn cat => cat = Summary.RAISE))),
         ("header", INT' (length
            (List.filter
                (fn ((_, Compiler.HEADER _, _), _) => true
                | _ => false)
                all_good_tests)))
      ]
   end

end


(**************************************************************************)
(* XXX code after this point needs to be better organized                 *)
(**************************************************************************)

   fun goodChar #"_" = true
     | goodChar #"-" = true
     | goodChar c = Char.isAlphaNum c
                    andalso (Char.isLower c orelse Char.isDigit c)

   fun goodExt (SOME "l1") = true
     | goodExt (SOME "l2") = true
     | goodExt (SOME "l3") = true
     | goodExt (SOME "l4") = true
     | goodExt (SOME "c0") = true
     | goodExt (SOME "c1") = true
     | goodExt _ = false

   (* If a test passes at EXT, it should **fail** at prevExt EXT *)
   fun prevExt (SOME "l2") = SOME "l1"
     | prevExt (SOME "l3") = SOME "l2"
     | prevExt (SOME "l4") = SOME "l3"
     | prevExt (SOME "c1") = SOME "c0"
     | prevExt _ = NONE

   (* Validate one file against the reference cc0 compiler *)
   fun validate1 (suiteDir, lib, file) =
   let
      val {base, ext} = OS.Path.splitBaseExt file
      val test = suiteDir // file
      val expected = Outcome.readTestDirective test
      val customLib = case lib of Compiler.HEADER _ => true | _ => false
   in
      if not (!Config.relax) andalso size base > !Config.MAX_FILENAME_BASE
         then ( Util.printq (4, Util.red ("-- Filename too long: "
                                          ^ file ^ " --"))
              ; NONE )
      else if not (!Config.relax) andalso not (List.all goodChar (explode base))
         then ( Util.printq (4, Util.red ("-- Disallowed characters in "
                                          ^ "filename " ^ file ^ " --"))
              ; Util.printq (4, Util.red ("-- (only lowercase letters, "
                                          ^ "numbers, '_' and '-' allowed) --"))
              ; NONE )

      (* NB: THIS CODE IS NECESSARY FOR SECURITY *)
      (* Files with custom headers can cause essentially arbitrary
       * system behavior, and so it is critical to make sure that the
       * autograder never runs code with custom headers. That's why we
       * only allow 'typecheck' or 'error' outcomes for custom
       * headers. *)
      else if not (!Config.relax)
              andalso (customLib)
              andalso (expected <> SOME Outcome.ERROR)
              andalso (expected <> SOME Outcome.TYPECHECK)
         then ( Util.printq (4, Util.red ("-- Bad directive for " ^ file
                                          ^ " --\n-- (tests with custom header "
                                          ^ "files can only test 'error' "
                                          ^ "or 'typecheck') --"))
              ; NONE )
      else if not (goodExt ext)
         then ( Util.printq (4, Util.red ("-- Bad extension for "
                                          ^ file ^ " --"))
              ; NONE )
      else if expected = NONE
         then ( Util.printq (4, Util.red ("-- Cannot read directive for "
                                          ^ file ^ " --"))
              ; NONE )

      (* Here's where we force tests to have the right extensions: make
       * sure an l3 test is not misclassified as an l4 test, etc. *)
      else if ( if expected = SOME Outcome.ERROR
                   then false (* Don't care about misclassified error tests *)
                else case prevExt ext of
                         NONE => false (* No good definition of 'previous' *)
                       | SOME prevLang =>
                         let
                            val () = Util.printq (0, "\n-- Testing " ^ test
                                                     ^ " at language "
                                                     ^ prevLang ^ " --")
                            val (actual, log) =
                               Compiler.runCC0
                                  (test, lib, ["--standard="^prevLang])
                                  Outcome.ERROR
                         in actual <> Outcome.ERROR
                         end )
         then ( Util.printq (4, Util.red ("-- Test " ^ file
                                          ^ " misclassified: it belongs to"
                                          ^ " a simpler language --"))
              ; NONE )
      else let
         val expected = valOf expected
         val () = Util.printq (0, "\n-- Testing " ^ test ^ " --")
         val (actual, log) = Compiler.runCC0 (test, lib, []) expected
      in
       ( Outcome.report test log {expected = expected, actual = actual}
       ; SOME (Summary.summarize {expected = expected, actual = actual}) )
      end
   end

   (* Test one file against student's c0c compiler *)
   fun test1 (suiteDir, lib, file) =
   let
      val test = suiteDir // file
   in
      case Outcome.readTestDirective test of
         NONE =>
          ( Util.printq (0, Util.dull ("-- Cannot read directive from "
                                       ^ test ^ " --"))
          ; NONE )
       | SOME expected =>
         let
            val () = Util.printq (0, "\n-- Testing " ^ test ^ " --")
            val (actual, log) =
               Compiler.runC0C (test, lib, !Config.compiler_args)
                  expected

            (* Generate summaries for every observed outcome. Then,
             * use the summaries to merge outcomes into one. Failure
             * outcomes are always reported; successes are favored over
             * timeouts. *)
            fun merge_outcomes (outcome1, outcome2) =
            let
               val (out1, (typ1, res1)) = outcome1
               val (out2, (typ2, res2)) = outcome2
            in
               case (typ1, res1, typ2, res2) of
                  (_, Summary.FAIL, _, _) => outcome1
                | (_, _, _, Summary.FAIL) => outcome2
                | (_, Summary.SUCC, _, _) => outcome1
                | (_, _, _, Summary.SUCC) => outcome2
                | (Summary.RET, _, _, _) => outcome1
                | (_, _, Summary.RET, _) => outcome2
                | (Summary.RAISE, _, _, _) => outcome1
                | (_, _, Summary.RAISE, _) => outcome2
                | (Summary.COMP, _, _, _) => outcome1
                | (_, _, Summary.COMP, _) => outcome2
                | _ => outcome1
            end
            val (actual, summary) =
               foldl merge_outcomes
                  (Outcome.TIMEOUT, (Summary.TC, Summary.TIME))
                  (map (fn actual => (actual,
                                      Summary.summarize {expected = expected,
                                                         actual = actual}))
                       actual)

         in
          ( Outcome.report test log {expected = expected, actual = actual}
          ; SOME summary )
         end
   end

   (* Benchmark all the files in suiteDir *)
   fun bench1 (suiteDir, lib, file) =
   let
      val test = suiteDir // file

      fun desc (optimize, safe) =
         (if safe then "            -O" else "   --unsafe -O")
         ^ Int.toString optimize ^ ": "

      (* Wrap calls to Compiler.benchmark in reporting code *)
      fun benchmark (test, optimize, safe) =
      let
         val cyc = Compiler.benchmark (test, optimize, safe)
      in
       ( Util.printq (1, Util.green (desc (optimize, safe)
                                     ^ IntInf.toString cyc))
       ; SOME cyc )
      end handle Fail msg =>
       ( Util.printq (4, Util.red (desc (optimize, safe) ^ "Failed: " ^ msg))
       ; NONE )

      fun cycMin (NONE, b) = b
        | cycMin (SOME m, NONE) = SOME m
        | cycMin (SOME m, SOME n) = SOME (IntInf.min (n, m))

   in
      case Outcome.readTestDirective test of
         SOME (Outcome.RETURN v) =>
         let
            val () = Util.printq (2, "")
            val () = Util.printq (5, Util.bold ("-- Timing file "^test^" --"))
            (*val safe0   = benchmark (test, 0, true)
            val unsafe0 = benchmark (test, 0, false)
            val safe1   = benchmark (test, 1, true)
            val unsafe1 = benchmark (test, 1, false)
            val safe2   = benchmark (test, 2, true)
            val unsafe2 = benchmark (test, 2, false)*)
            val llvm0 = benchmark(test, 0, true)
            val unsafe0 = benchmark(test, 0, false)
            val llvm2 = benchmark(test, 2, true)
            val unsafe2 = benchmark(test, 2, false)
            val min = List.foldl cycMin NONE
              [llvm0, unsafe0, llvm2, unsafe2]
         in
           case min of
                NONE => NONE
              | SOME n => SOME (INT n)
         end
       | _ =>
          ( Util.printq (0, Util.dull ("\n-- Cannot read return directive "
                                       ^ "from " ^ test ^ " --"))
          ; NONE )
   end

   fun prepareLogDirectory suiteName =
    ( Util.mkdir (".." // "log")
    ; Util.mkdir (".." // "log" // suiteName) )

   (* Reads (directory, necessary_library, testfile) information out
    * of a directory *)
   fun sourceFilesInDir suiteDir =
      List.mapPartial
         (* Secondary filter also checks for headers *)
         (fn filename =>
          let
             val {base, ext} = OS.Path.splitBaseExt filename
          in
             case ext of
                SOME "h0" => NONE (* Ignore header files *)
              | SOME (ext as ("l3" | "l4" | "l5" | "l6")) =>
                 (* Only l3 and l4 get to use headers *)
                 ( if Util.isRead (suiteDir // base ** SOME "h0")
                      then SOME (suiteDir,
                                 Compiler.HEADER (suiteDir, base),
                                 filename)
                   else SOME (suiteDir,
                              Compiler.DEFAULT ext,
                              filename) )
              | _ => SOME (suiteDir, Compiler.NOLIB, filename)
          end)
         (* Primary filter: optional, given on command line *)
         (case !Config.filter_ext of
             NONE => Util.readDirFiles suiteDir
           | SOME ext =>
             List.filter (fn filename => OS.Path.ext filename = SOME ext)
                (Util.readDirFiles suiteDir))

   fun validateSuite {suiteName, suiteDir} =
   let
      val () = Util.printq (6, Util.bold ("-- Validating " ^ suiteName
                                          ^ " in directory " ^ suiteDir
                                          ^ " --"))
      val () = prepareLogDirectory suiteName
      val (badFiles, summaries) =
         MapReduce.map
            (valOf o Option.map Summary.toValue o validate1)
            (sourceFilesInDir suiteDir)
   in
      (suiteName, map (fn (x, y, z) => (x, z)) badFiles,
       map (fn (x, v) => (x, Summary.fromValue v)) summaries)
   end

   fun testSuite {suiteName, suiteDir} =
   let
      val () = Util.printq (6, Util.bold ("-- Testing compiler on "
                                          ^ suiteName ^ " --"))
      val () = prepareLogDirectory suiteName
      val (badFiles, summaries) =
         MapReduce.map
            (valOf o Option.map Summary.toValue o test1)
            (sourceFilesInDir suiteDir)
   in
      (suiteName, map (fn (x, y, z) => (x, z)) badFiles,
       map (fn (x, v) => (x, Summary.fromValue v)) summaries)
   end

   fun benchSuite {suiteName, suiteDir} =
   let
      val () = prepareLogDirectory suiteName
      val (badFiles, measurements) =
         MapReduce.map (valOf o bench1) (sourceFilesInDir suiteDir)
   in
      (map (fn (x, y, z) => (z, NULL)) badFiles,
       map (fn ((x, y, z), v) => (z, v)) measurements)
   end

   exception BadOpt of string
   exception ShowUsage

   (* Checks that all given test suites are really test suites. *)
   (* Returns the suite's name and directory. *)
   val filterSuites =
      List.mapPartial
         (fn file =>
          let val file = OS.Path.mkCanonical file
          in case OS.Path.splitDirFile file of
                {dir = "", file = _} =>
                if Util.isDir file
                   then SOME {suiteName = file, suiteDir = file}
                else if Util.isDir ("../tests" // file)
                   then SOME {suiteName = file, suiteDir = "../tests" // file}
                else ( Util.printq (0, Util.dull ("-- Cannot find suite "
                                                  ^ file ^ " or ../tests/"
                                                  ^ file ^ ", ignoring --"))
                     ; NONE)
              | {dir = _, file = filepart} =>
                if Util.isDir file
                   then SOME {suiteName = filepart, suiteDir = file}
                else ( Util.printq (0, Util.dull ("-- Cannot find suite at "
                                                  ^ file ^ ", ignoring --"))
                     ; NONE)
          end)

   (* On AFS, we want the reference compiler to NOT be the 122 compiler *)
   fun findCC0 () =
   let
      val afs_compiler = "/afs/cs.cmu.edu/academic/class/15411-f15/cc0/bin/cc0"
   in
      if Util.isExec afs_compiler then afs_compiler else "cc0"
   end

   (* Adds all the args to all the current fields *)
   fun addargs args =
   let
      val current = !Config.compiler_args
      val new = String.fields (fn c => c = #",") args
      fun append (args, arg) = if arg = "" then args else (args @ [ arg ])
   in
      Config.compiler_args :=
         List.concat
            (map (fn arg => map (fn args => append (args, arg)) current) new)
   end

   fun intArg (name, f) =
      GetOpt.ReqArg (fn s =>
                       case IntInf.fromString s of
                          NONE => raise BadOpt s
                        | SOME i => f i,
                     name)

   val options =
      [{short = "c", long = ["color"],
        desc = GetOpt.ReqArg (fn "on" => Config.color := true
                               | "off" => Config.color := false
                               | c => raise BadOpt ("-c"^c), "{on,off}"),
        help = "Terminal coloring"},
       {short = "q", long = [],
        desc = GetOpt.OptArg (fn NONE => Config.quiet := 1
                               | SOME "q" => Config.quiet := 2
                               | SOME "qq" => Config.quiet := 3
                               | SOME "qqq" => Config.quiet := 4
                               | SOME "qqqq" => Config.quiet := 5
                               | SOME "qqqqq" => Config.quiet := 6
                               | SOME "qqqqqq" => Config.quiet := 7
                               | SOME c => raise BadOpt ("-q"^c), "qqqqqq"),
        help = "Quiet (use -q through -qqqqqqq)"},
       {short = "", long = ["follow-symlinks"],
        desc = GetOpt.NoArg (fn () => Config.symlink := true),
        help = "Use symlinked files for tests"},
       {short = "", long = ["nomake"],
        desc = GetOpt.NoArg (fn () => Config.make := NONE),
        help = "Don't rebuild bin/cc0"},
       {short = "m", long = ["make"],
        desc = GetOpt.ReqArg (fn arg => Config.make := SOME (SOME arg),
                              "<lab>"),
        help = "Build compiler as 'make <lab>'"},
       {short = "", long = ["cc0"],
        desc = GetOpt.ReqArg (fn s => Config.cc0 := s, "<path to cc0>"),
        help = "Path to reference compiler"},
       {short = "h", long = ["help"],
        desc = GetOpt.NoArg (fn () => raise ShowUsage),
        help = "Show this help message"},
       {short = "j", long = ["parallel"],
        desc = GetOpt.ReqArg (fn i =>
                                (case Int.fromString i of
                                    NONE => raise BadOpt ("-j"^i)
                                  | SOME i => Config.PARALLELISM := i), "i"),
        help = "Number of tests to run in parallel"},
       {short = "a", long = ["args"],
        desc = GetOpt.ReqArg (addargs, "a"),
        help = "Add comma-separated args for compiler"},
       {short = "e", long = ["emit"],
        desc = GetOpt.ReqArg (fn "abs" => Config.output := Config.INTERMEDIATE
                               | "x86-64" => Config.output := Config.X86_64
                               | "exe" => Config.output := Config.EXE
                               | "llvm" => Config.output := Config.LLVM
                               | "arm"  => Config.output := Config.ARM
                               | c => raise BadOpt ("-o"^c),
                              "x86-64"),
        help = "Compiler variant (x86-64, start, exe, llvm)"},

       {short = "", long = ["limit-make"],
        desc = intArg ("sec", fn i => Config.MAKE_TIMEOUT := i),
        help = "Compiler build time limit (1800 seconds)"},
       {short = "", long = ["limit-tc"],
        desc = intArg ("sec", fn i => Config.TC_TIMEOUT := i),
        help = "Typechecker time limit (4 seconds)"},
       {short = "", long = ["limit-compile"],
        desc = intArg ("sec", fn i => Config.COMPILER_TIMEOUT := i),
        help = "Compiler time limit (6 seconds)"},
       {short = "", long = ["limit-link"],
        desc = intArg ("sec", fn i => Config.GCC_TIMEOUT := i),
        help = "Linker time limit (8 seconds)"},
       {short = "", long = ["limit-run"],
        desc = intArg ("sec", fn i => Config.RUN_TIMEOUT := i),
        help = "Execution time limit (5 seconds)"},
       {short = "", long = ["limit-filename"],
        desc=intArg ("n", fn i => Config.MAX_FILENAME_BASE := Int.fromLarge i),
        help = "Max length of a filename (37 chars)"},
       {short = "", long = ["relax"],
        desc = GetOpt.NoArg (fn () => Config.relax := true),
        help = "Relaxed test case validation"},

       {short = "", long = ["nolog"],
        desc = GetOpt.NoArg (fn s => Config.log := false),
        help = "Delete all log files"},
       {short = "", long = ["debug"],
        desc = GetOpt.NoArg (fn s => Config.debug := true),
        help = "Debug information"},
       {short = "f", long = ["filter"],
        desc = GetOpt.ReqArg (fn ext => Config.filter_ext := SOME ext, "lN"),
        help = "Only test specific extension"},
       {short = "", long = ["autograder"],
        desc = GetOpt.NoArg (fn s => Config.autograder := true),
        help = "Produce autograder output"}]
   val usage = GetOpt.usageInfo {header = "411 Autograder",
                                 options = options}

   (* General initialization, shared by all graders *)
   fun init args =
    ( Config.debug := false
    ; Config.TC_TIMEOUT := 4
    ; Config.COMPILER_TIMEOUT := 6
    ; Config.MAKE_TIMEOUT := 1800
    ; Config.GCC_TIMEOUT := 8
    ; Config.RUN_TIMEOUT := 5
    ; Config.MAX_FILENAME_BASE := 37
    ; Config.PARALLELISM := 1
    ; Config.quiet := 0
    ; Config.symlink := false
    ; Config.color := true
    ; Config.cc0 := findCC0 ()
    ; Config.make := SOME NONE
    ; Config.filter_ext := NONE
    ; Config.autograder := false
    ; Config.relax := false
    ; Config.log := true
    ; Config.compiler_args := [[]]
    ; Config.output := Config.X86_64
    ; Config.time_sig :=
      [ Posix.Signal.ill, (* Rust compiler, possibly remove -rjs F15 *)
        Posix.Signal.bus, (* Stack overflow on Ubuntu/Autograder *)
        Posix.Signal.segv, (* Stack overflow on OSX/RHEL, sometimes Ubuntu *)
        Posix.Signal.alrm, (* Autograder in normal behavior *)
        Posix.Signal.kill ] (* Backup kill signal *)
    ; Config.time_status :=
      [ 0wx8 ] (* SBCL student solution, probably remove -rjs F15 *)
    ; #2 (GetOpt.getOpt {argOrder = GetOpt.Permute,
                         errFn = fn s => raise BadOpt s,
                         options = options} args) )

   (* General error handling, shared by all graders *)
   fun fail (Fail s) =
        ( Util.printq (7, Util.red ("-- " ^ s))
        ; Score.failure ()
        ; OS.Process.success )
     | fail (BadOpt s) =
        ( TextIO.output (TextIO.stdErr, "Bad option: "
                                        ^ s ^ "\n")
        ; TextIO.output (TextIO.stdErr, usage)
        ; OS.Process.failure )
     | fail ShowUsage =
        ( TextIO.output (TextIO.stdErr, usage)
        ; OS.Process.success )
     | fail exn =
        ( TextIO.output (TextIO.stdErr, "Unexpected error: "
                                        ^ exnMessage exn ^ "\n")
        ; OS.Process.failure )

   fun makeCompiler () =
   let
      val timer = Timer.startRealTimer ()
      val redirect =
         if !Config.quiet <= 3 then Runner.UNCHANGED else Runner.SILENCE
   in
    ( case !Config.make of
         NONE => ()
       | SOME optarg =>
         let val (args, strarg) =
                case optarg of
                   NONE => ([], "")
                 | SOME arg => ([ arg ], " " ^ arg)
         in case Runner.redirect {msg = "-- Building compiler "
                                        ^ "(make" ^ strarg ^ ") --\n",
                                  command = "make",
                                  args = args,
                                  secs = !Config.MAKE_TIMEOUT,
                                  out = redirect, err = redirect} of
                  Posix.Process.W_EXITED => ()
                | _ => raise Fail "make did not succeed"
         end
    ; if Util.isDir "bin"
         andalso OS.FileSys.access ("bin" // "c0c", [ OS.FileSys.A_EXEC ])
         then Util.printq (2, "-- Compiler built (elapsed time "
                              ^ Time.toString (Timer.checkRealTimer timer)
                              ^ "s) --")
      else raise Fail "make did not produce an executable bin/c0c"
    ; timer )
   end

   fun benchCompiler args =
   let
      val () = Util.printq (7, "-- 15-411 Compiler Benchmarks --")
      val suites =
         case init (String.tokens Char.isSpace args) of
            [] => [ "bench", "bench-small", "bench-large" ]
          | args => args
      val dirs = filterSuites suites
      val timer = makeCompiler ()
      val results = map benchSuite dirs
   in
    ( Util.printq (2, "-- Elapsed time "
                      ^ Time.toString (Timer.checkRealTimer timer) ^ "s --")
    ; Score.benchmarks results
    ; OS.Process.success )
   end handle exn => fail exn

   fun selfTestCompiler args =
   let
      val () = Util.printq (7, "-- 15-411 Compiler Tester (student tests) --")
      val () = ignore (init (String.tokens Char.isSpace args))
      val timer = makeCompiler ()
      val () = Util.printq (7, "-- Running 'make test' --")
      val status = Runner.run {command = "make",
                               args = ["test"],
                               secs = !Config.MAKE_TIMEOUT}
   in
    ( Util.printq (2, "-- Elapsed time "
                      ^ Time.toString (Timer.checkRealTimer timer) ^ "s --")
    ; if not (!Config.autograder)
         then ()
      else JSONPrinter.print
              (TextIO.stdOut,
               OBJECT [ ("compiles", BOOL true),
                        ("tests", INT (if Posix.Process.W_EXITED = status
                                       then 1
                                       else 0)),
                        ("results", ARRAY []) ])
    ; OS.Process.success )
   end handle exn => fail exn

   fun gradeCompiler args =
   let
      val () = Util.printq (7, "-- 15-411 Compiler Tester --")
      val suites =
         case init (String.tokens Char.isSpace args) of
            [] => [ "tests" ]
          | args => args
      val dirs = filterSuites suites
      val timer = makeCompiler ()
      val () = if !Config.PARALLELISM > 1
                  then Util.printq (2, "-- Evaluating bin/c0c compiler using "
                                       ^ Int.toString (!Config.PARALLELISM)
                                       ^ " cores --")
               else ()
      val results = map testSuite dirs
      val badFiles = List.concat (map #2 results)
   in
    ( Util.printq (5, "\n" ^ Util.bold "-- Summary --")
    ; Util.printq (2, "-- Elapsed time "
                      ^ Time.toString (Timer.checkRealTimer timer) ^ "s --")
    ; Stats.report results
    ; Score.compiler results
    ; OS.Process.success )
   end handle exn => fail exn

   fun gradeTests args =
   let
      val () = Util.printq (7, "-- 15-411 Test Case Verifier --")
      val suites =
         case init (String.tokens Char.isSpace args) of
            [] => [ "tests" ]
          | args => args
      val dirs = filterSuites suites

      (* Check reference compiler version *)
      val log = OS.FileSys.tmpName ()
      val () =
       ( case Runner.log {command = !Config.cc0, args = ["-V"],
                          secs = 1,
                          logBase = log} of
            Posix.Process.W_EXITED =>
            let
               val f = TextIO.openIn (log ** SOME "err")
               val vers =
                   List.nth (String.tokens Char.isSpace
                                               (valOf (TextIO.inputLine f)), 5)
                   before TextIO.closeIn f
            in
               case Int.fromString vers of
                  NONE => raise Fail ("Could not read " ^ vers ^
                                      " as a version number (cc0 -V)")
                | SOME n =>
                     if n >= Config.MIN_VERSION
                        then Util.printq (0, "-- Reference compiler: "
                                             ^ !Config.cc0 ^ " version "
                                             ^ vers ^ " --")
                     else raise Fail ("Reference compiler version " ^ vers
                                      ^ " < "
                                      ^ Int.toString Config.MIN_VERSION)
            end
          | _ => raise Fail ("Reference compiler absent or outdated ("
                             ^ !Config.cc0 ^ ")") )

      val () =
         if !Config.PARALLELISM > 1
            then Util.printq (2, "-- Validating tests using "
                                 ^ Int.toString (!Config.PARALLELISM)
                                 ^ " cores --")
         else ()
      val results = map validateSuite dirs
   in
    ( Util.printq (5, "\n" ^ Util.bold "-- Summary --")
    ; Stats.report results
    ; if !Config.autograder
         then JSONPrinter.print (TextIO.stdOut, Score.tests results)
      else ()
    ; OS.Process.success )
   end handle exn => fail exn


(*
   (* Two utility functions useful for populating the lab5 tests *)
   (* app Grade.copyBasedOnDirective (Grade.sourceFilesInDir ".") *)
   fun copyfile_s (_, lib, file) dest =
    ( case lib of
         Compiler.HEADER (_, base) =>
            print ("cp " ^ OS.Path.joinBaseExt {base = base, ext = SOME "h0"}
                   ^ " " ^ dest ^ "\n")
       | _ => ()
    ; print ("cp " ^ file ^ " " ^ dest ^ "\n") )

   fun copyBasedOnDirective (test as (_, _, file)) =
      case Outcome.readTestDirective file of
         NONE => print ("# ignoring " ^ file ^ "\n")
       | SOME Outcome.ERROR => copyfile_s test "../../lab5/tests3"
       | SOME Outcome.TYPECHECK => copyfile_s test "../../lab5/tests3"
       | SOME Outcome.COMPILE => copyfile_s test "../../lab5/tests3"
       | SOME (Outcome.RETURN _) => copyfile_s test "../../lab5/tests1"
       | SOME (Outcome.ABORT) => copyfile_s test "../../lab5/tests1"
       | _ => copyfile_s test "../../lab5/tests2"
*)

(* Gets the 'spectrum' of which languages a test matches *)
fun spectrum1 (dir, lib, filename) =
let
   val filepath = dir // filename
   fun test lang =
   let
      val (res, _) =
         Compiler.runCC0 (filepath, lib, ["--standard="^lang]) Outcome.ERROR
   in
      if (res = Outcome.TYPECHECK) then #"y" else #"N"
   end

   val res = List.map test ["l1", "l2", "l3", "l4", "c0", "c1"]

   val destName = "old-" ^ String.implode res
   val destDir = ".." // destName

   val {base, ext} = OS.Path.splitBaseExt filename
   val headname = base ** SOME "h0"
   val headnameDest = base ** SOME (String.implode [#"h", String.sub (valOf ext, 1)])
in
 ( Util.mkdir destDir
 ; Util.moveRead {old = dir // filename, new = destDir // filename}
 ; Util.moveRead {old = dir // headname, new = destDir // headname}
 ; STRING (String.implode res) )
end

fun spectra (suiteName, suiteDir) process =
let
   val () = prepareLogDirectory suiteName
in
   MapReduce.map spectrum1 (sourceFilesInDir suiteDir)
end

fun check_for_conflicts dir =
let
   val files = Util.readDirFiles dir
   val header_files = List.filter (fn file => #ext (OS.Path.splitBaseExt file) = SOME "h0") files
in
   List.app
      (fn headname =>
       let
          val {base, ext} = OS.Path.splitBaseExt headname
          val exts =
             List.filter
                (fn ext => Util.isRead (base ** SOME ext))
                ["l1", "l2", "l3", "l4"]
       in
          case exts of
             [] => print (headname ^ " has no companion")
           | [ext] =>
             let
                val destDir = ".." // "tests" // (ext ^ "-large")
                val filename = base ** SOME ext
             in
                Util.moveRead {old = headname, new = destDir // headname}
              ; Util.moveRead {old = filename, new = destDir // filename}
             end
           | _ =>  print (headname ^ " has conflicting extensions: " ^ String.concatWith ", " exts ^ "\n")
       end)
      header_files
end

fun merge (k, v) [] = [(k , 1, [v])]
  | merge (k, v) ((y, n, vs) :: ys) =
       if k = y
          then (y, n+1, v :: vs) :: ys
       else (y, n, vs) :: merge (k, v) ys

fun accum [] = [] | accum (x :: xs) = merge x (accum xs)

end
end

