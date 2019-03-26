(* L1 Compiler
 * Parse State System
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *
 * This tracks filename and newline characters
 * so character positions in lexer tokens
 * can be converted to line.column format for error messages
 *)

open Core

let currFilename = ref ""
let currLines = ref ([] : int list)

let setfile filename =
  currFilename := filename;
  currLines := []

let newline pos =
  currLines := pos :: !currLines

(* look (pos, newline_positions, line_number) = (line, col)
 * pos is buffer position
 * newline_positions is (reverse) list of newline positions in file
 * line_number is lenght of newline_positions
 *)
let rec look = function
  | (pos, a :: rest, n) ->
      (* a is end of line n *)
      if a < pos then (n + 1, pos - a)
      else look (pos, rest, n - 1)
  | (pos, [], n) ->
      (* first line pos is off by 1 *)
      (1, pos - 1)

(* ext (leftpos, rightpos) = SOME((leftline, leftcol),
                                  (rightline, rightcol), filename)
 * return NONE for invalid position (0,0)
 *)
let ext = function
  | (0, 0) -> None
  | (left, right) ->
      let lines = !currLines in
      let len = List.length lines in
      Some (look (left, lines, len),
            look (right, lines, len),
            !currFilename)
