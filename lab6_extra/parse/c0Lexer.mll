{
(* L1 Compiler
 * Lexer
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Lexes forward compatible fragment of C0
 *
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Updated to match 2014 spec
 *
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 * Updated to use Core instead of Core.Std and ppx
 *
 * Update this file to lex the necessary keywords and other tokens
 * in order to make the grammar forward compatible with C0.
 *)

open Core

module A = Ast
module S = Symbol
module T = C0Parser

let start = Lexing.lexeme_start
let l_end = Lexing.lexeme_end
let text = Lexing.lexeme

let commentLevel = ref 0
let commentPos = ref 0

let enterComment lexbuf =
  commentLevel := !commentLevel + 1 ;
  commentPos := start lexbuf

let exitComment () =
  commentLevel := !commentLevel - 1 ;
  !commentLevel = 0

let decnumber s lexbuf =
  try
    T.DECCONST (Int32.of_string (if s = "2147483648" then "0x80000000"
                                 else s))
  with Failure _ ->
    ErrorMsg.error (ParseState.ext (start lexbuf, l_end lexbuf))
      ("cannot parse integral constant `" ^ text lexbuf ^ "'");
    T.DECCONST Int32.zero

let hexnumber s lexbuf =
  try
    T.HEXCONST (Int32.of_string s)
  with Failure _ ->
    ErrorMsg.error (ParseState.ext (start lexbuf, l_end lexbuf))
      ("cannot parse integral constant `" ^ text lexbuf ^ "'");
    T.HEXCONST Int32.zero

let floatnumber s lexbuf =
  try
    T.FCONST (Float.of_string s)
  with Failure _ ->
    ErrorMsg.error (ParseState.ext (start lexbuf, l_end lexbuf))
      ("cannot parse integral constant `" ^ text lexbuf ^ "'");
    T.FCONST Float.zero

let eof () =
  (if !commentLevel > 0 then
    ErrorMsg.error (ParseState.ext (!commentPos, !commentPos))
      "unterminated comment");
  T.EOF

let check_ident name =
  let id = Symbol.symbol name in
  match Types.find name with
  | true ->  T.TYPEIDENT id
  | false -> T.IDENT id

}

let digit = ['0'-'9']
let frac = '.' digit*
let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let decnum = ("0" | ['1'-'9'](['0'-'9']*))
let hexnum = "0"['x' 'X']['0'-'9' 'a'-'f' 'A'-'F']+
let floatnum = decnum frac

let ws = [' ' '\t' '\r' '\011' '\012']

rule initial =
  parse
    ws+         { initial lexbuf }
  | '\n'        { ParseState.newline (start lexbuf); initial lexbuf }

  | '{'         { T.LBRACE }
  | '}'         { T.RBRACE }
  | '('         { T.LPAREN }
  | ')'         { T.RPAREN }
  | '['         { T.LBRACKET }
  | ']'         { T.RBRACKET }
  | "[]"        { T.BRACKETS }
  | "[     ]"   { T.BRACKETS }
  | "[ ]"       { T.BRACKETS }
  | "[  ]"      { T.BRACKETS }
  | ','         { T.COMA }

  | '.'         { T.DOT }
  | "->"        { T.RARROW }

  | ';'         { T.SEMI }
  | '?'         { T.QUESTIONMARK }
  | ':'         { T.COLON }

  | '='         { T.ASSIGN }
  | "+="        { T.PLUSEQ }
  | "-="        { T.MINUSEQ }
  | "*="        { T.STAREQ }
  | "/="        { T.SLASHEQ }
  | "%="        { T.PERCENTEQ }
  | "&="        { T.ANDEQ }
  | "^="        { T.XOREQ }
  | "|="        { T.OREQ }
  | "<<="       { T.LEFTSHIFTEQ }
  | ">>="       { T.RIGHTSHIFTEQ }

  | '<'         { T.LESSTHAN }
  | "<="        { T.LESSEQ }
  | '>'         { T.GREATERTHAN }
  | ">="        { T.GREATEQ }
  | "=="        { T.COMPAREEQ }
  | "!="        { T.COMPARENOTEQ }
  | "&&"        { T.LOGICALAND }
  | "||"        { T.LOGICALOR }
  | '&'         { T.BINARYAND }
  | '^'         { T.BINARYXOR }
  | '|'         { T.BINARYOR }
  | "<<"        { T.SHIFTLEFT }
  | ">>"        { T.SHIFTRIGHT }

  | '!'         { T.LOGICALNOT }
  | '~'         { T.BINARYNOT }

  | '+'         { T.PLUS }
  | '-'         { T.MINUS }
  | '*'         { T.STAR }
  | '/'         { T.SLASH }
  | '%'         { T.PERCENT }

  | "struct"      { T.STRUCT }
  | "typedef"     { T.TYPEDEF }
  | "if"          { T.IF }
  | "else"        { T.ELSE }
  | "while"       { T.WHILE }
  | "for"         { T.FOR }
  | "continue"    { T.CONTINUE }
  | "break"       { T.BREAK }
  | "assert"      { T.ASSERT }
  | "true"        { T.TRUE }
  | "false"       { T.FALSE }
  | "NULL"        { T.NULL }
  | "alloc"       { T.ALLOC }
  | "alloc_array" { T.ALLOCARRY }
  | "bool"        { T.BOOL }
  | "void"        { T.VOID }
  | "char"        { T.CHAR }
  | "string"      { T.STRING }
  | "float"       { T.FLOAT }

  | "--"          { T.MINUSMINUS }   (* Illegal *)
  | "++"          { T.PLUSPLUS }

  | "return"    { T.RETURN }
  | "int"       { T.INT }
  | "main"      { T.MAIN }
  | 'f'         { T.F }
  (* | "main()"    { T.MAIN }
  | "main ()"    { T.MAIN }
  | "main  ()"   { T.MAIN }
  | "main(/**/)" { T.MAIN } *)

  | decnum as n { decnumber n lexbuf }
  | hexnum as n { hexnumber n lexbuf }
  | floatnum as n { floatnumber n lexbuf }

  | id as name  { check_ident name }

  | "/*"        { enterComment lexbuf; comment lexbuf }
  | "*/"        { ErrorMsg.error (ParseState.ext (start lexbuf, start lexbuf))
                    "unbalanced comments"; initial lexbuf }

  | "//"        { comment_line lexbuf }
  | '#'         { assert false }
  | eof         { eof () }
  | _           { ErrorMsg.error (ParseState.ext (start lexbuf, start lexbuf))
                    ("illegal character: \"" ^ text lexbuf ^ "\"");
                  initial lexbuf }

and comment =
  parse
    "/*"       { enterComment lexbuf; comment lexbuf }
  | "*/"       { (if exitComment () then initial else comment) lexbuf }
  | '\n'       { ParseState.newline (start lexbuf); comment lexbuf }
  | _          { comment lexbuf }

and comment_line =
  parse
    '\n'       { ParseState.newline (start lexbuf); initial lexbuf }
  | _          { comment_line lexbuf }

{}
