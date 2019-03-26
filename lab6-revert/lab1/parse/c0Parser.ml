type token =
  | EOF
  | STRUCT
  | TYPEDEF
  | IF
  | ELSE
  | WHILE
  | FOR
  | CONTINUE
  | BREAK
  | ASSERT
  | TRUE
  | FALSE
  | NULL
  | ALLOC
  | ALLOCARRY
  | BOOL
  | VOID
  | CHAR
  | STRING
  | SEMI
  | DECCONST of (Int32.t)
  | HEXCONST of (Int32.t)
  | IDENT of (Symbol.t)
  | RETURN
  | INT
  | MAIN
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | ASSIGN
  | PLUSEQ
  | MINUSEQ
  | STAREQ
  | SLASHEQ
  | PERCENTEQ
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | UNARY
  | ASNOP
  | MINUSMINUS

open Parsing;;
let _ = parse_error;;
# 2 "lab1/parse/c0Parser.mly"
(* L1 Compiler
 * L1 grammar
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now conforms to the L1 fragment of C0
 *
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with 2014 spec
 *
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 * Updated to use Core instead of Core.Std and ppx
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

open Core

module A = Ast

let ploc (left, right) =
  Parsing.rhs_start left, Parsing.rhs_end right
let mark e (left, right) =
  A.Marked (Mark.mark' (e, ParseState.ext (ploc (left, right))))
let marks e (left, right) =
  A.Markeds (Mark.mark' (e, ParseState.ext (ploc (left, right))))

(* expand_asnop (id, "op=", exp) region = "id = id op exps"
 * or = "id = exp" if asnop is "="
 * syntactically expands a compound assignment operator
 *)
let expand_asnop a b =
  match a, b with
    (id, None, exp), (left, right) ->
      A.Assign(id, exp)
  | (id, Some oper, exp), (left, right) ->
      A.Assign(id, mark (A.OpExp (oper, [A.Var(id); exp])) (left, right))

# 89 "lab1/parse/c0Parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* STRUCT *);
  258 (* TYPEDEF *);
  259 (* IF *);
  260 (* ELSE *);
  261 (* WHILE *);
  262 (* FOR *);
  263 (* CONTINUE *);
  264 (* BREAK *);
  265 (* ASSERT *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* NULL *);
  269 (* ALLOC *);
  270 (* ALLOCARRY *);
  271 (* BOOL *);
  272 (* VOID *);
  273 (* CHAR *);
  274 (* STRING *);
  275 (* SEMI *);
  279 (* RETURN *);
  280 (* INT *);
  281 (* MAIN *);
  282 (* PLUS *);
  283 (* MINUS *);
  284 (* STAR *);
  285 (* SLASH *);
  286 (* PERCENT *);
  287 (* ASSIGN *);
  288 (* PLUSEQ *);
  289 (* MINUSEQ *);
  290 (* STAREQ *);
  291 (* SLASHEQ *);
  292 (* PERCENTEQ *);
  293 (* LBRACE *);
  294 (* RBRACE *);
  295 (* LPAREN *);
  296 (* RPAREN *);
  297 (* UNARY *);
  298 (* ASNOP *);
  299 (* MINUSMINUS *);
    0|]

let yytransl_block = [|
  276 (* DECCONST *);
  277 (* HEXCONST *);
  278 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\004\000\
\004\000\004\000\004\000\005\000\007\000\007\000\007\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\009\000\009\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000"

let yylen = "\002\000\
\008\000\000\000\002\000\003\000\002\000\002\000\003\000\002\000\
\004\000\002\000\004\000\003\000\001\000\001\000\003\000\003\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\027\000\019\000\018\000\000\000\
\000\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\003\000\005\000\006\000\028\000\029\000\030\000\031\000\032\000\
\033\000\000\000\025\000\000\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\015\000\001\000\000\000\
\016\000\000\000\000\000\022\000\023\000\024\000\000\000\000\000"

let yydgoto = "\002\000\
\004\000\015\000\016\000\017\000\018\000\026\000\019\000\042\000\
\027\000"

let yysindex = "\004\000\
\243\254\000\000\237\254\000\000\247\254\254\254\251\254\012\255\
\000\000\238\254\244\254\000\000\012\255\248\254\006\255\012\255\
\027\255\031\255\029\255\000\000\000\000\000\000\000\000\238\254\
\238\254\028\255\000\000\021\255\044\255\036\255\019\255\076\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\238\254\000\000\013\255\000\000\238\254\238\254\238\254\
\238\254\238\254\238\254\238\254\000\000\000\000\000\000\040\255\
\000\000\043\255\043\255\000\000\000\000\000\000\040\255\040\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\255\
\000\000\000\000\000\000\000\000\039\255\000\000\000\000\039\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\059\255\060\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\061\255\
\000\000\245\254\249\254\000\000\000\000\000\000\062\255\063\255"

let yygindex = "\000\000\
\000\000\032\000\000\000\000\000\000\000\232\255\069\000\000\000\
\000\000"

let yytablesize = 83
let yytable = "\043\000\
\044\000\020\000\021\000\022\000\001\000\005\000\023\000\020\000\
\024\000\028\000\003\000\021\000\029\000\009\000\020\000\020\000\
\012\000\056\000\021\000\021\000\025\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\020\000\006\000\014\000\008\000\
\021\000\009\000\010\000\011\000\012\000\007\000\046\000\047\000\
\048\000\049\000\050\000\032\000\030\000\034\000\045\000\033\000\
\013\000\035\000\014\000\051\000\057\000\046\000\047\000\048\000\
\049\000\050\000\054\000\036\000\037\000\038\000\039\000\040\000\
\041\000\046\000\047\000\048\000\049\000\050\000\048\000\049\000\
\050\000\053\000\052\000\055\000\002\000\008\000\010\000\012\000\
\009\000\011\000\031\000"

let yycheck = "\024\000\
\025\000\020\001\021\001\022\001\001\000\025\001\025\001\019\001\
\027\001\022\001\024\001\019\001\025\001\022\001\026\001\027\001\
\025\001\042\000\026\001\027\001\039\001\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\040\001\039\001\039\001\037\001\
\040\001\022\001\023\001\024\001\025\001\040\001\026\001\027\001\
\028\001\029\001\030\001\038\001\013\000\019\001\019\001\016\000\
\037\001\019\001\039\001\031\001\040\001\026\001\027\001\028\001\
\029\001\030\001\040\001\031\001\032\001\033\001\034\001\035\001\
\036\001\026\001\027\001\028\001\029\001\030\001\028\001\029\001\
\030\001\038\001\031\001\000\000\038\001\019\001\019\001\019\001\
\019\001\019\001\014\000"

let yynames_const = "\
  EOF\000\
  STRUCT\000\
  TYPEDEF\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  CONTINUE\000\
  BREAK\000\
  ASSERT\000\
  TRUE\000\
  FALSE\000\
  NULL\000\
  ALLOC\000\
  ALLOCARRY\000\
  BOOL\000\
  VOID\000\
  CHAR\000\
  STRING\000\
  SEMI\000\
  RETURN\000\
  INT\000\
  MAIN\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  PERCENT\000\
  ASSIGN\000\
  PLUSEQ\000\
  MINUSEQ\000\
  STAREQ\000\
  SLASHEQ\000\
  PERCENTEQ\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  UNARY\000\
  ASNOP\000\
  MINUSMINUS\000\
  "

let yynames_block = "\
  DECCONST\000\
  HEXCONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    Obj.repr(
# 81 "lab1/parse/c0Parser.mly"
                                                 ( _6 )
# 276 "lab1/parse/c0Parser.ml"
               : Ast.stm list))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "lab1/parse/c0Parser.mly"
                                ( [] )
# 282 "lab1/parse/c0Parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 86 "lab1/parse/c0Parser.mly"
                                ( _1::_2 )
# 290 "lab1/parse/c0Parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 87 "lab1/parse/c0Parser.mly"
                                ( _2 )
# 297 "lab1/parse/c0Parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    Obj.repr(
# 91 "lab1/parse/c0Parser.mly"
                                 ( marks (A.Declare _1) (1, 1) )
# 304 "lab1/parse/c0Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simp) in
    Obj.repr(
# 92 "lab1/parse/c0Parser.mly"
                                 ( marks _1 (1, 1) )
# 311 "lab1/parse/c0Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 93 "lab1/parse/c0Parser.mly"
                                 ( marks (A.Return _2) (1, 2) )
# 318 "lab1/parse/c0Parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Symbol.t) in
    Obj.repr(
# 97 "lab1/parse/c0Parser.mly"
                                ( A.NewVar _2 )
# 325 "lab1/parse/c0Parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Symbol.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 98 "lab1/parse/c0Parser.mly"
                                ( A.Init (_2, _4) )
# 333 "lab1/parse/c0Parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "lab1/parse/c0Parser.mly"
                                ( A.NewVar (Symbol.symbol "main") )
# 339 "lab1/parse/c0Parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 100 "lab1/parse/c0Parser.mly"
                                ( A.Init (Symbol.symbol "main", _4) )
# 346 "lab1/parse/c0Parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'asnop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 104 "lab1/parse/c0Parser.mly"
                                 ( expand_asnop (_1, _2, _3) (1, 3) )
# 355 "lab1/parse/c0Parser.ml"
               : 'simp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Symbol.t) in
    Obj.repr(
# 108 "lab1/parse/c0Parser.mly"
                                ( _1 )
# 362 "lab1/parse/c0Parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "lab1/parse/c0Parser.mly"
                                ( Symbol.symbol "main" )
# 368 "lab1/parse/c0Parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lvalue) in
    Obj.repr(
# 110 "lab1/parse/c0Parser.mly"
                                ( _2 )
# 375 "lab1/parse/c0Parser.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 114 "lab1/parse/c0Parser.mly"
                                 ( _2 )
# 382 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'intconst) in
    Obj.repr(
# 115 "lab1/parse/c0Parser.mly"
                                 ( _1 )
# 389 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "lab1/parse/c0Parser.mly"
                                 ( mark (A.Var (Symbol.symbol "main")) (1, 1) )
# 395 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Symbol.t) in
    Obj.repr(
# 117 "lab1/parse/c0Parser.mly"
                                 ( mark (A.Var _1) (1, 1) )
# 402 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 118 "lab1/parse/c0Parser.mly"
                                 ( mark (A.OpExp (A.PLUS, [_1; _3])) (1, 3) )
# 410 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 119 "lab1/parse/c0Parser.mly"
                                 ( mark (A.OpExp (A.MINUS, [_1; _3])) (1, 3) )
# 418 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 120 "lab1/parse/c0Parser.mly"
                                 ( mark (A.OpExp (A.TIMES, [_1; _3])) (1, 3) )
# 426 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 121 "lab1/parse/c0Parser.mly"
                                 ( mark (A.OpExp (A.DIVIDEDBY, [_1; _3]))
                                     (1, 3) )
# 435 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 123 "lab1/parse/c0Parser.mly"
                                 ( mark (A.OpExp (A.MODULO, [_1; _3])) (1, 3) )
# 443 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 124 "lab1/parse/c0Parser.mly"
                                 ( mark (A.OpExp (A.NEGATIVE, [_2])) (1, 2) )
# 450 "lab1/parse/c0Parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Int32.t) in
    Obj.repr(
# 128 "lab1/parse/c0Parser.mly"
                     ( A.ConstExp _1 )
# 457 "lab1/parse/c0Parser.ml"
               : 'intconst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Int32.t) in
    Obj.repr(
# 129 "lab1/parse/c0Parser.mly"
                     ( A.ConstExp _1 )
# 464 "lab1/parse/c0Parser.ml"
               : 'intconst))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "lab1/parse/c0Parser.mly"
                                ( None )
# 470 "lab1/parse/c0Parser.ml"
               : 'asnop))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "lab1/parse/c0Parser.mly"
                                ( Some A.PLUS )
# 476 "lab1/parse/c0Parser.ml"
               : 'asnop))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "lab1/parse/c0Parser.mly"
                                ( Some A.MINUS )
# 482 "lab1/parse/c0Parser.ml"
               : 'asnop))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "lab1/parse/c0Parser.mly"
                                ( Some A.TIMES )
# 488 "lab1/parse/c0Parser.ml"
               : 'asnop))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "lab1/parse/c0Parser.mly"
                                ( Some A.DIVIDEDBY )
# 494 "lab1/parse/c0Parser.ml"
               : 'asnop))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "lab1/parse/c0Parser.mly"
                                ( Some A.MODULO )
# 500 "lab1/parse/c0Parser.ml"
               : 'asnop))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stm list)
;;
