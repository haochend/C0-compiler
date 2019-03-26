%{
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
      A.Assign(id, mark (A.Binop (oper, A.Var(id), exp)) (left, right))

let expand_postop a b =
  let (id, oper) = a
  and (left, right) = b
  in A.Assign (id, mark (A.Binop (oper, A.Var(id), A.ConstExp Int32.one)) (left, right))

let check_for init e step body =
  match init with
  | A.Declare (a, _) ->
    begin
      match a with
      | A.NewVar (id, _) ->
        ErrorMsg.error None ("undeclared variable `" ^ Symbol.name id ^ "' in for loop");
        raise ErrorMsg.Error
      | _ ->
      begin
        match step with
        | A.Declare (_, _) ->
          ErrorMsg.error None ("step statement shouldn't be decl in for loop");
          raise ErrorMsg.Error
        | _ -> A.Declare (a, A.While(e, A.Seq(body, step)))
      end
    end
  | _ ->
    begin
      match step with
      | A.Declare (_, _) ->
        ErrorMsg.error None ("step statement shouldn't be decl in for loop");
        raise ErrorMsg.Error
      | _ -> A.Seq(init, A.While(e, A.Seq(body, step)))
    end
%}

%token EOF
%token STRUCT TYPEDEF IF ELSE WHILE FOR CONTINUE BREAK
%token ASSERT TRUE FALSE NULL ALLOC ALLOCARRY
%token BOOL VOID CHAR STRING
%token SEMI QUESTIONMARK COLON
%token <Int32.t> DECCONST
%token <Int32.t> HEXCONST
%token <Symbol.t> IDENT
%token RETURN
%token INT
%token MAIN
%token PLUS MINUS STAR SLASH PERCENT LESSTHAN LESSEQ GREATERTHAN GREATEQ
%token COMPAREEQ COMPARENOTEQ LOGICALAND LOGICALOR BINARYAND BINARYXOR
%token BINARYOR SHIFTLEFT SHIFTRIGHT
%token ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token ANDEQ XOREQ OREQ LEFTSHIFTEQ RIGHTSHIFTEQ
%token LBRACE RBRACE
%token LPAREN RPAREN
%token BINARYNOT LOGICALNOT
%token UNARY ASNOP POSTOP
%token MINUSMINUS PLUSPLUS

/* UNARY and ASNOP are dummy terminals.
 * We need dummy terminals if we wish to assign a precedence
 * to a rule that does not correspond to the precedence of
 * the rightmost terminal in that rule.
 * Implicit in this is that precedence can only be inferred
 * terminals. Therefore, don't try to assign precedence to "rules"
 *
 * MINUSMINUS is a dummy terminal to parse fail on.
 */

%type <Ast.stm> program

%right ANDEQ XOREQ OREQ LEFTSHIFTEQ RIGHTSHIFTEQ
%right ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%right QUESTIONMARK COLON
%left LOGICALOR
%left LOGICALAND
%left BINARYOR
%left BINARYXOR
%left BINARYAND
%left COMPAREEQ COMPARENOTEQ
%left LESSTHAN LESSEQ GREATERTHAN GREATEQ
%left SHIFTLEFT SHIFTRIGHT
%left PLUS MINUS
%left STAR SLASH PERCENT
%right LOGICALNOT BINARYNOT
%right PLUSPLUS MINUSMINUS
%right UNARY ASNOP POSTOP
%nonassoc LPAREN

%start program

%%

program :
  INT MAIN LPAREN RPAREN block EOF { $5 }
  ;

block :
  LBRACE stmts RBRACE           { (A.Block $2) }
  ;

typedef :
  INT                           { A.INT }
  | BOOL                        { A.BOOL }
  ;

stmts :
  /* empty */                   { A.Nop }
 | decl SEMI stmts              { marks (A.Declare ($1, $3)) (1, 3) }
 | stmt stmts                   { A.Seq ($1, $2) }
 ;

stmt :
 | simp SEMI                     { marks $1 (1, 1) }
 | control                       { marks $1 (1, 1) }
 | block                         { $1 }
 ;


decl :
  typedef IDENT                     { A.NewVar ($2 ,$1) }
 | typedef IDENT ASSIGN exp         { A.Init ($2, $1, $4) }
 | typedef MAIN                     { A.NewVar (Symbol.symbol "main", $1) }
 | typedef MAIN ASSIGN exp          { A.Init (Symbol.symbol "main", $1, $4) }
 ;

simp :
   lvalue asnop exp %prec ASNOP   { expand_asnop ($1, $2, $3) (1, 3) }
  | lvalue postop %prec POSTOP   { expand_postop ($1, $2) (1, 2) }
  | exp                          { A.Exp $1 }
  | decl                         { A.Declare ($1, A.Nop) }
  ;

control :
  IF LPAREN exp RPAREN stmt elseopt                       { (A.If ($3, $5, $6)) }
  | WHILE LPAREN exp RPAREN stmt                          { (A.While ($3, $5)) }
  | FOR LPAREN simpopt SEMI exp SEMI simpopt RPAREN stmt  { A.For (check_for $3 $5 $7 $9) }
  | RETURN exp SEMI                                       { (A.Return $2) }
  ;

simpopt :
  /* empty */                   { A.Nop }
  | simp                        { $1 }
  ;

elseopt :
  /* empty */                   { A.Nop }
  | ELSE stmt                   { $2 }
  ;

lvalue :
  IDENT                         { $1 }
 | MAIN                         { Symbol.symbol "main" }
 | LPAREN lvalue RPAREN         { $2 }
 ;

exp :
  LPAREN exp RPAREN               { $2 }
 | intconst                       { $1 }
 | boolconst                      { $1 }
 | lvalue                         { mark (A.Var $1) (1,1) }
 | MAIN                           { mark (A.Var (Symbol.symbol "main")) (1, 1) }
 | IDENT                          { mark (A.Var $1) (1, 1) }
 | exp QUESTIONMARK exp COLON exp { mark (A.Question ($1, $3, $5)) (1, 5)}
 | exp LOGICALOR exp              { mark (A.Binop (A.LOGICALOR, $1, $3)) (1, 3)}
 | exp LOGICALAND exp             { mark (A.Binop (A.LOGICALAND, $1, $3)) (1, 3)}
 | exp BINARYOR exp               { mark (A.Binop (A.BINARYOR, $1, $3)) (1, 3)}
 | exp BINARYXOR exp              { mark (A.Binop (A.BINARYXOR, $1, $3)) (1, 3)}
 | exp BINARYAND exp              { mark (A.Binop (A.BINARYAND, $1, $3)) (1, 3)}
 | exp COMPAREEQ exp              { mark (A.Binop (A.ISEQ, $1, $3)) (1, 3)}
 | exp COMPARENOTEQ exp           { mark (A.Binop (A.NOTEQ, $1, $3)) (1, 3)}
 | exp LESSTHAN exp               { mark (A.Binop (A.LESSTHAN, $1, $3)) (1, 3)}
 | exp LESSEQ exp                 { mark (A.Binop (A.LESSOREQ, $1, $3)) (1, 3)}
 | exp GREATERTHAN exp            { mark (A.Binop (A.GREATERTHAN, $1, $3)) (1, 3)}
 | exp GREATEQ exp                { mark (A.Binop (A.GREATEROREQ, $1, $3)) (1, 3)}
 | exp SHIFTLEFT exp              { mark (A.Binop (A.SHIFTLEFT, $1, $3)) (1, 3)}
 | exp SHIFTRIGHT exp             { mark (A.Binop (A.SHIFTRIGHT, $1, $3)) (1, 3) }
 | exp MINUS exp                  { mark (A.Binop (A.MINUS, $1, $3)) (1, 3)}
 | exp PLUS exp                   { mark (A.Binop (A.PLUS, $1, $3)) (1, 3)}
 | exp STAR exp                   { mark (A.Binop (A.TIMES, $1, $3)) (1, 3)}
 | exp SLASH exp                  { mark (A.Binop (A.DIVIDEDBY, $1, $3)) (1, 3)}
 | exp PERCENT exp                { mark (A.Binop (A.MODULO, $1, $3)) (1, 3)}
 | unop exp %prec UNARY           { mark (A.Unop ($1, $2)) (1, 2) }
 ;

intconst :
  DECCONST           { A.ConstExp $1 }
 | HEXCONST          { A.ConstExp $1 }
 ;

boolconst :
  TRUE                { A.BoolExp true }
  | FALSE             { A.BoolExp false }
  ;


unop :
  MINUS                { A.NEGATIVE }
  | LOGICALNOT         { A.LOGICALNOT }
  | BINARYNOT          { A.BINARYNOT }
  ;

asnop :
  ASSIGN                        { None }
 | PLUSEQ                       { Some A.PLUS }
 | MINUSEQ                      { Some A.MINUS }
 | STAREQ                       { Some A.TIMES }
 | SLASHEQ                      { Some A.DIVIDEDBY }
 | PERCENTEQ                    { Some A.MODULO }
 | ANDEQ                        { Some A.BINARYAND }
 | XOREQ                        { Some A.BINARYXOR }
 | OREQ                         { Some A.BINARYOR }
 | LEFTSHIFTEQ                  { Some A.SHIFTLEFT }
 | RIGHTSHIFTEQ                 { Some A.SHIFTRIGHT }
 ;

postop :
  MINUSMINUS                    { A.MINUS }
  | PLUSPLUS                    { A.PLUS }
  ;
%%
