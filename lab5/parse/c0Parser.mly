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
let markg e (left, right) =
  A.Markedg (Mark.mark' (e, ParseState.ext (ploc (left, right))))

(* expand_asnop (id, "op=", exp) region = "id = id op exps"
 * or = "id = exp" if asnop is "="
 * syntactically expands a compound ment operator
 *)
let expand_asnop a b =
  match a, b with
    (id, None, exp), (left, right) ->
      A.Assign(id, exp)
  | (id, Some oper, exp), (left, right) ->
    (* match id with
    | A.LArrDeref (v, e) -> A.ArrAsnop (id, oper, exp)
    | _ -> A.Assign(id, mark (A.Binop (oper, id, exp)) (left, right)) *)
    A.Asnop(id, oper, exp)
(*
let expand_postop a b =
  let (id, oper) = a
  and (left, right) = b
  in match id with
  | A.LArrDeref (v, e) -> A.ArrAsnop (id, oper, A.ConstExp Int32.one)
  | _ -> A.Assign (id, mark (A.Binop (oper, id, A.ConstExp Int32.one)) (left, right))
 *)

 let expand_postop a b =
   let (id, oper) = a
   and (left, right) = b
   in A.Asnop (id, oper, A.ConstExp Int32.one)



let check_for init e step body =
  match step with
  | A.Declare _ -> ErrorMsg.error None ("step statement shouldn't be decl in for loop");
          raise ErrorMsg.Error
  | _ -> match init with
         | A.Declare (a, _) -> A.Declare (a, A.While(e, A.Seq(body, step)))
         | _ -> A.Seq(init, A.While(e, A.Seq(body, step)))

let process_type id t =
  Types.types (Symbol.name id);
  A.NewType(id, t)

let process_star id1 id2 =
  match Types.find (Symbol.name id1) with
  | true -> A.Declare(A.NewVar (id2, A.POINTER ( A.IDENT id1)), A.Nop)
  | false -> A.Exp (A.Binop (A.TIMES, A.Var id1, A.Var id2))

let process_star_stmts id1 id2 stmts =
  match Types.find (Symbol.name id1) with
  | true -> A.Declare(A.NewVar (id2, A.POINTER (A.IDENT id1)), stmts)
  | false -> A.Seq (A.Exp (A.Binop (A.TIMES, A.Var id1, A.Var id2)), stmts)
%}

%token EOF
%token STRUCT TYPEDEF IF ELSE WHILE FOR CONTINUE BREAK
%token ASSERT TRUE FALSE NULL ALLOC ALLOCARRY
%token BOOL VOID CHAR STRING
%token SEMI QUESTIONMARK COLON COMA MAIN
%token DOT RARROW
%token <Int32.t> DECCONST
%token <Int32.t> HEXCONST
%token <Symbol.t> IDENT TYPEIDENT
%token RETURN
%token INT
%token PLUS MINUS STAR SLASH PERCENT LESSTHAN LESSEQ GREATERTHAN GREATEQ
%token COMPAREEQ COMPARENOTEQ LOGICALAND LOGICALOR BINARYAND BINARYXOR
%token BINARYOR SHIFTLEFT SHIFTRIGHT
%token ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token ANDEQ XOREQ OREQ LEFTSHIFTEQ RIGHTSHIFTEQ
%token LBRACE RBRACE
%token LPAREN RPAREN
%token LBRACKET RBRACKET BRACKETS
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

%type <Ast.gdecl list> program

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
%right UNARY DEREF
%right POSTOP
%right PLUSPLUS MINUSMINUS
%nonassoc LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET DOT RARROW BRACKETS

%start program

%%

program :
  /* empty */                   { [] }
  | gdecl program               { $1::$2 }
  ;

gdecl :
  fdecl                         { $1 }
  | fdefn                       { $1 }
  | typedef                     { $1 }
  | sdecl                       { $1 }
  | sdef                        { $1 }

fdecl :
  types IDENT paramlist SEMI        { A.FDecl ($1, $2, $3) }
  | types MAIN LPAREN RPAREN SEMI   { A.MainDecl $1 }

fdefn :
  types IDENT paramlist block       { A.FDefn ($1, $2, $3, $4) }
  | types MAIN LPAREN RPAREN block  {  A.Main ($1, $5) }

param :
  types IDENT                       { A.Param ($2, $1) }
  | types MAIN                      { A.Param (Symbol.symbol "main", $1)}

paramlist :
  LPAREN RPAREN                       { [] }
  | LPAREN param paramfollow RPAREN   { $2::$3 }

paramfollow :
  /* empty */                   { [] }
  | COMA param paramfollow      { $2::$3 }

typedef :
  TYPEDEF types IDENT SEMI      { process_type $3 $2 }
  | TYPEDEF bigtype IDENT SEMI  { process_type $3 $2 }

sdecl :
  STRUCT IDENT SEMI             { A.SDecl $2 }
  | STRUCT TYPEIDENT SEMI       { A.SDecl $2 }
  | STRUCT MAIN SEMI            { A.SDecl (Symbol.symbol "main") }

sdef :
  STRUCT IDENT LBRACE fieldlist RBRACE SEMI { A.SDef ($2, $4) }
  | STRUCT TYPEIDENT LBRACE fieldlist RBRACE SEMI { A.SDef ($2, $4) }
  | STRUCT MAIN LBRACE fieldlist RBRACE SEMI { A.SDef (Symbol.symbol "main", $4) }

field :
  types IDENT SEMI              { A.Param ($2, $1) }
  | bigtype IDENT SEMI          { A.Param ($2, $1) }
  | types TYPEIDENT SEMI              { A.Param ($2, $1) }
  | bigtype TYPEIDENT SEMI          { A.Param ($2, $1) }
  | types MAIN SEMI             { A.Param (Symbol.symbol "main", $1) }
  | bigtype MAIN SEMI           { A.Param (Symbol.symbol "main", $1) }

fieldlist :
  /* empty */                   { [] }
  | field fieldlist             { $1::$2 }

arglist :
  LPAREN RPAREN                 { [] }
  | LPAREN exp argfollow RPAREN { $2::$3 }

argfollow :
  /* empty */                   { [] }
  | COMA exp argfollow          { $2::$3 }

block :
  LBRACE stmts RBRACE           { (A.Block $2) }
  ;

types :
  INT                           { A.INT }
  | BOOL                        { A.BOOL }
  | VOID                        { A.VOID }
  | TYPEIDENT                   { A.IDENT $1}
  | types STAR                  { A.POINTER $1 }
  | bigtype STAR                { A.POINTER $1 }
  | types BRACKETS              { A.ARRAY $1 }
  | bigtype BRACKETS            { A.ARRAY $1 }
  ;

bigtype :
  STRUCT IDENT                  { A.STRUCT $2 }
  | STRUCT TYPEIDENT            { A.STRUCT $2 }
  | STRUCT MAIN                 { A.STRUCT (Symbol.symbol "main") }

stmts :
  /* empty */                   { A.Nop }
 | decl SEMI stmts              { marks (A.Declare ($1, $3)) (1, 3) }
 | stmt stmts                   { A.Seq ($1, $2) }
 ;

stmt :
 | STAR exp postop             { ErrorMsg.error None ("invalid operation"); raise ErrorMsg.Error }
 | simp SEMI                     {  marks $1 (1, 1) }
 | control                       { marks $1 (1, 1) }
 | block                         { $1 }
 ;


decl :
  types IDENT                     { A.NewVar ($2 ,$1) }
 | types IDENT ASSIGN exp         { A.Init ($2, $1, $4) }
 | types MAIN                     { A.NewVar (Symbol.symbol "main", $1) }
 | types MAIN ASSIGN exp          { A.Init (Symbol.symbol "main", $1, $4) }
 ;

simp :
    exp asnop exp %prec ASNOP   { expand_asnop ($1, $2, $3) (1, 3) }
  | exp postop %prec POSTOP   { expand_postop ($1, $2) (1, 2) }
  | exp                          { A.Exp $1 }
  | decl 			 { A.Declare ($1, A.Nop) }
  ;

control :
  IF LPAREN exp RPAREN stmt elseopt                       { (A.If ($3, $5, $6)) }
  | WHILE LPAREN exp RPAREN stmt                          { (A.While ($3, $5)) }
  | FOR LPAREN simpopt SEMI exp SEMI simpopt RPAREN stmt  { A.For (check_for $3 $5 $7 $9) }
  | RETURN exp SEMI                                       { (A.Return $2) }
  | RETURN SEMI                                           { (A.VoidReturn)}
  | ASSERT LPAREN exp RPAREN SEMI                         { A.Assert $3 }
  ;

simpopt :
  /* empty */                   { A.Nop }
  | simp                        { $1 }
  ;

elseopt :
  /* empty */                   { A.Nop }
  | ELSE stmt                   { $2 }
  ;

/* lvalue :
  IDENT                         { A.LVar $1 }
 | LPAREN lvalue RPAREN         { $2 }
 | lvalue DOT IDENT             { A.LDot ($1, $3) }
 | lvalue RARROW IDENT          { A.LRArrow ($1, $3) }
 | STAR lvalue %prec UNARY      { A.LDeref $2 }
 | lvalue LBRACKET exp RBRACKET { A.LArrDeref ($1, $3)}
 ; */

exp :
  LPAREN exp RPAREN               { $2 }
 | intconst                       { $1 }
 | boolconst                      { $1 }
 /* | lvalue                         { mark (A.LVAL $1) (1,1) } */
 | IDENT                          { (A.Var $1) }
 | MAIN                           { (A.Var (Symbol.symbol "main")) }
 | NULL                           { A.Null }
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
 | exp SLASH exp                  { mark (A.Binop (A.DIVIDEDBY, $1, $3)) (1, 3)}
 | exp STAR exp                   { mark (A.Binop (A.TIMES, $1, $3)) (1, 3)}
 | exp PERCENT exp                { mark (A.Binop (A.MODULO, $1, $3)) (1, 3)}
 | unop exp %prec UNARY           { mark (A.Unop ($1, $2)) (1, 2) }
 | IDENT arglist                  { mark (A.Call ($1, $2)) (1, 2)}
 | MAIN LPAREN RPAREN             { mark (A.Call ((Symbol.symbol "main"), [])) (1,2) }
 | exp DOT IDENT                  {  (A.LDot ($1, $3)) }
 | exp DOT MAIN                   { (A.LDot ($1, Symbol.symbol "main")) }
 | exp DOT TYPEIDENT                  {  (A.LDot ($1, $3))}
 | exp RARROW IDENT               {  (A.LRArrow ($1, $3)) }
 | exp RARROW MAIN                {  (A.LRArrow ($1, Symbol.symbol "main")) }
 | exp RARROW TYPEIDENT               {  (A.LRArrow ($1, $3)) }
 | ALLOC LPAREN types RPAREN      { mark (A.Alloc ($3)) (1, 4) }
 | ALLOC LPAREN bigtype RPAREN    { mark (A.Alloc ($3)) (1, 4) }
 | STAR exp %prec UNARY           {  (A.LDeref $2) }
 | ALLOCARRY LPAREN types COMA exp RPAREN {mark (A.AllocArray ($3, $5)) (1, 6) }
 | ALLOCARRY LPAREN bigtype COMA exp RPAREN {mark (A.AllocArray ($3, $5)) (1, 6) }
 | exp LBRACKET exp RBRACKET      {  (A.LArrDeref ($1, $3)) }
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
