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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.stm list
