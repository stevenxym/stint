type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | LBRACK
  | RBRACK
  | LPANGLE
  | LANGLE
  | RANGLE
  | ASSIGN
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NEQ
  | LESS
  | LEQ
  | GRT
  | GEQ
  | AT
  | SPLIT
  | SEARCH
  | RM
  | NOT
  | AND
  | OR
  | COUT
  | CIN
  | INT
  | STR
  | BOOL
  | IF
  | WHILE
  | RETURN
  | OPEN
  | CLOSE
  | BREAK
  | EOF
  | VOID
  | TRUE
  | FALSE
  | STD
  | END
  | LIT_INT of (int)
  | LIT_STR of (string)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
