type token =
  | ID of (string)
  | ID_Q of (string)
  | CSTDEC of (string)
  | CSTBASE of (string)
  | CSTSTR of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EXPONENT
  | EQUALITY
  | INEQUALITY
  | LESSEQUAL
  | SUPEQUAL
  | LESSTHAN
  | SUPTHAN
  | MOD
  | REM
  | AND
  | OR
  | XOR
  | UMINUS
  | ABS
  | NOT
  | AND_THEN
  | OR_ELSE
  | LPAREN
  | RPAREN
  | COMMA
  | SEMICOLON
  | RETIQUETTE
  | LETIQUETTE
  | COLON
  | LOOP
  | END_LOOP
  | WHILE
  | FOR
  | IN
  | REVERSE
  | RANGE
  | IF
  | THEN
  | ELSEIF
  | ELSE
  | END_IF
  | AFFECT
  | FROMTO
  | GOTO
  | EXIT
  | WHEN
  | CONST
  | IS
  | TYPE
  | SUBTYPE
  | RENAMES
  | PROCED
  | FUNC
  | OUT
  | IN_OUT
  | BEGIN
  | RETURN
  | END
  | NULL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.file
