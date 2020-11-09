type token =
| STRING of string
| IDENT of string
| DECIMAL of int
| CHAR of char
| INT of int
| FLOAT of float
| ID of string
| LBRACK
| RBRACK
| LEFT_BRACE
| RIGHT_BRACE
| LPAREN | RPAREN
| WHILE | DO | OD | FOR | REPEAT | UNTIL | RETURN
| IF | THEN | ELIF | ELSE | FI
| COMMA | MINUS | PLUS | TIMES | DIV
| LT | LE | GT | GE | NEQ | EQEQ
| PERCENT | LAND | LOR
| ASSGN
| LOCAL
| LENGTH
| DOT
| FUN
| SKIP
| SEMICOLON
| COLON
| BACKTICK
| EOF
