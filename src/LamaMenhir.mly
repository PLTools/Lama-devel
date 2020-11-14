%token <int> DECIMAL
%token <string> IDENT
%token <char> CHAR
%token <string> STRING
%token PLUS MINUS TIMES DIV
%token FUN
%token SKIP
%token LOCAL
%token RETURN
%token ASSGN LENGTH
%token IF FI THEN ELIF ELSE
%token DO OD FOR WHILE REPEAT UNTIL
%token LPAREN RPAREN LEFT_BRACE RIGHT_BRACE LBRACK RBRACK
%token GT GE LT LE EQEQ NEQ
%token PERCENT LAND LOR
%token SEMICOLON COMMA DOT
%token EOF
%start <Language.t> toplevel
%%

%inline plist(X):
| xs = loption(delimited(LPAREN, separated_list(COMMA, X), RPAREN)) { xs }

%inline op_mul:
  | TIMES { "*" }
  | DIV   { "/" }
  ;
%inline op_add:
  | PLUS  { "+" }
  | MINUS { "-" }
  | PERCENT { "%" }
  ;
%inline op_pred:
  | GT  { ">" }
  | GE  { ">=" }
  | LT  { "<" }
  | LE  { "<=" }
  | NEQ { "!=" }
  | EQEQ { "==" }
  ;
%inline op_log:
  | LAND { "&&" }
  | LOR  { "!!" }
  ;

expr_log:
  | l = expr_log; op = op_log; r = expr_pred { Language.Expr.Binop (op,l,r) }
  | e = expr_pred { e }
  ;
expr_pred:
  | l = expr_pred; op = op_pred; r = expr_add { Language.Expr.Binop (op,l,r) }
  | e = expr_add { e }
  ;
expr_add:
  | l = expr_add; op = op_add; r = expr_mul { Language.Expr.Binop (op,l,r) }
  | e = expr_mul { e }
  ;
expr_mul:
  | l = expr_mul; op = op_mul; r = expr_primary { Language.Expr.Binop (op,l,r) }
  | e = expr_primary { e }
  ;
expr_primary:
  | b = expr_base; is = myindex* {
      let open Language.Expr in
      List.fold_left (fun b -> function `Elem i -> Elem (b, i) | `Len -> Length b) b is
    }
  ;
%inline myindex:
  | LBRACK; idx = expr; RBRACK { `Elem idx }
  | DOT; LENGTH { `Len }
  ;
expr_base:
  | n = DECIMAL  { Language.Expr.Const n }
  | s = STRING   { Language.Expr.String s }
  | c = CHAR     { Language.Expr.Const (Char.code c) }
  | LPAREN; e = expr_log; RPAREN { e }
  | MINUS; e = expr_base { e (* BUG?*) }
  | f = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN { Language.Expr.Call (f, args) }
  | f = IDENT         { Language.Expr.Var f }
  | LBRACK; elems = separated_list(COMMA, expr); RBRACK  { Language.Expr.Array elems }
  ;

expr: e = expr_log { e };

stmts: ss = separated_nonempty_list(SEMICOLON,stmt)
  {
    match List.rev ss with
    | [] -> failwith "should not happen"
    | h::tl -> List.fold_left (fun acc x -> Language.Stmt.Seq (x, acc) ) h tl
  }
  ;

stmt:
  | SKIP { Language.Stmt.Skip }
  | IF; e=expr; THEN; the = stmts;
    elif=list(ELIF; e = expr; THEN; th = stmts { (e,th) });
    els=option(ELSE; br = stmts { br }); FI
      {
        let open Language.Stmt in
        If (e, the,
          List.fold_right
            (fun (e, t) elif -> If (e, t, elif))
            elif
            (match els with None -> Skip | Some s -> s)
        )
      }
  | WHILE; e=expr; DO; s = stmts; OD { Language.Stmt.While (e, s) }
  | FOR; i=stmt; COMMA; c = expr; COMMA; s=stmt; DO; b=stmts; OD
    { let open Language.Stmt in Seq (i, While (c, Seq (b, s))) }
  | REPEAT; s=stmts; UNTIL; e=expr { Language.Stmt.Repeat (s, e) }
  | RETURN; e=expr? { Return e }
  | x = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN  { Language.Stmt.Call (x, args) }
  | x = IDENT; is = list(LBRACK; e = expr; RBRACK { e }); ASSGN; e=expr { Language.Stmt.Assign (x, is, e) }
  ;

arg: a = IDENT  { a };
locals: LOCAL; locs = separated_list(COMMA, arg) { locs };
definition:
  | FUN; name = IDENT; LPAREN; args = separated_list(COMMA, arg); RPAREN;
    locs=locals?;
    LEFT_BRACE; body=stmts; RIGHT_BRACE;
    { (name, (args, (match locs with None -> [] | Some l -> l), body))
    };

toplevel: defs = list(definition); last=stmts; EOF { (defs, last) };
