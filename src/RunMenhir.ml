type parse_result =
  [ `Fail of string
  | `Ok of
     (string * (string list * string list * Language.Stmt.t)) list *
     Language.Stmt.t ]

let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let run_parser ~filename contents =
  let lexbuf = Lexing.from_string contents in
  let () = lexbuf.Lexing.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename } in
  try `Ok (LamaMenhir.toplevel LamaLexer.read lexbuf)
  with
  | LamaLexer.SyntaxError msg ->
    `Fail (Format.asprintf "%a: %s\n" print_position lexbuf msg)
  | LamaMenhir.Error ->
    `Fail (Format.asprintf "%a: syntax error\n" print_position lexbuf)
