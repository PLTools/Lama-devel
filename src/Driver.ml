open Ostap

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.string s
       inherit Util.Lexers.char   s
       inherit Util.Lexers.ident ["skip"; "if"; "then"; "else"; "elif"; "fi"; "while"; "do"; "od"; "repeat"; "until"; "for"; "fun"; "local"; "return"; "length"] s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip.nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.parse) -EOF))

module ArgInfo = struct
  type t =
    { mutable interpret: bool
    ; mutable stack: bool
    ; mutable file: string
    ; mutable menhir: bool
    ; mutable dparsetree: bool
    }
  let empty () = { interpret = false; stack=false; file=""; menhir=false; dparsetree = false }
  let to_compile { stack; interpret } = not (interpret || stack)
  let parse_args nfo =
    Arg.parse
      [ ("-i", Arg.Unit (fun () -> nfo.interpret <- true), "interpret")
      ; ("-s", Arg.Unit (fun () -> nfo.stack <- true), "stack")
      ; ("-m", Arg.Unit (fun () -> nfo.menhir <- true), "use menhir")
      ; ("-pc", Arg.Unit (fun () -> nfo.menhir <- false), "use ostap (default) ")
      ; ("-dparsetree", Arg.Unit (fun () -> nfo.dparsetree <- true), "dump parsetree")
      ]
      (fun s -> nfo.file <- s )
      "Usage: rc [-i | -s] <input file.expr>\n"

  let infile { file } = file
  let is_interpret { interpret } = interpret
  let dparsetree { dparsetree } = dparsetree

  let print_position outx lexbuf =
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Format.fprintf outx "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


  let parse { menhir; file } =
    if not menhir
    then parse file
    else
      let lexbuf = Lexing.from_string (Util.read file) in
      let () = lexbuf.Lexing.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file } in
      try `Ok (LamaMenhir.toplevel LamaLexer.read lexbuf)
    with
      | LamaLexer.SyntaxError msg ->
        `Fail (Format.asprintf "%a: %s\n" print_position lexbuf msg)
      | LamaMenhir.Error ->
        `Fail (Format.asprintf "%a: syntax error\n" print_position lexbuf)



end

let main =
  (* try *)
    let args = ArgInfo.empty () in
    let () = ArgInfo.parse_args args in
    (* let interpret  = Sys.argv.(1) = "-i"  in *)
    (* let stack      = Sys.argv.(1) = "-s"  in *)
    (* let to_compile = not (interpret || stack) in *)
    (* let infile     = Sys.argv.(if not to_compile then 2 else 1) in *)
    match ArgInfo.parse args with
    | `Ok prog ->
      let () =
        if ArgInfo.dparsetree args
        then (
          Format.printf "%s\n%!" (GT.show GT.list (GT.show Language.Definition.t) @@ fst prog);
          Format.printf "%s\n%!" (GT.show Language.Stmt.t @@ snd prog);
        )
      in
      if ArgInfo.to_compile args
      then
        let basename = Filename.chop_suffix (ArgInfo.infile args) ".expr" in
        ignore @@ X86.build prog basename
      else
        let rec read acc =
          try
            let r = read_int () in
            Printf.printf "> ";
            read (acc @ [r])
                with End_of_file -> acc
        in
        let input = read [] in
        let output =
          if ArgInfo.is_interpret args
          then Language.eval prog input
          else SM.run (SM.compile prog) input
        in
        List.iter (fun i -> Printf.printf "%d\n" i) output
    | `Fail er -> Printf.eprintf "Syntax error: %s\n" er
  (* with Invalid_argument _ ->
    Printf.printf "Usage: rc [-i | -s] <input file.expr>\n" *)
