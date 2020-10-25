

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

  let parse { menhir; file } =
    print_endline file;
    let s = Ostap.Util.read file in
    if not menhir
    then Language.run_parser s
    else RunMenhir.run_parser ~filename:file s

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
