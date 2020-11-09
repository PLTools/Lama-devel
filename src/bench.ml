(* How many repetitions should be performed *)
let repeat = 2
(* How much time we should spent on benchmark *)
let timeout = 2

let dirname,filenames =
  let dirname =
    let path1 = "./regression" in
    let path2 = "../regression" in
    if Sys.(file_exists path1 && is_directory path1) then path1
    else if Sys.(file_exists path2 && is_directory path2) then path2
    else failwith (Printf.sprintf "Can't find a directory '%s' or '%s'" path1 path2)
  in
  Format.printf "Looking for samples from: '%s'\n%!" dirname;
  let files =
    let fs = Sys.readdir dirname in
    let r = Str.regexp ".*\\.expr$" in
    List.filter (fun s -> (Str.string_match r s 0) && s <> "Ostap.lama") (Array.to_list fs)
  in
  Format.printf "Tests found: %s\n%!" (GT.show GT.list (GT.show GT.string) files);
  let files = List.map (Printf.sprintf "%s/%s" dirname) files in
  (dirname,files)

(* let filenames = ["regression/test036.expr"] *)

let bench_file file =
  Format.printf "Benchmarking file `%s`\n%!" file;

  let contents = Ostap.Util.read file in
  let wrap (parse: string -> RunMenhir.parse_result) =
    match parse contents with
      | `Ok  r -> snd r
      | `Fail s ->
          Printf.eprintf "Error: %s\n%s\n\n" s (Printexc.get_backtrace ());
          exit 1
  in

  let () =
    let ast2 = wrap (RunMenhir.run_parser ~filename:file) in
    let ast1 = wrap Language.run_parser in

    if ast1<>ast2
    then
      let () = Format.printf "Ostap AST:\n%s\n\nMenhir AST:\n%s\n\n%!"
        (GT.show Language.Stmt.t ast1)
        (GT.show Language.Stmt.t ast2)
      in
      failwith "Two ASTs are not equal"
  in
  Gc.full_major ();
  let run_ostap () =
    let _: Language.Stmt.t = wrap Language.run_parser in
    ()
  in
  let run_menhir () =
    let _: Language.Stmt.t = wrap (RunMenhir.run_parser ~filename:file) in
    ()
  in

  let open Benchmark in
  let res = throughputN ~style:Nil ~repeat timeout
    [ ("Ostap", run_ostap, ())
    ; ("menhir", run_menhir, ())
    ]
  in
  tabulate res

let () = List.iter bench_file filenames
