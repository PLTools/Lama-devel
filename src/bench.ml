(* How many repetitions should be performed *)
let repeat = 1

(* How much time we should spent on benchmark *)
let timeout = 1

let dirname, filenames =
  let dirname =
    let path1 = "./regression" in
    let path2 = "../regression" in
    if Sys.(file_exists path1 && is_directory path1) then path1
    else if Sys.(file_exists path2 && is_directory path2) then path2
    else
      failwith
        (Printf.sprintf "Can't find a directory '%s' or '%s'" path1 path2)
  in
  Format.printf "Looking for samples from: '%s'\n%!" dirname;
  let files =
    let fs = Sys.readdir dirname in
    let r = Str.regexp ".*\\.expr$" in
    List.filter
      (fun s -> Str.string_match r s 0 && s <> "Ostap.lama")
      (Array.to_list fs)
  in
  Format.printf "Tests found: %s\n%!"
    (GT.show GT.list (GT.show GT.string) files);
  let files = List.map (Printf.sprintf "%s/%s" dirname) files in
  (dirname, files)

(* let filenames = [ "regression/test036.expr" ] *)

let bench_file file =
  Format.printf "Benchmarking file `%s`\n%!" file;

  let contents = Ostap.Util.read file in
  let wrap (parse : string -> RunMenhir.parse_result) =
    match parse contents with
    | `Ok r -> snd r
    | `Fail s ->
        Printf.eprintf "Error: %s\n%s\n\n" s (Printexc.get_backtrace ());
        exit 1
  in

  let () =
    let check msg1 ast1 msg2 ast2 =
      if ast1 <> ast2 then
        let () =
          Format.printf "%s AST:\n%s\n\n%s AST:\n%s\n\n%!" msg1
            (GT.show Language.Stmt.t ast1)
            msg2
            (GT.show Language.Stmt.t ast2)
        in
        failwith "Two ASTs are not equal"
      else
        (* let () = Format.printf "%s and %s ASTs are OK!\n%!" msg1 msg2 in  *)
        ()
    in
    (* Printf.printf "Calling ostap parser"; *)
    let ast1 = wrap LamaOstapP5.run_parser in

    (* Printf.printf "Calling menhir parser\n"; *)
    let ast2 = wrap (RunMenhir.run_parser ~filename:file) in

    (* Printf.printf "Calling Opal parser\n"; *)
    let ast3 = wrap (LamaOpal.run_parser ~filename:file) in

    check "Ostap" ast1 "Menhir" ast2;
    check "Ostap" ast1 "Opal" ast3;
    check "menhir" ast2 "opal" ast3;
    ()
  in
  Gc.full_major ();
  let run_ostap () =
    let (_ : Language.Stmt.t) = wrap LamaOstapP5.run_parser in
    ()
  in
  let run_menhir () =
    let (_ : Language.Stmt.t) = wrap (RunMenhir.run_parser ~filename:file) in
    ()
  in
  (* let _run_angstrom () =
       let (_ : Language.Stmt.t) = wrap (LamaAngstrom.run_parser ~filename:file) in
       ()
     in *)
  let run_opal () =
    let (_ : Language.Stmt.t) = wrap (LamaOpal.run_parser ~filename:file) in
    ()
  in
  let open Benchmark in
  let res =
    throughputN ~style:Nil ~repeat timeout
      [
        ("Ostap", run_ostap, ());
        ("menhir", run_menhir, ());
        (* ; ("angstrom", run_angstrom, ()) *)
        ("opal", run_opal, ());
      ]
  in
  tabulate res

let () = List.iter bench_file filenames
