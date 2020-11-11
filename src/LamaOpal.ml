open Language
open GenParser

module OpalImpl = struct
  open Opal

  module I = struct
    type ('a, 'b) t = ('a, 'b) Opal.parser

    let opt p =
      (* print_endline "opt asked";
         ( (p => fun x -> Some x) <|> fun s ->
           print_endline "returnig none";
           return None s )
           s *)
      option None (p => fun x -> Some x)

    let guard p cond _ = p >>= fun r -> if cond r then return r else mzero

    let seq = ( >>= )

    let ( >>= ) = seq

    let return = return

    let empty : ('t, unit) t = fun s -> return () s

    let many = many

    let alt a b = choice [ a; b ]

    let altl l = List.fold_left ( <|> ) mzero l

    let map f x = x => f

    let ( => ) = ( => )

    let spaces stream =
      (* Printf.printf "spaces asked when stream %s empty: %s \n"
         (if stream = LazyStream.Nil then "IS" else "IS NOT")
         ( match stream with
         | LazyStream.Cons (c, t) ->
             Printf.sprintf "('%c'=%d) :: ???" c (Char.code c)
         | Nil -> "[]" ); *)
      skip_many (one_of [ '\t'; '\r'; '\n'; ' ' ]) stream

    let eof : (char, unit) t =
     fun stream ->
      (* Printf.printf "eof asked when stream %s empty: %s \n"
         (if stream = LazyStream.Nil then "IS" else "IS NOT")
         ( match stream with
         | LazyStream.Cons (c, t) ->
             Printf.sprintf "('%c'=%d) :: ???" c (Char.code c)
         | Nil -> "[]" ); *)
      (spaces >>= fun _ -> eof ()) stream

    let token s stream =
      (* Printf.printf "token '%s' asked\n" s; *)
      token s stream

    let ( *> ) f g = f >>= fun _ -> g

    let ( <* ) f g =
      f >>= fun r ->
      g >>= fun _ -> return r

    let rec fix : (('a, 'b) t -> ('a, 'b) t) -> ('a, 'b) t =
     fun p stream -> p (fun s -> fix p s) stream

    let string s =
      (* let () = print_endline "string called" in *)
      ( token "\"" *> many alpha_num <* token "\"" => fun xs ->
        Printf.sprintf "%S" (implode xs) )
        s

    let decimal : (char, int) t =
     fun stream ->
      (* let () =
           print_endline "decimal called";
           Printf.printf "decimal asked when stream %s empty: %s \n"
             (if stream = LazyStream.Nil then "IS" else "IS NOT")
             ( match stream with
             | LazyStream.Cons (c, t) ->
                 Printf.sprintf "('%c'=%d) :: ???" c (Char.code c)
             | Nil -> "[]" )
         in *)
      ( many1 digit => fun xs ->
        List.fold_left
          (fun acc x -> (acc * 10) + Char.code x - Char.code '0')
          0 xs )
        stream

    (* parses 'c' *)
    let char s =
      let () =
        (* print_endline "char called";
           Printf.printf "decimal asked when stream %s empty: %s \n"
             (if s = LazyStream.Nil then "IS" else "IS NOT")
             ( match s with
             | LazyStream.Cons (c, t) ->
                 Printf.sprintf "('%c'=%d) :: ???" c (Char.code c)
             | Nil -> "[]" ); *)
        ()
      in

      choice
        [
          Opal.token "'\n'" *> return '\n';
          Opal.token "'\t'" *> return '\t';
          Opal.exactly '\'' *> alpha_num <* Opal.exactly '\'';
        ]
        s

    let ident =
      spaces *> letter >>= fun h ->
      many (alpha_num <|> exactly '_') >>= fun tl -> return (implode (h :: tl))

    let lexeme l =
      spaces *> token l => fun x ->
      (* Printf.printf "lexeme %s eaten\n" x; *)
      x

    let debug msg stream =
      print_endline msg;
      return () stream
  end

  include I
end

let is_keyword s = List.mem s [ "return"; "if"; "fi"; "else"; "do"; "od" ]

module Expr (P : PExt) = struct
  open P
  module Util = OUtil (P)

  type 'a d = {
    parse : 'a d -> (char, 'a) t;
    primary : 'a d -> (char, 'a) t;
    base : 'a d -> (char, 'a) t;
  }

  let parse d =
    fix @@ fun _ ->
    Util.expr
      (fun x -> x)
      (Array.map
         (fun (a, s) ->
           ( a,
             List.map
               (fun s_ ->
                 ( (lexeme s_ >>= fun _ -> return ()),
                   fun x y -> Expr.Binop (s_, x, y) ))
               s ))
         [|
           (`Lefta, [ "!!" ]);
           (`Lefta, [ "&&" ]);
           (`Nona, [ "=="; "!="; "<="; "<"; ">="; ">" ]);
           (`Lefta, [ "+"; "-" ]);
           (`Lefta, [ "*"; "/"; "%" ]);
         |])
      (d.primary d)

  let primary d =
    fix @@ fun _ ->
    let suffix =
      alt
        (lexeme "[" *> d.parse d <* lexeme "]" => fun x -> `Elem x)
        (lexeme "." *> lexeme "length" => fun _ -> `Len)
    in
    d.base d >>= fun b ->
    many suffix >>= fun is ->
    return
      (List.fold_left
         (fun b -> function `Elem i -> Expr.Elem (b, i) | `Len -> Length b)
         b is)

  let ident = guard ident (fun k -> not (is_keyword k)) None

  let base d =
    fix @@ fun _ ->
    altl
      [
        (spaces *> decimal => fun x -> Expr.Const x);
        ( spaces *> string => fun s ->
          Expr.String (String.sub s 1 (String.length s - 2)) );
        (spaces *> char => fun c -> Expr.Const (Char.code c));
        (lexeme "[" *> list0 (d.parse d) <* lexeme "]" => fun a -> Expr.Array a);
        ( lexeme "`" *> ident >>= fun t ->
          opt (lexeme "(" *> list (d.parse d) <* lexeme ")") >>= fun args ->
          return
            (Expr.Sexp (t, match args with None -> [] | Some args -> args)) );
        ( ident >>= fun x ->
          alt
            ( lexeme "(" *> list0 (d.parse d) <* lexeme ")" => fun args ->
              Expr.Call (x, args) )
            (empty *> return (Expr.Var x)) );
        parens (d.parse d);
      ]

  let d = { parse; base; primary }
end

let __ () =
  let module E = Expr (Helpers (OpalImpl)) in
  match Opal.parse E.(d.parse d) (Opal.LazyStream.of_string "1+2") with
  | None -> failwith "It had to succeed"
  | Some x ->
      Printf.printf "%s\n" (GT.show Language.Expr.t x);
      ()

(* let () = Printf.printf "%s %d\n" __FILE__ __LINE__ *)

module Stmt (P : PExt) = struct
  open P
  module E = Expr (P)

  type 'a d = { parse : 'a d -> (char, 'a) t; stmt : 'a d -> (char, 'a) t }

  let foldr1_exn f xs =
    let rec helper = function
      | [] -> failwith "bad argument"
      | [ x ] -> x
      | x :: xs -> f x (helper xs)
    in

    match xs with [] -> failwith "bad argument" | xs -> helper xs

  let parse d =
    fix @@ fun self ->
    (* d.stmt d >>= fun h ->
       many (lexeme ";" *> self) >>= fun ss ->
       return
         ( match ss with
         | [] -> h
         | _ -> foldr1_exn (fun s ss -> Stmt.Seq (s, ss)) (h :: ss) ) *)
    alt
      ( d.stmt d >>= fun s ->
        (* debug "ask;" *>  *)
        lexeme ";" *> d.parse d >>= fun ss -> return (Stmt.Seq (s, ss)) )
      (d.stmt d)

  let ident = guard ident (fun k -> not (is_keyword k)) None

  let stmt d =
    fix @@ fun _ ->
    altl
      [
        lexeme "skip" *> return Stmt.Skip;
        ( (lexeme "if" *> E.(d.parse d)) >>= fun e ->
          lexeme "then" *> d.parse d >>= fun the ->
          many
            ( (lexeme "elif" *> E.(d.parse d)) >>= fun l ->
              lexeme "then" *> d.parse d >>= fun r -> return (l, r) )
          >>= fun elif ->
          opt (lexeme "else" *> d.parse d) >>= fun els ->
          lexeme "fi" => fun _ ->
          Stmt.If
            ( e,
              the,
              List.fold_right
                (fun (e, t) elif -> Stmt.If (e, t, elif))
                elif
                (match els with None -> Stmt.Skip | Some s -> s) ) );
        ( (lexeme "while" *> E.(d.parse d)) >>= fun e ->
          lexeme "do" *> d.parse d >>= fun s ->
          lexeme "od" *> return (Stmt.While (e, s)) );
        ( lexeme "for" *> d.parse d >>= fun i ->
          (lexeme "," *> E.(d.parse d)) >>= fun c ->
          lexeme "," *> d.parse d >>= fun s ->
          lexeme "do" *> d.parse d >>= fun b ->
          lexeme "od" *> return (Stmt.Seq (i, While (c, Seq (b, s)))) );
        ( lexeme "repeat" *> d.parse d >>= fun s ->
          (lexeme "until" *> E.(d.parse d)) >>= fun e ->
          return (Stmt.Repeat (s, e)) );
        ( lexeme "return" *> spaces *> opt E.(d.parse d) => fun e ->
          Stmt.Return e );
        ( ident >>= fun x ->
          alt
            ( many ((lexeme "[" *> E.(d.parse d)) <* lexeme "]") >>= fun is ->
              (lexeme ":=" *> E.(d.parse d)) >>= fun e ->
              return (Stmt.Assign (x, is, e)) )
            ( parens (list0 E.(d.parse d)) >>= fun args ->
              return (Stmt.Call (x, args)) ) );
      ]

  let d = { parse; stmt }

  let parse = d.parse d
end

let __ () =
  let module S = Stmt (Helpers (OpalImpl)) in
  let func = S.(d.parse d) in

  match
    Opal.parse func
      (Opal.LazyStream.of_string
         "n := read ();\nwhile  do\n\n\n      skip od\n")
  with
  | None -> failwith "It had to succeed"
  | Some x ->
      Printf.printf "%s\n" (GT.show Language.Stmt.t x);
      ()

module Definition (P : PExt) = struct
  open P
  module S = Stmt (P)

  let arg = ident

  let parse =
    lexeme "fun" *> ident >>= fun name ->
    parens (list0 arg) >>= fun args ->
    opt (lexeme "local" *> list arg) >>= fun locs ->
    (lexeme "{" *> S.(d.parse d)) <* lexeme "}" >>= fun body ->
    return (name, (args, (match locs with None -> [] | Some l -> l), body))
end

(* let () = Printf.printf "%s %d\n" __FILE__ __LINE__ *)

let run_parser ~filename contents =
  let parse =
    let module I = Helpers (OpalImpl) in
    let module D = Definition (I) in
    let module S = Stmt (I) in
    let open I in
    many D.parse >>= fun defs ->
    S.parse >>= fun s -> eof *> return (defs, s)
  in
  match Opal.parse parse (Opal.LazyStream.of_string contents) with
  | None -> `Fail ""
  | Some x -> `Ok x

let () =
  let module I = Helpers (OpalImpl) in
  let open I in
  let p = spaces *> opt decimal in
  let s = "  1" in
  match Opal.parse p (Opal.LazyStream.of_string s) with
  | None ->
      failwith (Printf.sprintf "%s %d It had to succeed" __FILE__ __LINE__)
  | Some _ -> ()

let () =
  let module S = Stmt (Helpers (OpalImpl)) in
  let s = "while do skip od" in
  let s = "repeat skip until 1" in

  let s = "while 1 do skip od" in
  let s = "fun f () { if 1 then return fi; } skip" in
  let s = "if 1 then return fi" in
  let s = "x := 'a'; skip" in
  (* let s = "if 'a' then skip fi" in *)
  match run_parser ~filename:"" s with
  | `Fail _ ->
      failwith (Printf.sprintf "%s %d It had to succeed" __FILE__ __LINE__)
  | `Ok (_, x) ->
      Printf.printf "%s\n" (GT.show Language.Stmt.t x);
      ()
