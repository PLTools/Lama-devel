module type P = sig
  type ('t, 'a) t

  val ( => ) : ('t, 'a) t -> ('a -> 'b) -> ('t, 'b) t

  val map : ('a -> 'b) -> ('t, 'a) t -> ('t, 'b) t

  val altl : ('t, 'a) t list -> ('t, 'a) t

  val alt : ('t, 'a) t -> ('t, 'a) t -> ('t, 'a) t

  val many : ('t, 'a) t -> ('t, 'a list) t

  val empty : ('t, unit) t

  val return : 'a -> ('t, 'a) t

  val ( >>= ) : ('t, 'a) t -> ('a -> ('t, 'b) t) -> ('t, 'b) t

  val ( *> ) : ('t, 'a) t -> ('t, 'b) t -> ('t, 'b) t

  val ( <* ) : ('t, 'a) t -> ('t, 'b) t -> ('t, 'a) t

  val seq : ('t, 'a) t -> ('a -> ('t, 'b) t) -> ('t, 'b) t

  (* val guard : ('a, 'b, 'c) t -> ('b -> bool) -> ('b -> 'c) option -> ('a, 'b, 'c) t *)

  val guard : ('a, 'b) t -> ('b -> bool) -> ('b -> unit) option -> ('a, 'b) t

  val opt : ('a, 'b) t -> ('a, 'b option) t

  val fix : (('a, 'b) t -> ('a, 'b) t) -> ('a, 'b) t

  val token : string -> (char, string) t

  (* almost token but modulo whitespace *)
  val lexeme : string -> (char, string) t

  val decimal : (char, int) t

  val ident : (char, string) t

  val eof : (char, unit) t

  (* should return a string with leading and ending quotes *)
  val string : (char, string) t

  val char : (char, char) t

  val debug : string -> ('tok, unit) t

  val spaces : (char, unit) t
end

module type PExt = sig
  include P

  (* comma-separated list of >=0 values *)
  val list0 : (char, 'r) t -> (char, 'r list) t

  (* comma-separated list of >=1 values *)
  val list : (char, 'r) t -> (char, 'r list) t

  val parens : (char, 'b) t -> (char, 'b) t
end

module OUtil (P : P) = struct
  open P

  let left f c x y = f (c x) y

  let right f c x y = c (f x y)

  let expr f ops opnd =
    let ops =
      Array.map
        (fun (assoc, list) ->
          let g =
            match assoc with `Lefta | `Nona -> left | `Righta -> right
          in
          ( assoc = `Nona,
            altl (List.map (fun (oper, sema) -> oper => fun _ -> g sema) list)
          ))
        ops
    in
    let n = Array.length ops in
    let op i = snd ops.(i) in
    let nona i = fst ops.(i) in
    let id x = x in
    let rec inner l c =
      (* Printf.printf "inner %d \n%!" l; *)
      f
        (alt
           (seq
              (guard empty (fun _ -> n = l) None)
              (fun _ -> map (fun (x as _0) -> c x) opnd))
           (alt
              (seq
                 (guard empty (fun _ -> n > l && not (nona l)) None)
                 (fun _ ->
                   seq
                     (inner (l + 1) id)
                     (fun (x as _1) ->
                       map
                         (fun (b as _0) ->
                           match b with None -> c x | Some x -> x)
                         (opt (seq (op l) (fun o -> inner l (o c x)))))))
              (seq
                 (guard empty (fun _ -> n > l && nona l) None)
                 (fun _ ->
                   seq
                     (inner (l + 1) id)
                     (fun (x as _1) ->
                       map
                         (fun (b as _0) ->
                           c (match b with None -> x | Some (o, y) -> o id x y))
                         (opt
                            (seq (op l) (fun (_ as _1) ->
                                 map
                                   (fun (_ as _0) -> (_1, _0))
                                   (inner (l + 1) id)))))))))
    in

    inner 0 id
end

module Helpers (P : P) : PExt with type ('a, 'b) t = ('a, 'b) P.t = struct
  include P

  let list0 p =
    alt
      ( p >>= fun h ->
        many (lexeme "," *> p) >>= fun tl -> return (h :: tl) )
      (empty => fun _ -> [])

  let list p =
    p >>= fun h ->
    many (lexeme "," *> p) >>= fun tl -> return (h :: tl)

  let parens p = lexeme "(" *> p <* lexeme ")"
end
