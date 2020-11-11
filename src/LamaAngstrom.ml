open Angstrom



(* module Expr = struct

  let util_expr f ops opnd =
  let ops =
    Array.map
      (fun (assoc, list) ->
        let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in
        assoc = `Nona, altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list)
      )
      ops
  in
  let n      = Array.length ops in
  let op   i = snd ops.(i)      in
  let nona i = fst ops.(i)      in
  let id   x = x                in
  let rec inner l c = f[ostap (
      {n = l                } => x:opnd {c x}
    | {n > l && not (nona l)} => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? {
        match b with None -> c x | Some x -> x
      }
    | {n > l && nona l} => x:inner[l+1][id] b:(op[l] inner[l+1][id])? {
        c (match b with None -> x | Some (o, y) -> o id x y)
      })]
  )
  in
  ostap (inner[0][id])


  let parse
end  *)


let run_parser ~filename _str = `Ok ([], Language.Stmt.Skip)
