open Ostap
open Ostap.Combinators

module Expr = struct
    open Language.Expr

    ostap (
      parse:
        !(Ostap.Util.expr
          (fun x -> x)
          (Array.map (fun (a, s) -> a,
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        )
          [|
            `Lefta, ["!!"];
            `Lefta, ["&&"];
            `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
            `Lefta, ["+" ; "-"];
            `Lefta, ["*" ; "/"; "%"];
          |])
          primary);
      primary:
          b:base is:(-"[" i:parse -"]" {`Elem i}
                    | "." %"length"    {`Len}) *
            {List.fold_left (fun b -> function `Elem i -> Elem (b, i) | `Len -> Length b) b is  };
      base:
        n:DECIMAL                                         {Const n}
      | s:STRING                                          {String (String.sub s 1 (String.length s - 2))}
      | c:CHAR                                            {Const  (Char.code c)}
      | "[" es:!(Util.list0)[parse] "]"                   {Array es}
      | "`" t:IDENT args:(-"(" !(Util.list)[parse] -")")? {Sexp (t, match args with None -> [] | Some args -> args)}
      | x:IDENT s:("(" args:!(Util.list0)[parse] ")"      {Call (x, args)}
                  | empty {Var x})
          {s}
      | -"(" parse -")"
    )
end

module Stmt = struct
  open Language.Stmt

  ostap (
    parse:
      s:stmt ";" ss:parse {Seq (s, ss)}
    | stmt;

    stmt:
      %"skip" {Skip}
    | %"if" e:!(Expr.parse)
      %"then" the:parse
      elif:(%"elif" !(Expr.parse) %"then" parse)*
      els:(%"else" parse)?
      %"fi" {
        If (e, the,
            List.fold_right
            (fun (e, t) elif -> If (e, t, elif))
            elif
            (match els with None -> Skip | Some s -> s)
        )
      }
    | %"while" e:!(Expr.parse) %"do" s:parse %"od"{While (e, s)}
    | %"for" i:parse "," c:!(Expr.parse) "," s:parse %"do" b:parse %"od" {
        Seq (i, While (c, Seq (b, s)))
      }
    | %"repeat" s:parse %"until" e:!(Expr.parse)  {Repeat (s, e)}
    | %"return" e:!(Expr.parse)?
                      {Return e}

    | x:IDENT
          s: (is:(-"[" !(Expr.parse) -"]")* ":=" e   :!(Expr.parse) {Assign (x, is, e)}
            | "("  args:!(Util.list0)[Expr.parse] ")" {Call   (x, args)}
            ) {s}
  )

end

module Definition = struct
  ostap (
    arg  : IDENT;
    parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
        locs:(%"local" !(Util.list arg))?
      "{" body:!(Stmt.parse) "}" {
      (name, (args, (match locs with None -> [] | Some l -> l), body))
    }
  )
end

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))

let run_parser s =
  Ostap.Util.parse
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
    end)
    (ostap (!(parse) -EOF))
