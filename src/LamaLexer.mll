{
open Lexing
open LamaMenhir
open MenhirLexemes

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { DECIMAL (int_of_string (Lexing.lexeme lexbuf) : int ) }
  (* | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) } *)
  | "skip"   { SKIP }
  | "return" { RETURN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "elif"   { ELIF }
  | "fi"      { FI }
  | "do"     { DO }
  | "od"     { OD }
  | "repeat" { REPEAT }
  | "until"  { UNTIL }
  | "for"    { FOR }
  | "while"  { WHILE }
  | "fun"    { FUN }
  | "local"  { LOCAL }
  | "length" { LENGTH }
  (* It's important that identifier goes below keywords  *)
  | id       { IDENT (Lexing.lexeme lexbuf) }
  | ":="     { ASSGN }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '\''     { read_char (Buffer.create 3) lexbuf }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | '`'      { BACKTICK }
  | '<'      { LT }
  | '>'      { GT }
  | "<="     { LE }
  | ">="     { GE }
  | "!="     { NEQ }
  | "=="     { EQEQ }
  | ';'      { SEMICOLON }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIV }
  | '%'      { PERCENT }
  | '.'      { DOT }
  | "&&"     { LAND }
  | "!!"     { LOR }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String literal is not terminated")) }

and read_char buf =
  parse
  | '\''          { let s = Buffer.contents buf in
                    assert(String.length s > 0);
                    CHAR s.[0]
                  }
  | '\\' '\\' '\'' { CHAR '\\' }
  | '\\' 'b'  '\'' { CHAR '\b' }
  (* | '\\' 'f'  '\'' { CHAR '\f' } *)
  | '\\' 'n'  '\'' { CHAR '\n' }
  | '\\' 'r'  '\'' { CHAR '\r' }
  | '\\' 't'  '\'' { CHAR '\t' }
  | [^ '\'' '\\']
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_char buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal char character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("Char literal is not terminated")) }
