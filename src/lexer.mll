{
open Parser
open Printf
exception Eof
exception Err
exception SyntaxError of string
}

(* Regex definitons *)

let white = [' ' '\t']
let num = '-'?['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let newline = ['\n' '\r']
let comment = "//" [^ '\r' '\n']* 
let str = ['"'] (['\"']|[ ^ '\r' '\n' '"' ])* ['"']

(* Lexer definition *)

rule read = parse
  | comment         { read lexbuf }
  | white           { read lexbuf }
  | newline         { Lexing.new_line lexbuf; read lexbuf }
  | "skip"          { SKIP }
  | "print"         { PRINT }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "int"           { INT }
  | "fst"           { FST }
  | "snd"           { SND }
  | "out"           { OUT }
  | ","             { COMMA }
  | "."             { DOT }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | "*"             { MULT }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "=="            { EQUAL }
  | "!"             { NOT }
  | "<="            { LEQ }
  | "||"            { OR }
  | "&&"            { AND }
  | ";"             { SEQ }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "="             { ASSIGN }
  | "fst"           { FST }
  | "snd"           { SND }
  (* Types *)
  | "int"           { INT }
  | "string"        { STRING }
  | "pair"          { PAIR }
  | "bool"          { BOOL }
  (* Collection and Trace *)
  | "Collection"    { COLLECTION }
  | "Trace"         { TRACE }
  | "empty"         { EMPTY }
  | "init"          { INIT }
  | "insertC"       { CINSERT }
  | "insertT"       { TINSERT }
  (* Built in function *)
  | "distinct"      { DISTINCT }

  | num as num      { NUM (int_of_string num) }
  | id as id        { ID id }
  | str as str      { VSTRING str }
  | eof             { EOF }
  | _ as c  {
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1
          }

