
%{
open ImpAST
open Str
open Collection

exception ParseException of string
%}

(* Tokens *)

%token EOF
%token <int> NUM
%token <string> ID
%token <string> VSTRING
%token SKIP
%token TRUE
%token FALSE
%token PRINT
%token MULT
%token PLUS
%token MINUS
%token EQUAL
%token NOT
%token LEQ
%token OR
%token AND
%token SEQ
%token LPAREN
%token RPAREN
%token ASSIGN
%token EMPTY

%token INT
%token STRING
%token PAIR
%token BOOL

%token COLLECTION
%token TRACE
%token CINSERT
%token TINSERT
%token DISTINCT

%token FST
%token SND

%token OUT
%token LBRACKET
%token RBRACKET

%token COMMA
%token DOT

(* Precedences *)
%left SEQ
%left PRINT

%left ASSIGN

%left AND OR

%left EQUAL LEQ
%left PLUS MINUS
%left MULT

%left NOT

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [ImpAST.prog]. *)

%start main
%type <ImpAST.prog> main

(* The following %% ends the declarations section of the grammar definition. *)

%%

main:
  | c = expr; EOF;
    { c }

typ:
  | INT; {Int}
  | BOOL; {Bool}
  | STRING; {String}
  | COLLECTION; {Collection}
  | TRACE; {Trace}
  | PAIR; {Pair}

expr:
  | c1 = expr; SEQ; c2 = expr;
    { Seq (c1, c2) }
  | c = expr; SEQ;
    { ESeq c }
  | LPAREN; c = expr; RPAREN;
    { Paren c }
  | SKIP;
    { Skip }
  | PRINT; a = expr;
    { Print(a) }
  | t = typ; v = ID; ASSIGN; a = expr;
    { Assign (t, v, a) }
  | v = ID; ASSIGN; a = expr;
    { ReAssign (v, a) }
  | TRUE;
    { True }
  | FALSE;
    { False }
  | COLLECTION; DOT; EMPTY;
     { CEmpty }
  | TRACE; DOT; EMPTY;
     { TEmpty }
  | n = NUM;
    { Num (n) }
  | v = VSTRING;
    { String v }
  | v = ID;
    { Var v }
  | FST; LPAREN; e = expr; RPAREN;
     { Fst (e) }
  | SND; LPAREN; e = expr; RPAREN;
     { Snd (e) }
  | LBRACKET; e1 = expr; COMMA; e2 = expr; RBRACKET;
     { Pair (e1, e2) }
  | OUT; LBRACKET; x = expr; RBRACKET; LPAREN; e = expr; RPAREN;
     { Out (x, e) }
  | e1 = expr; DOT; CINSERT; LPAREN; e2 = expr; RPAREN;
     { CInsert (e1, e2) }
  | e1 = expr; DOT; TINSERT; LPAREN; e2 = expr; RPAREN;
     { TInsert (e1, e2) }
  | e1 = expr; DOT; DISTINCT; LPAREN; RPAREN;
     { Distinct (e1) }
  | n1 = expr; EQUAL; n2 = expr;
   { Equal (n1, n2) }
  | n1 = expr; LEQ; n2 = expr;
   { Leq (n1, n2) }
  | NOT; b = expr;
   { Not (b) }
  | b1 = expr; OR; b2 = expr;
    { Or (b1, b2) }
  | b1 = expr; AND; b2 = expr;
    { And (b1, b2) }
  | n1 = expr; PLUS; n2 = expr;
     { Plus (n1, n2) }
  | n1 = expr; MINUS; n2 = expr;
     { Minus (n1, n2) }
  | n1 = expr; MULT; n2 = expr;
     { Mult (n1, n2) }

%%
