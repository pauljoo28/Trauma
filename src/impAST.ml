type typ =
  | Int
  | Bool
  | Unit

type group =
  | Unit
  | Base of typ
  | Pair of (group * typ)
  | Stream of group

type expr =
  (* Normal IMP expressions *)
  | Num of int
  | Var of string
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Mult of (expr * expr)
  | Paren of expr
  | True
  | False
  | Equal of (expr * expr)
  | Leq of (expr * expr)
  | Not of expr
  | Or of (expr * expr)
  | And of (expr * expr)
  | Skip
  | Print of expr
  | Seq of (expr * expr)
  | ESeq of expr
  | If of (expr * expr * expr)
  | Assign of (string * expr)
  | While of (expr * expr)
  (* Differential Dataflow Specific Expressions *)
  | Let of (expr * group * expr * expr)
  | Unit of (group)
  | Sum of (expr * expr)
  | Neg of (expr)
  | Prod of (expr * expr)
  | Fst of (expr)
  | Snd of (expr)
  | Iter of (expr * group * expr * expr)
  | Out of (int * expr)

type prog = expr
