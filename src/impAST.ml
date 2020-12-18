open Collection
open Trace

type typ =
  | Int
  | Bool
  | String
  | Collection
  | Trace
  | Pair

type group =
  | Unit
  | Base
  | Pair of (group * int)
  | Stream of group

type expr =
  (* Basic Imp expressions *)
  | Num of int
  | Var of string
  | String of string
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
  | Assign of (typ * string * expr)
  | ReAssign of (string * expr)
  | Pair of (expr * expr)
  | Fst of (expr)
  | Snd of (expr)
  (* Differential Dataflow Specific Expressions *)
  | CEmpty
  | TEmpty
  | Collection of (string collection)
  | Trace of (string trace)
  | Distinct of (expr)
  | TInsert of (expr*expr)
  | CInsert of (expr*expr)
  | Out of (expr * expr)

type prog = expr
