open ImpAST
open Assoc
open Collection
open Trace

type values = 
  | VInt of int
  | VBool of bool
  | VString of string
  | VCollection of string collection
  | VTrace of string trace
  | VPair of int list
  | VSkip

type sigma = values context

let rec step_expr (e : expr) (s : sigma) : (values * sigma) =
  match e with
  (* Basic Imp expressions *)
  | Num n -> eval_num n s
  | Var v -> eval_var v s
  | String str -> eval_str str s
  | Plus (n1, n2) -> eval_plus n1 n2 s
  | Minus (n1, n2) -> eval_minus n1 n2 s
  | Mult (n1, n2) -> eval_mult n1 n2 s
  | Paren a -> eval_paren a s
  | True -> VBool true, s
  | False -> VBool false, s
  | Equal (n1, n2) -> eval_equal n1 n2 s
  | Leq (n1, n2) -> eval_leq n1 n2 s
  | Not b -> eval_not b s
  | Or (b1, b2) -> eval_or b1 b2 s
  | And (b1, b2) -> eval_and b1 b2 s
  | Skip -> VSkip, s
  | Print a -> eval_print a s
  | Seq (c1, c2) -> eval_seq c1 c2 s
  | ESeq c -> eval_eseq c s
  | Assign (typ, str, exp) -> eval_assign typ str exp s
  | ReAssign (str, exp) -> eval_reassign str exp s
  | _ -> failwith "Unimplmented"
  (* | Assign of (typ * string * expr)
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
  | Out of (expr * expr) *)

and eval_num (n:int) (s:sigma) : values * sigma =
  VInt n, s

and eval_str (str:string) (s:sigma) : values * sigma =
  VString str, s

and eval_var (v:string) (s:sigma) : values * sigma =
  Assoc.lookup v s, s

and eval_minus (n1:expr) (n2:expr) (s:sigma) : values * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (VInt n1', _), (VInt n2', _) -> VInt (n1' - n2'), s
  | _ -> failwith "Minus did not reduce down num value")

and eval_plus (n1:expr) (n2:expr) (s:sigma) : values * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (VInt n1', _), (VInt n2', _) -> VInt (n1' + n2'), s
  | _ -> failwith "Minus did not reduce down num value")

and eval_mult (n1:expr) (n2:expr) (s:sigma) : values * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (VInt n1', _), (VInt n2', _) -> VInt (n1' * n2'), s
  | _ -> failwith "Minus did not reduce down num value")

and eval_paren (a:expr) s : values * sigma = step_expr a s

and eval_equal (n1:expr) (n2:expr) (s:sigma) : values * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (VInt n1', _), (VInt n2', _) -> if n1' = n2' then VBool true, s else VBool false, s
  | _ -> failwith "Equal did not reduce down to num value")

and eval_leq (n1:expr) (n2:expr) (s:sigma) : values * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (VInt n1', _), (VInt n2', _) -> if n1' <= n2' then VBool true, s else VBool false, s
  | _ -> failwith "Equal did not reduce down to VInt value")

and eval_not (b:expr) (s:sigma) : values * sigma =
  (match step_expr b s with
  | VBool true, _ -> VBool false, s
  | VBool false, _ -> VBool true, s
  | _ -> failwith "Not did not reduce down to bool value")

and eval_or (b1:expr) (b2:expr) (s:sigma) : values * sigma =
  (match step_expr b1 s, step_expr b2 s with
  | (VBool x, _), (VBool y, _) -> VBool (x || y), s
  | _ -> failwith "Or did not reduce down to bool value")

and eval_and (b1:expr) (b2:expr) (s:sigma) : values * sigma =
  (match step_expr b1 s, step_expr b2 s with
  | (VBool x, _), (VBool y, _) -> VBool (x && y), s
  | _ -> failwith "Or did not reduce down to bool value")

and eval_print (a:expr) (s:sigma) : values * sigma =
  (match step_expr a s with
    | VInt n, _ -> print_endline (string_of_int n); (VSkip, s)
    | VBool x, _ -> print_endline (string_of_bool x); (VSkip, s)
    | VString x, _ -> print_endline (x); (VSkip, s)
    | _ -> failwith "aexp did not reduce down VInt value")

and eval_seq (c1:expr) (c2:expr) (s:sigma) : values * sigma =
  (match step_expr c1 s with
    | (VSkip, s1') -> 
        (match step_expr c2 s1' with
          | (VSkip, s2') -> (VSkip, s2')
          | _ -> failwith "Did not fully evaluate program with seq")
    | _ -> failwith "Did not fully evaluate program with seq")

and eval_eseq (e:expr) (s:sigma) : values * sigma =
  (match step_expr e s with
    | (VSkip, s') -> (VSkip, s')
    | _ -> failwith "Did not fully evaluate program with seq") 

and eval_assign (typ:typ) (str:string) (exp:expr) (s:sigma) : values * sigma =
  let declare s v c = 
    if Assoc.mem s c then failwith "This variable already defined"
    else VSkip, Assoc.update s v c in
  let (v, _) = step_expr exp s in
  match typ with
  | Int -> (match v with 
    | VInt _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | Bool -> (match v with 
    | VBool _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | String -> (match v with 
    | VString _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | Collection -> (match v with 
    | VCollection _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | Trace -> (match v with 
    | VTrace _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | Pair -> (match v with 
    | VPair _ -> declare str v s 
    | _ -> failwith "Types don't match")

and eval_reassign (str:string) (exp:expr) (s:sigma) : values * sigma =
  let declare s v c = 
    if Assoc.mem s c then VSkip, Assoc.update s v c
    else failwith "This variable is not yet defined" in
  let (v, _) = step_expr exp s in
  match Assoc.lookup str s with
  | VInt _ -> (match v with 
    | VInt _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | VBool _ -> (match v with 
    | VBool _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | VString _ -> (match v with 
    | VString _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | VCollection _ -> (match v with 
    | VCollection _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | VTrace _ -> (match v with 
    | VTrace _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | VPair _ -> (match v with 
    | VPair _ -> declare str v s 
    | _ -> failwith "Types don't match")
  | VSkip -> failwith "Variable never going to become VSkip"

let rec eval_prog (p : prog) : unit =
  match (step_expr p Assoc.empty) with
  | VSkip, _ -> ()
  | _ -> failwith "Program got stuck"
