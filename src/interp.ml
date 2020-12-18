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
  | VSkip
  | VPair of (values * values)

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
  | Pair (e1, e2) -> eval_pair e1 e2 s
  | Fst e -> eval_fst e s
  | Snd e -> eval_snd e s
  (* Differential Dataflow Specific Expressions *)
  | CEmpty -> eval_cempty s
  | TEmpty (e) -> eval_tempty e s
  | CInsert (c, k, v) -> eval_cinsert c k v s
  | TInsert (t, c) -> eval_tinsert t c s
  | Distinct (e) -> eval_distinct e s
  | Out (k, e) -> eval_out k e s

and eval_num (n:int) (s:sigma) : values * sigma =
  VInt n, s

and eval_str (str:string) (s:sigma) : values * sigma =
  VString str, s

and eval_var (v:string) (s:sigma) : values * sigma =
  Assoc.lookup v s, s

and eval_pair (e1:expr) (e2:expr) (s:sigma) : values * sigma =
  match step_expr e1 s, step_expr e2 s with
  | (x, _), (y, _) -> VPair (x, y), s

and eval_fst (e:expr) (s:sigma) : values * sigma =
  match step_expr e s with
  | VPair (x, y), _ -> x, s
  | _ -> failwith "Not a VPair"

and eval_snd (e:expr) (s:sigma) : values * sigma =
  match step_expr e s with
  | VPair (x, y), _ -> y, s
  | _ -> failwith "Not a VPair"

and eval_tempty (e:expr) (s:sigma) : values * sigma =
  match step_expr e s with
  | VCollection e, s -> VTrace (Trace.init e), s
  | _ -> failwith "Not a collection"

and eval_cempty (s:sigma) : values * sigma =
  VCollection (Collection.empty), s

and eval_cinsert (c:expr) (k:expr) (v:expr) (s:sigma) : values * sigma =
  let key = 
    (match step_expr k s with
    | VString x, _ -> x
    | _ -> failwith "Not a string key") in
  let value =
    (match step_expr v s with
    | VInt x, _ -> x
    | _ -> failwith "Not an int value") in
  let col =
    (match step_expr c s with
    | VCollection x, _ -> x
    | _ -> failwith "Not a collection") in
  VCollection (Collection.insert (key, value) col), s

and eval_tinsert (tr:expr) (c:expr) (s:sigma) : values * sigma =
  let trace =
    (match step_expr tr s with
    | VTrace x, _ -> x
    | _ -> failwith "Not a string key") in
  let col =
    (match step_expr c s with
    | VCollection x, _ -> x
    | _ -> failwith "Not a string key") in
  if Trace.get_dim trace = 0 then VTrace (Trace.add_dim trace |> Trace.add_diff [] col), s
  else if Trace.get_dim trace = 1 then VTrace (Trace.add_diff [] col trace), s
  else failwith "Cannot add diffs to a trace with more than 1 dimension"

and eval_distinct (e:expr) (s:sigma) : values * sigma =
  match step_expr e s with
  | (VTrace tr, _) -> VTrace (Trace.distinct tr), s
  | _ -> failwith "Not operating on a trace"

and eval_out (k:expr) (e:expr) (s:sigma) : values * sigma =
  let rec time p acc = 
    match p with
    | VInt x -> x :: acc
    | VPair (x, y) -> 
        (match x with
        | VInt h -> time y (h :: acc)
        | _ -> failwith "Head is not an VInt")
    | _ -> failwith "Not a VInt or VPair"
  in
  let insert_time = 
    match step_expr k s with
    | VInt x, _ -> [x]
    | VPair (x, y), _ -> time (VPair (x, y)) []
    | _ -> failwith "Must be int or pair" in
  match step_expr e s with
  | (VTrace tr, _) -> VCollection ((Trace.get_version insert_time tr) |> Trace.to_collection), s
  | _ -> failwith "Not operating on a trace"

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
    | VCollection x, _ -> print_endline (Collection.debug_string_collection_tostring x); (VSkip,s)
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
