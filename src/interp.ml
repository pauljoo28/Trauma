open ImpAST
open Util

let rec step_expr (e : expr) (s : sigma) : (expr * sigma) =
  match e with
  | Num n -> eval_num n s
  | Var v -> eval_var v s
  | Minus (n1, n2) -> eval_minus n1 n2 s
  | Plus (n1, n2) -> eval_plus n1 n2 s
  | Mult (n1, n2) -> eval_mult n1 n2 s
  | Paren a -> eval_paren a s
  | True -> True, s
  | False -> False, s
  | Equal (n1, n2) -> eval_equal n1 n2 s
  | Leq (n1, n2) -> eval_leq n1 n2 s
  | Not b -> eval_not b s
  | Or (b1, b2) -> eval_or b1 b2 s
  | And (b1, b2) -> eval_and b1 b2 s
  | Skip -> Skip, s
  | Print a -> eval_print a s
  | Seq (c1, c2) -> eval_seq c1 c2 s
  | ESeq c -> eval_eseq c s
  | If (b, c1, c2) -> eval_if b c1 c2 s
  | Assign (v, a) -> eval_assign v a s
  | While (b, c) -> eval_while b c s
  | Let (v, t, e1, e2) -> eval_let v t e1 e2 s
  | Unit g -> eval_unit g s
  | Sum (e1, e2) -> eval_sum e1 e2 s
  | Neg (e) -> eval_neg e s
  | Prod (e1, e2) -> eval_prod e1 e2 s
  | Fst (e) -> eval_fst e s
  | Snd (e) -> eval_snd e s
  | Iter e -> Iter e, s
  | Out (k, e) -> eval_out k e s

and eval_num (n:int) (s:sigma) : expr * sigma =
  Num n, s

and eval_var (v:string) (s:sigma) : expr * sigma =
  if (Util.memstore v s) then  
    (Num (Util.lookupstore v s), s)
  else
    (Util.lookupfstore v s, s)

and eval_minus (n1:expr) (n2:expr) (s:sigma) : expr * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (Num n1', _), (Num n2', _) -> Num (n1' - n2'), s
  | _ -> failwith "Minus did not reduce down num value")

and eval_plus (n1:expr) (n2:expr) (s:sigma) : expr * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (Num n1', _), (Num n2', _) -> Num (n1' + n2'), s
  | _ -> failwith "Minus did not reduce down num value")

and eval_mult (n1:expr) (n2:expr) (s:sigma) : expr * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (Num n1', _), (Num n2', _) -> Num (n1' * n2'), s
  | _ -> failwith "Minus did not reduce down num value")

and eval_paren (a:expr) s : expr * sigma = step_expr a s

and eval_equal (n1:expr) (n2:expr) (s:sigma) : expr * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (Num n1', _), (Num n2', _) -> if n1' = n2' then True, s else False, s
  | _ -> failwith "Equal did not reduce down to num value")

and eval_leq (n1:expr) (n2:expr) (s:sigma) : expr * sigma =
  (match step_expr n1 s, step_expr n2 s with
  | (Num n1', _), (Num n2', _) -> if n1' <= n2' then True, s else False, s
  | _ -> failwith "Equal did not reduce down to num value")

and eval_not (b:expr) (s:sigma) : expr * sigma =
  (match step_expr b s with
  | True, _ -> False, s
  | False, _ -> True, s
  | _ -> failwith "Not did not reduce down to bool value")

and eval_or (b1:expr) (b2:expr) (s:sigma) : expr * sigma =
  (match step_expr b1 s, step_expr b2 s with
  | (True, _), (True, _) -> True, s
  | (True, _), (False, _) -> True, s
  | (False, _), (True, _) -> True, s
  | (False, _), (False, _) -> False, s
  | _ -> failwith "Or did not reduce down to bool value")

and eval_and (b1:expr) (b2:expr) (s:sigma) : expr * sigma =
  (match step_expr b1 s, step_expr b2 s with
  | (True, _), (True, _) -> True, s
  | (True, _), (False, _) -> False, s
  | (False, _), (True, _) -> False, s
  | (False, _), (False, _) -> False, s
  | _ -> failwith "Or did not reduce down to bool value")

and eval_print (a:expr) (s:sigma) : expr * sigma =
  (match step_expr a s with
    | Num n, _ -> print_endline (string_of_int n); (Skip, s)
    | True, _ -> print_endline (string_of_bool true); (Skip, s)
    | False, _ -> print_endline (string_of_bool false); (Skip, s)
    | _ -> failwith "aexp did not reduce down num value")

and eval_seq (c1:expr) (c2:expr) (s:sigma) : expr * sigma =
  (match step_expr c1 s with
    | (Skip, s1') -> 
        (match step_expr c2 s1' with
          | (Skip, s2') -> (Skip, s2')
          | _ -> failwith "Did not fully evaluate program with seq")
    | _ -> failwith "Did not fully evaluate program with seq")

and eval_eseq (e:expr) (s:sigma) =
  (match step_expr e s with
    | (Skip, s') -> (Skip, s')
    | _ -> failwith "Did not fully evaluate program with seq")

and eval_if (b:expr) (c1:expr) (c2:expr) (s:sigma) : expr * sigma =
  (match step_expr b s with
    | True, _ -> step_expr c1 s
    | False, _ -> step_expr c2 s
    | _ -> failwith "If statement did not fully evaluate program")

and eval_assign (v:string) (a:expr) (s:sigma) : expr * sigma =
  (match step_expr a s with
    | Num n, _ -> (Skip, Util.updatestore v n s)
    | Iter e, _ -> 
      (Skip, Util.updatefstore v (Iter e) s)
    | _ -> failwith "aexp did not reduce down num value")

and eval_while (b:expr) (c:expr) (s:sigma) : expr * sigma =
  let c1' = Seq (c, While (b, c)) in
  let c2' = Skip in
  step_expr (If (b, c1', c2')) s

and eval_let (v:expr) (t:group) (e1:expr) (e2:expr) (c:sigma) : expr * sigma =
  match v with
  | Var x -> (match step_expr e1 c with
    | Num n, _ -> step_expr e2 (Util.updatestore x n c)
    | _ -> failwith "Not a base type in let"
    )
  | _ -> failwith "Not Var in let"

and eval_unit (g:group) (c:sigma) : expr * sigma = Num 1, c
and eval_zero (g:group) (c:sigma) : expr * sigma = Num 0, c
and eval_sum (e1:expr) (e2:expr) (c:sigma) : expr * sigma = eval_plus e1 e2 c
and eval_neg (e1:expr) (c:sigma) : expr * sigma = eval_minus (Num 0) e1 c

and eval_prod (e1:expr) (e2:expr) (c:sigma) : expr * sigma =  
  let x, _ = step_expr e1 c in
  let y, _ = step_expr e2 c in
  Prod (x, y), c

and eval_fst (e:expr) (c:sigma) : expr * sigma =
  match step_expr e c with
  | Prod (x, y), c' -> x, c'
  | _ -> failwith "Did not result in prod"

and eval_snd (e:expr) (c:sigma) : expr * sigma =
  match step_expr e c with
  | Prod (x, y), c' -> y, c'
  | _ -> failwith "Did not result in prod"

and eval_out (k:int) (iter:expr) (c:sigma) : expr * sigma =
  match step_expr iter c with
  | Iter (v, s, e1, e2), c' -> 
    if k = 0 then e2, c' else
    let (e', c'') = eval_let v s e2 e1 c' in
    eval_out (k-1) (Iter (v, s, e1, e')) c''
  | _ -> failwith "Eval Out has to be applied to a stream"

let rec eval_prog (p : prog) : unit =
  match p with
  | Skip -> ()
  | _ -> 
      (* check_prog p; *)
      match (step_expr p Util.empty) with
      | p', _ -> eval_prog p'
