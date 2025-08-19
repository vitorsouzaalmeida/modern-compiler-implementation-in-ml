type id = string
type binop = Plus | Minus | Times 

type stm = CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list
  and exp = IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

  (* 
  a := 5 + 3; b := (print (a, a-1 ), 10 * a); print (b) 
  *)
let prog = CompoundStm(
  AssignStm("a", OpExp(NumExp(5), Plus, NumExp(3))), 
  CompoundStm(AssignStm(
    "b", 
    EseqExp(PrintStm([IdExp("a"); OpExp(IdExp("a"), Minus, NumExp(1))]), 
    OpExp(NumExp(10), Times, IdExp("a")))), 
    PrintStm([IdExp("b")])))


let maxargs (stm: stm) : int =
  let rec aux (s: stm) : int =
    match s with 
    | CompoundStm(s1, s2) -> max (aux s1) (aux s2)
    | AssignStm(_, e) -> aux_exp e 
    | PrintStm(exps) -> 
      let count = List.length exps in 
      let max_in_reps = List.fold_left (fun acc e -> max acc (aux_exp e)) 0 exps in 
      max count max_in_reps
  and aux_exp (e: exp) : int = 
    match e with 
    | IdExp _ -> 0
    | NumExp _ -> 0
    | OpExp(e1, _, e2) -> max (aux_exp e1) (aux_exp e2)
    | EseqExp(s, e) -> max (aux s) (aux_exp e)
  in 
  aux stm

(*I'm not sure if this is really what the book asked for*)
let interp (stm: stm) : unit = 
  let rec interpStm (s: stm) (env: (id * int) list) : (id * int) list = 
    match s with 
    | CompoundStm(s1, s2) -> 
      let env1 = interpStm s1 env in
      interpStm s2 env1
    | AssignStm(id, e) -> 
      let value = interpExp e env in
      (id, value) :: (List.remove_assoc id env)
    | PrintStm(exps) -> 
      List.iter (fun e -> 
        let v = interpExp e env in
        Printf.printf "%d " v
      ) exps;
      Printf.printf "\n";
      env
  and interpExp (e: exp) (env: (id * int) list) : int =
    match e with
    | IdExp id ->
      (try List.assoc id env 
       with Not_found -> failwith (Printf.sprintf "Undefined variable: %s" id))
    | NumExp n -> n
    | OpExp(e1, op, e2) ->
      let v1 = interpExp e1 env in
      let v2 = interpExp e2 env in
      (match op with
      | Plus -> v1 + v2
      | Minus -> v1 - v2
      | Times -> v1 * v2)
    | EseqExp(s, e) ->
      let new_env = interpStm s env in
      interpExp e new_env
  in
  ignore (interpStm stm []);
  ()

let () = 
  Printf.printf "Max args: %d\n" (maxargs prog);
  Printf.printf "Running interpreter:\n";
  interp prog