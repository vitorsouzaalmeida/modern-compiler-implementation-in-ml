type id = string
type binop = Plus | Minus | Times 

type stm = CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list
  and exp = IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

(* a := 5 + 3; b := (print (a, a-1 ), 10 * a); print (b) *)
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

type key = string
type color = RED | BLACK
type 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree * color
let empty = LEAF

let is_red = function
  | TREE(_, _, _, _, RED) -> true
  | _ -> false

let rotate_left = function
  | TREE(left, k, v, TREE(rl, rk, rv, rr, _), c) ->
    TREE(TREE(left, k, v, rl, RED), rk, rv, rr, c)
  | t -> t

let rotate_right = function
  | TREE(TREE(ll, lk, lv, lr, _), k, v, right, c) ->
    TREE(ll, lk, lv, TREE(lr, k, v, right, RED), c)
  | t -> t

let flip_colors = function
  | TREE(TREE(ll, lk, lv, lr, _), k, v, TREE(rl, rk, rv, rr, _), _) ->
    TREE(TREE(ll, lk, lv, lr, BLACK), k, v, TREE(rl, rk, rv, rr, BLACK), RED)
  | t -> t

let rec insert_helper (key: key) (value: 'a) (t: 'a tree) : 'a tree =
  match t with
  | LEAF -> TREE(LEAF, key, value, LEAF, RED)
  | TREE(left, k, v, right, c) ->
    let t' = 
      if key < k then 
        TREE(insert_helper key value left, k, v, right, c)
      else if key > k then 
        TREE(left, k, v, insert_helper key value right, c)
      else 
        TREE(left, key, value, right, c) in
    
    let t' = if is_red (match t' with TREE(_, _, _, r, _) -> r | _ -> LEAF) && 
                not (is_red (match t' with TREE(l, _, _, _, _) -> l | _ -> LEAF))
             then rotate_left t' else t' in
    
    let t' = if is_red (match t' with TREE(l, _, _, _, _) -> l | _ -> LEAF) && 
                is_red (match t' with TREE(TREE(ll, _, _, _, _), _, _, _, _) -> ll | _ -> LEAF)
             then rotate_right t' else t' in
    
    let t' = if is_red (match t' with TREE(l, _, _, _, _) -> l | _ -> LEAF) && 
                is_red (match t' with TREE(_, _, _, r, _) -> r | _ -> LEAF)
             then flip_colors t' else t' in
    t'

let insert key value t =
  let t' = insert_helper key value t in
  match t' with
  | TREE(l, k, v, r, _) -> TREE(l, k, v, r, BLACK)
  | LEAF -> LEAF

let rec lookup (key: key) (t: 'a tree) : 'a option =
  match t with
  | LEAF -> None
  | TREE(left, k, v, right, _) ->
    if key = k then Some v
    else if key < k then lookup key left
    else lookup key right

let test_sedgewick () =
  let chars = ["t"; "s"; "p"; "i"; "p"; "f"; "b"; "s"; "t"] in
  let tree = List.fold_left (fun acc c -> 
    insert c 1 acc
  ) empty chars in
  Printf.printf "\nTesting Sedgewick tree with: t s p i p f b s t\n";
  List.iter (fun c ->
    match lookup c tree with
    | Some v -> Printf.printf "  %s -> %d\n" c v
    | None -> Printf.printf "  %s -> not found\n" c
  ) ["b"; "f"; "i"; "p"; "s"; "t"];
  tree

let () = 
  Printf.printf "Max args: %d\n" (maxargs prog);
  Printf.printf "Running interpreter:\n";
  interp prog;
  let _ = test_sedgewick () in
  ()