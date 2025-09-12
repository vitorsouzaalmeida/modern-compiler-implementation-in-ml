let rec last (lst : 'a list) =
  match lst with [] -> None | [ x ] -> Some x | _ :: tail -> last tail

let () =
  match last [ 1; 2; 3; 4; 5; 6; 7 ] with
  | None -> Printf.printf "Empty list\n"
  | Some x -> Printf.printf "Last element: %d\n" x

let rec two_last (lst : 'a list) =
  match lst with
  | [] -> None
  | [ x; y ] -> Some (x, y) (* Return a tuple *)
  | [ x ] -> Some (x, x) (* If only one element, return it twice *)
  | _ :: tail -> two_last tail

let () =
  match two_last [ 1; 2; 3; 4; 5; 6; 7 ] with
  | None -> Printf.printf "Empty\n"
  | Some (x, y) -> Printf.printf "Last two elements: %d %d\n" x y

let at (num : int) (lst : 'a list) =
  try Some (List.nth lst num) with _ -> None

let () =
  match at 5 [ "a"; "b"; "c" ] with
  | Some result -> Printf.printf "element: %s" result
  | None -> Printf.printf "Index out of bounds\n"

let mylength (lst : 'a list) =
  let length = List.fold_left (fun acc _ -> acc + 1) 0 lst in
  Printf.printf "len: %d\n" length;
  length

let _ = mylength [ 1; 2; 3 ]

(** polymorphic linked list, where:
    - [Nil] represents the empty list (end of the list)
    - Cons represents a node containing an element of type ['a] and a pointer to
      the rest of the list *)
type 'a mylist = Nil | Cons of 'a * 'a mylist

let my_list = Cons (3, Cons (2, Cons (1, Nil)))

let rec lst_iter fn lst =
  match lst with
  | Nil -> ()
  | Cons (item, rest) ->
      fn item;
      lst_iter fn rest

let rec lst_find (e : 'a) lst =
  match lst with
  | Nil -> None
  | Cons (head, rest) -> if head = e then Some head else lst_find e rest

let lst_insert item lst = Cons (item, lst)

let () =
  let new_list = lst_insert 20 my_list in
  lst_iter (fun x -> Printf.printf "%d, " x) new_list

let () = lst_iter (fun x -> print_endline (string_of_int x)) my_list

let () =
  match lst_find 0 my_list with
  | None -> print_endline "This item do not exists"
  | Some result -> Printf.printf "Found: %s" (string_of_int result)

(* Binary Search Tree - fast access to two elements - the median
  elements above and below the hiven given node. Fast search and flexible update.
  It's basically a linked list with two pointers per node

  Rooted binary tree is recursively defined as either being empty or consisting 
  of a nood called the root, together with two rooted binary trees called the left
  and the right subtrees. The nodes order matters in rooted trees, so left is different
  from right

  The binary search tree labels each node with a single key, where all nodes in left
  subtree of x, have keys < x, while all nodes in the right subtree of x have keys > x.

  Binary tree nodes have left and right pointers fields, an optional parent pointer,
  and a data field.
*)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let sample_tree () =
  Node
    ( 5,
      Node (3, Node (1, Empty, Empty), Node (4, Empty, Empty)),
      Node (8, Node (7, Empty, Empty), Node (10, Empty, Empty)) )

let rec search_tree (tree : 'a tree) item =
  match tree with
  | Empty -> Empty
  | Node (nitem, left, right) ->
      if nitem = item then Node (nitem, left, right)
      else if item < nitem then search_tree left item
      else search_tree right item

let rec find_min (tree : 'a tree) =
  match tree with
  | Empty -> Empty
  | Node (nitem, Empty, right) -> Node (nitem, Empty, right)
  | Node (_, left, _) -> find_min left

let rec list_labels (tree : 'a tree) fn acc =
  match tree with
  | Empty -> acc
  | Node (nitem, left, right) ->
      let acc' = fn acc nitem in
      let acc'' = list_labels left fn acc' in
      list_labels right fn acc''

let () =
  let result = list_labels (sample_tree ()) (fun acc x -> x :: acc) [] in
  Printf.printf "Tree values: ";
  List.iter (fun x -> Printf.printf "%d " x) result;
  print_endline ""

let () =
  let result = find_min (sample_tree ()) in
  match result with
  | Empty -> print_endline "nhe"
  | Node (item, _, _) -> Printf.printf "Min tree value: %d\n" item

let () =
  let result = search_tree (sample_tree ()) 7 in
  match result with
  | Empty -> print_endline "Item not found tree"
  | Node (item, _, _) -> Printf.printf "Item found: %d\n" item

(* leetcode *)
(* merge two already sorted arrays *)
let merge_sorted_array nums1 nums2 =
  let rec merge acc lst1 lst2 =
    match (lst1, lst2) with
    | [], [] -> List.rev acc
    | [], rest | rest, [] -> List.rev acc @ rest
    | h1 :: t1, h2 :: t2 ->
        if h1 <= h2 then merge (h1 :: acc) t1 lst2
        else merge (h2 :: acc) lst1 t2
  in
  merge [] (Array.to_list nums1) (Array.to_list nums2) |> Array.of_list

let () =
  let merged = merge_sorted_array [| 1; 2; 3 |] [| 2; 4; 5 |] in
  match merged with
  | [||] -> print_endline "Empty"
  | _ ->
      Array.iter (fun x -> Printf.printf "Items: %s\n" (string_of_int x)) merged
