let () = Printf.printf "hello world"

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
