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
