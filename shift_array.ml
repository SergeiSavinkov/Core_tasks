(* 6.Shift array right *)
let create_lst n =
  let rec aux i acc =
    if i < 1 then acc
    else aux (i - 1) (i :: acc)
  in
  aux n [];;

let split_lst lst n =
  let rec aux i acc rest =
    if i = 0 then (List.rev acc, rest)
    else
      match rest with
      | [] -> (List.rev acc, [])
      | x :: xs -> aux (i - 1) (x :: acc) xs
  in
  aux n [] lst;;

let rotate_right lst k =
  let len = List.length lst in
  if len = 0 then []
  else
    let k = k mod len in
    if k = 0 then lst
    else
      let split_d = len - k in
      let left, right = split_lst lst split_d in
      right @ left;; (*union*)

let shift_right n k =
  let lst = create_lst n in
  rotate_right lst k;;
    
shift_right 5 2 ;;