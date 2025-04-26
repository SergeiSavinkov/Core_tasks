(* 9.Replace submatrix*) 
let replace_submatrix (x, y) m2 m1 =
  let m1_copy = Array.map Array.copy m1 in (*copy m1*)
  let m2_rows = Array.length m2 in (*determine size m2*)
  let m2_cols = Array.length m2.(0) in (*determine size m2*)
  let m1_rows = Array.length m1 in (*determine size m1*)
  let m1_cols = Array.length m1.(0) in(*determine size m1*)
  for i = 0 to min (m2_rows - 1) (m1_rows - x - 1) do
    for j = 0 to min (m2_cols - 1) (m1_cols - y - 1) do
      m1_copy.(x + i).(y + j) <- m2.(i).(j) (*output element*)
    done
  done;
  m1_copy;; (*copy matrix*)

let m1=
  [|[|1;2;3|];
    [|4;5;6|];
    [|7;8;9|];
    [|10;11;12|]|];;
let m2=
  [|[|10;20;30|];
    [|40;50;60|]|];;

replace_submatrix (1,1) m2 m1;;

replace_submatrix (2,0) m2 m1 ;;

replace_submatrix (0,1) m1 m2 ;;