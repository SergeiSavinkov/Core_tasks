(*2.Count elements*)
let elem_count lst=
  List.fold_left (fun acc x ->
      match acc with
      | (y,n)::rest when y = x -> (y,n+1)::rest (*If we have same element increase counter n*)
      | _-> (x,1)::acc (*If we have new element, start counter n from 1*)
    ) [] lst |> List.rev;; (*Reverse list use List.rev*)
elem_count [1; 1; 2; 2; 3; 4; 4; 4];;
elem_count ['a'; 'b'; 'b'; 'b'; 'c'; 'd'; 'd'; 'e'];;