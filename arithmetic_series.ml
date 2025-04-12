(*1.Arithmetic series*)
type int_series=int list;;
let arithmetic_series d a1 n=
  let rec generate_series current count acc=
    if count = n then acc 
    else generate_series (current+d) (count+1) (current::acc)
  in generate_series a1 0 [];;
arithmetic_series 3 10 7;;
arithmetic_series (-2) 6 10;;