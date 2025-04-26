(* 7.Prime number scanner *)
let print_primes n = 
  if n < 2 then invalid_arg "Number n must be greater than 2!";
  let is_prime k = 
    let rec loop p =
      p * p > k || (k  mod p <> 0 && loop (p + 1))
    in
    k > 1 && loop 2
  in
  let printed = ref 0 in 
  let first = ref true in
  for x = 2 to max_int do
    if !printed < n && is_prime x then 
      begin 
        if not !first then print_char ' ';
        print_int x;
        first := false;
        incr printed
      end
  done;
  print_char;;

print_primes 5;;
print_primes 10;;