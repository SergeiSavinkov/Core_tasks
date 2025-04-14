(*Currency converter*)

let rates = [
  ("USD", 1.0); 
  ("EUR", 0.92);
  ("GBP", 0.79);
  ("JPY", 151.5);
  ("RUB", 92.0);
]

let get_rate currency =
  try List.assoc currency rates
  with Not_found -> failwith ("Unknown currency: " ^ currency)

let convert amount from_currency to_currency =
  let from_rate = get_rate from_currency in
  let to_rate = get_rate to_currency in
  amount /. from_rate *. to_rate

let () =
  print_endline "Currency Converter";
  print_string "Enter amount: ";
  let amount = read_float () in
  print_string "From currency (e.g. USD): ";
  let from_currency = read_line () |> String.uppercase_ascii in
  print_string "To currency (e.g. EUR): ";
  let to_currency = read_line () |> String.uppercase_ascii in

  try
    let result = convert amount from_currency to_currency in
    Printf.printf "%.2f %s = %.2f %s\n"
      amount from_currency result to_currency
  with Failure msg ->
    Printf.printf "Error: %s\n" msg
