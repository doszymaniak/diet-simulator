module Simulation = struct
  open Types


  let kcal_per_kg = 7700.


  let rec print_weight_change weight daily_diff days curr_days =
    if curr_days > days then ()
    else
      let daily_change = daily_diff /. kcal_per_kg in
      let weight = weight +. daily_change in
      Printf.printf "Day %d: weight = %.2f kg\n" curr_days weight;
      print_weight_change weight daily_diff days (curr_days + 1)


  let simulate_weight user =
    let () = print_endline "Enter days to simulate: " in
    let days = read_int () in
    let daily_diff =
      begin match user.goal with
      | Maintain -> 0.
      | Reduce _ -> -500.
      | Gain _ -> 500. end
    in print_weight_change user.weight daily_diff days 1
end