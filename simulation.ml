module Simulation = struct
  open Types


  let kcal_per_kg = 7700.


  let rec print_weight_change weight daily_diff days curr_days =
    if curr_days > days then weight
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
    in ignore (print_weight_change user.weight daily_diff days 1)


  let simulate_weight_scenario user =
    let () = print_endline "Enter days to simulate: " in
    let days = read_int () in
    let () = print_endline "On which day would you like to eat a different number of calories?" in
    let () = print_endline "Enter the day number: " in
    let day = read_int () in
    if day <= 0 || day > days then print_endline "Invalid day number!"
    else
      let () = print_endline "Enter the number of calories you would like to eat on this day: " in
      let special_calories = read_int () in
      let daily_diff =
        begin match user.goal with
        | Maintain -> 0.
        | Reduce _ -> -500.
        | Gain _ -> 500. end
      in 
      let weight_before = print_weight_change user.weight daily_diff (day - 1) 1 in
      let special_day_diff = float_of_int (special_calories - user.calorie_intake) in
      let after_special = print_weight_change weight_before special_day_diff day day in
      ignore (print_weight_change after_special daily_diff days (day + 1))
end