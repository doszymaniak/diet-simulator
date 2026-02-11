module Simulation = struct
  open Types
  open Utils


  let kcal_per_kg = 7700.


  let daily_weight_change goal intake special =
    let diff =
      match goal with
      | Maintain -> 0.
      | Reduce _ -> -500.
      | Gain _ -> 500.
    in diff +. special


  let rec print_weight_change weight daily_diff days curr_days =
    if curr_days > days then weight
    else
      let daily_change = daily_diff /. kcal_per_kg in
      let weight = weight +. daily_change in
      Printf.printf "Day %d: weight = %.2f kg\n" curr_days weight;
      print_weight_change weight daily_diff days (curr_days + 1)


  let simulate_weight user =
    let days = Utils.read_int_safe "Enter days to simulate: " in
    let daily_diff = daily_weight_change user.goal user.calorie_intake 0.
    in ignore (print_weight_change user.weight daily_diff days 1)


  let simulate_weight_scenario user =
    let days = Utils.read_int_safe "Enter days to simulate: " in
    let () = print_endline "On which day would you like to eat a different number of calories?" in
    let day = Utils.read_int_safe "Enter the day number: " in
    if day <= 0 || day > days then print_endline "Invalid day number!"
    else
      let special_calories = Utils.read_float_safe "Enter the number of calories you would like to eat on this day: " in
      let daily_diff = daily_weight_change user.goal user.calorie_intake 0. in
      let special_diff = daily_weight_change user.goal user.calorie_intake special_calories in
      let weight_before = print_weight_change user.weight daily_diff (day - 1) 1 in
      let after_special = print_weight_change weight_before special_diff day day in
      ignore (print_weight_change after_special daily_diff days (day + 1))
end