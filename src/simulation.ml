module Simulation = struct
  open Types
  open Utils


  let kcal_per_kg = 7700.


  let daily_weight_change user =
    let diff =
      match user.goal with
      | Maintain -> 0.
      | Reduce target -> if user.weight <= target then 0. else -500.
      | Gain target -> if user.weight >= target then 0. else 500.
    in diff


  let rec print_weight_change weight daily_diff days curr_days goal =
    if curr_days > days then weight
    else
      let daily_change = daily_diff /. kcal_per_kg in
      let weight = weight +. daily_change in
      match goal with
      | Reduce target when weight <= target ->
          (Printf.printf "Simulation stopped! Goal reached!\n"; weight)
      | Gain target when weight >= target ->
          (Printf.printf "Simulation stopped! Goal reached!\n"; weight)
      | _ ->
        if weight < 35. then
          (Printf.printf "Simulation stopped! Weight dangerously low!\n"; weight)
        else if weight > 300. then
          (Printf.printf "Simulation stopped! Weight dangerously high!\n"; weight)
        else
          (Printf.printf "Day %d: weight = %.2f kg\n" curr_days weight;
          print_weight_change weight daily_diff days (curr_days + 1) goal)


  let simulate_weight user =
    let days = Utils.read_int_safe "Enter days to simulate: " in
    let daily_diff = daily_weight_change user
    in ignore (print_weight_change user.weight daily_diff days 1 user.goal)


  let simulate_weight_scenario user =
    let days = Utils.read_int_safe "Enter days to simulate: " in
    let () = print_endline "On which day would you like to eat a different number of calories?" in
    let day = Utils.read_int_safe "Enter the day number: " in
    if day <= 0 || day > days then print_endline "Invalid day number!"
    else
      let special_calories = Utils.read_float_safe "Enter the number of calories you would like to eat on this day: " in
      let daily_diff = daily_weight_change user in
      let special_diff = special_calories -. float_of_int user.calorie_intake in
      let weight_before = print_weight_change user.weight daily_diff (day - 1) 1 user.goal in
      let after_special = print_weight_change weight_before special_diff day day user.goal in
      ignore (print_weight_change after_special daily_diff days (day + 1) user.goal)
end