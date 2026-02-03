module Calories = struct
  open Types


  let calculate_calorie_intake gender age weight height pal goal =
    let part = 10. *. weight +. 6.25 *. float_of_int height -. 5. *. float_of_int age in
    let bmr =
      match gender with
      | Female -> part -. 161.
      | Male -> part +. 5.
    in let calories = bmr *. pal in
    match goal with
    | Maintain -> calories
    | Reduce _ -> calories -. 500.
    | Gain _ -> calories +. 500.


  let calculate_pal steps workouts =
    let pal =
      if steps <= 5000 then 1.3
      else if steps <= 7500 then 1.5
      else if steps <= 10000 then 1.6
      else if steps <= 12500 then 1.7
      else 1.8
    in
    if workouts <= 1 then pal
    else if workouts = 2 then pal +. 0.05
    else if workouts <= 4 then pal +. 0.1 
    else if workouts = 5 then pal +. 0.15
    else pal +. 0.2
end