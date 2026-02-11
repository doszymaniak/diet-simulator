module Calories = struct
  open Types
  open Utils


  let calculate_calorie_intake gender age weight height pal goal =
    let part = 10. *. weight +. 6.25 *. float_of_int height -. 5. *. float_of_int age in
    let bmr =
      match gender with
      | Female -> part -. 161.
      | Male -> part +. 5.
    in let calories = bmr *. pal in
    match goal with
    | Maintain -> int_of_float calories
    | Reduce _ -> int_of_float (calories -. 500.)
    | Gain _ -> int_of_float (calories +. 500.)


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


  let calculate_day_total day =
    let empty = {
      calories = 0;
      proteins = 0;
      carbohydrates = 0;
      fats = 0;
    } in
    let meal_total total meal =
      match meal with
      | Some meal -> {
          calories = total.calories + meal.calories;
          proteins = total.proteins + meal.proteins;
          carbohydrates = total.carbohydrates + meal.carbohydrates;
          fats = total.fats + meal.fats
        }
      | None -> total
    in
    let meal_list = [day.breakfast; day.snack_1; day.lunch; day.snack_2; day.dinner] in
    List.fold_left meal_total empty meal_list

    
  let lazy_total_calories days =
    lazy (List.fold_left (fun acc day ->
      let meals = Utils.get_meals day in
      let day_total = List.fold_left (fun t m ->
        match m with
        | Some meal -> t + meal.calories
        | None -> t) 0 meals
      in acc + day_total) 0 days)

  
  let lazy_total_proteins days =
    lazy (List.fold_left (fun acc day ->
      let meals = Utils.get_meals day in
      let day_total = List.fold_left (fun t m ->
        match m with
        | Some meal -> t + meal.proteins
        | None -> t) 0 meals
      in acc + day_total) 0 days)


  let lazy_total_carbohydrates days =
    lazy (List.fold_left (fun acc day ->
      let meals = Utils.get_meals day in
      let day_total = List.fold_left (fun t m ->
        match m with
        | Some meal -> t + meal.carbohydrates
        | None -> t) 0 meals
      in acc + day_total) 0 days)


  let lazy_total_fats days =
    lazy (List.fold_left (fun acc day ->
      let meals = Utils.get_meals day in
      let day_total = List.fold_left (fun t m ->
        match m with
        | Some meal -> t + meal.fats
        | None -> t) 0 meals
      in acc + day_total) 0 days)

  
  let lazy_calories_burned days =
    lazy (List.fold_left (fun acc day ->
      let workout =
        match day.workout with
        | None -> 0
        | Some w -> w
      in acc + workout) 0 days)
end