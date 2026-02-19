module Utils = struct
  open Types
  open Validate

  
  let get_month str =
    match str with
    | "January" -> Some January
    | "February" -> Some February
    | "March" -> Some March
    | "April" -> Some April
    | "May" -> Some May
    | "June" -> Some June
    | "July" -> Some July
    | "August" -> Some August
    | "September" -> Some September
    | "October" -> Some October
    | "November" -> Some November
    | "December" -> Some December
    | _ -> None

  
  let map_month month =
    match month with
    | January -> 1
    | February -> 2
    | March -> 3
    | April -> 4
    | May -> 5
    | June -> 6
    | July -> 7
    | August -> 8
    | September -> 9
    | October -> 10
    | November -> 11
    | December -> 12

    
  let rec read_int_safe prompt =
    print_endline prompt;
    try int_of_string (read_line ())
    with Failure _ ->
      print_endline "Invalid input! Please enter a number";
      read_int_safe prompt

  
  let rec read_float_safe prompt =
    print_endline prompt;
    try float_of_string (read_line ())
    with Failure _ ->
      print_endline "Invalid input! Please enter a decimal number";
      read_float_safe prompt


  let rec read_age_safe () =
    let age = read_int_safe "Enter your age: " in
    if age < 5 || age > 120 then
      (print_endline "Invalid age! Please enter a value between 5 and 120"; read_age_safe ())
    else age


  let rec read_gender_safe () =
    print_endline "Enter your gender (F/M): ";
    match read_line () with
    | "F" | "f" -> Female
    | "M" | "m" -> Male
    | _ -> print_endline "Invalid gender! Please enter F or M"; read_gender_safe ()

  
  let rec read_weight_safe () =
    let weight = read_float_safe "Enter your current weight: " in
    if weight < 35. || weight > 300. then
      (print_endline "Invalid weight! Please enter a value between 35 and 300"; read_weight_safe ())
    else weight

  
  let rec read_height_safe () =
    let height = read_int_safe "Enter your height: " in
    if height < 100 || height > 250 then
      (print_endline "Invalid height! Please enter a value between 100 and 250"; read_height_safe ())
    else height

  
  let rec read_goal_safe current_weight =
    print_endline "Enter your goal (R/M/G): ";
    let goal = read_line () in
    match goal with
    | "M" | "m" -> Maintain
    | "R" | "r" ->
        let goal_weight = read_float_safe "Enter your goal weight: " in
        if goal_weight < 35. || goal_weight >= current_weight then
          (
            print_endline "Invalid weight! Please enter a value >= 35 kg and less than your current weight";
            read_goal_safe current_weight
          )
        else Reduce goal_weight
    | "G" | "g" ->
        let goal_weight = read_float_safe "Enter your goal weight: " in
        if goal_weight > 300. || goal_weight <= current_weight then
          (
            print_endline "Invalid weight! Please enter a value greater than your current weight and <= 300 kg";
            read_goal_safe current_weight
          )
        else Gain goal_weight
    | _ -> print_endline "Invalid goal! Try again"; read_goal_safe current_weight


  let rec read_steps_safe () =
    let steps = read_int_safe "Enter a number of steps you take per day: " in
    if steps < 0 || steps > 100000 then
      (print_endline "Invalid steps number! Enter a value between 0 and 100000"; read_steps_safe ())
    else steps
  

  let rec read_workouts_safe () =
    let workouts = read_int_safe "Enter a number of workouts you do per week: " in
    if workouts < 0 || workouts > 30 then
      (print_endline "Invalid workouts number! Enter a value between 0 and 30"; read_workouts_safe ())
    else workouts

  
  let get_date () =
    let day = read_int_safe "Enter day: " in
    let () = print_endline "Enter month: " in
    let month = read_line () in
    let year = read_int_safe "Enter year: " in
    let month_opt = get_month month in
    if (Validate.validate_date day month_opt year) = false then
      (print_endline "Invalid data!"; None)
    else
      let month = Option.get month_opt in
      let date = { day = day; month = month; year = year } in
      Some date


  let compare_dates date1 date2 =
    if date1.year < date2.year then -1
    else if date1.year > date2.year then 1
    else
      let first_month_num = map_month date1.month in
      let second_month_num = map_month date2.month in
      if first_month_num < second_month_num then -1
      else if first_month_num > second_month_num then 1
      else 
        if date1.day < date2.day then -1
        else if date1.day > date2.day then 1
        else 0

  
  let get_meals day = [day.breakfast; day.snack_1; day.lunch; day.snack_2; day.dinner]


  let print_day_list days =
    List.iteri (fun i d ->
      Printf.printf "%d. %02d-%02d-%d\n" (i + 1) d.date.day (map_month d.date.month) d.date.year
    ) days


  let rec choose_day days =
    match days with
    | [] -> None
    | _ ->
        let display_days = List.rev days in
        print_day_list display_days;
        let option = read_int_safe "Enter a day number: " in
        let rec aux list cnt =
          match list with
          | [] -> None
          | x :: xs ->
              if cnt = option - 1 then Some x
              else aux xs (cnt + 1)
        in 
        match aux display_days 0 with
        | None -> print_endline "Invalid day number! Try again!"; choose_day days
        | Some day -> Some day
end