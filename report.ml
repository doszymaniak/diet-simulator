module Report = struct
  open Utils
  open Types
  open Calories


  let rec find_day target_date days =
    match days with
    | [] -> None
    | curr_day :: rest ->
        let curr_date = curr_day.date in
        if curr_date = target_date then Some curr_day
        else find_day target_date rest

  
  let print_summarization day user =
    let total = Calories.calculate_day_total day in
    Printf.printf "You ate %d calories, %d carbohydrates, %d proteins and %d fats.\n"
      total.calories total.carbohydrates total.proteins total.fats;
    
    let diff = total.calories - user.calorie_intake in
    if diff > 0 then
      Printf.printf "That's %d calories above your calorie intake.\n" diff 
    else if diff < 0 then
      Printf.printf "That's %d calories under your calorie intake.\n" (-diff)
    else Printf.printf "You reached your calorie intake.\n"


  let report_day user =
    let () = print_endline "Enter day to summarize: " in
    let day = read_int () in
    let () = print_endline "Enter month: " in
    let month = read_line () in
    let () = print_endline "Enter year: " in
    let year = read_int () in
    let month_opt = Utils.get_month month in
    match month_opt with
    | None -> print_endline "Invalid month!"; ()
    | Some month ->
        let date = { day = day; month = month; year = year } in
        let day = find_day date user.days in
        match day with
        | None -> print_endline "Wrong day!"; ()
        | Some day -> print_summarization day user; ()
end