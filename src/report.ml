module Report = struct
  open Utils
  open Types
  open Calories


  let empty_report =
  {
    start_date = None;
    end_date = None;
    total_calories = lazy 0;
    calories_burned = lazy 0;
    total_carbohydrates = lazy 0;
    total_proteins = lazy 0;
    total_fats = lazy 0;
    report_days = [];
  }


  let rec find_day target_date days =
    match days with
    | [] -> None
    | curr_day :: rest ->
        let curr_date = curr_day.date in
        if curr_date = target_date then Some curr_day
        else find_day target_date rest

  
  let print_report report user =
    Printf.printf "You burned %d calories in workouts.\n" (Lazy.force report.calories_burned);
    
    Printf.printf "You ate %d calories, %d carbohydrates, %d proteins and %d fats.\n"
      (Lazy.force report.total_calories) (Lazy.force report.total_carbohydrates)
       (Lazy.force report.total_proteins) (Lazy.force report.total_fats);
    
    let diff = (Lazy.force report.total_calories) - user.calorie_intake in
    if diff > 0 then
      Printf.printf "That's %d calories above your calorie intake.\n" diff 
    else if diff < 0 then
      Printf.printf "That's %d calories under your calorie intake.\n" (-diff)
    else Printf.printf "You reached your calorie intake.\n"

  
  let add_day_report day report =
    let new_days = day :: report.report_days in
    {
      report with
      total_calories = Calories.lazy_total_calories new_days;
      calories_burned = Calories.lazy_calories_burned new_days;
      total_carbohydrates = Calories.lazy_total_carbohydrates new_days;
      total_proteins = Calories.lazy_total_proteins new_days;
      total_fats = Calories.lazy_total_fats new_days;
      report_days = new_days;
    }


  let report_day user =
    let date_opt = Utils.get_date () in
    match date_opt with
    | None -> ()
    | Some date ->
        let day = find_day date user.days in
        match day with
        | None -> print_endline "Wrong day!"; ()
        | Some day ->
            let report = { empty_report with start_date = Some day.date } in
            let report = { report with end_date = Some day.date } in
            let report = add_day_report day report in
            print_report report user


  let list_days_in_range start_date end_date days =
    let rec aux curr ds =
      match ds with
      | [] -> List.rev curr
      | curr_day :: rest ->
          let d = curr_day.date in
          if Utils.compare_dates d start_date = -1 then List.rev curr
          else if Utils.compare_dates d end_date = 1 then aux curr rest
          else aux (curr_day :: curr) rest
    in aux [] days
  

  let rec make_list_report report list_to_report =
    match list_to_report with
    | [] -> report
    | curr_day :: rest ->
        let curr_report = add_day_report curr_day report in
        make_list_report curr_report rest


  let report_range user =
    let date_first_opt = Utils.get_date () in
    let date_second_opt = Utils.get_date () in
    match date_first_opt, date_second_opt with
    | None, _ -> ()
    | _, None -> ()
    | Some date1, Some date2 ->
        let cmp = Utils.compare_dates date1 date2 in
        let date1, date2 =
          if cmp = 1 then (date2, date1)
          else (date1, date2)
        in
        let list_to_report = list_days_in_range date1 date2 user.days in
        if list_to_report = [] then print_endline "No days to report!"
        else
          let report = { empty_report with start_date = Some date1; end_date = Some date2 } in
          let report = make_list_report report list_to_report in
          print_report report user
end