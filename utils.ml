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

  
  let get_date () =
    let () = print_endline "Enter day: " in
    let day = read_int () in
    let () = print_endline "Enter month: " in
    let month = read_line () in
    let () = print_endline "Enter year: " in
    let year = read_int () in
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
end