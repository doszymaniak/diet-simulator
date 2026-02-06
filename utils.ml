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
end