module Validate = struct
  open Types


  let validate_date day month_opt year =
    if year < 1 || year > 2026 then false else
    if day < 1 || day > 31 then false else
    let is_leap_year year =
      if year mod 4 <> 0 then false
      else if year mod 100 <> 0 then true
      else if year mod 400 = 0 then true
      else false
    in 
    let leap = is_leap_year year in
    match month_opt with
    | None -> false
    | Some month ->
        match month with
        | January | March | May | July | August | October | December -> true
        | April | June | September | November -> day <= 30
        | February ->
            if leap then day <= 29
            else day <= 28
end