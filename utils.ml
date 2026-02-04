module Utils = struct
  open Types

  
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
end