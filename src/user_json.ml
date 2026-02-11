module User_json = struct
  open Yojson.Safe
  open Yojson.Safe.Util
  open Types

  
  let gender_of_json json =
    match to_string json with
    | "Female" -> Female
    | "Male" -> Male
    | _ -> failwith "Invalid gender!"

  
  let goal_of_json json =
    match to_assoc json with
    | [("Maintain", `Null)] -> Maintain
    | [("Reduce", `Float f)] -> Reduce f
    | [("Gain", `Float f)] -> Gain f
    | _ -> failwith "Invalid goal!"

  
  let user_of_json json =
    let name = to_string (member "name" json) in
    let age = to_int (member "age" json) in
    let gender = gender_of_json (member "gender" json) in
    let weight = to_float (member "weight" json) in
    let height = to_int (member "height" json) in
    let goal = goal_of_json (member "goal" json) in
    let pal = to_float (member "pal" json) in
    let calorie_intake = to_int (member "calorie_intake" json) in
    {
      name = name;
      age = age;
      gender = gender;
      weight = weight;
      height = height;
      goal = goal;
      pal = pal;
      calorie_intake = calorie_intake;
      days = [];
    }

  
  let load_user filename =
    try
      let json = from_file filename in Some (user_of_json json)
    with
    | _ -> None
end