type goal =
| Reduce of float
| Maintain
| Gain of float

type gender =
| Male
| Female

type day =
{
  breakfast : int;
  lunch : int;
  dinner : int;
  workout: int option
}

type user =
{
  name : string;
  age : int;
  gender : gender;
  weight : float;
  height : int;
  goal : goal;
  pal : float;
  calorie_intake : float;
  days : day list
}


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

let create_new_user () =
  let () = print_endline "Enter your name: " in
  let name = read_line () in

  let () = print_endline "Enter your age: " in
  let age = read_int () in 

  let () = print_endline "Enter your gender (F/M): " in
  let gender = 
    match read_line () with
    | "F" -> Female
    | "M" -> Male
    | _ -> failwith "invalid gender!"
  in

  let () = print_endline "Enter your current weight: " in
  let weight = read_float () in

  let () = print_endline "Enter your current height: " in
  let height = read_int () in

  let () = print_endline "Enter your current goal (R/M/G): " in
  let goal = read_line () in

  let goal = 
    if goal = "R" || goal = "G" then
      let () = print_endline "Enter your goal weight: " in
      let goal_weight = read_float () in 
      if goal = "R" then Reduce goal_weight else Gain goal_weight
    else Maintain
  in

  let () = print_endline "Enter a number of steps you take per day: " in
  let steps = read_int () in

  let () = print_endline "Enter a number of workouts you do per week: " in
  let workouts = read_int () in

  let pal = calculate_pal steps workouts in

  let calorie_intake = calculate_calorie_intake gender age weight height pal goal in

  let new_user = {
    name = name;
    age = age;
    gender = gender;
    weight = weight;
    height = height;
    goal = goal;
    pal = pal;
    calorie_intake = calorie_intake;
    days = []
  } in
  new_user

let add_day user =
  let new_day = {
    breakfast = 0;
    lunch = 0;
    dinner = 0;
    workout = None
  } in
  { user with days = new_day :: user.days }

let modify_meal user =
  print_endline "1. Add breakfast";
  print_endline "2. Add lunch";
  print_endline "3. Add dinner";
  print_endline "4. Exit";
  let command = read_line () in
  match command with
  | "1" | "2" | "3" ->
      let () = print_endline "Enter calories: " in
      let calories = read_int () in
      match user.days with
      | [] -> print_endline "No existing days!"; user
      | last_day :: xs ->
          let modified_day =
            match command with
            | "1" -> { last_day with breakfast = calories }
            | "2" -> { last_day with lunch = calories }
            | _ -> { last_day with dinner = calories }
          in { user with days = modified_day :: xs }
  | _ -> user

let modify_workout user =
  let () = print_endline "Enter burned calories: " in
  let calories = read_int () in
  match user.days with
  | [] -> print_endline "No existing days!"; user
  | last_day :: xs ->
      let modified_day = { last_day with workout = Some calories } in
      { user with days = modified_day :: xs }
          
let rec main_menu user =
  print_endline "1. Add a new day";
  print_endline "2. Add a new meal";
  print_endline "3. Add a workout";
  print_endline "4. Exit";
  match read_line () with
  | "1" -> 
      let user = add_day user in
      print_endline "New day added!";
      main_menu user
  | "2" ->
      let user = modify_meal user in
      print_endline "New meal added!";
      main_menu user
  | "3" -> 
      let user = modify_workout user in
      print_endline "New workout added!";
      main_menu user
  | "4" -> ()
  | _ -> 
      print_endline "Invalid option! Try again";
      main_menu user

let example_user = {
  name = "Anna";
  age = 20;
  gender = Female;
  weight = 60.;
  height = 160;
  goal = Maintain;
  pal = 1.3;
  calorie_intake = 2000.;
  days = [];
}


let () =
  print_endline "Welcome!";
  (*
  let user = create_new_user () in
  main_menu user
  *)
  let user = example_user in main_menu user