module User = struct
  open Types
  open Calories
  open Utils


  let create_new_user () =
    let () = print_endline "Enter your name: " in
    let name = read_line () in
    let age = Utils.read_int_safe "Enter your age: " in 
    let () = print_endline "Enter your gender (F/M): " in
    let gender = 
      match read_line () with
      | "F" | "f" -> Female
      | "M" | "m" -> Male
      | _ -> failwith "invalid gender!"
    in
    let weight = Utils.read_float_safe "Enter your current weight: " in
    let height = Utils.read_int_safe "Enter your current height: " in
    let () = print_endline "Enter your current goal (R/M/G): " in
    let goal = read_line () in
    let goal = 
      if goal = "R" || goal = "G" then
        let goal_weight = Utils.read_float_safe "Enter your goal weight: " in 
        if goal = "R" then Reduce goal_weight else Gain goal_weight
      else Maintain
    in
    let steps = Utils.read_int_safe "Enter a number of steps you take per day: " in
    let workouts = Utils.read_int_safe "Enter a number of workouts you do per week: " in
    let pal = Calories.calculate_pal steps workouts in
    let calorie_intake = Calories.calculate_calorie_intake gender age weight height pal goal in
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
    let date_opt = Utils.get_date () in
    match date_opt with
    | None -> user
    | Some date ->
        match user.days with
        | last_day :: _ when Utils.compare_dates date last_day.date <= 0 ->
            print_endline "New date must be later than the last added day"; user
        | _ ->
            print_endline "New day added!";
            let new_day = {
              date = date;
              breakfast = None;
              snack_1 = None;
              lunch = None;
              snack_2 = None;
              dinner = None;
              workout = None;
            } in
            { user with days = new_day :: user.days }


  let modify_meal user =
    print_endline "1. Add breakfast";
    print_endline "2. Add first snack";
    print_endline "3. Add lunch";
    print_endline "4. Add second snack";
    print_endline "5. Add dinner";
    print_endline "6. Exit";
    let command = read_line () in
    match command with
    | "1" | "2" | "3" | "4" | "5" ->
        let calories = Utils.read_int_safe "Enter calories: " in
        let carbs = Utils.read_int_safe "Enter carbohydrates: " in
        let proteins = Utils.read_int_safe "Enter proteins: " in
        let fats = Utils.read_int_safe "Enter fats: " in
        let meal = {
          calories = calories;
          proteins = proteins;
          carbohydrates = carbs;
          fats = fats
        } in
        begin match Utils.get_date () with
        | None -> user
        | Some date ->
            let rec update days =
              match days with
              | [] -> ([], false)
              | curr_day :: rest ->
                  if curr_day.date = date then
                    let modified_day = 
                      match command with
                      | "1" -> { curr_day with breakfast = Some meal }
                      | "2" -> { curr_day with snack_1 = Some meal }
                      | "3" -> { curr_day with lunch = Some meal }
                      | "4" -> { curr_day with snack_2 = Some meal }
                      | _ -> { curr_day with dinner = Some meal }
                    in
                    let rest_days, _ = update rest in
                    (modified_day :: rest_days, true)
                  else 
                    let (new_rest, found) = update rest in
                    (curr_day :: new_rest, found)
            in 
            let (new_days, found) = update user.days in
            if not found then print_endline "No day found with this date!"
            else print_endline "New meal added!";
            { user with days = new_days } end
    | _ -> user


  let modify_workout user =
    let calories = Utils.read_int_safe "Enter burned calories: " in
    match Utils.get_date () with
    | None -> user
    | Some date ->
        let rec update days =
          match days with
          | [] -> ([], false)
          | curr_day :: rest ->
              if curr_day.date = date then
                ({ curr_day with workout = Some calories } :: rest, true)
              else 
                let (new_rest, found) = update rest in
                (curr_day :: new_rest, found)
        in 
        let (new_days, found) = update user.days in
        if not found then print_endline "No day found with this date!"
        else print_endline "New workout added!";
        { user with days = new_days }

    
  let update_goal user =
    let () = print_endline "Enter the goal you would like to reach (R/M/G): " in
    let option = read_line () in
    let new_goal_opt =
      match option with
      | "R" | "G" ->
          let goal_weight = Utils.read_float_safe "Enter your goal weight: " in
          if option = "R" then if goal_weight > user.weight then None else Some (Reduce goal_weight)
          else if goal_weight < user.weight then None else Some (Gain goal_weight)
      | "M" -> Some (Maintain)
      | _ -> None
    in 
    match new_goal_opt with
    | None -> print_endline "Failed to change the goal!"; user
    | Some goal -> 
        print_endline "Goal updated successfully!";
        let new_calorie_intake = Calories.calculate_calorie_intake user.gender
          user.age user.weight user.height user.pal goal in
        { user with goal = goal; calorie_intake = new_calorie_intake }


  let check_goal user =
    let goal_str =
      match user.goal with
      | Maintain -> "maintain weight"
      | Reduce w -> Printf.sprintf "reduce to %.1f kg" w
      | Gain w -> Printf.sprintf "gain to %.1f kg" w
    in
    Printf.printf "Your daily calorie intake: %d kcal\n" user.calorie_intake;
    Printf.printf "Your goal: %s\n" goal_str


  let print_added_days user =
    match user.days with
    | [] -> print_endline "No days added yet!"
    | _ ->
        print_endline "Your added days: ";
        List.iter (fun d ->
          Printf.printf "- %02d-%02d-%d\n" d.date.day (Utils.map_month d.date.month) d.date.year
        ) (List.rev user.days)
end