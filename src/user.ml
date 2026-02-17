module User = struct
  open Types
  open Calories
  open Utils


  let create_new_user () =
    let () = print_endline "Enter your name: " in
    let name = read_line () in
    let age = Utils.read_age_safe () in
    let gender = Utils.read_gender_safe () in
    let weight = Utils.read_weight_safe () in
    let height = Utils.read_height_safe () in
    let goal = Utils.read_goal_safe weight in
    let steps = Utils.read_steps_safe () in
    let workouts = Utils.read_workouts_safe () in
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
        let date = (Utils.choose_day user.days).date in
        let rec update days =
          begin match days with
          | [] -> ([], false)
          | curr_day :: rest ->
              if curr_day.date = date then
                let modified_day = 
                  begin match command with
                  | "1" -> { curr_day with breakfast = Some meal }
                  | "2" -> { curr_day with snack_1 = Some meal }
                  | "3" -> { curr_day with lunch = Some meal }
                  | "4" -> { curr_day with snack_2 = Some meal }
                  | _ -> { curr_day with dinner = Some meal } end
                in
                let rest_days, _ = update rest in
                (modified_day :: rest_days, true)
              else 
                let (new_rest, found) = update rest in
                (curr_day :: new_rest, found) end
        in 
        let (new_days, found) = update user.days in
        if not found then print_endline "No day found with this date!"
        else print_endline "New meal added!";
        { user with days = new_days }
    | _ -> user


  let modify_workout user =
    let calories = Utils.read_int_safe "Enter burned calories: " in
    let date = (Utils.choose_day user.days).date in
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
    let goal = Utils.read_goal_safe user.weight in
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
        Utils.print_day_list (List.rev user.days)
end