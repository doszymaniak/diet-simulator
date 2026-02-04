module User = struct
  open Types
  open Calories


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
    let new_day = {
      breakfast = None;
      snack_1 = None;
      lunch = None;
      snack_2 = None;
      dinner = None;
      workout = None
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
        let () = print_endline "Enter calories: " in
        let calories = read_int () in
        let () = print_endline "Enter carbohydrates: " in
        let carbs = read_int () in
        let () = print_endline "Enter proteins: " in
        let proteins = read_int () in
        let () = print_endline "Enter fats: " in
        let fats = read_int () in
        let meal = {
          calories = calories;
          proteins = proteins;
          carbohydrates = carbs;
          fats = fats
        } in
        match user.days with
        | [] -> print_endline "No existing days!"; user
        | last_day :: xs ->
            let modified_day =
              match command with
              | "1" -> { last_day with breakfast = Some meal }
              | "2" -> { last_day with snack_1 = Some meal }
              | "3" -> { last_day with lunch = Some meal }
              | "4" -> { last_day with snack_2 = Some meal }
              | _ -> { last_day with dinner = Some meal }
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
end