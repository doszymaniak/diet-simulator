open User
open Calories
open Types
open Report
open Simulation


let rec main_menu user =
  print_endline "1. Add a new day";
  print_endline "2. Add a new meal";
  print_endline "3. Add a workout";
  print_endline "4. Summarize a day";
  print_endline "5. Summarize a date range";
  print_endline "6. Simulate weight change";
  print_endline "7. Simulate weight change - 'what if' scenario";
  print_endline "8. Exit";
  match read_line () with
  | "1" -> 
      let user = User.add_day user in
      main_menu user
  | "2" ->
      let user = User.modify_meal user in
      main_menu user
  | "3" -> 
      let user = User.modify_workout user in
      main_menu user
  | "4" -> Report.report_day user; main_menu user;
  | "5" -> Report.report_range user; main_menu user;
  | "6" -> Simulation.simulate_weight user; main_menu user;
  | "7" -> Simulation.simulate_weight_scenario user; main_menu user;
  | "8" -> ()
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
  calorie_intake = 2000;
  days = [];
}


let () =
  print_endline "Welcome!";
  (*
  let user = User.create_new_user () in
  main_menu user
  *)
  let user = example_user in main_menu user