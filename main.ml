open User
open Calories
open Types
open Report


let rec main_menu user =
  print_endline "1. Add a new day";
  print_endline "2. Add a new meal";
  print_endline "3. Add a workout";
  print_endline "4. Summarize a day";
  print_endline "5. Summarize a date range";
  print_endline "6. Exit";
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
  | "6" -> ()
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
  let user = create_new_user () in
  main_menu user
  *)
  let user = example_user in main_menu user