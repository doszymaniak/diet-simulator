open User
open Calories
open Types
open Report
open Simulation
open User_json


let rec main_menu user =
  print_endline "1. Add a new day";
  print_endline "2. Add a new meal";
  print_endline "3. Add a workout";
  print_endline "4. Summarize a day";
  print_endline "5. Summarize a date range";
  print_endline "6. Simulate weight change";
  print_endline "7. Simulate weight change - 'what if' scenario";
  print_endline "8. Update your goal";
  print_endline "9. Exit";
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
  | "8" -> let user = User.update_goal user in main_menu user;
  | "9" -> ()
  | _ -> 
      print_endline "Invalid option! Try again";
      main_menu user


let rec intro () =
  print_endline "Choose: ";
  print_endline "1. Load user data from file";
  print_endline "2. Enter user data";
  let option = read_line () in
  match option with
  | "1" -> 
      let () = print_endline "Enter name of file: " in
      let filename = read_line () in
      let user = User_json.load_user filename in
      begin match user with
      | Some user -> main_menu user
      | None -> 
          let () = print_endline "Failed to load file!" in intro () end
  | "2" -> 
      let user = User.create_new_user () in main_menu user
  | _ -> let () = print_endline "Invalid option!" in intro ()


let () =
  print_endline "Welcome!";
  intro ()