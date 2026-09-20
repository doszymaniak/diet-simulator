open Diet_simulator.User
open Diet_simulator.Report
open Diet_simulator.Simulation
open Diet_simulator.User_json


let print_main_menu () =
  print_endline "";
  print_endline "=== Diet Simulator Menu ===";
  print_endline "1. Add a new day";
  print_endline "2. Add a new meal";
  print_endline "3. Add a workout";
  print_endline "4. Summarize a day";
  print_endline "5. Summarize a date range";
  print_endline "6. Simulate weight change";
  print_endline "7. Simulate weight change - 'what if' scenario";
  print_endline "8. Update your goal";
  print_endline "9. Check your calorie intake and goal";
  print_endline "10. Print added days";
  print_endline "11. Exit";
  print_endline "---------------------------";
  print_endline "Select an option: "


let rec main_menu user =
  print_main_menu ();
  match read_line () with
  | "1" -> 
      let user = User.add_day user in
      continue user
  | "2" ->
      let user = User.modify_meal user in
      continue user
  | "3" -> 
      let user = User.modify_workout user in
      continue user
  | "4" -> Report.report_day user; continue user;
  | "5" -> Report.report_range user; continue user;
  | "6" -> Simulation.simulate_weight user; continue user;
  | "7" -> Simulation.simulate_weight_scenario user; continue user;
  | "8" -> let user = User.update_goal user in continue user;
  | "9" -> User.check_goal user; continue user;
  | "10" -> User.print_added_days user; continue user;
  | "11" -> ()
  | _ -> 
      print_endline "Invalid option! Try again";
      main_menu user


and continue user =
  let () = print_endline "Do you want to continue? (Y/N): " in
  let command = read_line () in
  match command with
  | "Y" | "y" -> main_menu user
  | "N" | "n" -> ()
  | _ ->
      print_endline "Invalid option! Try again";
      continue user


let rec intro () =
  print_endline "";
  print_endline "=== Welcome ===";
  print_endline "1. Load user data from file";
  print_endline "2. Enter user data";
  print_endline "Choose an option: ";
  let option = read_line () in
  match option with
  | "1" ->
      let () = print_endline "Enter file name (or press Enter for data/user.json): " in
      let filename = read_line () in
      let resolved_name = if filename = "" then "data/user.json" else filename in
      let user = User_json.load_user resolved_name in
      begin match user with
      | Some user ->
          print_endline "Profile loaded successfully.";
          main_menu user
      | None -> 
          let () = Printf.printf "Failed to load file: %s\n" resolved_name in intro () end
  | "2" ->
      let user = User.create_new_user () in main_menu user
  | _ -> let () = print_endline "Invalid option!" in intro ()


let () =
  print_endline "Welcome to your diet simulator!";
  intro ()