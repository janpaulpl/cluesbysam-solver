(** Interactive CLI for the puzzle solver *)

open Types

type cli_state = {
  mutable puzzle : puzzle;
  mutable solver : Solver.Z3Solver.solver_state option;
  mutable constraints : (string * constraint_expr list) list;
}

let create_state puzzle =
  { puzzle; solver = None; constraints = [] }

let rebuild_solver state =
  let solver = Solver.Z3Solver.create state.puzzle in
  Solver.Z3Solver.add_known_status solver;
  List.iter (fun (_, exprs) ->
    List.iter (Solver.Z3Solver.add_constraint solver) exprs
  ) state.constraints;
  state.solver <- Some solver

(** Initialize state by parsing any existing clues in the puzzle *)
let init_existing_clues state =
  let all_names = Puzzle.all_names state.puzzle in
  List.iter (fun person ->
    match person.clue with
    | Some clue ->
      let cs = Parser.parse_clue ~speaker:person.name ~clue ~all_names in
      state.constraints <- (person.name, cs) :: state.constraints
    | None -> ()
  ) state.puzzle.people;
  if state.constraints <> [] then
    rebuild_solver state

let add_clue state name clue =
  state.puzzle <- Puzzle.update_clue state.puzzle name clue;
  let all_names = Puzzle.all_names state.puzzle in
  let constraints = Parser.parse_clue ~speaker:name ~clue ~all_names in
  state.constraints <- (name, constraints) :: state.constraints;
  rebuild_solver state;
  constraints

let set_status state name status =
  state.puzzle <- Puzzle.update_status state.puzzle name status;
  rebuild_solver state

let rec get_deductions state =
  match state.solver with
  | None -> rebuild_solver state; get_deductions state
  | Some solver -> Solver.Z3Solver.find_forced_assignments solver

let rec is_satisfiable state =
  match state.solver with
  | None -> rebuild_solver state; is_satisfiable state
  | Some solver -> Solver.Z3Solver.check_sat solver

let rec get_solution state =
  match state.solver with
  | None -> rebuild_solver state; get_solution state
  | Some solver -> Solver.Z3Solver.get_model solver

let print_help () =
  print_endline "\nCommands:";
  print_endline "  grid                    - Show the puzzle grid";
  print_endline "  clues                   - Show all revealed clues";
  print_endline "  add <name> <clue>       - Add/update clue for a person";
  print_endline "  set <name> innocent     - Mark person as innocent";
  print_endline "  set <name> criminal     - Mark person as criminal";
  print_endline "  deduce                  - Find all forced deductions";
  print_endline "  solve                   - Find a possible solution";
  print_endline "  check                   - Check if puzzle is satisfiable";
  print_endline "  save <filename>         - Save puzzle to file";
  print_endline "  load <filename>         - Load puzzle from file";
  print_endline "  help                    - Show this help";
  print_endline "  quit                    - Exit the program";
  print_endline ""

let execute_command state line =
  let parts = String.split_on_char ' ' line in
  let parts = List.filter (fun s -> s <> "") parts in
  match parts with
  | [] -> ()
  
  | ["grid"] ->
    Puzzle.print_grid state.puzzle
  
  | ["clues"] ->
    List.iter (fun person ->
      match person.clue with
      | Some clue -> Printf.printf "%s (%s): %s\n" person.name (Position.to_string person.pos) clue
      | None -> ()
    ) state.puzzle.people
  
  | "add" :: name :: clue_parts ->
    let clue = String.concat " " clue_parts in
    let name = String.trim name in
    (match Puzzle.find_by_name state.puzzle name with
     | None -> Printf.printf "Error: Unknown person '%s'\n" name
     | Some _ ->
       let constraints = add_clue state name clue in
       Printf.printf "Added clue for %s.\n" name;
       Printf.printf "Parsed constraints:\n";
       List.iter (fun c -> Printf.printf "  %s\n" (show_constraint_expr c)) constraints)
  
  | ["set"; name; "innocent"] ->
    (match Puzzle.find_by_name state.puzzle name with
     | None -> Printf.printf "Error: Unknown person '%s'\n" name
     | Some _ ->
       set_status state name Innocent;
       Printf.printf "%s marked as INNOCENT.\n" name)
  
  | ["set"; name; "criminal"] ->
    (match Puzzle.find_by_name state.puzzle name with
     | None -> Printf.printf "Error: Unknown person '%s'\n" name
     | Some _ ->
       set_status state name Criminal;
       Printf.printf "%s marked as CRIMINAL.\n" name)
  
  | ["deduce"] ->
    let deductions = get_deductions state in
    if deductions = [] then
      print_endline "No forced deductions found."
    else begin
      print_endline "Forced deductions:";
      List.iter (fun (name, status) ->
        let status_str = match status with
          | Criminal -> "CRIMINAL"
          | Innocent -> "INNOCENT"
          | Unknown -> "UNKNOWN"
        in
        Printf.printf "  %s must be %s\n" name status_str
      ) deductions
    end
  
  | ["solve"] ->
    (match get_solution state with
     | None -> print_endline "No solution found (puzzle may be unsatisfiable)."
     | Some assignments ->
       print_endline "Possible solution:";
       List.iter (fun (name, status) ->
         let status_str = match status with
           | Criminal -> "CRIMINAL"
           | Innocent -> "INNOCENT"
           | Unknown -> "UNKNOWN"
         in
         Printf.printf "  %s: %s\n" name status_str
       ) assignments)
  
  | ["check"] ->
    if is_satisfiable state then
      print_endline "Puzzle is satisfiable."
    else
      print_endline "Puzzle is UNSATISFIABLE! There may be a contradiction."
  
  | ["save"; filename] ->
    Puzzle.save_to_file state.puzzle filename;
    Printf.printf "Puzzle saved to %s\n" filename
  
  | ["load"; filename] ->
    (try
      state.puzzle <- Puzzle.load_from_file filename;
      state.constraints <- [];
      List.iter (fun person ->
        match person.clue with
        | Some clue ->
          let all_names = Puzzle.all_names state.puzzle in
          let cs = Parser.parse_clue ~speaker:person.name ~clue ~all_names in
          state.constraints <- (person.name, cs) :: state.constraints
        | None -> ()
      ) state.puzzle.people;
      rebuild_solver state;
      Printf.printf "Puzzle loaded from %s\n" filename;
      Puzzle.print_grid state.puzzle
    with e ->
      Printf.printf "Error loading file: %s\n" (Printexc.to_string e))
  
  | ["help"] | ["?"] ->
    print_help ()
  
  | ["quit"] | ["exit"] | ["q"] ->
    print_endline "Goodbye!";
    exit 0
  
  | _ ->
    Printf.printf "Unknown command. Type 'help' for available commands.\n"

let run state =
  print_endline "Clues by Sam Solver";
  print_endline "==================";
  print_endline "Type 'help' for available commands.\n";
  
  Puzzle.print_grid state.puzzle;
  print_newline ();
  
  while true do
    print_string "> ";
    flush stdout;
    let line = read_line () in
    try
      execute_command state line
    with e ->
      Printf.printf "Error: %s\n" (Printexc.to_string e)
  done