(** Main entry point for the Clues by Sam solver *)

open Clues_solver

let () =
  let usage = "Usage: clues_solver [puzzle_file]" in
  let puzzle_file = ref None in
  let anon_fun s = puzzle_file := Some s in
  Arg.parse [] anon_fun usage;
  
  let puzzle = match !puzzle_file with
    | Some filename -> 
      Printf.printf "Loading puzzle from %s...\n" filename;
      Puzzle.load_from_file filename
    | None ->
      (* Create default empty 4x5 puzzle *)
      Printf.printf "No puzzle file provided. Creating empty 4x5 grid.\n";
      Printf.printf "Use 'load <filename>' to load a puzzle, or add people manually.\n\n";
      Puzzle.create_empty ()
  in
  
  let state = Cli.create_state puzzle in
  Cli.init_existing_clues state;
  Cli.run state