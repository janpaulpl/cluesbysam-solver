(** Puzzle state management *)

open Types

(** Create an empty puzzle with default 4x5 grid *)
let create_empty () =
  { people = []; width = 4; height = 5 }

(** Add a person to the puzzle *)
let add_person puzzle person =
  { puzzle with people = person :: puzzle.people }

(** Find a person by name *)
let find_by_name puzzle name =
  List.find_opt (fun p -> 
    String.lowercase_ascii p.name = String.lowercase_ascii name
  ) puzzle.people

(** Find a person by position *)
let find_by_position puzzle pos =
  List.find_opt (fun p -> equal_position p.pos pos) puzzle.people

(** Get all people in a region *)
let people_in_region puzzle region =
  match region with
  | Row r ->
    List.filter (fun p -> equal_row p.pos.row r) puzzle.people
  | Column c ->
    List.filter (fun p -> equal_column p.pos.col c) puzzle.people
  | Entire_grid ->
    puzzle.people
  | Edges ->
    List.filter (fun p -> Position.is_edge p.pos) puzzle.people
  | Corners ->
    List.filter (fun p -> Position.is_corner p.pos) puzzle.people
  | Neighbors name ->
    (match find_by_name puzzle name with
     | None -> []
     | Some person ->
       let neighbor_positions = Position.neighbors person.pos in
       List.filter (fun p -> 
         List.exists (equal_position p.pos) neighbor_positions
       ) puzzle.people)
  | CommonNeighbors (name1, name2) ->
    (match find_by_name puzzle name1, find_by_name puzzle name2 with
     | Some p1, Some p2 ->
       let n1 = Position.neighbors p1.pos in
       let n2 = Position.neighbors p2.pos in
       let common = List.filter (fun pos ->
         List.exists (equal_position pos) n2
       ) n1 in
       List.filter (fun p ->
         List.exists (equal_position p.pos) common &&
         p.name <> name1 && p.name <> name2
       ) puzzle.people
     | _ -> [])
  | Between (name1, name2) ->
    (match find_by_name puzzle name1, find_by_name puzzle name2 with
     | Some p1, Some p2 ->
       let between_positions = Position.between p1.pos p2.pos in
       List.filter (fun p ->
         List.exists (equal_position p.pos) between_positions
       ) puzzle.people
     | _ -> [])
  | Below name ->
    (match find_by_name puzzle name with
     | None -> []
     | Some person ->
       let below_positions = Position.below person.pos in
       List.filter (fun p ->
         List.exists (equal_position p.pos) below_positions
       ) puzzle.people)
  | Above name ->
    (match find_by_name puzzle name with
     | None -> []
     | Some person ->
       let above_positions = Position.above person.pos in
       List.filter (fun p ->
         List.exists (equal_position p.pos) above_positions
       ) puzzle.people)
  | LeftOf name ->
    (match find_by_name puzzle name with
     | None -> []
     | Some person ->
       let left_positions = Position.left_of person.pos in
       List.filter (fun p ->
         List.exists (equal_position p.pos) left_positions
       ) puzzle.people)
  | RightOf name ->
    (match find_by_name puzzle name with
     | None -> []
     | Some person ->
       let right_positions = Position.right_of person.pos in
       List.filter (fun p ->
         List.exists (equal_position p.pos) right_positions
       ) puzzle.people)
  | Custom names ->
    List.filter (fun p ->
      List.exists (fun n -> 
        String.lowercase_ascii p.name = String.lowercase_ascii n
      ) names
    ) puzzle.people

(** Update a person's clue *)
let update_clue puzzle name clue =
  let people = List.map (fun p ->
    if String.lowercase_ascii p.name = String.lowercase_ascii name then
      { p with clue = Some clue }
    else p
  ) puzzle.people in
  { puzzle with people }

(** Update a person's known status *)
let update_status puzzle name status =
  let people = List.map (fun p ->
    if String.lowercase_ascii p.name = String.lowercase_ascii name then
      { p with known_status = status }
    else p
  ) puzzle.people in
  { puzzle with people }

(** Get all names in the puzzle *)
let all_names puzzle =
  List.map (fun p -> p.name) puzzle.people

(** Parse a puzzle from file content *)
let parse_from_string content =
  let lines = String.split_on_char '\n' content in
  let lines = List.filter (fun s -> String.trim s <> "") lines in
  let people = List.filter_map (fun line ->
    (* Expected format: "A1, Name, Description" or "A1, Name, NO DESCRIPTION YET" *)
    match String.split_on_char ',' line with
    | pos_str :: name :: rest ->
      let pos_str = String.trim pos_str in
      let name = String.trim name in
      let desc = String.trim (String.concat "," rest) in
      (try
        let pos = Position.parse pos_str in
        let clue = 
          if desc = "NO DESCRIPTION YET" || desc = "" then None 
          else Some desc 
        in
        Some { pos; name; clue; known_status = Unknown }
      with _ -> None)
    | _ -> None
  ) lines in
  { people; width = 4; height = 5 }

(** Serialize puzzle to string *)
let to_string puzzle =
  let sorted = List.sort (fun p1 p2 ->
    let c = compare (Position.col_to_int p1.pos.col) (Position.col_to_int p2.pos.col) in
    if c <> 0 then c
    else compare (Position.row_to_int p1.pos.row) (Position.row_to_int p2.pos.row)
  ) puzzle.people in
  String.concat "\n" (List.map (fun p ->
    let clue_str = match p.clue with None -> "NO DESCRIPTION YET" | Some c -> c in
    let status_str = match p.known_status with
      | Unknown -> ""
      | Innocent -> " [INNOCENT]"
      | Criminal -> " [CRIMINAL]"
    in
    Printf.sprintf "%s, %s, %s%s" 
      (Position.to_string p.pos) p.name clue_str status_str
  ) sorted)

(** Load puzzle from file *)
let load_from_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  parse_from_string content

(** Save puzzle to file *)
let save_to_file puzzle filename =
  let oc = open_out filename in
  output_string oc (to_string puzzle);
  close_out oc

(** Get people with unknown status *)
let unknown_people puzzle =
  List.filter (fun p -> p.known_status = Unknown) puzzle.people

(** Get people with revealed clues *)
let revealed_people puzzle =
  List.filter (fun p -> Option.is_some p.clue) puzzle.people

(** Pretty print the puzzle grid *)
let print_grid puzzle =
  Printf.printf "\n    A       B       C       D\n";
  Printf.printf "  +-------+-------+-------+-------+\n";
  List.iter (fun row ->
    let row_num = match row with R1 -> "1" | R2 -> "2" | R3 -> "3" | R4 -> "4" | R5 -> "5" in
    Printf.printf "%s |" row_num;
    List.iter (fun col ->
      let pos = { col; row } in
      match find_by_position puzzle pos with
      | None -> Printf.printf "       |"
      | Some p ->
        let short_name = 
          if String.length p.name > 5 then String.sub p.name 0 5
          else p.name
        in
        let status_char = match p.known_status with
          | Unknown -> ' '
          | Innocent -> 'I'
          | Criminal -> 'C'
        in
        Printf.printf " %5s%c|" short_name status_char
    ) [A; B; C; D];
    Printf.printf "\n  +-------+-------+-------+-------+\n"
  ) [R1; R2; R3; R4; R5]