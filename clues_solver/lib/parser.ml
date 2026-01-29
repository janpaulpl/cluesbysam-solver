(** Natural language clue parser *)

open Types

(** Helper to extract names from a clue *)
let extract_names puzzle_names clue =
  List.filter (fun name ->
    let pattern = Re.Pcre.regexp ~flags:[`CASELESS] (Printf.sprintf "\\b%s\\b" (Re.Pcre.quote name)) in
    Re.execp pattern clue
  ) puzzle_names

(** Parse a number word to int *)
let parse_number_word s =
  match String.lowercase_ascii s with
  | "zero" | "no" -> Some 0
  | "one" | "a" | "an" -> Some 1
  | "two" -> Some 2
  | "three" -> Some 3
  | "four" -> Some 4
  | "five" -> Some 5
  | "six" -> Some 6
  | "seven" -> Some 7
  | "eight" -> Some 8
  | s -> int_of_string_opt s

(** Parse a row reference *)
let parse_row s =
  let s = String.lowercase_ascii (String.trim s) in
  (* Handle both "row X" and "rows X" *)
  let rest = 
    if String.length s >= 5 && String.sub s 0 4 = "rows" then
      String.trim (String.sub s 4 (String.length s - 4))
    else if String.length s >= 4 && String.sub s 0 3 = "row" then
      String.trim (String.sub s 3 (String.length s - 3))
    else ""
  in
  match rest with
  | "1" -> Some R1 | "2" -> Some R2 | "3" -> Some R3 
  | "4" -> Some R4 | "5" -> Some R5 | _ -> None

(** Parse a column reference *)  
let parse_column s =
  let s = String.lowercase_ascii (String.trim s) in
  (* Handle both "column X" and "columns X" *)
  if String.length s >= 7 && String.sub s 0 7 = "columns" then
    match String.trim (String.sub s 7 (String.length s - 7)) |> String.uppercase_ascii with
    | "A" -> Some A | "B" -> Some B | "C" -> Some C | "D" -> Some D | _ -> None
  else if String.length s >= 6 && String.sub s 0 6 = "column" then
    match String.trim (String.sub s 6 (String.length s - 6)) |> String.uppercase_ascii with
    | "A" -> Some A | "B" -> Some B | "C" -> Some C | "D" -> Some D | _ -> None
  else if String.length s >= 4 && String.sub s 0 4 = "cols" then
    match String.trim (String.sub s 4 (String.length s - 4)) |> String.uppercase_ascii with
    | "A" -> Some A | "B" -> Some B | "C" -> Some C | "D" -> Some D | _ -> None
  else if String.length s >= 3 && String.sub s 0 3 = "col" then
    match String.trim (String.sub s 3 (String.length s - 3)) |> String.uppercase_ascii with
    | "A" -> Some A | "B" -> Some B | "C" -> Some C | "D" -> Some D | _ -> None
  else None

(** Main clue parser - returns a list of constraints *)
let parse_clue ~speaker ~clue ~all_names : constraint_expr list =
  let clue_lower = String.lowercase_ascii clue in
  let _mentioned_names = extract_names all_names clue in
  
  let constraints = ref [] in
  let add c = constraints := c :: !constraints in
  
  (* Pattern: "X is innocent" or "X is criminal" *)
  let is_pattern = Re.Pcre.regexp ~flags:[`CASELESS] 
    "\\b(\\w+)\\s+is\\s+(innocent|criminal|guilty)\\b" in
  (try
    let groups = Re.all is_pattern clue in
    List.iter (fun g ->
      let name = Re.Group.get g 1 in
      let status = String.lowercase_ascii (Re.Group.get g 2) in
      if List.mem name all_names then
        add (if status = "innocent" then IsInnocent name else IsCriminal name)
    ) groups
  with _ -> ());
  
  (* Pattern: "I am innocent/criminal" *)
  if Re.execp (Re.Pcre.regexp ~flags:[`CASELESS] "\\bI\\s+am\\s+innocent\\b") clue then
    add (IsInnocent speaker)
  else if Re.execp (Re.Pcre.regexp ~flags:[`CASELESS] "\\bI\\s+am\\s+(criminal|guilty)\\b") clue then
    add (IsCriminal speaker);
  
  (* Pattern: "All criminals in row/column X are connected" *)
  let connected_row = Re.Pcre.regexp ~flags:[`CASELESS]
    "all\\s+criminals\\s+in\\s+row\\s+(\\d)\\s+are\\s+connected" in
  (* Use [A-Da-d] to match both uppercase and lowercase column letters *)
  let connected_col = Re.Pcre.regexp ~flags:[`CASELESS]
    "all\\s+criminals\\s+in\\s+column\\s+([A-Da-d])\\s+are\\s+connected" in
  (try
    let g = Re.exec connected_row clue_lower in
    let row = match Re.Group.get g 1 with
      | "1" -> R1 | "2" -> R2 | "3" -> R3 | "4" -> R4 | "5" -> R5 | _ -> failwith ""
    in
    add (Connected (Row row, Criminals))
  with _ -> ());
  (try
    let g = Re.exec connected_col clue_lower in
    let col = match String.uppercase_ascii (Re.Group.get g 1) with
      | "A" -> A | "B" -> B | "C" -> C | "D" -> D | _ -> failwith ""
    in
    add (Connected (Column col, Criminals))
  with _ -> ());
  
  (* Pattern: "X is one of N innocents/criminals on the edges/corners/in row/column" *)
  let one_of_region = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+is\\s+one\\s+of\\s+(\\d+|one|two|three|four|five|six|seven|eight)\\s+(innocents?|criminals?)\\s+(?:in|on)\\s+(?:the\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|edges?|corners?)" in
  (try
    let g = Re.exec one_of_region clue in
    let name = Re.Group.get g 1 in
    let count_str = Re.Group.get g 2 in
    let target_str = String.lowercase_ascii (Re.Group.get g 3) in
    let region_str = String.lowercase_ascii (Re.Group.get g 4) in
    if List.mem name all_names then begin
      let count = Option.value ~default:0 (parse_number_word count_str) in
      let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
      let region = 
        if String.length region_str >= 3 && String.sub region_str 0 3 = "row" then
          match parse_row region_str with Some r -> Row r | None -> failwith "bad row"
        else if String.length region_str >= 3 && String.sub region_str 0 3 = "col" then
          match parse_column region_str with Some c -> Column c | None -> failwith "bad col"
        else if String.length region_str >= 4 && String.sub region_str 0 4 = "edge" then Edges
        else if String.length region_str >= 4 && String.sub region_str 0 4 = "corn" then Corners
        else failwith "unknown region"
      in
      add (Count (region, target, Eq count));
      add (if target = Innocents then IsInnocent name else IsCriminal name)
    end
  with _ -> ());
  
  (* Pattern: "X is one of Y's N innocent/criminal neighbors" *)
  (* e.g. "Terry is one of Saga's 6 criminal neighbors" *)
  let one_of_neighbors = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+is\\s+one\\s+of\\s+(\\w+)'s\\s+(\\d+|one|two|three|four|five|six|seven|eight)\\s+(innocent|criminal)\\s+neighbors?" in
  (try
    let g = Re.exec one_of_neighbors clue in
    let name1 = Re.Group.get g 1 in
    let name2 = Re.Group.get g 2 in
    let count_str = Re.Group.get g 3 in
    let target_str = String.lowercase_ascii (Re.Group.get g 4) in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      let count = Option.value ~default:0 (parse_number_word count_str) in
      let neighbor_target = if target_str = "innocent" then InnocentNeighbors else CriminalNeighbors in
      (* Y has N innocent/criminal neighbors *)
      add (PersonCount (name2, neighbor_target, Eq count));
      (* X is innocent/criminal *)
      add (if target_str = "innocent" then IsInnocent name1 else IsCriminal name1)
    end
  with _ -> ());
  
  (* Pattern: "Only N innocents/criminals in/on [region]" - exact count *)
  (* Note: Do NOT match "more" - "only one more" is comparative, not absolute *)
  let only_count_region = Re.Pcre.regexp ~flags:[`CASELESS]
    "only\\s+(\\d+|one|two|three|four|five|six|seven|eight|zero|no)\\s+(innocents?|criminals?)\\s+(?:in|on)\\s+(?:the\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|edges?|corners?|entire\\s+grid)" in
  (try
    let g = Re.exec only_count_region clue_lower in
    let count_str = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    let region_str = Re.Group.get g 3 in
    let count = Option.value ~default:0 (parse_number_word count_str) in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    let region = 
      if String.length region_str >= 3 && String.sub region_str 0 3 = "row" then
        match parse_row region_str with Some r -> Row r | None -> failwith "bad row"
      else if String.length region_str >= 3 && String.sub region_str 0 3 = "col" then
        match parse_column region_str with Some c -> Column c | None -> failwith "bad col"
      else if String.length region_str >= 4 && String.sub region_str 0 4 = "edge" then Edges
      else if String.length region_str >= 4 && String.sub region_str 0 4 = "corn" then Corners
      else if String.length region_str >= 6 && String.sub region_str 0 6 = "entire" then Entire_grid
      else failwith "unknown region"
    in
    add (Count (region, target, Eq count))
  with _ -> ());
  
  (* Pattern: "Only N more innocents/criminals in/on [region] than [other region]" *)
  (* e.g. "Only one more innocent on the edges than in the corners" *)
  let more_than_region = Re.Pcre.regexp ~flags:[`CASELESS]
    "only\\s+(\\d+|one|two|three|four|five|six|seven|eight|zero|no)\\s+more\\s+(innocents?|criminals?)\\s+(?:in|on)\\s+(?:the\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|edges?|corners?)\\s+than\\s+(?:in|on)?\\s*(?:the\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|edges?|corners?)" in
  (try
    let g = Re.exec more_than_region clue_lower in
    let diff_str = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    let region1_str = Re.Group.get g 3 in
    let region2_str = Re.Group.get g 4 in
    let _diff = Option.value ~default:0 (parse_number_word diff_str) in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    let parse_region_str s =
      if String.length s >= 3 && String.sub s 0 3 = "row" then
        match parse_row s with Some r -> Row r | None -> failwith "bad row"
      else if String.length s >= 3 && String.sub s 0 3 = "col" then
        match parse_column s with Some c -> Column c | None -> failwith "bad col"
      else if String.length s >= 4 && String.sub s 0 4 = "edge" then Edges
      else if String.length s >= 4 && String.sub s 0 4 = "corn" then Corners
      else failwith "unknown region"
    in
    let region1 = parse_region_str region1_str in
    let region2 = parse_region_str region2_str in
    (* "N more X in R1 than R2" means count(R1) > count(R2) *)
    (* For now, just encode as "more in R1 than R2" since we don't have a DiffEq constraint *)
    add (MoreThan (region1, target, region2, target))
  with _ -> ());

  (* Pattern: "There are N criminals/innocents in row/column X" *)
  let count_region = Re.Pcre.regexp ~flags:[`CASELESS]
    "there\\s+(?:are|is)\\s+(\\d+|one|two|three|four|five|zero|no)\\s+(criminals?|innocents?)\\s+in\\s+(row\\s+\\d|column\\s+[A-Da-d])" in
  (try
    let g = Re.exec count_region clue_lower in
    let count_str = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    let region_str = Re.Group.get g 3 in
    let count = Option.value ~default:0 (parse_number_word count_str) in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    let region = 
      match parse_row region_str with
      | Some r -> Row r
      | None -> 
        match parse_column region_str with
        | Some c -> Column c
        | None -> failwith "unknown region"
    in
    add (Count (region, target, Eq count))
  with _ -> ());
  
  (* Pattern: "odd/even number of innocents/criminals below/above/to the left of/to the right of X" *)
  let relative_count = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:there(?:'s|\\s+is|\\s+are)\\s+)?(?:an?\\s+)?(odd|even)\\s+number\\s+of\\s+(innocents?|criminals?)\\s+(below|above|to\\s+the\\s+left\\s+of|to\\s+the\\s+right\\s+of)\\s+(\\w+)" in
  (try
    let g = Re.exec relative_count clue in
    let parity = String.lowercase_ascii (Re.Group.get g 1) in
    let target_str = String.lowercase_ascii (Re.Group.get g 2) in
    let direction = String.lowercase_ascii (Re.Group.get g 3) in
    let name = Re.Group.get g 4 in
    if List.mem name all_names then begin
      let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
      let comparison = if parity = "odd" then Odd else Even in
      let region = 
        if String.sub direction 0 5 = "below" then Below name
        else if String.sub direction 0 5 = "above" then Above name
        else if String.length direction > 10 && String.sub direction 0 11 = "to the left" then LeftOf name
        else RightOf name
      in
      add (Count (region, target, comparison))
    end
  with _ -> ());
  
  (* Pattern: "N innocents/criminals below/above X" *)
  let simple_relative_count = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:there(?:'s|\\s+is|\\s+are)\\s+)?(\\d+|one|two|three|four|five|zero|no)\\s+(innocents?|criminals?)\\s+(below|above|to\\s+the\\s+left\\s+of|to\\s+the\\s+right\\s+of)\\s+(\\w+)" in
  (try
    let g = Re.exec simple_relative_count clue in
    let count_str = Re.Group.get g 1 in
    let target_str = String.lowercase_ascii (Re.Group.get g 2) in
    let direction = String.lowercase_ascii (Re.Group.get g 3) in
    let name = Re.Group.get g 4 in
    if List.mem name all_names then begin
      let count = Option.value ~default:0 (parse_number_word count_str) in
      let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
      let region = 
        if String.sub direction 0 5 = "below" then Below name
        else if String.sub direction 0 5 = "above" then Above name
        else if String.length direction > 10 && String.sub direction 0 11 = "to the left" then LeftOf name
        else RightOf name
      in
      add (Count (region, target, Eq count))
    end
  with _ -> ());
  
  (* Pattern: "X has [only/exactly] N criminal/innocent neighbors" *)
  let neighbor_count = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+has?\\s+(?:only\\s+|exactly\\s+)?(\\d+|one|two|three|four|five|six|seven|eight|zero|no|an?\\s+even|an?\\s+odd)\\s+(criminal|innocent)\\s+neighbors?" in
  (try
    let groups = Re.all neighbor_count clue in
    List.iter (fun g ->
      let name = Re.Group.get g 1 in
      let count_str = String.lowercase_ascii (Re.Group.get g 2) in
      let target_str = Re.Group.get g 3 in
      if List.mem name all_names then begin
        let target = if target_str = "innocent" then InnocentNeighbors else CriminalNeighbors in
        let len = String.length count_str in
        let comparison = 
          if (len >= 3 && String.sub count_str 0 3 = "eve") || 
             (len >= 4 && String.sub count_str (len - 4) 4 = "even") then Even
          else if (len >= 3 && String.sub count_str 0 3 = "odd") || 
                  (len >= 3 && String.sub count_str (len - 3) 3 = "odd") then Odd
          else Eq (Option.value ~default:0 (parse_number_word count_str))
        in
        add (PersonCount (name, target, comparison))
      end
    ) groups
  with _ -> ());
  
  (* Pattern: "I have [only/exactly] N criminal/innocent neighbors" *)
  let i_have_neighbors = Re.Pcre.regexp ~flags:[`CASELESS]
    "I\\s+have\\s+(?:only\\s+|exactly\\s+)?(\\d+|one|two|three|four|five|six|seven|eight|zero|no|an?\\s+even|an?\\s+odd)\\s+(criminal|innocent)\\s+neighbors?" in
  (try
    let g = Re.exec i_have_neighbors clue in
    let count_str = String.lowercase_ascii (Re.Group.get g 1) in
    let target_str = Re.Group.get g 2 in
    let target = if target_str = "innocent" then InnocentNeighbors else CriminalNeighbors in
    let len = String.length count_str in
    let comparison = 
      if (len >= 3 && String.sub count_str 0 3 = "eve") || 
         (len >= 4 && String.sub count_str (len - 4) 4 = "even") then Even
      else if (len >= 3 && String.sub count_str 0 3 = "odd") || 
              (len >= 3 && String.sub count_str (len - 3) 3 = "odd") then Odd
      else Eq (Option.value ~default:0 (parse_number_word count_str))
    in
    add (PersonCount (speaker, target, comparison))
  with _ -> ());
  
  (* Pattern: "There's an odd/even number of criminals/innocents neighboring X" *)
  (* e.g. "There's an odd number of criminals neighboring Saga" *)
  let neighboring_count = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:there(?:'s|\\s+is|\\s+are)\\s+)?(?:an?\\s+)?(odd|even)\\s+number\\s+of\\s+(criminals?|innocents?)\\s+neighboring\\s+(\\w+)" in
  (try
    let g = Re.exec neighboring_count clue in
    let parity = String.lowercase_ascii (Re.Group.get g 1) in
    let target_str = String.lowercase_ascii (Re.Group.get g 2) in
    let name = Re.Group.get g 3 in
    if List.mem name all_names then begin
      let target = if String.sub target_str 0 1 = "i" then InnocentNeighbors else CriminalNeighbors in
      let comparison = if parity = "odd" then Odd else Even in
      add (PersonCount (name, target, comparison))
    end
  with _ -> ());
  
  (* Pattern: "X and Y are both innocent/criminal" *)
  let both_same = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+and\\s+(\\w+)\\s+are\\s+both\\s+(innocent|criminal)" in
  (try
    let g = Re.exec both_same clue in
    let name1 = Re.Group.get g 1 in
    let name2 = Re.Group.get g 2 in
    let status = Re.Group.get g 3 in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      if status = "innocent" then begin
        add (IsInnocent name1);
        add (IsInnocent name2)
      end else begin
        add (IsCriminal name1);
        add (IsCriminal name2)
      end
    end
  with _ -> ());
  
  (* Pattern: "X has the same number of criminal neighbors as Y" *)
  let same_neighbors = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+has?\\s+the\\s+same\\s+number\\s+of\\s+(criminal|innocent)\\s+neighbors\\s+as\\s+(\\w+)" in
  (try
    let g = Re.exec same_neighbors clue in
    let name1 = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    let name2 = Re.Group.get g 3 in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      let target = if target_str = "innocent" then InnocentNeighbors else CriminalNeighbors in
      add (SameAs (name1, target, name2))
    end
  with _ -> ());
  
  (* Pattern: "X has the most criminal/innocent neighbors" *)
  let the_most = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+has?\\s+(?:the\\s+)?most\\s+(criminal|innocent)\\s+neighbors?" in
  (try
    let g = Re.exec the_most clue in
    let name = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    if List.mem name all_names then begin
      let target = if target_str = "innocent" then InnocentNeighbors else CriminalNeighbors in
      add (TheMost (name, target))
    end
  with _ -> ());
  
  (* Pattern: "Exactly N of X, Y, Z are innocent/criminal" *)
  let exactly_n = Re.Pcre.regexp ~flags:[`CASELESS]
    "exactly\\s+(\\d+|one|two|three)\\s+of\\s+(.+?)\\s+(?:are|is)\\s+(innocent|criminal)" in
  (try
    let g = Re.exec exactly_n clue in
    let count_str = Re.Group.get g 1 in
    let names_str = Re.Group.get g 2 in
    let status_str = Re.Group.get g 3 in
    let count = Option.value ~default:0 (parse_number_word count_str) in
    let target = if status_str = "innocent" then Innocents else Criminals in
    (* Extract names from the list *)
    let names_in_list = extract_names all_names names_str in
    if List.length names_in_list > 0 then
      add (ExactlyN (names_in_list, target, count))
  with _ -> ());
  
  (* Pattern: "If X is innocent/criminal, then Y is innocent/criminal" *)
  let if_then = Re.Pcre.regexp ~flags:[`CASELESS]
    "if\\s+(\\w+)\\s+is\\s+(innocent|criminal),?\\s+then\\s+(\\w+)\\s+is\\s+(innocent|criminal)" in
  (try
    let g = Re.exec if_then clue in
    let name1 = Re.Group.get g 1 in
    let status1 = Re.Group.get g 2 in
    let name2 = Re.Group.get g 3 in
    let status2 = Re.Group.get g 4 in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      let antecedent = if status1 = "innocent" then IsInnocent name1 else IsCriminal name1 in
      let consequent = if status2 = "innocent" then IsInnocent name2 else IsCriminal name2 in
      add (Implies (antecedent, consequent))
    end
  with _ -> ());
  
  (* Pattern: "[There are] more criminals/innocents in [region] than [region]" *)
  (* Handles rows, columns, edges, corners *)
  let more_than_regions = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:there\\s+are\\s+)?more\\s+(criminals?|innocents?)\\s+in\\s+(row\\s+\\d|column\\s+[A-Da-d]|the\\s+edges?|the\\s+corners?)\\s+than\\s+(?:in\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|the\\s+edges?|the\\s+corners?)" in
  let parse_region_string s =
    let s = String.lowercase_ascii (String.trim s) in
    if String.length s >= 3 && String.sub s 0 3 = "row" then
      match parse_row s with Some r -> Some (Row r) | None -> None
    else if String.length s >= 3 && String.sub s 0 3 = "col" then
      match parse_column s with Some c -> Some (Column c) | None -> None
    else if String.length s >= 6 && String.sub s 0 6 = "column" then
      match parse_column s with Some c -> Some (Column c) | None -> None
    else if String.length s >= 4 && (String.sub s 0 4 = "edge" || 
            (String.length s >= 9 && String.sub s 0 9 = "the edge")) then Some Edges
    else if String.length s >= 4 && (String.sub s 0 4 = "corn" ||
            (String.length s >= 10 && String.sub s 0 10 = "the corner")) then Some Corners
    else None
  in
  (try
    let g = Re.exec more_than_regions clue_lower in
    let target_str = Re.Group.get g 1 in
    let region1_str = Re.Group.get g 2 in
    let region2_str = Re.Group.get g 3 in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    match parse_region_string region1_str, parse_region_string region2_str with
    | Some region1, Some region2 ->
      add (MoreThan (region1, target, region2, target))
    | _ -> ()
  with _ -> ());
  
  (* Pattern: "[There's an] equal number of innocents/criminals in [region1] and [region2]" *)
  (* e.g. "There's an equal number of innocents in rows 1 and 3" *)
  (* Handles: "rows 1 and 3", "row 1 and row 3", "columns A and B" *)
  let equal_count_regions = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:there(?:'s|\\s+is|\\s+are)\\s+)?(?:an?\\s+)?equal\\s+number\\s+of\\s+(innocents?|criminals?)\\s+in\\s+(rows?\\s+\\d|columns?\\s+[A-Da-d])\\s+and\\s+(?:rows?\\s+|columns?\\s+)?(\\d|[A-Da-d])" in
  (try
    let g = Re.exec equal_count_regions clue_lower in
    let target_str = Re.Group.get g 1 in
    let region1_str = Re.Group.get g 2 in
    let region2_ref = String.uppercase_ascii (Re.Group.get g 3) in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    (* Determine if we're dealing with rows or columns from region1 *)
    let is_row = String.length region1_str >= 3 && String.sub region1_str 0 3 = "row" in
    let region1 = 
      if is_row then
        match parse_row region1_str with Some r -> Row r | None -> failwith "bad row"
      else
        match parse_column region1_str with Some c -> Column c | None -> failwith "bad col"
    in
    let region2 =
      if is_row then
        match region2_ref with
        | "1" -> Row R1 | "2" -> Row R2 | "3" -> Row R3 | "4" -> Row R4 | "5" -> Row R5
        | _ -> failwith "bad row ref"
      else
        match region2_ref with
        | "A" -> Column A | "B" -> Column B | "C" -> Column C | "D" -> Column D
        | _ -> failwith "bad col ref"
    in
    add (EqualCount (region1, target, region2, target))
  with _ -> ());
  
  (* Pattern: "Row/Column X has more innocents/criminals than any other row/column" *)
  (* e.g. "Row 5 has more innocents than any other row" *)
  let more_than_any_other = Re.Pcre.regexp ~flags:[`CASELESS]
    "(row\\s+\\d|column\\s+[A-Da-d])\\s+has\\s+more\\s+(innocents?|criminals?)\\s+than\\s+any\\s+other\\s+(row|column)" in
  (try
    let g = Re.exec more_than_any_other clue_lower in
    let region_str = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    let region_type = Re.Group.get g 3 in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    let region = 
      if String.length region_str >= 3 && String.sub region_str 0 3 = "row" then
        match parse_row region_str with Some r -> Row r | None -> failwith "bad row"
      else
        match parse_column region_str with Some c -> Column c | None -> failwith "bad col"
    in
    (* Generate MoreThan constraints for each other row/column *)
    if region_type = "row" then begin
      let all_rows = [R1; R2; R3; R4; R5] in
      let this_row = match region with Row r -> r | _ -> failwith "expected row" in
      List.iter (fun other_row ->
        if not (equal_row this_row other_row) then
          add (MoreThan (region, target, Row other_row, target))
      ) all_rows
    end else begin
      let all_cols = [A; B; C; D] in
      let this_col = match region with Column c -> c | _ -> failwith "expected column" in
      List.iter (fun other_col ->
        if not (equal_column this_col other_col) then
          add (MoreThan (region, target, Column other_col, target))
      ) all_cols
    end
  with _ -> ());
  
  (* Pattern: "Row/Column X is the only row/column with exactly N criminals/innocents" *)
  (* e.g. "Row 5 is the only row with exactly one criminal" *)
  let only_with_exactly = Re.Pcre.regexp ~flags:[`CASELESS]
    "(row\\s+\\d|column\\s+[A-Da-d])\\s+is\\s+the\\s+only\\s+(row|column)\\s+with\\s+(?:exactly\\s+)?(\\d+|one|two|three|four|five|zero|no)\\s+(criminals?|innocents?)" in
  (try
    let g = Re.exec only_with_exactly clue_lower in
    let region_str = Re.Group.get g 1 in
    let region_type = Re.Group.get g 2 in
    let count_str = Re.Group.get g 3 in
    let target_str = Re.Group.get g 4 in
    let count = Option.value ~default:0 (parse_number_word count_str) in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    let region = 
      if String.length region_str >= 3 && String.sub region_str 0 3 = "row" then
        match parse_row region_str with Some r -> Row r | None -> failwith "bad row"
      else
        match parse_column region_str with Some c -> Column c | None -> failwith "bad col"
    in
    (* This row/column has exactly N *)
    add (Count (region, target, Eq count));
    (* All other rows/columns do NOT have exactly N *)
    if region_type = "row" then begin
      let all_rows = [R1; R2; R3; R4; R5] in
      let this_row = match region with Row r -> r | _ -> failwith "expected row" in
      List.iter (fun other_row ->
        if not (equal_row this_row other_row) then
          add (Not (Count (Row other_row, target, Eq count)))
      ) all_rows
    end else begin
      let all_cols = [A; B; C; D] in
      let this_col = match region with Column c -> c | _ -> failwith "expected column" in
      List.iter (fun other_col ->
        if not (equal_column this_col other_col) then
          add (Not (Count (Column other_col, target, Eq count)))
      ) all_cols
    end
  with _ -> ());
  
  (* Pattern: "X and Y share an odd/even number of innocent/criminal neighbors" *)
  let share_neighbors = Re.Pcre.regexp ~flags:[`CASELESS]
    "(\\w+)\\s+and\\s+(\\w+)\\s+share\\s+(?:an?\\s+)?(odd|even)\\s+number\\s+of\\s+(innocent|criminal)\\s+neighbors?" in
  (try
    let g = Re.exec share_neighbors clue in
    let name1 = Re.Group.get g 1 in
    let name2 = Re.Group.get g 2 in
    let parity = Re.Group.get g 3 in
    let target_str = Re.Group.get g 4 in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      let target = if target_str = "innocent" then Innocents else Criminals in
      let comparison = if parity = "odd" then Odd else Even in
      add (ShareNeighbors (name1, name2, target, comparison))
    end
  with _ -> ());
  
  (* Pattern: "Exactly N of X's M innocent/criminal neighbors also neighbor Y" *)
  (* e.g. "Exactly 2 of Peter's 3 innocent neighbors also neighbor Oscar" *)
  let neighbors_also_neighbor = Re.Pcre.regexp ~flags:[`CASELESS]
    "exactly\\s+(\\d+|one|two|three|four|five|six|seven|eight)\\s+of\\s+(\\w+)'s\\s+(\\d+|one|two|three|four|five|six|seven|eight)\\s+(innocent|criminal)\\s+neighbors?\\s+also\\s+neighbors?\\s+(\\w+)" in
  (try
    let g = Re.exec neighbors_also_neighbor clue in
    let shared_count_str = Re.Group.get g 1 in
    let name1 = Re.Group.get g 2 in
    let total_count_str = Re.Group.get g 3 in
    let target_str = String.lowercase_ascii (Re.Group.get g 4) in
    let name2 = Re.Group.get g 5 in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      let shared_count = Option.value ~default:0 (parse_number_word shared_count_str) in
      let total_count = Option.value ~default:0 (parse_number_word total_count_str) in
      let target = if target_str = "innocent" then Innocents else Criminals in
      let neighbor_target = if target_str = "innocent" then InnocentNeighbors else CriminalNeighbors in
      (* X has M innocent/criminal neighbors *)
      add (PersonCount (name1, neighbor_target, Eq total_count));
      (* N of the common neighbors of X and Y are innocent/criminal *)
      add (ShareNeighbors (name1, name2, target, Eq shared_count))
    end
  with _ -> ());
  
  (* Pattern: "Only/Exactly N of the M innocents/criminals in [region] is/are X's neighbor(s)" *)
  (* e.g. "Only 1 of the 2 innocents in column B is Laura's neighbor" *)
  (* Also handles: "on the edges", "on the corners", "in row N" *)
  let region_neighbor_count = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:only|exactly)\\s+(\\d+|one|two|three|four|five|six|seven|eight)\\s+of\\s+(?:the\\s+)?(\\d+|one|two|three|four|five|six|seven|eight)\\s+(innocents?|criminals?)\\s+(?:in|on)\\s+(?:the\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|edges?|corners?)\\s+(?:is|are)\\s+(\\w+)'s\\s+neighbors?" in
  (try
    let g = Re.exec region_neighbor_count clue in
    let neighbor_count_str = Re.Group.get g 1 in
    let total_count_str = Re.Group.get g 2 in
    let target_str = String.lowercase_ascii (Re.Group.get g 3) in
    let region_str = String.lowercase_ascii (Re.Group.get g 4) in
    let person_name = Re.Group.get g 5 in
    if List.mem person_name all_names then begin
      let neighbor_count = Option.value ~default:0 (parse_number_word neighbor_count_str) in
      let total_count = Option.value ~default:0 (parse_number_word total_count_str) in
      let target = if String.length target_str >= 1 && String.sub target_str 0 1 = "i" then Innocents else Criminals in
      let region = 
        if String.length region_str >= 3 && String.sub region_str 0 3 = "row" then
          match parse_row region_str with Some r -> Row r | None -> failwith "bad row"
        else if String.length region_str >= 3 && String.sub region_str 0 3 = "col" then
          match parse_column region_str with Some c -> Column c | None -> failwith "bad col"
        else if String.length region_str >= 4 && String.sub region_str 0 4 = "edge" then Edges
        else if String.length region_str >= 4 && String.sub region_str 0 4 = "corn" then Corners
        else failwith "unknown region"
      in
      (* M innocents/criminals in region *)
      add (Count (region, target, Eq total_count));
      (* N of them are person's neighbors *)
      add (RegionNeighborCount (region, target, person_name, Eq neighbor_count))
    end
  with _ -> ());
  
  (* Pattern: "Exactly/Only N innocents/criminals in [region] is/are neighboring X" *)
  (* e.g. "Exactly 1 innocent in row 5 is neighboring Saga" *)
  (* Simpler version without specifying total count in region *)
  let simple_region_neighbor = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:only|exactly)\\s+(\\d+|one|two|three|four|five|six|seven|eight|zero|no)\\s+(innocents?|criminals?)\\s+(?:in|on)\\s+(?:the\\s+)?(row\\s+\\d|column\\s+[A-Da-d]|edges?|corners?)\\s+(?:is|are)\\s+(?:neighboring|neighbour(?:ing)?|neighbors?\\s+of|neighbours?\\s+of)\\s+(\\w+)" in
  (try
    let g = Re.exec simple_region_neighbor clue in
    let count_str = Re.Group.get g 1 in
    let target_str = String.lowercase_ascii (Re.Group.get g 2) in
    let region_str = String.lowercase_ascii (Re.Group.get g 3) in
    let person_name = Re.Group.get g 4 in
    if List.mem person_name all_names then begin
      let count = Option.value ~default:0 (parse_number_word count_str) in
      let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
      let region = 
        if String.length region_str >= 3 && String.sub region_str 0 3 = "row" then
          match parse_row region_str with Some r -> Row r | None -> failwith "bad row"
        else if String.length region_str >= 3 && String.sub region_str 0 3 = "col" then
          match parse_column region_str with Some c -> Column c | None -> failwith "bad col"
        else if String.length region_str >= 4 && String.sub region_str 0 4 = "edge" then Edges
        else if String.length region_str >= 4 && String.sub region_str 0 4 = "corn" then Corners
        else failwith "unknown region"
      in
      add (RegionNeighborCount (region, target, person_name, Eq count))
    end
  with _ -> ());
  
  (* Pattern: "There are N criminals/innocents in total" or "N criminals/innocents total" *)
  let total_count = Re.Pcre.regexp ~flags:[`CASELESS]
    "(?:there\\s+are\\s+)?(\\d+|one|two|three|four|five|six|seven|eight|nine|ten)\\s+(criminals?|innocents?)\\s+(?:in\\s+)?total" in
  (try
    let g = Re.exec total_count clue_lower in
    let count_str = Re.Group.get g 1 in
    let target_str = Re.Group.get g 2 in
    let count = Option.value ~default:0 (parse_number_word count_str) in
    let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
    add (Count (Entire_grid, target, Eq count))
  with _ -> ());
  
  (* Pattern: "between X and Y" with some property *)
  let between_innocents = Re.Pcre.regexp ~flags:[`CASELESS]
    "between\\s+(\\w+)\\s+and\\s+(\\w+).*?(\\d+|one|two|three|zero|no)\\s+(criminals?|innocents?)" in
  (try
    let g = Re.exec between_innocents clue in
    let name1 = Re.Group.get g 1 in
    let name2 = Re.Group.get g 2 in
    let count_str = Re.Group.get g 3 in
    let target_str = Re.Group.get g 4 in
    if List.mem name1 all_names && List.mem name2 all_names then begin
      let count = Option.value ~default:0 (parse_number_word count_str) in
      let target = if String.sub target_str 0 1 = "i" then Innocents else Criminals in
      add (Count (Between (name1, name2), target, Eq count))
    end
  with _ -> ());
  
  (* If no constraints were parsed, add an Unparsed marker *)
  if !constraints = [] then
    [Unparsed clue]
  else
    !constraints

(** Test if a clue was fully parsed *)
let is_fully_parsed constraints =
  not (List.exists (function Unparsed _ -> true | _ -> false) constraints)

(** Get unparsed parts of a clue *)
let get_unparsed constraints =
  List.filter_map (function Unparsed s -> Some s | _ -> None) constraints