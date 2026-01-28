(** Types for the Clues by Sam puzzle solver *)

(** Column identifiers A-D *)
type column = A | B | C | D
[@@deriving eq, ord, show]

(** Row identifiers 1-5 *)
type row = R1 | R2 | R3 | R4 | R5
[@@deriving eq, ord, show]

(** A position on the grid *)
type position = { col : column; row : row }
[@@deriving eq, ord, show]

(** Status of a person *)
type status = 
  | Unknown
  | Innocent
  | Criminal
[@@deriving eq, show]

(** A person in the puzzle *)
type person = {
  pos : position;
  name : string;
  clue : string option;  (* None means not yet revealed *)
  known_status : status; (* What we've determined so far *)
}
[@@deriving show]

(** The full puzzle state *)
type puzzle = {
  people : person list;
  width : int;   (* Number of columns, typically 4 *)
  height : int;  (* Number of rows, typically 5 *)
}
[@@deriving show]

(** Parsed clue expressions for the solver *)
type region =
  | Row of row
  | Column of column
  | Entire_grid
  | Edges
  | Corners
  | Neighbors of string        (* Neighbors of person by name *)
  | CommonNeighbors of string * string
  | Between of string * string (* People between two named persons *)
  | Custom of string list      (* Explicit list of names *)
[@@deriving show]

type comparison =
  | Eq of int
  | Gt of int
  | Lt of int
  | Gte of int
  | Lte of int
  | Odd
  | Even
[@@deriving show]

type count_target =
  | Criminals
  | Innocents
  | CriminalNeighbors
  | InnocentNeighbors
[@@deriving show]

(** Constraint types that clues translate into *)
type constraint_expr =
  (* Direct status assignment *)
  | IsCriminal of string
  | IsInnocent of string
  
  (* Counting constraints *)
  | Count of region * count_target * comparison
  | PersonCount of string * count_target * comparison  (* e.g., "X has N criminal neighbors" *)
  
  (* Comparisons between regions/people *)
  | MoreThan of region * count_target * region * count_target
  | TheMost of string * count_target   (* X has uniquely the most *)
  | TheLeast of string * count_target
  | SameAs of string * count_target * string  (* X has same count as Y *)
  
  (* Connectivity *)
  | Connected of region * count_target
  
  (* Positional relationships *)
  | SameStatus of string * string      (* X and Y have same status *)
  | DifferentStatus of string * string
  
  (* Logical combinations *)
  | Implies of constraint_expr * constraint_expr
  | And of constraint_expr list
  | Or of constraint_expr list
  | Not of constraint_expr
  
  (* One of N is criminal/innocent *)
  | ExactlyN of string list * count_target * int
  
  (* Share constraints - common neighbors *)
  | ShareNeighbors of string * string * count_target * comparison
  
  (* Location-based *)
  | SomeoneInRegion of region * count_target  (* At least one criminal/innocent in region *)
  
  (* All in region have property *)
  | AllInRegion of region * status
  
  (* Relative positions *)
  | DirectlyAdjacent of string * string * [`Left | `Right | `Above | `Below]
  | InSameRow of string * string
  | InSameColumn of string * string
  
  (* Complex constraints *)
  | Unparsed of string  (* Fallback for clues we couldn't parse *)
[@@deriving show]

(** Module for position operations *)
module Position = struct
  let make col row = { col; row }
  
  let col_to_int = function A -> 0 | B -> 1 | C -> 2 | D -> 3
  let int_to_col = function 0 -> A | 1 -> B | 2 -> C | 3 -> D | _ -> failwith "Invalid column"
  
  let row_to_int = function R1 -> 0 | R2 -> 1 | R3 -> 2 | R4 -> 3 | R5 -> 4
  let int_to_row = function 0 -> R1 | 1 -> R2 | 2 -> R3 | 3 -> R4 | 4 -> R5 | _ -> failwith "Invalid row"
  
  let col_of_char = function
    | 'A' | 'a' -> A | 'B' | 'b' -> B | 'C' | 'c' -> C | 'D' | 'd' -> D
    | c -> failwith (Printf.sprintf "Invalid column: %c" c)
  
  let row_of_char = function
    | '1' -> R1 | '2' -> R2 | '3' -> R3 | '4' -> R4 | '5' -> R5
    | c -> failwith (Printf.sprintf "Invalid row: %c" c)
  
  let parse s =
    if String.length s < 2 then failwith "Position too short"
    else
      let col = col_of_char s.[0] in
      let row = row_of_char s.[1] in
      { col; row }
  
  let to_string pos =
    let col_char = match pos.col with A -> 'A' | B -> 'B' | C -> 'C' | D -> 'D' in
    let row_char = match pos.row with R1 -> '1' | R2 -> '2' | R3 -> '3' | R4 -> '4' | R5 -> '5' in
    Printf.sprintf "%c%c" col_char row_char
  
  (** Check if position is on the edge of a 4x5 grid *)
  let is_edge pos =
    pos.col = A || pos.col = D || pos.row = R1 || pos.row = R5
  
  (** Check if position is a corner *)
  let is_corner pos =
    (pos.col = A || pos.col = D) && (pos.row = R1 || pos.row = R5)
  
  (** Get all 8 neighbors (including diagonals) *)
  let neighbors pos =
    let ci = col_to_int pos.col in
    let ri = row_to_int pos.row in
    let candidates = [
      (ci-1, ri-1); (ci, ri-1); (ci+1, ri-1);
      (ci-1, ri);               (ci+1, ri);
      (ci-1, ri+1); (ci, ri+1); (ci+1, ri+1);
    ] in
    List.filter_map (fun (c, r) ->
      if c >= 0 && c <= 3 && r >= 0 && r <= 4 then
        Some { col = int_to_col c; row = int_to_row r }
      else None
    ) candidates
  
  (** Get orthogonal neighbors only (for connectivity) *)
  let orthogonal_neighbors pos =
    let ci = col_to_int pos.col in
    let ri = row_to_int pos.row in
    let candidates = [
      (ci, ri-1); (ci-1, ri); (ci+1, ri); (ci, ri+1)
    ] in
    List.filter_map (fun (c, r) ->
      if c >= 0 && c <= 3 && r >= 0 && r <= 4 then
        Some { col = int_to_col c; row = int_to_row r }
      else None
    ) candidates
  
  (** Get all positions in a row *)
  let positions_in_row r =
    List.map (fun c -> { col = c; row = r }) [A; B; C; D]
  
  (** Get all positions in a column *)
  let positions_in_column c =
    List.map (fun r -> { col = c; row = r }) [R1; R2; R3; R4; R5]
  
  (** Get all edge positions *)
  let edge_positions () =
    let all = List.concat_map positions_in_row [R1; R2; R3; R4; R5] in
    List.filter is_edge all
  
  (** Get all corner positions *)
  let corner_positions () =
    [{ col = A; row = R1 }; { col = D; row = R1 };
     { col = A; row = R5 }; { col = D; row = R5 }]
  
  (** Get positions between two positions (exclusive) *)
  let between p1 p2 =
    if equal_column p1.col p2.col then
      (* Same column - get rows between *)
      let r1 = row_to_int p1.row in
      let r2 = row_to_int p2.row in
      let min_r, max_r = min r1 r2, max r1 r2 in
      List.filter_map (fun r ->
        if r > min_r && r < max_r then
          Some { col = p1.col; row = int_to_row r }
        else None
      ) [0; 1; 2; 3; 4]
    else if equal_row p1.row p2.row then
      (* Same row - get columns between *)
      let c1 = col_to_int p1.col in
      let c2 = col_to_int p2.col in
      let min_c, max_c = min c1 c2, max c1 c2 in
      List.filter_map (fun c ->
        if c > min_c && c < max_c then
          Some { col = int_to_col c; row = p1.row }
        else None
      ) [0; 1; 2; 3]
    else
      [] (* Not in same row or column - no "between" defined *)
end