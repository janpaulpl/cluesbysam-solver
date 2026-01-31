(** Z3 SMT Solver interface for encoding and solving constraints *)

open Types

module Z3Solver = struct
  
  type solver_state = {
    ctx : Z3.context;
    solver : Z3.Solver.solver;
    (* Map from person name to their Z3 boolean variable (true = criminal) *)
    vars : (string, Z3.Expr.expr) Hashtbl.t;
    puzzle : puzzle;
  }
  
  (** Create a new solver state *)
  let create puzzle =
    let ctx = Z3.mk_context [] in
    let solver = Z3.Solver.mk_solver ctx None in
    let vars = Hashtbl.create 20 in
    (* Create a boolean variable for each person *)
    List.iter (fun person ->
      let var = Z3.Boolean.mk_const_s ctx person.name in
      Hashtbl.add vars person.name var
    ) puzzle.people;
    { ctx; solver; vars; puzzle }
  
  (** Get variable for a person (true = criminal, false = innocent) *)
  let get_var state name =
    try Hashtbl.find state.vars name
    with Not_found -> failwith (Printf.sprintf "Unknown person: %s" name)
  
  (** Create "is criminal" expression *)
  let is_criminal state name = get_var state name
  
  (** Create "is innocent" expression *)
  let is_innocent state name = Z3.Boolean.mk_not state.ctx (get_var state name)
  
  (** Get people in a region *)
  let people_in_region state region =
    Puzzle.people_in_region state.puzzle region
  
  (** Count criminals/innocents in a list of people *)
  let count_in_people state target people =
    match target with
    | Everyone ->
      (* Just count all people in the list *)
      Z3.Arithmetic.Integer.mk_numeral_i state.ctx (List.length people)
    | _ ->
      let vars = List.map (fun p ->
        match target with
        | Criminals | CriminalNeighbors -> is_criminal state p.name
        | Innocents | InnocentNeighbors -> is_innocent state p.name
        | Everyone -> failwith "Already handled"
      ) people in
      if vars = [] then
        Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
      else
        let ones = List.map (fun v ->
          Z3.Boolean.mk_ite state.ctx v
            (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 1)
            (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0)
        ) vars in
        Z3.Arithmetic.mk_add state.ctx ones
  
  (** Count neighbors of a person with given status *)
  let count_neighbors state name target =
    match Puzzle.find_by_name state.puzzle name with
    | None -> Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
    | Some person ->
      let neighbor_positions = Position.neighbors person.pos in
      let neighbors = List.filter (fun p ->
        List.exists (equal_position p.pos) neighbor_positions
      ) state.puzzle.people in
      count_in_people state target neighbors
  
  (** Create comparison constraint *)
  let make_comparison state count comparison =
    let open Z3.Arithmetic in
    match comparison with
    | Eq n -> Z3.Boolean.mk_eq state.ctx count (Integer.mk_numeral_i state.ctx n)
    | Gt n -> mk_gt state.ctx count (Integer.mk_numeral_i state.ctx n)
    | Lt n -> mk_lt state.ctx count (Integer.mk_numeral_i state.ctx n)
    | Gte n -> mk_ge state.ctx count (Integer.mk_numeral_i state.ctx n)
    | Lte n -> mk_le state.ctx count (Integer.mk_numeral_i state.ctx n)
    | Even -> 
      let two = Integer.mk_numeral_i state.ctx 2 in
      let zero = Integer.mk_numeral_i state.ctx 0 in
      Z3.Boolean.mk_eq state.ctx (Integer.mk_mod state.ctx count two) zero
    | Odd ->
      let two = Integer.mk_numeral_i state.ctx 2 in
      let one = Integer.mk_numeral_i state.ctx 1 in
      Z3.Boolean.mk_eq state.ctx (Integer.mk_mod state.ctx count two) one
  
  (** Check if criminals in a row are connected (no innocents between them) *)
  let connected_in_row state row =
    let positions = Position.positions_in_row row in
    let people = List.filter_map (fun pos ->
      Puzzle.find_by_position state.puzzle pos
    ) positions in
    if List.length people <= 1 then
      Z3.Boolean.mk_true state.ctx
    else begin
      let constraints = ref [] in
      (* Per game rules: "All always means there's at least one" *)
      let at_least_one = Z3.Boolean.mk_or state.ctx 
        (List.map (fun p -> is_criminal state p.name) people) in
      constraints := at_least_one :: !constraints;
      (* For each pair of criminals, all people between must also be criminal *)
      for i = 0 to List.length people - 1 do
        for j = i + 2 to List.length people - 1 do
          let pi = List.nth people i in
          let pj = List.nth people j in
          (* If pi and pj are both criminals, everyone between must be criminal *)
          let both_criminal = Z3.Boolean.mk_and state.ctx [
            is_criminal state pi.name;
            is_criminal state pj.name
          ] in
          let between = List.filter (fun k -> k > i && k < j) 
            (List.init (List.length people) Fun.id) in
          let all_between_criminal = 
            if between = [] then Z3.Boolean.mk_true state.ctx
            else Z3.Boolean.mk_and state.ctx (List.map (fun k ->
              is_criminal state (List.nth people k).name
            ) between)
          in
          constraints := Z3.Boolean.mk_implies state.ctx both_criminal all_between_criminal :: !constraints
        done
      done;
      Z3.Boolean.mk_and state.ctx !constraints
    end
  
  (** Check if criminals in a column are connected *)
  let connected_in_column state col =
    let positions = Position.positions_in_column col in
    let people = List.filter_map (fun pos ->
      Puzzle.find_by_position state.puzzle pos
    ) positions in
    if List.length people <= 1 then
      Z3.Boolean.mk_true state.ctx
    else begin
      let constraints = ref [] in
      (* Per game rules: "All always means there's at least one" *)
      (* So "All criminals in column X are connected" implies at least one criminal exists *)
      let at_least_one = Z3.Boolean.mk_or state.ctx 
        (List.map (fun p -> is_criminal state p.name) people) in
      constraints := at_least_one :: !constraints;
      (* Connectivity: for each pair of criminals, everyone between must be criminal *)
      for i = 0 to List.length people - 1 do
        for j = i + 2 to List.length people - 1 do
          let pi = List.nth people i in
          let pj = List.nth people j in
          let both_criminal = Z3.Boolean.mk_and state.ctx [
            is_criminal state pi.name;
            is_criminal state pj.name
          ] in
          let between = List.filter (fun k -> k > i && k < j) 
            (List.init (List.length people) Fun.id) in
          let all_between_criminal = 
            if between = [] then Z3.Boolean.mk_true state.ctx
            else Z3.Boolean.mk_and state.ctx (List.map (fun k ->
              is_criminal state (List.nth people k).name
            ) between)
          in
          constraints := Z3.Boolean.mk_implies state.ctx both_criminal all_between_criminal :: !constraints
        done
      done;
      Z3.Boolean.mk_and state.ctx !constraints
    end
  
  (** Encode a constraint expression into Z3 *)
  let rec encode_constraint state expr =
    match expr with
    | IsCriminal name -> is_criminal state name
    | IsInnocent name -> is_innocent state name
    
    | Count (region, target, comparison) ->
      let people = people_in_region state region in
      let count = count_in_people state target people in
      make_comparison state count comparison
    
    | PersonCount (name, target, comparison) ->
      let count = count_neighbors state name target in
      make_comparison state count comparison
    
    | Connected (Row r, Criminals) -> connected_in_row state r
    | Connected (Column c, Criminals) -> connected_in_column state c
    | Connected (_, _) -> Z3.Boolean.mk_true state.ctx (* TODO: general connectivity *)
    
    | SameStatus (name1, name2) ->
      Z3.Boolean.mk_eq state.ctx (get_var state name1) (get_var state name2)
    
    | DifferentStatus (name1, name2) ->
      Z3.Boolean.mk_xor state.ctx (get_var state name1) (get_var state name2)
    
    | Implies (ante, cons) ->
      Z3.Boolean.mk_implies state.ctx
        (encode_constraint state ante)
        (encode_constraint state cons)
    
    | And exprs ->
      Z3.Boolean.mk_and state.ctx (List.map (encode_constraint state) exprs)
    
    | Or exprs ->
      Z3.Boolean.mk_or state.ctx (List.map (encode_constraint state) exprs)
    
    | Not expr ->
      Z3.Boolean.mk_not state.ctx (encode_constraint state expr)
    
    | ExactlyN (names, target, n) ->
      let vars = List.map (fun name ->
        match target with
        | Criminals -> is_criminal state name
        | Innocents -> is_innocent state name
        | _ -> failwith "Invalid target for ExactlyN"
      ) names in
      let count = 
        if vars = [] then Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
        else
          let ones = List.map (fun v ->
            Z3.Boolean.mk_ite state.ctx v
              (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 1)
              (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0)
          ) vars in
          Z3.Arithmetic.mk_add state.ctx ones
      in
      Z3.Boolean.mk_eq state.ctx count (Z3.Arithmetic.Integer.mk_numeral_i state.ctx n)
    
    | MoreThan (region1, target1, region2, target2) ->
      let people1 = people_in_region state region1 in
      let people2 = people_in_region state region2 in
      let count1 = count_in_people state target1 people1 in
      let count2 = count_in_people state target2 people2 in
      Z3.Arithmetic.mk_gt state.ctx count1 count2
    
    | EqualCount (region1, target1, region2, target2) ->
      let people1 = people_in_region state region1 in
      let people2 = people_in_region state region2 in
      let count1 = count_in_people state target1 people1 in
      let count2 = count_in_people state target2 people2 in
      Z3.Boolean.mk_eq state.ctx count1 count2
    
    | TheMost (name, target) ->
      let my_count = count_neighbors state name target in
      let others = List.filter (fun p -> p.name <> name) state.puzzle.people in
      let constraints = List.map (fun other ->
        let other_count = count_neighbors state other.name target in
        Z3.Arithmetic.mk_gt state.ctx my_count other_count
      ) others in
      if constraints = [] then Z3.Boolean.mk_true state.ctx
      else Z3.Boolean.mk_and state.ctx constraints
    
    | TheLeast (name, target) ->
      let my_count = count_neighbors state name target in
      let others = List.filter (fun p -> p.name <> name) state.puzzle.people in
      let constraints = List.map (fun other ->
        let other_count = count_neighbors state other.name target in
        Z3.Arithmetic.mk_lt state.ctx my_count other_count
      ) others in
      if constraints = [] then Z3.Boolean.mk_true state.ctx
      else Z3.Boolean.mk_and state.ctx constraints
    
    | SameAs (name1, target, name2) ->
      let count1 = count_neighbors state name1 target in
      let count2 = count_neighbors state name2 target in
      Z3.Boolean.mk_eq state.ctx count1 count2
    
    | ShareNeighbors (name1, name2, target, comparison) ->
      (* Get common neighbors *)
      let common_neighbors = people_in_region state (CommonNeighbors (name1, name2)) in
      let count = count_in_people state target common_neighbors in
      make_comparison state count comparison
    
    | RegionNeighborCount (region, target, person_name, comparison) ->
      (* Count of [target] in [region] who are also neighbors of [person] *)
      let region_people = people_in_region state region in
      let neighbor_people = people_in_region state (Neighbors person_name) in
      (* Find intersection: people in both region and neighbors *)
      let intersection = List.filter (fun p ->
        List.exists (fun n -> n.name = p.name) neighbor_people
      ) region_people in
      let count = count_in_people state target intersection in
      make_comparison state count comparison
    
    | CountPeopleWithNeighbors (region, target, neighbor_comparison, total_comparison) ->
      (* Count how many people in [region] have [neighbor_comparison] [target] neighbors *)
      let region_people = people_in_region state region in
      (* For each person, create: if (neighbor_count satisfies comparison) then 1 else 0 *)
      let person_satisfies = List.map (fun p ->
        let neighbor_count = count_neighbors state p.name target in
        let satisfies = make_comparison state neighbor_count neighbor_comparison in
        Z3.Boolean.mk_ite state.ctx satisfies
          (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 1)
          (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0)
      ) region_people in
      let total = 
        if person_satisfies = [] then Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
        else Z3.Arithmetic.mk_add state.ctx person_satisfies
      in
      make_comparison state total total_comparison
    
    | CountPeopleWithDirectlyAdjacent (region, target, dir, comparison) ->
      (* Count how many people in [region] have a [target] directly [dir] of them *)
      let region_people = people_in_region state region in
      let person_satisfies = List.map (fun p ->
        (* Get the position directly adjacent in the given direction *)
        let adj_pos = match dir with
          | DirAbove -> Position.directly_above p.pos
          | DirBelow -> Position.directly_below p.pos
          | DirLeft -> Position.directly_left p.pos
          | DirRight -> Position.directly_right p.pos
        in
        match adj_pos with
        | None -> 
          (* No cell in that direction, so condition is false -> 0 *)
          Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
        | Some pos ->
          (* Find person at that position *)
          match Puzzle.find_by_position state.puzzle pos with
          | None ->
            Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
          | Some adj_person ->
            let is_target = match target with
              | Criminals -> is_criminal state adj_person.name
              | Innocents -> is_innocent state adj_person.name
              | _ -> failwith "Invalid target for directly adjacent"
            in
            Z3.Boolean.mk_ite state.ctx is_target
              (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 1)
              (Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0)
      ) region_people in
      let total = 
        if person_satisfies = [] then Z3.Arithmetic.Integer.mk_numeral_i state.ctx 0
        else Z3.Arithmetic.mk_add state.ctx person_satisfies
      in
      make_comparison state total comparison
    
    | SomeoneInRegion (region, target) ->
      let people = people_in_region state region in
      let vars = List.map (fun p ->
        match target with
        | Criminals -> is_criminal state p.name
        | Innocents -> is_innocent state p.name
        | _ -> failwith "Invalid target"
      ) people in
      if vars = [] then Z3.Boolean.mk_false state.ctx
      else Z3.Boolean.mk_or state.ctx vars
    
    | AllInRegion (region, status) ->
      let people = people_in_region state region in
      let vars = List.map (fun p ->
        match status with
        | Criminal -> is_criminal state p.name
        | Innocent -> is_innocent state p.name
        | Unknown -> Z3.Boolean.mk_true state.ctx
      ) people in
      if vars = [] then Z3.Boolean.mk_true state.ctx
      else Z3.Boolean.mk_and state.ctx vars
    
    | DirectlyAdjacent (_, _, _) -> Z3.Boolean.mk_true state.ctx (* TODO *)
    | InSameRow (_, _) -> Z3.Boolean.mk_true state.ctx (* TODO *)
    | InSameColumn (_, _) -> Z3.Boolean.mk_true state.ctx (* TODO *)
    
    | Unparsed _ -> Z3.Boolean.mk_true state.ctx (* Can't encode unparsed clues *)
  
  (** Add a constraint to the solver *)
  let add_constraint state expr =
    let z3_expr = encode_constraint state expr in
    Z3.Solver.add state.solver [z3_expr]
  
  (** Add known status constraints *)
  let add_known_status state =
    List.iter (fun person ->
      match person.known_status with
      | Unknown -> ()
      | Innocent -> add_constraint state (IsInnocent person.name)
      | Criminal -> add_constraint state (IsCriminal person.name)
    ) state.puzzle.people
  
  (** Check if the current constraints are satisfiable *)
  let check_sat state =
    match Z3.Solver.check state.solver [] with
    | Z3.Solver.SATISFIABLE -> true
    | _ -> false
  
  (** Get a model if satisfiable *)
  let get_model state =
    match Z3.Solver.check state.solver [] with
    | Z3.Solver.SATISFIABLE ->
      (match Z3.Solver.get_model state.solver with
       | Some model ->
         Some (List.map (fun person ->
           let var = get_var state person.name in
           let value = Z3.Model.eval model var true in
           let is_criminal = match value with
             | Some v -> Z3.Boolean.is_true v
             | None -> false
           in
           (person.name, if is_criminal then Criminal else Innocent)
         ) state.puzzle.people)
       | None -> None)
    | _ -> None
  
  (** Check if a person must be criminal *)
  let must_be_criminal state name =
    Z3.Solver.push state.solver;
    add_constraint state (IsInnocent name);
    let result = not (check_sat state) in
    Z3.Solver.pop state.solver 1;
    result
  
  (** Check if a person must be innocent *)
  let must_be_innocent state name =
    Z3.Solver.push state.solver;
    add_constraint state (IsCriminal name);
    let result = not (check_sat state) in
    Z3.Solver.pop state.solver 1;
    result
  
  (** Find all forced assignments (people who must be criminal or innocent) *)
  let find_forced_assignments state =
    List.filter_map (fun person ->
      if person.known_status <> Unknown then None
      else if must_be_criminal state person.name then 
        Some (person.name, Criminal)
      else if must_be_innocent state person.name then 
        Some (person.name, Innocent)
      else None
    ) state.puzzle.people
  
  (** Reset solver (for reloading constraints) *)
  let reset state =
    Z3.Solver.reset state.solver
end