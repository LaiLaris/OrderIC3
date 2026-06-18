type lustre_hint = {
  temporal_score : int;
  property_coi_score : int;
  call_group_score : int;
  arithmetic_score : int;
}

type relation_kind =
  | TemporalRelation
  | EquationRelation
  | PropertyRelation
  | CallGroupRelation
  | ArithmeticRelation

type value_hint_kind =
  | InitValue
  | TemporalSeedValue
  | TemporalValue
  | StepValue
  | CompareValue
  | GuardValue
  | CallActualValue

type literal_context_score = {
  variable_score : int;
  relation_score : int;
  direct_relation_score : int;
  intra_relation_score : int;
  transitive_relation_score : int;
  value_score : int;
  total_score : int;
}

let empty =
  {
    temporal_score = 0;
    property_coi_score = 0;
    call_group_score = 0;
    arithmetic_score = 0;
  }

let pp_print ppf
    { temporal_score; property_coi_score; call_group_score; arithmetic_score } =
  Format.fprintf ppf
    "{temporal=%d; property_coi=%d; call_group=%d; arithmetic=%d}"
    temporal_score property_coi_score call_group_score arithmetic_score

let importance
    { temporal_score; property_coi_score; call_group_score; arithmetic_score } =
  (3 * temporal_score) + (2 * property_coi_score) + call_group_score
  + (2 * arithmetic_score)

let pp_relation_kind ppf = function
  | TemporalRelation -> Format.fprintf ppf "temporal"
  | EquationRelation -> Format.fprintf ppf "equation"
  | PropertyRelation -> Format.fprintf ppf "property"
  | CallGroupRelation -> Format.fprintf ppf "call_group"
  | ArithmeticRelation -> Format.fprintf ppf "arithmetic"

let pp_value_hint_kind ppf = function
  | InitValue -> Format.fprintf ppf "init"
  | TemporalSeedValue -> Format.fprintf ppf "temporal_seed"
  | TemporalValue -> Format.fprintf ppf "temporal"
  | StepValue -> Format.fprintf ppf "step"
  | CompareValue -> Format.fprintf ppf "compare"
  | GuardValue -> Format.fprintf ppf "guard"
  | CallActualValue -> Format.fprintf ppf "call_actual"

module RelationKey = struct
  type t = relation_kind * StateVar.t * StateVar.t

  let compare (kind1, src1, dst1) (kind2, src2, dst2) =
    match compare kind1 kind2 with
    | 0 -> (
        match StateVar.compare_state_vars src1 src2 with
        | 0 -> StateVar.compare_state_vars dst1 dst2
        | c -> c)
    | c -> c
end

module RelationMap = Map.Make (RelationKey)

module ValueHintKey = struct
  type t = StateVar.t * value_hint_kind * string * string

  let compare (sv1, kind1, value1, source1) (sv2, kind2, value2, source2) =
    match StateVar.compare_state_vars sv1 sv2 with
    | 0 -> (
        match compare kind1 kind2 with
        | 0 -> (
            match String.compare value1 value2 with
            | 0 -> String.compare source1 source2
            | c -> c)
        | c -> c)
    | c -> c
end

module ValueHintMap = Map.Make (ValueHintKey)

let hint_map = ref StateVar.StateVarMap.empty
let string_hints = ref []
let relation_map : int RelationMap.t ref = ref RelationMap.empty
let value_hint_map : int ValueHintMap.t ref = ref ValueHintMap.empty

let string_of_state_var state_var =
  Format.asprintf "%a" StateVar.pp_print_state_var state_var

let string_contains s sub =
  let sub_len = String.length sub in
  let s_len = String.length s in
  if sub_len = 0 then true
  else if sub_len > s_len then false
  else
    let max_i = s_len - sub_len in
    let rec loop i =
      i <= max_i
      && (String.sub s i sub_len = sub || loop (i + 1))
    in
    loop 0

let set hints =
  hint_map := hints;
  string_hints :=
    StateVar.StateVarMap.bindings hints
    |> List.filter_map (fun (state_var, hint) ->
           let score = importance hint in
           if score = 0 then None
           else Some (string_of_state_var state_var, score))
    |> List.sort (fun (s1, score1) (s2, score2) ->
           match compare (String.length s2) (String.length s1) with
           | 0 -> compare score2 score1
           | c -> c)

let clear () = set StateVar.StateVarMap.empty

let canonical_pair state_var1 state_var2 =
  if StateVar.compare_state_vars state_var1 state_var2 <= 0 then
    (state_var1, state_var2)
  else (state_var2, state_var1)

let add_relation kind weight state_var1 state_var2 relations =
  if StateVar.equal_state_vars state_var1 state_var2 then relations
  else
    let src, dst = canonical_pair state_var1 state_var2 in
    let key = (kind, src, dst) in
    let old_weight =
      match RelationMap.find_opt key relations with
      | Some weight -> weight
      | None -> 0
    in
    RelationMap.add key (old_weight + weight) relations

let add_relation_clique kind weight state_vars relations =
  let state_vars = StateVar.StateVarSet.elements state_vars in
  let rec add_from_head relations = function
    | [] -> relations
    | state_var :: tl ->
        let relations =
          List.fold_left
            (fun relations state_var' ->
              add_relation kind weight state_var state_var' relations)
            relations tl
        in
        add_from_head relations tl
  in
  add_from_head relations state_vars

let set_relations relations = relation_map := relations

let relation_bindings () =
  RelationMap.bindings !relation_map
  |> List.map (fun ((kind, src, dst), weight) -> (src, dst, kind, weight))

let pp_print_relation ppf (src, dst, kind, weight) =
  Format.fprintf ppf "%a <-> %a [%a, weight=%d]"
    StateVar.pp_print_state_var src StateVar.pp_print_state_var dst
    pp_relation_kind kind weight

let add_value_hint state_var kind value source hints =
  let key = (state_var, kind, value, source) in
  let old_weight =
    match ValueHintMap.find_opt key hints with
    | Some weight -> weight
    | None -> 0
  in
  ValueHintMap.add key (old_weight + 1) hints

let set_value_hints hints = value_hint_map := hints

let value_hint_bindings () =
  ValueHintMap.bindings !value_hint_map
  |> List.map (fun ((state_var, kind, value, source), weight) ->
         (state_var, kind, value, source, weight))

let pp_print_value_hint ppf (state_var, kind, value, source, weight) =
  Format.fprintf ppf "%a -> %a:%s source=%s weight=%d"
    StateVar.pp_print_state_var state_var pp_value_hint_kind kind value source
    weight

let pp_print_literal_context_score ppf
    {
      variable_score;
      relation_score = _;
      direct_relation_score = _;
      intra_relation_score = _;
      transitive_relation_score = _;
      value_score;
      total_score;
    } =
  Format.fprintf ppf "var=%d, value=%d, total=%d" variable_score value_score
    total_score

let importance_of_state_var state_var =
  match StateVar.StateVarMap.find_opt state_var !hint_map with
  | Some hint -> importance hint
  | None ->
      let state_var_string = string_of_state_var state_var in
      let rec find_best = function
        | [] -> 0
        | (hint_string, score) :: tl ->
            if string_contains state_var_string hint_string then score
            else find_best tl
      in
      find_best !string_hints

let importance_of_term term =
  StateVar.StateVarSet.fold
    (fun state_var accum -> max accum (importance_of_state_var state_var))
    (Term.state_vars_of_term term) 0

let state_var_matches state_var hint_state_var =
  StateVar.equal_state_vars state_var hint_state_var
  ||
  let state_var_string = string_of_state_var state_var in
  let hint_string = string_of_state_var hint_state_var in
  string_contains state_var_string hint_string

let relation_weight_between_state_vars state_var1 state_var2 =
  if StateVar.equal_state_vars state_var1 state_var2 then 0
  else
    RelationMap.fold
      (fun (_, src, dst) weight accum ->
        if
          (state_var_matches state_var1 src && state_var_matches state_var2 dst)
          || (state_var_matches state_var1 dst
             && state_var_matches state_var2 src)
        then accum + weight
        else accum)
      !relation_map 0

let relation_score_between_sets vars1 vars2 =
  StateVar.StateVarSet.fold
    (fun state_var1 accum ->
      StateVar.StateVarSet.fold
        (fun state_var2 accum ->
          accum + relation_weight_between_state_vars state_var1 state_var2)
        vars2 accum)
    vars1 0

let relation_score_within_set vars =
  let vars = StateVar.StateVarSet.elements vars in
  let rec score_from_head accum = function
    | [] -> accum
    | state_var :: tl ->
        let accum =
          List.fold_left
            (fun accum state_var' ->
              accum + relation_weight_between_state_vars state_var state_var')
            accum tl
        in
        score_from_head accum tl
  in
  score_from_head 0 vars

let direct_neighbors state_var =
  RelationMap.fold
    (fun (_, src, dst) weight neighbors ->
      if state_var_matches state_var src then (dst, weight) :: neighbors
      else if state_var_matches state_var dst then (src, weight) :: neighbors
      else neighbors)
    !relation_map []

let transitive_relation_weight_between_state_vars ?(max_depth = 3) state_var1
    state_var2 =
  if relation_weight_between_state_vars state_var1 state_var2 > 0 then 0
  else
    let rec search visited depth frontier =
      if depth > max_depth then 0
      else
        let best, next_frontier, visited =
          List.fold_left
            (fun (best, next_frontier, visited) state_var ->
              direct_neighbors state_var
              |> List.fold_left
                   (fun (best, next_frontier, visited) (neighbor, weight) ->
                     if
                       StateVar.StateVarSet.exists
                         (fun visited_state_var ->
                           state_var_matches neighbor visited_state_var
                           || state_var_matches visited_state_var neighbor)
                         visited
                     then (best, next_frontier, visited)
                     else
                       let visited =
                         StateVar.StateVarSet.add neighbor visited
                       in
                       if state_var_matches neighbor state_var2 then
                         let discounted = max 1 (weight / depth) in
                         (max best discounted, next_frontier, visited)
                       else (best, neighbor :: next_frontier, visited))
                   (best, next_frontier, visited))
            (0, [], visited) frontier
        in
        if best > 0 then best else search visited (depth + 1) next_frontier
    in
    search
      (StateVar.StateVarSet.singleton state_var1)
      1 [ state_var1 ]

let transitive_relation_score_between_sets vars1 vars2 =
  StateVar.StateVarSet.fold
    (fun state_var1 accum ->
      StateVar.StateVarSet.fold
        (fun state_var2 accum ->
          accum
          + transitive_relation_weight_between_state_vars state_var1 state_var2)
        vars2 accum)
    vars1 0

let transitive_relation_score_within_set vars =
  let vars = StateVar.StateVarSet.elements vars in
  let rec score_from_head accum = function
    | [] -> accum
    | state_var :: tl ->
        let accum =
          List.fold_left
            (fun accum state_var' ->
              accum
              + transitive_relation_weight_between_state_vars state_var
                  state_var')
            accum tl
        in
        score_from_head accum tl
  in
  score_from_head 0 vars

let vars_of_terms terms =
  List.fold_left
    (fun accum term ->
      StateVar.StateVarSet.union accum (Term.state_vars_of_term term))
    StateVar.StateVarSet.empty terms

let is_value_token_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '.'
  | '@'
  | '-' ->
      true
  | _ -> false

let strip_compare_prefix value =
  let len = String.length value in
  if len >= 2 && (String.sub value 0 2 = "<=" || String.sub value 0 2 = ">=")
  then String.sub value 2 (len - 2)
  else if
    len >= 1
    &&
    match value.[0] with
    | '=' | '<' | '>' -> true
    | _ -> false
  then String.sub value 1 (len - 1)
  else value

let is_integer_token token =
  let len = String.length token in
  if len = 0 then false
  else
    let start = if token.[0] = '-' then 1 else 0 in
    start < len
    &&
    let rec loop i =
      i = len
      ||
      match token.[i] with
      | '0' .. '9' -> loop (i + 1)
      | _ -> false
    in
    loop start

let normalized_arithmetic_contains_int term_string token =
  string_contains term_string (" " ^ token ^ ") 0")
  || string_contains term_string (" " ^ token ^ ")) 0")
  || string_contains term_string ("(- " ^ token ^ ")) 0")
  || string_contains term_string ("(- " ^ token ^ "))) 0")

let token_contains string token =
  let token_len = String.length token in
  let string_len = String.length string in
  if token_len = 0 || token_len > string_len then false
  else
    let max_i = string_len - token_len in
    let rec loop i =
      if i > max_i then false
      else if String.sub string i token_len = token then
        let before_ok = i = 0 || not (is_value_token_char string.[i - 1]) in
        let after_i = i + token_len in
        let after_ok =
          after_i = string_len || not (is_value_token_char string.[after_i])
        in
        (before_ok && after_ok) || loop (i + 1)
      else loop (i + 1)
    in
    loop 0

let value_hint_matches_term term_string kind value =
  match kind with
  | InitValue | StepValue | CompareValue | GuardValue ->
      let value = strip_compare_prefix value in
      if is_integer_token value then
        normalized_arithmetic_contains_int term_string value
      else token_contains term_string value || string_contains term_string value
  | TemporalSeedValue | TemporalValue | CallActualValue -> false

let value_score_of_term term =
  let vars = Term.state_vars_of_term term in
  let term_string = Format.asprintf "%a" Term.pp_print_term term in
  ValueHintMap.fold
    (fun (state_var, kind, value, _) weight accum ->
      if
        StateVar.StateVarSet.exists
          (fun term_state_var -> state_var_matches term_state_var state_var)
          vars
        && value_hint_matches_term term_string kind value
      then accum + weight
      else accum)
    !value_hint_map 0

let context_score_of_term_in_clause term clause_terms =
  ignore clause_terms;
  let variable_score = importance_of_term term in
  let direct_relation_score = 0 in
  let intra_relation_score = 0 in
  let transitive_relation_score = 0 in
  let relation_score = 0 in
  let value_score = value_score_of_term term in
  let total_score = variable_score + relation_score + value_score in
  {
    variable_score;
    relation_score;
    direct_relation_score;
    intra_relation_score;
    transitive_relation_score;
    value_score;
    total_score;
  }
