(* InvParser.ml
   Lightweight SMT-LIB S-expression -> Term.t parser for loading pre-inferred
   constraints (not verified invariants) into IC3 frames.
   Supports: and/or/not/=>/=/+/*/-/div/mod/integers/true/false/state vars/ite
*)

open Term
open Type

let strip_outer_braces s =
  let s = String.trim s in
  if String.length s >= 2 && s.[0] = '{' && s.[String.length s - 1] = '}' then
    String.sub s 1 (String.length s - 2) |> String.trim
  else s

let tokenize s =
  let s = strip_outer_braces s in
  let buf = Buffer.create 64 in
  let tokens = ref [] in
  let push () =
    if Buffer.length buf > 0 then (
      tokens := Buffer.contents buf :: !tokens;
      Buffer.clear buf)
  in
  String.iter
    (fun c ->
      match c with
      | '(' | ')' ->
          push ();
          tokens := String.make 1 c :: !tokens
      | ' ' | '\n' | '\t' | '\r' -> push ()
      | _ -> Buffer.add_char buf c)
    s;
  push ();
  List.rev !tokens

type sexp = Atom of string | List of sexp list

exception Parse_error of string

let rec parse_sexp toks =
  match toks with
  | [] -> raise (Parse_error "Unexpected EOF")
  | "(" :: rest ->
      let rec parse_list acc ts =
        match ts with
        | [] -> raise (Parse_error "Unexpected EOF in list")
        | ")" :: rest' -> (List (List.rev acc), rest')
        | "(" :: _ ->
            let sub, rest1 = parse_sexp ts in
            parse_list (sub :: acc) rest1
        | atom :: rest' -> parse_list (Atom atom :: acc) rest'
      in
      parse_list [] rest
  | ")" :: _ -> raise (Parse_error "Unexpected )")
  | atom :: rest -> (Atom atom, rest)

let parse_all toks =
  let rec loop acc ts =
    match ts with
    | [] -> List.rev acc
    | _ ->
        let sx, rest = parse_sexp ts in
        loop (sx :: acc) rest
  in
  loop [] toks

let is_int_literal s =
  let len = String.length s in
  if len = 0 then false
  else
    let start = if s.[0] = '-' then 1 else 0 in
    if start = len then false
    else
      let ok = ref true in
      for i = start to len - 1 do
        if s.[i] < '0' || s.[i] > '9' then ok := false
      done;
      !ok

(* Split "foo@N" into ("foo", N), defaulting offset to 0 *)
let split_at_offset s =
  match String.rindex_opt s '@' with
  | Some i ->
      let base = String.sub s 0 i in
      let off_s = String.sub s (i + 1) (String.length s - i - 1) in
      let off = try int_of_string off_s with Failure _ -> 0 in
      (base, off)
  | None -> (s, 0)

(* Resolve a name to a Term: try all (scope, name) splits, fall back to UF symbol *)
let resolve_atom v ty =
  let v_base, off = split_at_offset v in
  (* Try every split point: scope = first k components, name = rest joined *)
  let parts = String.split_on_char '.' v_base in
  let n = List.length parts in
  let parts_arr = Array.of_list parts in
  let rec try_split k =
    if k >= n then None
    else
      let scope = Array.to_list (Array.sub parts_arr 0 k) in
      let name = String.concat "." (Array.to_list (Array.sub parts_arr k (n - k))) in
      match StateVar.state_var_of_string (name, scope) with
      | sv -> Some sv
      | exception Not_found -> try_split (k + 1)
  in
  match try_split 1 with
  | Some sv ->
      ignore ty;
      mk_var (Var.mk_state_var_instance sv (Numeral.of_int off))
  | None ->
      let u = UfSymbol.mk_uf_symbol v [] ty in
      mk_uf u []

let rec term_of_sexp ?(expected_ty = None) sexp =
  match sexp with
  | Atom "true" -> mk_true ()
  | Atom "false" -> mk_false ()
  | Atom s when is_int_literal s -> mk_num_of_int (int_of_string s)
  | Atom v -> (
      let ty = match expected_ty with Some t -> t | None -> mk_bool () in
      resolve_atom v ty)
  | List (Atom "and" :: args) ->
      mk_and (List.map (term_of_sexp ~expected_ty:(Some (mk_bool ()))) args)
  | List (Atom "or" :: args) ->
      mk_or (List.map (term_of_sexp ~expected_ty:(Some (mk_bool ()))) args)
  | List [ Atom "not"; a ] ->
      mk_not (term_of_sexp ~expected_ty:(Some (mk_bool ())) a)
  | List [ Atom "=>"; a; b ] ->
      mk_implies
        [
          term_of_sexp ~expected_ty:(Some (mk_bool ())) a;
          term_of_sexp ~expected_ty:(Some (mk_bool ())) b;
        ]
  | List (Atom "=" :: [ a; b ]) ->
      let ta = term_of_sexp a in
      let tb = term_of_sexp b in
      mk_eq [ ta; tb ]
  | List (Atom "=" :: args) -> mk_eq (List.map term_of_sexp args)
  | List (Atom "<=" :: args) ->
      mk_leq (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom "<" :: args) ->
      mk_lt (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom ">=" :: args) ->
      mk_geq (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom ">" :: args) ->
      mk_gt (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom "+" :: args) ->
      mk_plus (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom "*" :: args) ->
      mk_times (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom "-" :: [ a ]) ->
      mk_minus [ term_of_sexp ~expected_ty:(Some (mk_int ())) a ]
  | List (Atom "-" :: args) ->
      mk_minus (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom "/" :: args) ->
      mk_div (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List (Atom "div" :: args) ->
      mk_intdiv (List.map (term_of_sexp ~expected_ty:(Some (mk_int ()))) args)
  | List [ Atom "mod"; a; b ] ->
      mk_mod
        (term_of_sexp ~expected_ty:(Some (mk_int ())) a)
        (term_of_sexp ~expected_ty:(Some (mk_int ())) b)
  | List [ Atom "ite"; c; t; e ] ->
      mk_ite
        (term_of_sexp ~expected_ty:(Some (mk_bool ())) c)
        (term_of_sexp t) (term_of_sexp e)
  | sexp ->
      let repr = match sexp with Atom s -> s | List _ -> "<complex>" in
      raise (Parse_error ("Unsupported S-expression construct: " ^ repr))

let load_inv_from_string s =
  let toks = tokenize s in
  let sexps = parse_all toks in
  match sexps with
  | [] -> raise (Parse_error "Empty input")
  | [ sx ] -> term_of_sexp sx
  | sxs -> mk_and (List.map term_of_sexp sxs)

let load_inv filename =
  let ic = open_in filename in
  let buf = really_input_string ic (in_channel_length ic) in
  close_in ic;
  load_inv_from_string buf
