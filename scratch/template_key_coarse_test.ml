let pp label = function
  | None -> Format.printf "%s: <none>@." label
  | Some s -> Format.printf "%s: %s@." label s

let pp_term label term = Format.printf "%s: %a@." label Term.pp_print_term term

let mk_state_var name scope ty = StateVar.mk_state_var name scope ty
let at0 sv = Var.mk_state_var_instance sv Numeral.zero |> Term.mk_var

let () =
  let call24 = mk_state_var "call_24" [ "top"; "res" ] Type.t_bool in
  let call21_0 = mk_state_var "call_21_0" [ "top"; "res"; "2" ] Type.t_int in
  let call21_2 = mk_state_var "call_21_2" [ "top"; "res"; "2" ] Type.t_int in
  let call21_3 = mk_state_var "call_21_3" [ "top"; "res"; "2" ] Type.t_int in
  let call25 = mk_state_var "call_25" [ "top"; "res" ] Type.t_int in
  let lit1 = Term.mk_not (at0 call24) in
  let lit2 = Term.mk_not (Term.mk_gt [ at0 call21_0; Term.mk_num Numeral.zero ]) in
  let lit3 =
    Term.mk_not
      (Term.mk_gt
         [
           Term.mk_plus
             [
               Term.mk_times [ Term.mk_num (Numeral.of_int (-1)); at0 call25 ];
               at0 call21_2;
               at0 call21_3;
               Term.mk_num (Numeral.of_int 5);
             ];
           Term.mk_num Numeral.zero;
         ])
  in
  let clause = Term.mk_or [ lit1; lit2; lit3 ] in
  pp_term "clause term" clause;
  pp "clause" (Clause.template_key_coarse clause)
