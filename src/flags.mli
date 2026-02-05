(* This file is part of the Kind 2 model checker.

   Copyright (c) 2015 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License.

*)

(** Parsing of command line arguments

    {1 Workflow}

    Flags are separated based on the technique(s) they impact. {e Global flags}
    are the ones that don't impact any technique, or impact all of them. Log
    flags, help flags, timeout flags... are global flags.

    {e NB:} when adding a boolean flag, make sure to parse its value with the
    [bool_of_string] function.

    {2 Adding a new (non-global) flag to an existing module}

    Adding a new flag impacts three pieces of code. The first is the body of the
    module you're adding the flag to. Generally speaking, adding a flag looks
    like

    {[
      (* Default value of the flag. *)
      let my_flag_default = ...
      (* Reference storing the value of the flag. *)
      let my_flag = ref my_flag_default
      (* Add flag specification to module specs. *)
      let _ = add_spec (
        (* The actual flag. *)
        "--my_flag",
        (* What to do with the value given to the flag, see other flags. *)
        ...,
        (* Flag description. *)
        fun fmt ->
          Format.fprintf fmt
            "@[<v>Description of my flag.@ Default: %a@]"
            pp_print_default_value_of_my_flag my_flag_default
      )
      (* Flag value accessor. *)
      let my_flag () = !my_flag
    ]}

    At this point your flag is integrated in the Kind 2 flags.

    To make it available to the rest of Kind 2, you need to modify the signature
    of the module you added the flag to
    - in this file, where the module is declared, and
    - in [flags.mli].

    The update to the signature is typically

    {[
      val my_flag : unit -> type_of_my_flag
    ]}

    {b Adding a new flag module}

    The template to add a new module is

    {[
      module MyModule : sig
        include FlagModule
      end = struct
        (* Identifier of the module. No space or special characters. *)
        let id = "..."

        (* Short description of the module. *)
        let desc = "..."

        (* Explanation of the module. *)
        let fmt_explain fmt = Format.fprintf fmt "@[<v>...@]"

        (* All the flag specification of this module. *)
        let all_specs = ref []
        let add_specs specs = all_specs := !all_specs @ specs
        let add_spec spec = add_specs [ spec ]

        (* Returns all the flag specification of this module. *)
        let all_specs () = !all_specs
      end
    ]}

    Don't forget to update `flags.mli`:

    {[
      module MyModule : sig
        include FlagModule
      end
    ]}

    You then need to add your module to the [module_map], the association map
    between module identifiers and modules. Make sure the identifier for your
    module is not used yet.

    You can now add modules following the instructions in the previous section.

    @author Christoph Sticksel, Adrien Champion *)

exception Error
exception SolverNotFound
exception UnsupportedSolver

(** {1 Accessors for flags} *)

(** {2 Meta flags} *)

(** {2 Generic flags} *)

val input_file : unit -> string
(** Input file *)

val all_input_files : unit -> string list
(** All lustre files in the cone of influence of the input file. *)

val clear_input_files : unit -> unit
(** Clears the lustre files in the cone of influence of the input file. *)

val add_input_file : string -> bool
(** Adds a lustre file in the cone of influence of the input file.

    Returns false if the cone of influence already contains the file *)

val lus_main : unit -> string option
(** Main node in Lustre file *)

val lus_main_type : unit -> string option
(** Main type alias in Lustre file *)

type input_format = [ `Lustre | `Horn | `Native ]
(** Format of input file *)

val input_format : unit -> input_format

val output_dir : unit -> string
(** Output directory for the files Kind 2 generates. *)

val include_dirs : unit -> string list

type real_precision = [ `Rational | `Float ]

val real_precision : unit -> real_precision

val log_invs : unit -> bool
(** Minimizes and logs invariants as contracts. *)

val print_invs : unit -> bool
(** Prints invariants **)

val print_cex : unit -> bool
(** Print counterexamples **)

val print_witness : unit -> bool
(** Print witnesses **)

val dump_cex : unit -> bool
(** Dump counterexample to disproven invariant property to a file **)

val set_dump_cex : bool -> unit
(** Set whether dumping counterexamples *)

val dump_witness : unit -> bool
(** Dump witness of proven reachability property to a file **)

val debug : unit -> string list
(** Debug sections to enable *)

val debug_log : unit -> string option
(** Logfile for debug output *)

type exit_code_convention = [ `RESULTS_AND_ERRORS | `ONLY_ERRORS ]

val exit_code_mode : unit -> exit_code_convention

val log_level : unit -> Lib.log_level
(** Verbosity level *)

val log_format_xml : unit -> bool
(** Output in XML format *)

val log_format_json : unit -> bool
(** Output in XML format *)

val timeout_wall : unit -> float
(** Wallclock timeout. *)

val timeout_analysis : unit -> float
(** Per-run wallclock timeout. *)

type enable = Lib.kind_module list
(** The Kind modules enabled is a list of [kind_module]s. *)

val only_parse : unit -> bool
(** Only parse the Lustre program. No analysis is performed. *)

val old_frontend : unit -> bool
(** Use the old Lustre front-end. *)

val enabled : unit -> enable
(** The modules enabled. *)

val invgen_enabled : unit -> enable
(** Returns the invariant generation techniques currently enabled. *)

val disable : Lib.kind_module -> unit
(** Manually disables a module. *)

val modular : unit -> bool
(** Modular analysis. *)

val slice_nodes : unit -> bool
(** Node slicing *)

val check_reach : unit -> bool
(** Check reachability properties *)

val check_nonvacuity : unit -> bool
(** Check non-vacuity of contract modes and conditional properties *)

val check_subproperties : unit -> bool
(** Check properties of subnodes *)

val lus_strict : unit -> bool
(** Strict Lustre mode. *)

val lus_compile : unit -> bool
(** Activates compilation to Rust. *)

val lus_push_pre : unit -> bool
(** Activates transformation that pushes pre expressions *)

val color : unit -> bool
(** Colored output. *)

(** {2 SMT solver flags} *)
module Smt : sig
  type logic = [ `None | `detect | `Logic of string ]
  (** Logic sendable to the SMT solver. *)

  val logic : unit -> logic
  (** Logic to send to the SMT solver *)

  type solver =
    [ `Bitwuzla_SMTLIB
    | `cvc5_SMTLIB
    | `MathSAT_SMTLIB
    | `OpenSMT_SMTLIB
    | `SMTInterpol_SMTLIB
    | `Yices2_SMTLIB
    | `Yices_native
    | `Z3_SMTLIB
    | `detect ]
  (** Legal SMT solvers. *)

  val set_solver : solver -> unit
  (** Set SMT solver and executable *)

  val solver : unit -> solver
  (** Which SMT solver to use. *)

  type qe_solver = [ `cvc5_SMTLIB | `Z3_SMTLIB | `detect ]

  val set_qe_solver : qe_solver -> unit
  (** Set SMT solver for QE *)

  val qe_solver : unit -> qe_solver
  (** Which SMT solver for QE to use. *)

  type itp_solver =
    [ `cvc5_QE
    | `MathSAT_SMTLIB
    | `OpenSMT_SMTLIB
    | `SMTInterpol_SMTLIB
    | `Z3_QE
    | `detect ]

  val set_itp_solver : itp_solver -> unit
  (** Set SMT solver for interpolation *)

  val itp_solver : unit -> itp_solver
  (** Which SMT solver for interpolation to use. *)

  val get_itp_solver : unit -> solver

  val check_sat_assume : unit -> bool
  (** Use check-sat with assumptions, or simulate with push/pop *)

  val set_check_sat_assume : bool -> unit

  val short_names : unit -> bool
  (** Send short names to SMT solver *)

  val set_short_names : bool -> unit
  (** Change sending of short names to SMT solver *)

  val bitwuzla_bin : unit -> string
  (** Executable of Bitwuzla solver *)

  val cvc5_bin : unit -> string
  (** Executable of cvc5 solver *)

  val mathsat_bin : unit -> string
  (** Executable of MathSAT solver *)

  val opensmt_bin : unit -> string
  (** Executable of OpenSMT solver *)

  val smtinterpol_jar : unit -> string
  (** JAR of SMTInterpol solver *)

  val yices2smt2_bin : unit -> string
  (** Executable of Yices2 SMT2 solver *)

  val yices2_smt2models : unit -> bool
  (** Yices 2 binary supports models in SMT2 format **)

  val set_yices2_smt2models : bool -> unit

  val yices_bin : unit -> string
  (** Executable of Yices solver *)

  val z3_bin : unit -> string
  (** Executable of Z3 solver *)

  val z3_qe_light : unit -> bool
  (** Whether Z3 qe-light strategy is used in addition to qe *)

  val set_z3_qe_light : bool -> unit
  (** Specify if Z3 qe-light strategy should be used in addition to qe *)

  val set_trace : bool -> unit
  (** Forces SMT traces. *)

  val trace : unit -> bool
  (** Write all SMT commands to files *)

  val trace_dir : unit -> string
  (** Path to the smt trace directory. *)

  val trace_subdir : unit -> string
  (** Name of subdirectory within the smt trace directory. *)

  val set_trace_subdir : string -> unit
end

(** {2 BMC / k-induction flags} *)
module BmcKind : sig
  val max : unit -> int
  (** Maximal number of iterations in BMC. *)

  val check_unroll : unit -> bool
  (** Check that the unrolling of the system alone is satisfiable. *)

  val ind_print_cex : unit -> bool
  (** Print counterexamples to induction. *)

  val compress : unit -> bool
  (** Compress inductive counterexample. *)

  val set_compress : bool -> unit

  val compress_equal : unit -> bool
  (** Compress inductive counterexample when states are equal modulo inputs. *)

  val compress_same_succ : unit -> bool
  (** Compress inductive counterexample when states have same successors. *)

  val compress_same_pred : unit -> bool
  (** Compress inductive counterexample when states have same predecessors. *)

  val lazy_invariants : unit -> bool
  (** Lazy assertion of invariants. *)
end

(** {2 IC3QE flags} *)
module IC3QE : sig
  val cex_inv_map : unit -> bool

  val check_inductive : unit -> bool
  (** Check inductiveness of blocking clauses. *)

  val print_to_file : unit -> string option
  (** File for inductive blocking clauses. *)

  val reuse_tree_pdf : unit -> string option
  (** PDF file for reuse tree. *)

  val inductively_generalize : unit -> int
  (** Tighten blocking clauses to an unsatisfiable core. *)

  val block_in_future : unit -> bool
  (** Block counterexample in future frames. *)

  val block_in_future_first : unit -> bool
  (** Block counterexample in future frames first before returning to frame. *)

  val fwd_prop_non_gen : unit -> bool
  (** Also propagate clauses before generalization. *)

  val fwd_prop_ind_gen : unit -> bool
  (** Inductively generalize all clauses after forward propagation. *)

  val fwd_prop_subsume : unit -> bool
  (** Subsumption in forward propagation. *)

  val ltr_sort : unit -> bool
  (** Enable LTR-based heuristic literal ordering. *)

  val use_invgen : unit -> bool
  (** Use invariants from invariant generators. *)

  type abstr = [ `None | `IA ]
  (** DEPRECATED: Legal abstraction mechanisms for in IC3. *)

  val abstr : unit -> abstr
  (** DEPRECATED: Abstraction mechanism IC3 should use. *)
end

(** {2 IC3IA flags} *)
module IC3IA : sig
  val max_processes : unit -> int
end

(** {2 QE flags} *)
module QE : sig
  type qe_method = [ `Precise | `Impl | `Impl2 | `Cooper ]
  (** Methods available for quantifier elimination. *)

  val qe_method : unit -> qe_method
  (** The QE method used. *)

  val set_qe_method : qe_method -> unit
  (** Set [qe_method]. *)

  type extract = [ `First | `Vars ]
  (** Supported heuristics for extraction of implicants. *)

  val extract : unit -> extract
  (** Heuristic for extraction of implicants. *)

  val ae_val_use_ctx : unit -> bool
  (** Use context (premises) in ae_val procedure *)

  val order_var_by_elim : unit -> bool
  (** Order variables in polynomials by order of elimination **)

  val general_lbound : unit -> bool
  (** Choose lower bounds containing variables **)
end

(** {2 Contracts flags} *)
module Contracts : sig
  val compositional : unit -> bool
  (** Compositional analysis. *)

  val translate_contracts : unit -> string option
  (** Translate contracts. *)

  val check_modes : unit -> bool
  (** Check modes. *)

  val check_environment : unit -> bool
  (** Check realizability of node environments. *)

  val check_implem : unit -> bool
  (** Check modes. *)

  val contract_gen : unit -> bool
  (** Contract generation. *)

  val contract_gen_depth : unit -> int
  (** Contract generation: max depth. *)

  val assumption_gen : unit -> bool
  (** Assumption generation. *)

  (* val assump_include_outputs : unit -> bool *)

  val two_state_assumption : unit -> bool

  val assumption_gen_iter : unit -> int
  (** Assumption generation: generalization iterations *)

  val refinement : unit -> bool
  (** Activate refinement. *)

  val enforce_func_congruence : unit -> bool
  (** Enforce functional congruence on abstract functions *)

  val print_deadlock : unit -> bool
  (** Print deadlocking trace and a conflict *)

  val dump_deadlock : unit -> bool
  (** Dump deadlocking trace to a file **)

  val check_contract_is_sat : unit -> bool
  (** Check whether a unrealizable contract is satisfiable *)

  (* Print set of viable states as a lustre-like constraint *)
  val print_viable_states : unit -> bool
end

(** {2 Certificates and Proofs} *)
module Certif : sig
  type mink = [ `No | `Fwd | `Bwd | `Dicho | `FrontierDicho | `Auto ]
  (** Minimization stragegy for k *)

  type mininvs = [ `Easy | `Medium | `MediumOnly | `Hard | `HardOnly ]
  (** Minimization stragegy for invariants *)

  val certif : unit -> bool
  (** Certification only. *)

  val proof : unit -> bool
  (** Proof production. *)

  val smaller_holes : unit -> bool
  (** Reduce the size of trusted holes. *)

  val flatten_proof : unit -> bool
  (** Breakdown proof into smaller steps. *)

  val log_trust : unit -> bool
  (** Log trusted parts of proofs. *)

  val mink : unit -> mink
  (** Minimization stragegy for k *)

  val mininvs : unit -> mininvs
  (** Minimization stragegy for invariants *)

  val jkind_bin : unit -> string
  (** Binary for JKind *)

  val only_user_candidates : unit -> bool
end

(** {2 Inductive Validity Cores} *)
module IVC : sig
  type minimize_mode = [ `DO_NOT_MINIMIZE | `VALID_LUSTRE | `CONCISE ]

  type ivc_element =
    [ `NODE_CALL | `CONTRACT_ITEM | `EQUATION | `ASSERTION | `ANNOTATIONS ]

  val compute_ivc : unit -> bool
  (** Enable computation of Inductive Validity Cores *)

  val ivc_all : unit -> bool

  val ivc_category : unit -> ivc_element list
  (** Specify on which elements we want to minimize *)

  val print_ivc : unit -> bool
  (** Print the model elements of the computed IVC *)

  val print_ivc_compl : unit -> bool
  (** Print the model elements NOT in the computed IVC *)

  val ivc_approximate : unit -> bool
  (** If true, compute an approximation of a MIVC *)

  val ivc_must_set : unit -> bool
  (** If true, compute the MUST set first and compute the IVCs starting from it
  *)

  val ivc_smallest_first : unit -> bool
  (** If true, compute a smallest IVC first *)

  val ivc_only_main_node : unit -> bool
  (** If true, compute IVC over elements of the main node only *)

  val ivc_precomputed_mcs : unit -> int
  (** Parameter k of the UMIVC algorithm. Correspond to the parameter 'k' of the
      implementation UMIVC. In particular, the value 0 implements the MARCO
      algorithm, and the value -1 (infinity) implements the CAMUS algorithm. *)

  val ivc_per_property : unit -> bool
  (** If true, IVCs will be computed for each properties separately *)

  val minimize_program : unit -> minimize_mode
  (** Generate a minimize lustre program *)

  val minimized_program_dir : unit -> string
  (** The directory where minimized programs should be saved *)

  val ivc_uc_timeout : unit -> int
  (** Timeout for unsat core computation *)

  val ivc_disable_must_opt : unit -> bool
  (** DEBUG flag: disable the must-set optimisation (ignored if ivc_must_set is
      true) *)
end

(** {2 Minimal Cut Sets} *)
module MCS : sig
  type mcs_element =
    [ `NODE_CALL | `CONTRACT_ITEM | `EQUATION | `ASSERTION | `ANNOTATIONS ]

  val compute_mcs : unit -> bool
  (** Enable computation of Minimal Cut Sets *)

  val mcs_all : unit -> bool
  (** Specify whether all the Minimal Cut Sets must be computed or just one *)

  val mcs_approximate : unit -> bool
  (** If true, compute an approximation of a MCS *)

  val mcs_category : unit -> mcs_element list
  (** Specify on which elements we want to minimize *)

  val mcs_max_cardinality : unit -> int
  (** Only search for MCSs of cardinality lower or equal to this parameter *)

  val print_mcs : unit -> bool
  (** Print the model elements of the computed MCS *)

  val print_mcs_compl : unit -> bool
  (** Print the model elements NOT in the computed MCS *)

  val print_mcs_legacy : unit -> bool
  (** Print the MCS using the legacy format *)

  val print_mcs_cex : unit -> bool
  (** Print the counterexample found for each MCS *)

  val mcs_only_main_node : unit -> bool
  (** If true, compute MCS over elements of the main node only *)

  val mcs_per_property : unit -> bool
  (** If true, MCSs will be computed for each properties separately *)
end

(** {2 Arrays flags} *)
module Arrays : sig
  val smt : unit -> bool
  (** Use builtin theory of arrays in SMT solver *)

  val set_smt : bool -> unit

  val inline : unit -> bool
  (** Inline arrays with fixed bounds *)

  val recdef : unit -> bool
  (** Define recursive functions for arrays *)

  val var_size : unit -> bool
  (** Allow non constant array sizes *)
end

(** {2 Testgen flags} *)

module Testgen : sig
  val active : unit -> bool
  (** Activates test generation. *)

  val graph_only : unit -> bool
  (** Only generate graph of reachable modes, do not log testcases. *)

  val len : unit -> int
  (** Length of the test case generated. *)
end

(** {2 Invgen flags} *)
module Invgen : sig
  val prune_trivial : unit -> bool
  (** InvGen will remove trivial invariants, i.e. invariants implied by the
      transition relation. *)

  val set_max_depth : int option -> unit
  (** Sets the max depth for invariant generation. *)

  val max_depth : unit -> int option
  (** Gets the max depth for invariant generation. *)

  val max_succ : unit -> int
  (** Number of unrollings invariant generation should perform between switching
      to a different systems. *)

  val lift_candidates : unit -> bool
  (** InvGen will lift candidate terms from subsystems. **)

  val top_only : unit -> bool
  (** InvGen will generate invariants only for top level. **)

  val all_out : unit -> bool
  (** Forces invgen to consider a huge number of candidates. *)

  val mine_trans : unit -> bool
  (** InvGen will look for candidate terms in the transition predicate. *)

  val two_state : unit -> bool
  (** InvGen will run in two state mode. *)

  val bool_eq_only : unit -> bool
  (** Forces bool invgen to look for equalities only. *)

  val arith_eq_only : unit -> bool
  (** Forces arith invgen to look for equalities only. *)

  val renice : unit -> int
  (** Renice invariant generation process. *)
end

(** {2 C2I flags} *)
module C2I : sig
  val dnf_size : unit -> int
  (** Number of disjuncts in the DNF constructed by C2I. *)

  val int_cube_size : unit -> int
  (** Number of int cubes in the DNF constructed by C2I. *)

  val real_cube_size : unit -> int
  (** Number of real cubes in the DNF constructed by C2I. *)

  val modes : unit -> bool
  (** Whether mode sub candidate is activated in c2i. *)
end

(** {2 Interpreter flags} *)
module Interpreter : sig
  val input_file : unit -> string
  (** Read input from file. *)

  val steps : unit -> int
  (** Run number of steps, override the number of steps given in the input file.
  *)
end

(** {2 LSP flags} *)
module Lsp : sig
  val lsp : unit -> bool
  (** Provide AST info for language-servers. *)

  val fake_filepath : unit -> string
  (** Provide fake filepath for error messages. *)
end

(** {1 Convenience functions} *)

val subdir_for : string list -> string
(** Path to subdirectory for a system (in the output directory). *)

(** {1 Parsing of the command line} *)

val parse_argv : unit -> unit
(** Parse the command line *)

(*
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End:
*)
