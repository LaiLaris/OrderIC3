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

(** Utilty functions for transition systems

    Functions that use term data structures and can be used by any module above
    {!TransSys} go here. *)

(*
type invariants = Term.t list
type model = (Var.t * Term.t) list
type path = (StateVar.t * Term.t list) list
type property = (string * Term.t)
type properties = property list
type cex = (property list * path)
type cexs = cex list
*)

(** {2 Default values} *)

val default_of_type : Type.t -> Term.t
(** Return the default value of the type:

    By default, a Boolean value is false, integer and real values are zero,
    values in a range are equal to the lower bound of the range. Array scalar
    types do not have defaults. The function fails with [Invalid_argument] in
    this case. *)

(** {2 Logic fragments} *)

(** A feature of a logic fragment for terms *)
type feature =
  | Q  (** Quantifiers *)
  | UF  (** Equality over uninterpreted functions *)
  | A  (** Arrays *)
  | IA  (** Integer arithmetic *)
  | RA  (** Real arithmetic *)
  | LA  (** Linear arithmetic *)
  | NA  (** Non-linear arithmetic *)
  | BV  (** Bit vectors *)

module FeatureSet : Set.S with type elt = feature
(** Set of features *)

type features = FeatureSet.t
(** Logic fragments for terms *)

val sup_logics : features list -> features
(** Returns the sup of the logics given as arguments *)

val logic_of_term : UfSymbol.t list -> Term.t -> features
(** Returns the logic fragment used by a term *)

val logic_of_sort : Type.t -> features
(** Returns the logic fragment of a type *)

type logic = [ `None | `Inferred of features | `SMTLogic of string ]
(** Logic fragments for terms *)

val pp_print_logic : ?enforce_logic:bool -> Format.formatter -> logic -> unit
(** Print a logic *)

val string_of_logic : ?enforce_logic:bool -> logic -> string
(** String correspinding to a logic *)

val logic_allow_arrays : logic -> bool
(** Returns [true] if the logic potentially has arrays *)

(** Gathers signal related stuff. *)
module Signals : sig
  val pp_print_signals : Format.formatter -> unit -> unit
  (** Pretty printer for signal info. *)

  val ignore_sigalrm : unit -> unit
  (** Sets the handler for sigalrm to ignore. *)

  val ignoring_sigalrm : (unit -> 'a) -> 'a
  (** Runs something while ignoring [sigalrm]. *)

  val ignore_sigint : unit -> unit
  (** Sets the handler for sigint to ignore. *)

  val ignore_sigquit : unit -> unit
  (** Sets the handler for sigquit to ignore. *)

  val ignore_sigterm : unit -> unit
  (** Sets the handler for sigterm to ignore. *)

  val ignore_sigpipe : unit -> unit
  (** Sets the handler for sigpipe to ignore. *)

  val set_sigalrm_timeout : unit -> unit
  (** Sets a timeout handler for sigalrm. *)

  val set_sigalrm_exn : unit -> unit
  (** Sets an exception handler for sigalarm. *)

  val set_sigint : unit -> unit
  (** Sets a handler for sigint. *)

  val set_sigquit : unit -> unit
  (** Sets a handler for sigquit. *)

  val set_sigterm : unit -> unit
  (** Sets a handler for sigterm. *)

  val set_sigpipe : unit -> unit
  (** Sets a handler for sigpipe. *)

  val set_timeout : float -> unit
  (** Sets a timeout. *)

  val set_sigalrm_timeout_from_flag : unit -> unit
  (** Sets a timeout based on the flag value and the total time elapsed this
      far. If no timeout is specified, set an exception handler for [sigalrm].
  *)

  val unset_timeout : unit -> unit
  (** Deactivates timeout. *)

  val catch_break : bool -> unit
  (** Raise exception on ctrl+c if true. *)
end

val add_quantifiers : logic -> logic

(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
