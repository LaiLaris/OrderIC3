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

(** Wraps the candidate invariants with info about their cost etc. *)

type t
(** Contains the info needed for the moves on a candidate. *)

val mk : TransSys.t -> t
(** Creates a new candidate that can be randomly moved. Should be called only
    once as it mines the init and trans predicates. Use [reset] to launch a new
    C2I run. *)

val reset : t -> t
(** Resets a candidate. *)

val terms_of : t -> Term.t list list
(** Term of a candidate as a [Term.t list list] (dnf). *)

val term_of : t -> Term.t
(** Term of a candidate as a [Term.t]. *)

val move : t -> t
(** Makes a move on a candidate. *)

type rated_t
(** A candidate augmented with a cost and a rating. *)

val cost_of_rated : rated_t -> int
(** The cost of a rated candidate. *)

val candidate_of_rated : rated_t -> t
(** The candidate of a rated candidate. *)

val rated_cost_function :
  t -> Model.t list -> (Model.t * Model.t) list -> Model.t list -> rated_t
(** Computes the cost of a candidate and rates its atoms. *)

val rated_move : rated_t -> t
(** Moves a rated candidate. *)

(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
