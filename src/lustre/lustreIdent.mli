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

(** Lustre identifier

    An identifier is a string with a (possibly empty) list of integer indexes.

    This module also provides some pre-defined identifiers that are used in the
    translation.

    @author Christoph Sticksel *)

type t = private Ident.t * int list
(** An identifier is a string with integer indexes *)

val equal : t -> t -> bool
(** Equality on identifiers *)

val hash : t -> int
(** Hash an identifier *)

val compare : t -> t -> int
(** Total ordering of identifiers *)

module Hashtbl : Hashtbl.S with type key = t
(** Hash table over identifiers *)

module Set : Set.S with type elt = t
(** Set of identifiers *)

module Map : Map.S with type key = t
(** Map of identifiers *)

(** {1 Constructors and Converters} *)

val string_of_ident : bool -> t -> string
(** Return a string representation of the identifier

    [string_of_ident safe ident] returns the identifier with the indexes
    appended in [\[] and [\]] if [safe] is [false]. Otherwise the indexes are
    appended separated by [_], which makes the string a valid Lustre identifier.
*)

val push_index : t -> int -> t
(** Add the given integer as an index to the identifier *)

val mk_string_ident : string -> t
(** Construct an identifier of a string *)

val of_scope : Scope.t -> t
(** Construct an identifier of a scope *)

val to_scope : t -> Scope.t
(** Return a scope of an identifier

    The indexes of the identifier become separate scope levels. *)

val pp_print_ident : bool -> Format.formatter -> t -> unit
(** Pretty-print an identifier

    [pp_print_ident safe ident] prints the indexes separated by [_] if [safe] is
    [true] as in {!string_of_ident}. *)

(** {1 Reserved Identifiers} *)

val reserved_scope : Scope.t
(** Scope for reserved identifiers *)

val user_scope : Scope.t
(** Scope for identifiers in user input *)

val abs_ident : t
(** Identifier for abstracted variables *)

val oracle_ident : t
(** Identifier for oracle inputs *)

val instance_ident : t
(** Identifier for unique identifier for node instance *)

val init_flag_ident : t
(** Identifier for first instant flag *)

val inst_ident : t
(** Identifier for instantiated variables in node calls *)

val eq_vars_ident : t
(** Identifier for variables stating two variables are equal so far *)

val index_ident : t
(** Identifier for index variables in arrays *)

(* 
   Local Variables:
   compile-command: "make -k -C .."
   indent-tabs-mode: nil
   End: 
*)
