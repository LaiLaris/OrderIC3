(* This file is part of the Kind 2 model checker.

   Copyright (c) 2014 by the Board of Trustees of the University of Iowa

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

(** Managing of scopes to avoid clashes

    @author Christoph Sticksel *)

type t = Ident.t list
(** Scope as a sequence of identifiers

    This type will become private later *)

val equal : t -> t -> bool
(** Equality of scopes *)

val compare : t -> t -> int
(** Total order of scopes *)

val hash : t -> int
(** Hash of a scope *)

module Set : Set.S with type elt = t
(** Set of scopes *)

module Map : Map.S with type key = t
(** Map of scopes *)

val mk_scope : Ident.t list -> t
(** Construct a scope from a list of identifiers *)

val pp_print_scope : Format.formatter -> t -> unit
(** Pretty-print a scope *)

val to_string : t -> string

(* 
   Local Variables:
   compile-command: "make -C .. -k"
   indent-tabs-mode: nil
   End: 
*)
