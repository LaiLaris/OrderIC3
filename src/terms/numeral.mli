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

(** Arbitrary precision integers

    @author Christoph Sticksel *)

type t
(** Type of arbitrary precision numerals *)

(** {1 Pretty-printing and String Representation} *)

val pp_print_numeral : Format.formatter -> t -> unit
(** Pretty-print a numeral, e.g. -1 *)

val pp_print_numeral_sexpr : Format.formatter -> t -> unit
(** Pretty-print a numeral in s-expression, e.g. (- 1) *)

val string_of_numeral : t -> string
(** Return a string representation of a numeral *)

(** {1 Conversions} *)

val of_int : int -> t
(** Convert an integer to a numeral *)

val of_big_int : Big_int.big_int -> t
(** Convert an arbitrary large integer numeral to numeral *)

val of_string : string -> t
(** Convert a string to numeral *)

val to_int : t -> int
(** Convert a numeral to an integer

    Raises the exception [Failure "int_of_big_int"] if the numeral cannot be
    represented as an integer. *)

val to_big_int : t -> Big_int.big_int
(** Convert a numeral to an arbitrary large integer *)

(** {1 Constructors} *)

val zero : t
(** The numeral zero *)

val one : t
(** The numeral one *)

(** {1 Arithmetic operations} *)

val succ : t -> t
(** Successor *)

val pred : t -> t
(** Predecessor *)

val incr : t ref -> unit
(** Increment a numeral in a reference by one *)

val decr : t ref -> unit
(** Decrement a numeral in a reference by one *)

val abs : t -> t
(** Absolute value *)

val neg : t -> t
(** Unary negation *)

val add : t -> t -> t
(** Sum *)

val sub : t -> t -> t
(** Difference *)

val mult : t -> t -> t
(** Product *)

val div : t -> t -> t
(** Division *)

val rem : t -> t -> t
(** Remainder

    Identical to [mod], but the latter is an infix operator. *)

val min : t -> t -> t
(** Return smaller of two numerals *)

val max : t -> t -> t
(** Return greater of two numerals *)

(** {2 Infix arithmetic operators} *)

val ( ~- ) : t -> t
(** Unary negation *)

val ( + ) : t -> t -> t
(** Sum *)

val ( - ) : t -> t -> t
(** Difference *)

val ( * ) : t -> t -> t
(** Product *)

val ( / ) : t -> t -> t
(** Quotient *)

val ( mod ) : t -> t -> t
(** Remainder *)

(** {1 Comparison operators} *)

val equal : t -> t -> bool
(** Equality *)

val compare : t -> t -> int
(** Comparison *)

val leq : t -> t -> bool
(** Less than or equal predicate *)

val lt : t -> t -> bool
(** Less than predicate *)

val geq : t -> t -> bool
(** Greater than or equal predicate *)

val gt : t -> t -> bool
(** Greater than predicate *)

(** {2 Infix comparison operators} *)

val ( = ) : t -> t -> bool
(** Equality *)

val ( <> ) : t -> t -> bool
(** Disequality *)

val ( <= ) : t -> t -> bool
(** Less than or equal predicate *)

val ( < ) : t -> t -> bool
(** Less than predicate *)

val ( >= ) : t -> t -> bool
(** Greater than or equal predicate *)

val ( > ) : t -> t -> bool
(** Greater than predicate *)

(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
