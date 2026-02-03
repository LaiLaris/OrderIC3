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

(** Arbitrary precision real numbers

    @author Christoph Sticksel *)

type t
(** Type of arbitrary precision rational numbers *)

(** {1 Pretty-printing and String Representation} *)

val pp_print_decimal : Format.formatter -> t -> unit
(** Pretty-print a rational *)

val pp_print_decimal_as_float : Format.formatter -> t -> unit
(** Pretty-print a rational as an f64 (used by compilation to Rust) *)

val pp_print_decimal_as_lus_real : Format.formatter -> t -> unit
(** Pretty-print a rational as a Lustre real *)

val pp_print_decimal_approximation : Format.formatter -> t -> unit
(** Pretty-print a rational in scientific format with the error magnitude *)

val pp_print_decimal_sexpr : Format.formatter -> t -> unit
(** Pretty-print a rational as an S-expression *)

val pp_print_decimal_as_json : Format.formatter -> t -> unit

val string_of_decimal : t -> string
(** Return a string representation of a rational *)

val string_of_decimal_sexpr : t -> string
(** Return an S-expression string representation of a rational *)

val string_of_decimal_lustre : t -> string
(** Return a Lustre string representation of a rational *)

(** {1 Conversions} *)

val of_int : int -> t
(** Convert an integer to a rational *)

val of_big_int : Big_int.big_int -> t
(** Convert an arbitrary large integer to a rational *)

val of_num : Num.num -> t
(** Convert an ocaml Num to a rational *)

val of_string : string -> t
(** Convert a string in floating-point notation [1.2E3] to rational number *)

val to_int : t -> int
(** Convert a rational number to a rational

    Truncates the rational number to an integer and raises the exception
    [Failure "int_of_big_int"] if the numeral cannot be represented as an
    integer. *)

val to_big_int : t -> Big_int.big_int
(** Convert a rational number to an arbitrary large integer *)

val is_int : t -> bool
(** Return true if decimal coincides with an integer *)

val sign : t -> int
(** Returns 0 on zero, 1 for positives and -1 for negatives. Works also on
    infinites but fails on undefined. *)

val epsilon : int -> t
(** Returns the rational [2^p] with signed p. *)

val magnitude : t -> int
(** Returns a signed integer [n] such that [2^(n-1) < |dec| < 2^n ] or [0] if
    [dec] is null. *)

(** {1 Constants} *)

val zero : t
(** The rational number zero *)

val one : t
(** The rational number one *)

(** {1 Arithmetic operations} *)

val succ : t -> t
(** Successor *)

val pred : t -> t
(** Predecessor *)

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
(** Quotient *)

val rem : t -> t -> t
(** Remainder *)

(** {2 Infix operators} *)

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

(** {2 Infix operators} *)

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
