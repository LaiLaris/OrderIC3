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

(** General-purpose library functions

    @author Christoph Sticksel *)

exception Unsupported of string
(** thunk for an unimplimented function *)

val todo : string -> 'a
(** {1 Helper functions} *)

val identity : 'a -> 'a
(** Identity function. *)

val print_pass : string -> 'a -> 'a
(** Prints the first argument and returns the second. *)

val true_of_unit : unit -> bool
(** Returns true when given unit. *)

val false_of_unit : unit -> bool
(** Returns false when given unit. *)

val none_of_unit : unit -> 'a option
(** Returns None when given unit. *)

val true_of_any : 'a -> bool
(** Return true *)

val false_of_any : 'a -> bool
(** Return false *)

val mk_dir : string -> unit
(** Creates a directory if it does not already exist. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** Flips the expected argument of the function *)

(** {1 Option types} *)

val get : 'a option -> 'a
(** Return the value of an option type, raise [Invalid_argument "get"] if the
    option value is [None] *)

val is_some : 'a option -> bool
(** Check if an option has some content *)

val join : 'a option option -> 'a option
(** Flatten a nested option *)

val min_option : float option -> float option -> float option
(** Return the min between two optional floats. Return None if both floats are
    None. *)

(** {1 Integer functions} *)

val string_starts_with : string -> string -> bool
(** [string_starts_with s1 s2] returns true if the first characters of [s1] up
    to the length of [s2] are ientical to [s2]. Return false if [s2] is longer
    than [s1]. *)

(** {1 Integer functions} *)

val safe_hash_interleave : int -> int -> int -> int
(** [safe_hash_interleave h m i] compute [m * h + i] and makes sure that the
    result does not overflow to a negtive number *)

(** {1 List functions} *)

val find_opt_index : ('a -> bool) -> 'a list -> int option

val ( @:: ) : 'a option -> 'a list -> 'a list
(** Add element to the head of the list if the option value is not [None].

    The function symbol is right-associative and infix:

    {[
      Some 1 @:: None @:: Some 2 @:: [ 3; 4 ]
    ]}

    returns

    {[
      \[1;2;3;4\]
    ]} *)

val list_init : (int -> 'a) -> int -> 'a list
(** Creates a size-n list equal to [f 0; f 1; ... ; f (n-1)] *)

val list_max : 'a list -> 'a
(** Returns the maximum element of a non-empty list *)

val list_min : 'a list -> 'a
(** Returns the minimum element of a non-empty list *)

val list_index : ('a -> bool) -> 'a list -> int
(** Return the index of the first element that satisfies the predicate [p],
    raise excpetion [Not_found] if no element satisfies the predicate. *)

val list_indexes : 'a list -> 'a list -> int list
(** [list_indexes l1 l2] returns the indexes in list [l2] of elements in list
    [l1] *)

val list_filter_nth : 'a list -> int list -> 'a list
(** [list_filter_nth l [p1; p2; ...]] returns the elements [l] at positions
    [p1], [p2] etc. *)

val list_extract_nth : 'a list -> int -> 'a * 'a list
(** Remove and returns the nth element from a list *)

val list_remove_nth : int -> 'a list -> 'a list
(** Remove the nth element from a list *)

val list_insert_at : 'a -> int -> 'a list -> 'a list
(** Insert an element at a given position into a list *)

val list_apply_at : ('a -> 'a) -> int -> 'a list -> 'a list
(** Apply a function to the nth element of a list *)

val list_slice : 'a list -> int -> int -> 'a list
(** [list_slice l i k] returns a list containing the elements between the [i]th
    and [k]th element of [l]*)

val list_suffix : 'a list -> int -> 'a list
(** [list_suffix l i] returns a list containing the elements between the [i]th
    and the last element of [l] *)

val list_split : int -> 'a list -> 'a list * 'a list
(** [list_split n l] divides list [l] into two at index [n]
    The first will contain all indices from [0,n) and
    the second will contain all indices from [n,len) *)

val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
(** [split3 l] takes a list of triples and produces a triple of lists *)

val chain_list : 'a list -> 'a list list
(** [chain_list [e1; e2; ...]] is [[[e1; e2]; [e2; e3]; ... ]] *)

val chain_list_p : 'a list -> ('a * 'a) list
(** [chain_list_p [e1; e2; ...]] is [[(e1, e2); (e2, e3); ... ]] *)

val list_subtract : 'a list -> 'a list -> 'a list
(** Return a list containing all values in the first list that are not in the
    second list *)

val list_uniq : 'a list -> 'a list
(** From a sorted list return a list with physical duplicates removed *)

val list_merge_uniq : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merge two sorted lists without physical duplicates to a sorted list without
    physical duplicates *)

val list_inter_uniq : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** From two sorted lists without physical duplicates return a sorted list
    without physical duplicates containing elements in both lists *)

val list_diff_uniq : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** From two sorted lists without physical duplicates return a sorted list
    without physical duplicates containing elements in the first but not in the
    second list *)

val list_subset_uniq : ('a -> 'a -> int) -> 'a list -> 'a list -> bool
(** For two sorted lists without physical duplicates return true if the first
    list contains a physically equal element for each element in the second list
*)

val list_join :
  ('a -> 'a -> bool) ->
  ('a * 'b) list ->
  ('a * 'b list) list ->
  ('a * 'b list) list
(** Given two ordered association lists with identical keys, push the values of
    each element of the first association list to the list of elements of the
    second association list.

    The returned association list is in the order of the input lists, the
    function [equal] is used to compare keys. Raise [Failure "list_join"] if the
    lists are not of identical length and the keys at each element are equal. *)

val list_filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** Apply a map over a list where if the output is None then the element is
    dropped from the resulting list and if the output is Some value then that
    value is added to the resulting list *)

val compare_pairs :
  ('a -> 'a -> int) -> ('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int
(** Lexicographic comparison of pairs *)

val compare_lists : ('a -> 'a -> int) -> 'a list -> 'a list -> int
(** Lexicographic comparison of lists *)

val list_apply : ('a -> 'b) list -> 'a -> 'b list
(** Apply a list of functions to a single argument. Kind of List.map but not
    really *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option

val drop_last : 'a list -> 'a list
(** Returns the list with its last element dropped *)

(** {1 Array functions} *)

val array_max : 'a array -> 'a
(** Returns the maximum element of a non-empty array *)

(** {1 Set functions} *)

module IntegerSet : Set.S with type elt = int
(** Sets of integers *)

module IntegerHashtbl : Hashtbl.S with type key = int
(** Hashtable of integers *)

(** {1 Pretty-printing helpers} *)

val pp_print_pair :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'c -> 'd) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a * 'c ->
  'd
(** Pretty-print a pair. it excepts two formatters and a separator and formats
    the pair *)

val pp_print_triple :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> 'd) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a * 'b * 'c ->
  'd
(** Pretty-print a triple. it excepts three formatters and a separator and
    formats the triple *)

val pp_print_arrayi :
  (Format.formatter -> int -> 'a -> unit) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a array ->
  unit
(** Pretty-print an array with given separator

    [pp_print_array elem_printer separator formatter array] calls, for each
    index [i] of the array whose corresponding element is [element],
    [elem_printer formatter i element]. Between each of these calls it prints
    the string [separator].

    In order to get line breaks between the elements, do not use a line feed
    character [\n] as separator, this might mess up indentation. Instead wrap
    the list into a vertical box with the format string [@[<v>%a@]] and the
    empty string as separator. *)

val pp_print_list :
  (Format.formatter -> 'a -> unit) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a list ->
  unit
(** Pretty-print a list with given separator

    [pp_print_list p s f l] pretty-prints the elements in the list [l] by
    calling the pretty-printer [p] on each, separating the elements by printing
    the string [s].

    In order to get line breaks between the elements, do not use a line feed
    character [\n] as separator, this might mess up indentation. Instead wrap
    the list into a vertical box with the format string [@[<v>%a@]] and the
    empty string as separator. *)

val pp_print_listi :
  (Format.formatter -> int -> 'a -> unit) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a list ->
  unit
(** Pretty-print a list with given separator and maintain a counter of elements

    See {!pp_print_list}, except that the pretty-printer is passes an zero-based
    counter for the list's elements as the argument preceding the list element.
*)

val pp_print_list2i :
  (Format.formatter -> int -> 'a -> 'b -> unit) ->
  (unit, Format.formatter, unit) format ->
  Format.formatter ->
  'a list ->
  'b list ->
  unit
(** Pretty-print two lists of the same length with given separator and maintain
    a counter of their elements.*)

val pp_print_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
(** Pretty-print an option type *)

val pp_print_if_not_empty :
  (unit, Format.formatter, unit) format -> Format.formatter -> 'a list -> unit
(** Pretty-print if list is not empty *)

val string_of_t : (Format.formatter -> 'a -> unit) -> 'a -> string
(** Pretty-print into a string *)

val width_of_string : string -> int
(** Return the width of the string, meaning the wisth of it's longest line *)

val paren_string_of_string_list : string list -> string
(** Return the strings as a parenthesized and space separated list *)

val escape_json_string : string -> string
val escape_xml_string : string -> string

(** {1 Logging} *)

(** Levels of log messages

    - [L_fatal] A severe error that will lead to an immediate abort

    - [L_error] An error event that might still allow to continue

    - [L_warn] A potentially harmful situation

    - [L_note] An important note (soft warning)

    - [L_info] An informational message that highlight progress at a
      coarse-grained level

    - [L_debug] A fine-grained informational event that is useful for debugging
      but not for an end user

    - [L_trace] A finer-grained informational event than [L_debug] *)
type log_level =
  | L_off
  | L_fatal
  | L_error
  | L_warn
  | L_note
  | L_info
  | L_debug
  | L_trace

val default_log_level : log_level
(** Default log level. *)

val int_of_log_level : log_level -> int
(** Associate an integer with each level to induce a total ordering *)

val log_level_of_int : int -> log_level
val string_of_log_level : log_level -> string

val log_ppf : Format.formatter ref
(** Current formatter for output *)

val log_to_file : string -> unit
(** Ouputs all log messages to the given file *)

val log_to_stdout : unit -> unit
(** Write all log messages to the standard output *)

val set_log_level : log_level -> unit
(** Set log level

    Only output messages of levels with equal or higher priority *)

val get_log_level : unit -> log_level
(** Gets the log level. *)

val output_on_level : log_level -> bool
(** Return true if given log level is of higher or equal priority than current
    log level? *)

val ignore_or_fprintf :
  log_level -> Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** Return Format.fprintf if level is is of higher or equal priority than
    current log level, otherwise return Format.ifprintf *)

val ignore_or_kfprintf :
  log_level ->
  (Format.formatter -> 'a) ->
  Format.formatter ->
  ('b, Format.formatter, unit, 'a) format4 ->
  'b
(** Return Format.kfprintf if level is is of higher or equal priority than
    current log level, otherwise return Format.ikfprintf *)

(** {1 System functions} *)

val pp_print_banner : Format.formatter -> unit -> unit
(** Output the banner on the formatter *)

val pp_print_version : Format.formatter -> unit
(** Output the version number on the formatter *)

type kind_module =
  [ `IC3
  | `IC3QE
  | `IC3QEl1l2
  | `IC3QEl2l1 
  | `IC3IA
  | `BMC
  | `BMCSKIP
  | `IND
  | `IND2
  | `INVGEN
  | `INVGENOS
  | `INVGENINT
  | `INVGENINTOS
  | `INVGENMACH
  | `INVGENMACHOS
  | `INVGENINT8
  | `INVGENINT8OS
  | `INVGENINT16
  | `INVGENINT16OS
  | `INVGENINT32
  | `INVGENINT32OS
  | `INVGENINT64
  | `INVGENINT64OS
  | `INVGENUINT8
  | `INVGENUINT8OS
  | `INVGENUINT16
  | `INVGENUINT16OS
  | `INVGENUINT32
  | `INVGENUINT32OS
  | `INVGENUINT64
  | `INVGENUINT64OS
  | `INVGENREAL
  | `INVGENREALOS
  | `C2I
  | `Interpreter
  | `Supervisor
  | `Parser
  | `Certif
  | `MCS
  | `CONTRACTCK ]
(** Kind modules *)

exception TimeoutWall
(** Wallclock timeout *)

exception TimeoutVirtual
(** CPU timeout *)

exception Signal of int
(** System signal caught *)

val string_of_signal : int -> string
(** String representation of signal *)

val pp_print_process_status : Format.formatter -> Unix.process_status -> unit
(** Pretty-print the termination status of a process *)

val exception_on_signal : int -> 'a
(** Raise exception on signal *)

val pp_print_kind_module : Format.formatter -> kind_module -> unit
(** Pretty-print the name of a kind module *)

val string_of_kind_module : kind_module -> string
(** String representation of a process type *)

val int_of_kind_module : kind_module -> int
(** String representation of a process type *)

val short_name_of_kind_module : kind_module -> string
(** Return a short representation of kind module *)

val kind_module_of_string : string -> kind_module
(** Kind module of a string *)

val minisleep : float -> unit
(** Sleep for seconds, resolution is in ms *)

val find_on_path : string -> string
(** Return full path to executable, search PATH environment variable and current
    working directory *)

val find_file : string -> string list -> string option
(** Return full path to file if the file is found in the list of directories, or
    None otherwise **)

(** {1 Positions in the input} *)

type position
(** A position in the input *)

val dummy_pos : position
(** Dummy position different from any valid position *)

val equal_pos : position -> position -> bool
(** [equal_pos p1 p2] is true if both positions are equal *)

val compare_pos : position -> position -> int
(** Comparision on positions *)

val is_dummy_pos : position -> bool
(** Return [true] if the position is not a valid position in the input *)

val set_stdin_id : string -> unit
(** [set_stdin_id name] sets [name] as the filename used in positions printed
    with [pp_print_position] when reading from standard input *)

val pp_print_position : Format.formatter -> position -> unit
(** Pretty-print a position *)

val pp_print_line_and_column : Format.formatter -> position -> unit
(** Pretty-print line and column *)

val pp_print_lines_and_columns : Format.formatter -> position list -> unit

val file_row_col_of_pos : position -> string * int * int
(** Return the file, line and column of a position; fail with
    [Invalid_argument "file_row_col_of_pos"] if the position is a dummy position
*)

val file_of_pos : position -> string
(** Return the file of a position *)

val row_col_of_pos : position -> int * int
(** Return the line and column of a position; fail with
    [Invalid_argument "file_row_col_of_pos"] if the position is a dummy position
*)

val pos_of_file_row_col : string * int * int -> position
(** Inverse of {!file_row_col_of_pos} *)

val position_of_lexing : Lexing.position -> position
(** Convert a position of the lexer to a position *)

val set_lexer_filename : Lexing.lexbuf -> string -> unit
(** set the filename in lexing buffer*)

val print_backtrace : Format.formatter -> Printexc.raw_backtrace -> unit
(** Pretty print a backtrace *)

val extract_scope_name : string -> string * string list
(** Extract scope from a concatenated name *)

val create_dir : string -> unit
(** Create a directory if it does not already exists. *)

val file_copy : string -> string -> unit
(** Copying file: [file_copy from to] copies file [from] to file [to] *)

val files_cat_open :
  ?add_prefix:(Format.formatter -> unit) ->
  string list ->
  string ->
  Unix.file_descr

val syscall : string -> string
(** Get standard output of command *)

val set_liberal_gc : unit -> unit
(** Changes garbage collector parameters limit its effect *)

val reset_gc_params : unit -> unit
(** Reset the parameters of the GC to its default values. Call after
    {!set_liberal_gc}. *)

(* Print bound of (possibly) open interval *)
val pp_print_bound_opt : Format.formatter -> Numeral.t option -> unit

(** Paths Kind 2 can write some files. Factored to avoid clashes. *)
module Paths : sig
  val testgen : string
  (** Test generation files path. *)

  val oracle : string
  (** Test generation oracle path. *)

  val implem : string
  (** Rust generation path. *)
end

(** Reserved identifiers. *)
module ReservedIds : sig
  val abs_ident_string : string
  (** New variables from abstraction. *)

  val oracle_ident_string : string
  (** New oracle input. *)

  val instance_ident_string : string
  (** Unique identifier for node instance. *)

  val init_flag_ident_string : string
  (** First instant flag. *)

  val all_req_ident_string : string
  (** Observer for contract requirements. *)

  val all_ens_ident_string : string
  (** Observer for contract ensures. *)

  val inst_ident_string : string
  (** New variables from node instance. *)

  val eq_vars_ident_string : string
  (** Observer for variable equivalence *)

  val init_uf_string : string
  (** Initial predicate. *)

  val trans_uf_string : string
  (** Transition relation. *)

  val index_ident_string : string
  (** New clock initialization flag. *)

  val state_string : string
  (** Automaton state encoding. *)

  val restart_string : string
  val state_selected_string : string
  val restart_selected_string : string
  val state_selected_next_string : string
  val restart_selected_next_string : string
  val handler_string : string
  val unless_string : string

  val init_flag_string : string
  (** Init flag string. *)

  val depth_input_string : string
  (** Abstraction depth input string. *)

  val max_depth_input_string : string
  (** Abstraction depth input string. *)

  val function_of_inputs : string
  (** Suffix used for the name of the function encoding functional systems. *)

  val reserved_strings : string list
  (** All reserved identifiers. *)
end

val unbounded_limit_string : string
(** String representing an unbounded subrange limit *)

(** Exit codes. *)
module ExitCodes : sig
  val success : int
  (** Exit code for a complete and successful analysis *)

  val error : int
  (** Exit code for a general error *)

  val usage_error : int
  (** Exit code for a command line usage error *)

  val parse_error : int
  (** Exit code for a parse error *)

  val not_found_error : int
  (** Exit code for a solver not found error *)

  val unsupported_solver : int
  (** Exit code for an unknown or unsupported version of a solver *)

  val incomplete_analysis : int
  (** Exit code for an incomplete analysis (unknown result, timeout) *)

  val unsafe_result : int
  (** Exit code for a complete analysis with an unsafe result *)

  val kid_status : int
  (** Exit status if kid caught a signal, the signal number is added to the
      value *)
end

(** File names. *)
module Names : sig
  val contract_gen_file : string
  (** Contract generation. *)

  val contract_name : string list -> string
  (** Contract name for contract generation. *)

  val inv_log_file : string
  (** Invariant logging. *)

  val inv_log_contract_name : string list -> string
  (** Contract name for invariant logging. *)
end

(* 
   Local Variables:
   compile-command: "make -C .. -k"
   tuareg-interactive-program: "./kind2.top -I ./_build -I ./_build/SExpr"
   indent-tabs-mode: nil
   End: 
*)
