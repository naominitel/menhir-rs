(* This module provides utilities that are shared by the two versions
   of the parser. *)

open Syntax

(* [new_precedence_level pos1 pos2] creates a new precendence level, which is
   stronger than any levels previously created by this function. It should be
   called every time a [%left], [%right], or [%nonassoc] declaration is found.
   The positions are the positions of this declaration in the source code. The
   precedence levels created by this function are attached to tokens and (via
   %prec) to productions. They are used in solving shift/reduce and
   shift/reduce/reduce conflicts. *)

val new_precedence_level: Lexing.position -> Lexing.position -> precedence_level

(* [new_production_level()] creates a new production level, which is stronger
   than any levels previously created by this function. It should be called
   every time a new production is found. The production levels created by this
   function are attached to productions. They are used in solving
   reduce/reduce conflicts: following ocamlyacc and bison, the production that
   appears first in the grammar receives preference. It may seem very strange
   that %prec annotations do not influence this process, but that's how it is,
   at least for the moment. *)

val new_production_level: unit -> branch_production_level

(* [check_production_group] accepts a production group and checks that all
   productions in the group define the same set of identifiers. *)

val check_production_group:
  ((Positions.t * identifier Positions.located option * parameter) list * 'a * 'b * 'c) list ->
  unit

(* [normalize_producers] accepts a list of producers where identifiers are
   optional and returns a list of producers where identifiers are mandatory.
   A missing identifier in the [i]-th position receives the conventional
   name [_i]. *)

val normalize_producers:
  (Positions.t * identifier Positions.located option * parameter) list ->
  producer list

(* [override pos oprec1 oprec2] decides which of the two optional
   %prec declarations [oprec1] and [oprec2] applies to a
   production. It signals an error if the two are present. *)

val override: Positions.t -> 'a option -> 'a option -> 'a option

(* Support for on-the-fly expansion of anonymous rules. When such a
   rule is encountered, invoke [anonymous], which creates a fresh
   non-terminal symbol, records the definition of this symbol to a
   global variable, and returns this symbol. In the end, invoke
   [rules], so as to obtain a list of all recorded definitions. *)

val anonymous: Positions.t -> parameterized_branch list -> string
val rules: unit -> parameterized_rule list

(* [producer_names producers] returns an array [names] such that
   [names.(idx) = None] if the (idx + 1)-th producer is unnamed
   and [names.(idx) = Some id] if it is called [id]. *)
val producer_names :
  (_ * Syntax.identifier Positions.located option * _) list ->
  Syntax.identifier option array
