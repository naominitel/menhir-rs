(* This module parses the command line. *)

(* The list of file names that appear on the command line. *)

val filenames: string list

(* How to deal with the type of tokens. *)

type token_type_mode =
  | TokenTypeAndCode   (* produce the definition of the [token] type and code for the parser *)
  | TokenTypeOnly      (* produce the type definition only *)
  | CodeOnly of string (* produce the code only, by relying on an external token type *)

val token_type_mode: token_type_mode

(* How to construct the automaton. *)

type construction_mode =
  | ModeCanonical     (* --canonical: canonical Knuth LR(1) automaton *)
  | ModeInclusionOnly (* --no-pager : states are merged when there is an inclusion
                                      relationship, default reductions are used *)
  | ModePager         (* normal mode: states are merged as per Pager's criterion,
                                      default reductions are used *)
  | ModeLALR          (* --lalr     : states are merged as in an LALR generator,
                                      i.e. as soon as they have the same LR(0) core *)

val construction_mode: construction_mode

(* Whether conflicts should be explained. *)

val explain: bool

(* Whether the automaton should be dumped. *)

val dump: bool

(* Whether the automaton's construction should be explained (very verbose). *)

val follow: bool

(* Whether the grammar's dependence graph should be dumped. *)

val graph: bool

(* Whether tracing instructions should be generated. *)

val trace: bool

(* Whether one should stop and print the grammar after joining and
   expanding the grammar. *)

type print_mode =
    | PrintNormal
    | PrintUnitActions
    | PrintUnitActionsUnitTokens

type preprocess_mode =
    | PMNormal                       (* preprocess and continue *)
    | PMOnlyPreprocess of print_mode (* preprocess, print grammar, stop *)

val preprocess_mode: preprocess_mode

(* Whether one should invoke ocamlc in order to infer types for all
   nonterminals. *)

val infer: bool

(* Whether one should inline the non terminal definitions marked
   with the %inline keyword. *)

val inline: bool

(* Whether and how one should invoke ocamldep in order to compute and
   display dependencies. *)

type ocamldep_mode =
  | OMNone        (* do not invoke ocamldep *)
  | OMRaw         (* invoke ocamldep and echo its raw output *)
  | OMPostprocess (* invoke ocamldep and postprocess its output *)

val depend: ocamldep_mode

(* Whether comments should be printed or discarded. *)

val comment: bool

(* This undocumented flag suppresses prefixing of identifiers with an
   unlikely prefix in the generated code. This increases the code's
   readability, but can cause identifiers in semantic actions to be
   captured. *)

val noprefix: bool

(* This undocumented flag causes the code to be transformed by
   [Inline]. It is on by default. *)

val code_inlining: bool

(* How [ocamlc] and [ocamldep] should be invoked. *)

val ocamlc: string
val ocamldep: string

(* How verbose we should be. *)

val logG: int (* diagnostics on the grammar *)
val logA: int (* diagnostics on the automaton *)
val logC: int (* diagnostics on the generated code *)

(* Whether tasks should be timed. *)

val timings: bool

(* The extension of the input file. *)

val file_ext: string

(* The base name that should be used for the files that we create.
   This name can contain a path. *)

val base: string

(* The filename of the standard library. *)

val stdlib_filename : string

(* Whether Menhir should behave as an interpreter. *)

val interpret : bool

(* Whether the interpreter should build and display concrete syntax trees. *)

val interpret_show_cst : bool

(* Whether Menhir should behave as an interpreter, in a special mode where
   it checks one input sentence, expecting it to trigger an error at the
   last token, and displays which state was reached. *)

val interpret_error : bool

(* Whether to use the table-based back-end ([true]) or the code-based
   back-end ([false]). *)

val table : bool

(* Whether to generate the inspection API (which requires GADTs, and
   requires producing more tables). *)

val inspection : bool

(* Whether to generate a coq description of the grammar and automaton. *)

val coq : bool

(* Whether the coq description must contain completeness proofs. *)

val coq_no_complete : bool

(* Whether the coq backend should ignore types and semantic actions. *)

val coq_no_actions : bool

(* Whether to generate a Rust parser. *)

val rust : bool

(* Whether unresolved LR(1) conflicts, useless precedence declarations,
   productions that are never reduced, etc. should be treated as errors. *)

val strict: bool

(* This flag causes the exception [Error] should be declared equal to
   [Parsing.Parse_error]. This is useful when full compatibility with
   ocamlyacc is desired. In particular, this is used when building
   Menhir itself, since Menhir is compiled first using ocamlyacc, then
   using Menhir. *)

val fixedexc: bool

(* This is a set of tokens which may be unused and about which we should not
   emit a warning. *)

val ignored_unused_tokens: StringSet.t

(* This flag supersedes the set [ignored_unused_tokens]. If it is set, then
   we should not emit a warning about any unused tokens. *)

val ignore_all_unused_tokens: bool

(* This flag causes Menhir to produce a list of erroneous input sentences.
   Enough sentences are computed to produce exactly one error in every state
   where an error can occur. *)

val list_errors: bool

(* This flag causes Menhir to read the error message descriptions stored in
   [filename] and compile them to OCaml code. *)

val compile_errors: string option

(* If present, this is a pair of .messages files whose contents should
   be compared. *)

val compare_errors: (string * string) option

(* This flag causes Menhir to read the error message descriptions stored in
   [filename] and re-generate the auto-generated comments, which begin with
   [##]. This allows bringing these comments up to date when the grammar
   evolves. *)

val update_errors: string option

(* This flag causes Menhir to read the error message descriptions stored in
   [filename] and echo the error sentences (and nothing else; no messages,
   no comments). *)

val echo_errors: string option

