open Printf

(* ------------------------------------------------------------------------- *)
(* Prepare for parsing the command line. *)

type token_type_mode =
  | TokenTypeAndCode   (* produce the definition of the [token] type and code for the parser *)
  | TokenTypeOnly      (* produce the type definition only *)
  | CodeOnly of string (* produce the code only; import token type from specified module *)

let token_type_mode =
  ref TokenTypeAndCode

let tokentypeonly () =
  token_type_mode := TokenTypeOnly

let is_uppercase_ascii c =
  c >= 'A' && c <= 'Z'

let is_capitalized_ascii s =
  String.length s > 0 &&
  is_uppercase_ascii s.[0]

let codeonly m =
  if not (is_capitalized_ascii m) then begin
    (* Not using module [Error] to avoid a circular dependency. *)
    fprintf stderr "Error: %s is not a valid OCaml module name.\n" m;
    exit 1
  end;
  token_type_mode := CodeOnly m

let version =
  ref false

type construction_mode =
  | ModeCanonical     (* --canonical: canonical Knuth LR(1) automaton *)
  | ModeInclusionOnly (* --no-pager : states are merged when there is an inclusion
                                      relationship *)
  | ModePager         (* normal mode: states are merged as per Pager's criterion *)
  | ModeLALR          (* --lalr     : states are merged as in an LALR generator,
                                      i.e. as soon as they have the same LR(0) core *)

(* Note that --canonical overrides --no-pager. If both are specified, the result
   is a canonical automaton. *)

let construction_mode =
  ref ModePager

let explain =
  ref false

let base =
  ref ""

let dump =
  ref false

let follow =
  ref false

let graph =
  ref false

let trace =
  ref false

let noprefix =
  ref false

type print_mode =
    | PrintNormal
    | PrintUnitActions
    | PrintUnitActionsUnitTokens

type preprocess_mode =
    | PMNormal                       (* preprocess and continue *)
    | PMOnlyPreprocess of print_mode (* preprocess, print grammar, stop *)

let preprocess_mode =
  ref PMNormal

let recovery =
  ref false

let v () =
  dump := true;
  explain := true

let infer =
  ref false

let inline =
  ref true

type ocamldep_mode =
  | OMNone        (* do not invoke ocamldep *)
  | OMRaw         (* invoke ocamldep and echo its raw output *)
  | OMPostprocess (* invoke ocamldep and postprocess its output *)

let depend =
  ref OMNone

let code_inlining =
  ref true

let comment =
  ref false

let ocamlc =
  ref "ocamlc"

let ocamldep =
  ref "ocamldep"

let logG, logA, logC =
  ref 0, ref 0, ref 0

let timings =
  ref false

let filenames =
  ref StringSet.empty

let no_stdlib =
  ref false

let stdlib_path =
  ref Installation.libdir

let insert name =
  filenames := StringSet.add name !filenames

let interpret =
  ref false

let interpret_show_cst =
  ref false

let interpret_error =
  ref false

let table =
  ref false

let inspection =
  ref false

let coq =
  ref false

let coq_no_complete =
  ref false

let coq_no_actions =
  ref false

let rust =
  ref false

let strict =
  ref false

let fixedexc =
  ref false

type suggestion =
  | SuggestNothing
  | SuggestCompFlags
  | SuggestLinkFlags of string (* "cmo" or "cmx" *)
  | SuggestWhereIsMenhirLibSource
  | SuggestUseOcamlfind

let suggestion =
  ref SuggestNothing

let ignored_unused_tokens =
  ref StringSet.empty

let ignore_unused_token t =
  ignored_unused_tokens := StringSet.add t !ignored_unused_tokens

let ignore_all_unused_tokens =
  ref false

let list_errors =
  ref false

let compile_errors =
  ref None

let set_compile_errors filename =
  compile_errors := Some filename

let compare_errors =
  ref []

let add_compare_errors filename =
  compare_errors := filename :: !compare_errors

let update_errors =
  ref None

let set_update_errors filename =
  update_errors := Some filename

let echo_errors =
  ref None

let set_echo_errors filename =
  echo_errors := Some filename

let options = Arg.align [
  "--base", Arg.Set_string base, "<basename> Specifies a base name for the output file(s)";
  "--canonical", Arg.Unit (fun () -> construction_mode := ModeCanonical), " Construct a canonical Knuth LR(1) automaton";
  "--comment", Arg.Set comment, " Include comments in the generated code";
  "--compare-errors", Arg.String add_compare_errors, "<filename> (used twice) Compare two .messages files.";
  "--compile-errors", Arg.String set_compile_errors, "<filename> Compile a .messages file to OCaml code.";
  "--coq", Arg.Set coq, " Generate a formally verified parser, in Coq";
  "--coq-no-complete", Arg.Set coq_no_complete, " Do not generate a proof of completeness";
  "--coq-no-actions", Arg.Set coq_no_actions, " Ignore semantic actions in the Coq output";
  "--rust", Arg.Set rust, " Generate a Rust parser";
  "--depend", Arg.Unit (fun () -> depend := OMPostprocess), " Invoke ocamldep and display dependencies";
  "--dump", Arg.Set dump, " Describe the automaton in <basename>.automaton";
  "--echo-errors", Arg.String set_echo_errors, "<filename> Echo the sentences in a .messages file";
  "--error-recovery", Arg.Set recovery, " (no longer supported)";
  "--explain", Arg.Set explain, " Explain conflicts in <basename>.conflicts";
  "--external-tokens", Arg.String codeonly, "<module> Import token type definition from <module>";
  "--fixed-exception", Arg.Set fixedexc, " Declares Error = Parsing.Parse_error";
  "--follow-construction", Arg.Set follow, " (undocumented)";
  "--graph", Arg.Set graph, " Write grammar's dependency graph to <basename>.dot";
  "--infer", Arg.Set infer, " Invoke ocamlc for ahead of time type inference";
  "--inspection", Arg.Set inspection, " Generate the inspection API (requires --table)";
  "--interpret", Arg.Set interpret, " Interpret the sentences provided on stdin";
  "--interpret-show-cst", Arg.Set interpret_show_cst, " Show a concrete syntax tree upon acceptance";
  "--interpret-error", Arg.Set interpret_error, " Interpret one sentence that should end in an error";
  "--lalr", Arg.Unit (fun () -> construction_mode := ModeLALR), " Construct an LALR(1) automaton";
  "--list-errors", Arg.Set list_errors, " Produce a list of erroneous inputs";
  "--log-automaton", Arg.Set_int logA, "<level> Log information about the automaton";
  "--log-code", Arg.Set_int logC, "<level> Log information about the generated code";
  "--log-grammar", Arg.Set_int logG, "<level> Log information about the grammar";
  "--no-code-inlining", Arg.Clear code_inlining, " (undocumented)";
  "--no-inline", Arg.Clear inline, " Ignore the %inline keyword.";
  "--no-pager", Arg.Unit (fun () -> if !construction_mode = ModePager then construction_mode := ModeInclusionOnly), " (undocumented)";
  "--no-prefix", Arg.Set noprefix, " (undocumented)";
  "--no-stdlib", Arg.Set no_stdlib, " Do not load the standard library";
  "--ocamlc", Arg.Set_string ocamlc, "<command> Specifies how ocamlc should be invoked";
  "--ocamldep", Arg.Set_string ocamldep, "<command> Specifies how ocamldep should be invoked";
  "--only-preprocess", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintNormal),
                       " Print grammar and exit";
  "--only-preprocess-u", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintUnitActions),
                         " Print grammar with unit actions and exit";
  "--only-preprocess-uu", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintUnitActionsUnitTokens),
                          " Print grammar with unit actions & tokens and exit";
  "--only-tokens", Arg.Unit tokentypeonly, " Generate token type definition only, no code";
  "--raw-depend", Arg.Unit (fun () -> depend := OMRaw), " Invoke ocamldep and echo its raw output";
  "--stdlib", Arg.Set_string stdlib_path, "<directory> Specify where the standard library lies";
  "--strict", Arg.Set strict, " Warnings about the grammar are errors";
  "--suggest-comp-flags", Arg.Unit (fun () -> suggestion := SuggestCompFlags),
                          " Suggest compilation flags for ocaml{c,opt}";
  "--suggest-link-flags-byte", Arg.Unit (fun () -> suggestion := SuggestLinkFlags "cmo"),
                               " Suggest link flags for ocamlc";
  "--suggest-link-flags-opt", Arg.Unit (fun () -> suggestion := SuggestLinkFlags "cmx"),
                              " Suggest link flags for ocamlopt";
  "--suggest-menhirLib", Arg.Unit (fun () -> suggestion := SuggestWhereIsMenhirLibSource),
                         " Suggest where is MenhirLib";
  "--suggest-ocamlfind", Arg.Unit (fun () -> suggestion := SuggestUseOcamlfind),
                         " Show whether Menhir was installed using ocamlfind";
  "--table", Arg.Set table, " Use the table-based back-end";
  "--timings", Arg.Set timings, " Display internal timings";
  "--trace", Arg.Set trace, " Include tracing instructions in the generated code";
  "--unused-token", Arg.String ignore_unused_token, "<token> Do not warn that <token> is unused";
  "--unused-tokens", Arg.Set ignore_all_unused_tokens, " Do not warn about any unused token";
  "--update-errors", Arg.String set_update_errors, "<filename> Update auto-comments in a .messages file";
  "--version", Arg.Set version, " Show version number and exit";
  "-b", Arg.Set_string base, "<basename> Synonymous with --base <basename>";
  "-lg", Arg.Set_int logG, " Synonymous with --log-grammar";
  "-la", Arg.Set_int logA, " Synonymous with --log-automaton";
  "-lc", Arg.Set_int logC, " Synonymous with --log-code";
  "-t", Arg.Set table, " Synonymous with --table";
  "-v", Arg.Unit v, " Synonymous with --dump --explain";
]

let usage =
  sprintf "Usage: %s <options> <filenames>" Sys.argv.(0)

(* ------------------------------------------------------------------------- *)
(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* ------------------------------------------------------------------------- *)
(* If required, print a version number and stop. *)

let () =
  if !version then begin
    printf "menhir, version %s\n" Version.version;
    exit 0
  end

(* ------------------------------------------------------------------------- *)

(* Menhir is able to suggest compile and link flags to be passed to the
   OCaml compilers. If required, do so and stop. *)

(* If [--table] is not passed, no flags are necessary. If [--table] is
   passed, then [MenhirLib] needs to be visible (at compile time) and
   linked in (at link time). This is done either via [ocamlfind], if
   it was available at installation time, or manually. *)

(* The compilation flags are in fact meant to be used both at compile-
   and link-time. *)

let () =
  match !suggestion with
  | SuggestNothing ->
      ()
  | SuggestCompFlags ->
      if !table then
        if Installation.ocamlfind then
          printf "-package menhirLib\n%!"
        else
          printf "-I %s\n%!" Installation.libdir;
      exit 0
  | SuggestLinkFlags extension ->
      if !table then
        if Installation.ocamlfind then
          printf "-linkpkg\n%!"
        else
          printf "menhirLib.%s\n%!" extension;
      exit 0
  | SuggestWhereIsMenhirLibSource ->
      if Installation.ocamlfind then
        let _ = Sys.command "ocamlfind query menhirLib" in
        ()
      else
        printf "%s\n%!" Installation.libdir;
      exit 0
  | SuggestUseOcamlfind ->
      printf "%b\n" Installation.ocamlfind;
      exit 0

(* ------------------------------------------------------------------------- *)
(* Export the settings. *)

let stdlib_filename =
  !stdlib_path ^ "/standard.mly"

let filenames =
  StringSet.elements !filenames

let file_ext = if !coq then ".vy" else if !rust then ".rsy" else ".mly"

let base =
  if !base = "" then
    match filenames with
    | [] ->
        fprintf stderr "%s\n" usage;
        exit 1
    | [ filename ] ->
	    Filename.chop_suffix filename file_ext
    | _ ->
        fprintf stderr "Error: you must specify --base when providing multiple input files.\n";
        exit 1
  else
    !base

let filenames =
  if !no_stdlib || !coq then
    filenames
  else
    stdlib_filename :: filenames

let token_type_mode =
  !token_type_mode

let construction_mode =
  !construction_mode

let explain =
  !explain

let dump =
  !dump

let follow =
  !follow

let graph =
  !graph

let trace =
  !trace

let () =
  if !recovery then begin
    fprintf stderr "Error: --error-recovery mode is no longer supported.\n";
    exit 1
  end

let noprefix =
  !noprefix

let infer =
  !infer

let code_inlining =
  !code_inlining

let depend =
  !depend

let inline =
  !inline

let comment =
  !comment

let preprocess_mode =
  !preprocess_mode

let ocamlc =
  !ocamlc

let ocamldep =
  !ocamldep

let logG, logA, logC =
  !logG, !logA, !logC

let timings =
  !timings

let interpret =
  !interpret

let interpret_show_cst =
  !interpret_show_cst

let interpret_error =
  !interpret_error

let table =
  !table

let inspection =
  !inspection

let () =
  if inspection && not table then begin
    fprintf stderr "Error: --inspection requires --table.\n";
    exit 1
  end

let coq =
  !coq

let coq_no_complete =
  !coq_no_complete

let coq_no_actions =
  !coq_no_actions

let rust =
  !rust

let strict =
  !strict

let fixedexc =
  !fixedexc

let ignored_unused_tokens =
  !ignored_unused_tokens

let ignore_all_unused_tokens =
  !ignore_all_unused_tokens

let list_errors =
  !list_errors

let compile_errors =
  !compile_errors

let compare_errors =
  match !compare_errors with
  | [] ->
      None
  | [ filename2; filename1 ] -> (* LIFO *)
      Some (filename1, filename2)
  | _ ->
      eprintf
        "To compare two .messages files, please use:\n\
         --compare-errors <filename1> --compare-errors <filename2>.\n";
      exit 1

let update_errors =
  !update_errors

let echo_errors =
  !echo_errors

