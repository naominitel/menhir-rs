/* This is the fancy version of the parser, to be processed by menhir.
   It is kept in sync with [Parser], but exercises menhir's features. */

/* As of 2014/12/02, the $previouserror keyword and the --error-recovery
   mode no longer exists. Thus, we replace all calls to [Error.signal]
   with calls to [Error.error], and report just one error. */

/* ------------------------------------------------------------------------- */
/* Imports. */

%{

open ConcreteSyntax
open Syntax
open Positions

%}

/* ------------------------------------------------------------------------- */
/* Tokens. */

%token TOKEN TYPE LEFT RIGHT NONASSOC START PREC PUBLIC COLON BAR EOF EQUAL
%token INLINE LPAREN RPAREN COMMA QUESTION STAR PLUS PARAMETER ON_ERROR_REDUCE
%token <string Positions.located> LID UID
%token <Stretch.t> HEADER
%token <Stretch.ocamltype> OCAMLTYPE
%token <Stretch.t Lazy.t> PERCENTPERCENT
%token <Syntax.identifier option array -> Action.t> ACTION

/* ------------------------------------------------------------------------- */
/* Start symbol. */

%start <ConcreteSyntax.grammar> grammar

/* ------------------------------------------------------------------------- */
/* Priorities. */

/* These declarations solve a shift-reduce conflict in favor of
   shifting: when the declaration of a non-terminal symbol begins with
   a leading bar, it is understood as an (insignificant) leading
   optional bar, *not* as an empty right-hand side followed by a bar.
   This ambiguity arises due to the existence of a new notation for
   letting several productions share a single semantic action. */

%nonassoc no_optional_bar
%nonassoc BAR

%%

/* ------------------------------------------------------------------------- */
/* A grammar consists of declarations and rules, followed by an optional
   trailer, which we do not parse. */

grammar:
  ds = declaration* PERCENTPERCENT rs = rule* t = trailer
    {
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = List.flatten ds;
        pg_rules             = rs @ ParserAux.rules();
        pg_trailer           = t
      }
    }

/* ------------------------------------------------------------------------- */
/* A declaration is an %{ OCaml header %}, or a %token, %start,
   %type, %left, %right, or %nonassoc declaration. */

declaration:

| h = HEADER /* lexically delimited by %{ ... %} */
    { [ with_poss $startpos $endpos (DCode h) ] }

| TOKEN t = OCAMLTYPE? ts = clist(terminal)
    { List.map (Positions.map (fun terminal -> DToken (t, terminal))) ts }

| START t = OCAMLTYPE? nts = clist(nonterminal)
    /* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo */
    {
      match t with
      | None ->
          List.map (Positions.map (fun nonterminal -> DStart nonterminal)) nts
      | Some t ->
          Misc.mapd (fun ntloc ->
            Positions.mapd (fun nt -> DStart nt, DType (t, ParameterVar ntloc)) ntloc) nts
    }

| TYPE t = OCAMLTYPE ss = clist(strict_actual)
    { List.map (Positions.map (fun nt -> DType (t, nt)))
        (List.map Parameters.with_pos ss) }

| k = priority_keyword ss = clist(symbol)
    { let prec = ParserAux.new_precedence_level $startpos(k) $endpos(k) in
      List.map (Positions.map (fun symbol -> DTokenProperties (symbol, k, prec))) ss }

| PARAMETER t = OCAMLTYPE
    { [ with_poss $startpos $endpos (DParameter t) ] }

| ON_ERROR_REDUCE ss = clist(strict_actual)
    { List.map (Positions.map (fun nt -> DOnErrorReduce nt))
        (List.map Parameters.with_pos ss) }

/* This production recognizes tokens that are valid in the rules section,
   but not in the declarations section. This is a hint that a %% was
   forgotten. */

| rule_specific_token
    {
      Error.error (Positions.two $startpos $endpos)
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    }

priority_keyword:
  LEFT
    { LeftAssoc }
| RIGHT
    { RightAssoc }
| NONASSOC
    { NonAssoc }

%inline rule_specific_token:
| PUBLIC
| INLINE
| COLON
| EOF
    { () }

/* ------------------------------------------------------------------------- */
/* Our lists of symbols are separated with optional commas. Order is
   irrelevant. */

%inline clist(X):
  xs = separated_nonempty_list(COMMA?, X)
    { xs }

/* ------------------------------------------------------------------------- */
/* A symbol is a terminal or nonterminal symbol. One would like to
   require nonterminal symbols to begin with a lowercase letter, so as
   to lexically distinguish them from terminal symbols, which must
   begin with an uppercase letter. However, for compatibility with
   ocamlyacc, this is impossible. It can be required only for
   nonterminal symbols that are also start symbols. */

symbol:
  id = LID
| id = UID
    { id }

/* ------------------------------------------------------------------------- */
/* Terminals must begin with an uppercase letter. Nonterminals that are
   declared to be start symbols must begin with a lowercase letter. */

%inline terminal:
  id = UID
    { id }

%inline nonterminal:
  id = LID
    { id }

/* ------------------------------------------------------------------------- */
/* A rule defines a symbol. It is optionally declared %public, and optionally
   carries a number of formal parameters. The right-hand side of the definition
   consists of a list of productions. */

rule:
  flags = flags                                             /* flags */
  symbol = symbol                                           /* the symbol that is being defined */
  params = plist(symbol)                                    /* formal parameters */
  COLON
  optional_bar
  branches = branches
    {
      let public, inline = flags in
      {
        pr_public_flag = public;
        pr_inline_flag = inline;
        pr_nt          = Positions.value symbol;
        pr_positions   = [ Positions.position symbol ];
        pr_parameters  = List.map Positions.value params;
        pr_branches    = branches
      }
    }

%inline branches:
  prods = separated_nonempty_list(BAR, production_group)
    { List.flatten prods }

flags:
  /* epsilon */
    { false, false }
| PUBLIC
    { true, false }
| INLINE
    { false, true }
| PUBLIC INLINE
| INLINE PUBLIC
    { true, true }

optional_bar:
  /* epsilon */ %prec no_optional_bar
| BAR
    { () }

/* ------------------------------------------------------------------------- */
/* A production group consists of a list of productions, followed by a
   semantic action and an optional precedence specification. */

production_group:
  productions = separated_nonempty_list(BAR, production)
  action = ACTION
  oprec2 = ioption(precedence)
    {
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pr_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action. Also, check that every [$i]
           is within bounds. *)
        let action : Syntax.identifier option array -> Action.t = action in
        let pr_action = action (ParserAux.producer_names producers) in
        {
          pr_producers;
          pr_action;
          pr_branch_prec_annotation   = ParserAux.override pos oprec1 oprec2;
          pr_branch_production_level  = level;
          pr_branch_position          = pos
        })
      productions
    }

%inline precedence:
  PREC symbol = symbol
    { symbol }

/* ------------------------------------------------------------------------- */
/* A production is a list of producers, optionally followed by a
   precedence declaration. */

production:
  producers = producer* oprec = ioption(precedence)
    { producers,
      oprec,
      ParserAux.new_production_level(),
      Positions.lex_join $startpos $endpos
    }

/* ------------------------------------------------------------------------- */
/* A producer is an actual parameter, possibly preceded by a
   binding.

   Because both [ioption] and [terminated] are defined as inlined by
   the standard library, this definition expands to two productions,
   one of which begins with id = LID, the other of which begins with
   p = actual. The token LID is in FIRST(actual),
   but the LR(1) formalism can deal with that. If [option] was used
   instead of [ioption], an LR(1) conflict would arise -- looking
   ahead at LID would not allow determining whether to reduce an
   empty [option] or to shift. */

producer:
| id = ioption(terminated(LID, EQUAL)) p = actual
    { position (with_poss $startpos $endpos ()), id, p }

/* ------------------------------------------------------------------------- */
/* The ideal syntax of actual parameters includes:
   1. a symbol, optionally applied to a list of actual parameters;
   2. an actual parameter followed with a modifier;
   3. an anonymous rule. (Not delimited by parentheses! Otherwise
      one would often end up writing two pairs of parentheses.) */

/* In order to avoid a few ambiguities, we restrict this ideal syntax as
   follows:
   a. Within a %type declaration, we use [strict_actual], which
      allows 1- and 2- (this is undocumented; the documentation says we
      require a symbol) but not 3-, which would not make semantic sense
      anyway.
   b. Within a producer, we use [actual], which allows 1- and
      2- but not 3-. Case 3- is allowed by switching to [lax_actual]
      within the actual arguments of an application, which are clearly
      delimited by parentheses and commas.
   c. In front of a modifier, we can never allow [lax_actual],
      as this would create an ambiguity: basically, [A | B?] could be
      interpreted either as [(A | B)?] or as [A | (B?)].
*/

%inline generic_actual(A, B):
(* 1- *)
  symbol = symbol actuals = plist(A)
    { Parameters.app symbol actuals }
(* 2- *)
| p = B m = modifier
    { ParameterApp (m, [ p ]) }

strict_actual:
  p = generic_actual(strict_actual, strict_actual)
    { p }

actual:
  p = generic_actual(lax_actual, actual)
    { p }

lax_actual:
  p = generic_actual(lax_actual, /* cannot be lax_ */ actual)
    { p }
(* 3- *)
| /* leading bar disallowed */
  branches = branches
    { let position = position (with_poss $startpos $endpos ()) in
      let symbol = ParserAux.anonymous position branches in
      ParameterVar (with_pos position symbol) }

/* ------------------------------------------------------------------------- */
/* Formal or actual parameter lists are delimited with parentheses and
   separated with commas. They are optional. */

%inline plist(X):
  params = loption(delimited(LPAREN, separated_nonempty_list(COMMA, X), RPAREN))
    { params }

/* ------------------------------------------------------------------------- */
/* The "?", "+", and "*" modifiers are short-hands for applications of
   certain parameterized nonterminals, defined in the standard library. */

modifier:
  QUESTION
    { with_poss $startpos $endpos "option" }
| PLUS
    { with_poss $startpos $endpos "nonempty_list" }
| STAR
    { with_poss $startpos $endpos "list" }

/* ------------------------------------------------------------------------- */
/* A trailer is announced by %%, but is optional. */

trailer:
  EOF
    { None }
| p = PERCENTPERCENT /* followed by actual trailer */
    { Some (Lazy.force p) }

%%

