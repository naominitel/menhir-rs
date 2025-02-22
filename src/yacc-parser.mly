/* This is the crude version of the parser. It is meant to be processed
   by ocamlyacc. Its existence is necessary for bootstrapping. It is kept
   in sync with [fancy-parser], with a few differences:
   1. [fancy-parser] exploits many features of Menhir;
   2. [fancy-parser] performs slightly more refined error handling;
   3. [fancy-parser] supports anonymous rules. */

%{

open ConcreteSyntax
open Syntax
open Positions

%}

%token TOKEN TYPE LEFT RIGHT NONASSOC START PREC PUBLIC COLON BAR EOF EQUAL
%token INLINE LPAREN RPAREN COMMA QUESTION STAR PLUS PARAMETER ON_ERROR_REDUCE
%token <string Positions.located> LID UID
%token <Stretch.t> HEADER
%token <Stretch.ocamltype> OCAMLTYPE
%token <Stretch.t Lazy.t> PERCENTPERCENT
%token <Syntax.identifier option array -> Syntax.action> ACTION
%start grammar
%type <ConcreteSyntax.grammar> grammar

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
  declarations PERCENTPERCENT rules trailer
    {
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = List.rev $1;
        pg_rules             = $3;
        pg_trailer           = $4
      }
    }

trailer:
  EOF
    { None }
| PERCENTPERCENT /* followed by actual trailer */
    { Some (Lazy.force $1) }

/* ------------------------------------------------------------------------- */
/* A declaration is an %{ OCaml header %}, or a %token, %start,
   %type, %left, %right, or %nonassoc declaration. */

declarations:
  /* epsilon */
    { [] }
| declarations declaration
    { $2 @ $1 }

declaration:
| HEADER /* lexically delimited by %{ ... %} */
    { [ unknown_pos (DCode $1) ] }

| TOKEN optional_ocamltype terminals
    { List.map (Positions.map (fun terminal -> DToken ($2, terminal))) $3 }

| START nonterminals
    { List.map (Positions.map (fun nonterminal -> DStart nonterminal)) $2 }

| TYPE OCAMLTYPE actuals
    { List.map (Positions.map (fun nt -> DType ($2, nt)))
        (List.map Parameters.with_pos $3) }

| START OCAMLTYPE nonterminals
    /* %start <ocamltype> foo is syntactic sugar for %start foo %type <ocamltype> foo */
    { Misc.mapd (fun ntloc ->
        Positions.mapd (fun nt -> DStart nt, DType ($2, ParameterVar ntloc)) ntloc) $3 }

| priority_keyword symbols
    { let prec = ParserAux.new_precedence_level (rhs_start_pos 1) (rhs_end_pos 1) in
      List.map (Positions.map (fun symbol -> DTokenProperties (symbol, $1, prec))) $2 }

| PARAMETER OCAMLTYPE
    { [ unknown_pos (DParameter $2) ] }

| ON_ERROR_REDUCE actuals
    { List.map (Positions.map (fun nt -> DOnErrorReduce nt))
        (List.map Parameters.with_pos $2) }

optional_ocamltype:
  /* epsilon */
    { None }
| OCAMLTYPE /* lexically delimited by angle brackets */
    { Some $1 }

priority_keyword:
  LEFT
    { LeftAssoc }
| RIGHT
    { RightAssoc }
| NONASSOC
    { NonAssoc }

/* ------------------------------------------------------------------------- */
/* A symbol is a terminal or nonterminal symbol. One would like to
   require nonterminal symbols to begin with a lowercase letter, so as
   to lexically distinguish them from terminal symbols, which must
   begin with an uppercase letter. However, for compatibility with
   ocamlyacc, this is impossible. It can be required only for
   nonterminal symbols that are also start symbols. */

symbols:
  /* epsilon */
    { [] }
| symbols optional_comma symbol
    { $3 :: $1 }

symbol:
  LID
    { $1 }
| UID
    { $1 }

optional_comma:
  /* epsilon */
    { () }
| COMMA
    { () }

/* ------------------------------------------------------------------------- */
/* Terminals must begin with an uppercase letter. Nonterminals that are
   declared to be start symbols must begin with a lowercase letter. */

terminals:
  /* epsilon */
    { [] }
| terminals optional_comma UID
    { $3 :: $1 }

nonterminals:
  /* epsilon */
    { [] }
| nonterminals LID
    { $2 :: $1 }

/* ------------------------------------------------------------------------- */
/* A rule defines a symbol. It is optionally declared %public, and optionally
   carries a number of formal parameters. The right-hand side of the definition
   consists of a list of production groups. */

rules:
  /* epsilon */
    { [] }
| rules rule
    { $2 :: $1 }

rule:
  flags
  symbol
  optional_formal_parameters
  COLON
  optional_bar
  production_group production_groups
    {
      let public, inline = $1 in
      { pr_public_flag = public;
        pr_inline_flag = inline;
        pr_nt          = Positions.value $2;
        pr_positions   = [ Positions.position $2 ];
        pr_parameters  = $3;
        pr_branches    = List.flatten ($6 :: List.rev $7)
      }
    }

flags:
  /* epsilon */
    { false, false }
| PUBLIC
    { true, false }
| INLINE
    { false, true }
| PUBLIC INLINE
    { true, true }
| INLINE PUBLIC
    { true, true }

/* ------------------------------------------------------------------------- */
/* Parameters are surroundered with parentheses and delimited by commas.
   The syntax of actual parameters allows applications, whereas the syntax
   of formal parameters does not. It also allows use of the "?", "+", and
   "*" shortcuts. */

optional_formal_parameters:
  /* epsilon */
    { [] }
| LPAREN formal_parameters RPAREN
    { $2 }

formal_parameters:
  symbol
    { [ Positions.value $1 ] }
| symbol COMMA formal_parameters
    { Positions.value $1 :: $3 }

optional_actuals:
  /* epsilon */
    { [] }
| LPAREN actuals_comma RPAREN
    { $2 }

actuals_comma:
  actual
    { [ $1 ] }
| actual COMMA actuals_comma
    { $1 :: $3 }

actual:
  symbol optional_actuals
    { Parameters.app $1 $2 }
| actual modifier
    { ParameterApp ($2, [ $1 ]) }

actuals:
  /* epsilon */
    { [] }
| actuals optional_comma actual
    { $3::$1 }

optional_bar:
  /* epsilon */ %prec no_optional_bar
    { () }
| BAR
    { () }

/* ------------------------------------------------------------------------- */
/* The "?", "+", and "*" modifiers are short-hands for applications of
   certain parameterized nonterminals, defined in the standard library. */

modifier:
  QUESTION
    { unknown_pos "option" }
| PLUS
    { unknown_pos "nonempty_list" }
| STAR
    { unknown_pos "list" }

/* ------------------------------------------------------------------------- */
/* A production group consists of a list of productions, followed by a
   semantic action and an optional precedence specification. */

production_groups:
  /* epsilon */
    { [] }
| production_groups BAR production_group
    { $3 :: $1 }

production_group:
  productions ACTION /* action is lexically delimited by braces */ optional_precedence
    {
      let productions, action, oprec2 = $1, $2, $3 in
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pr_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action. Also, check that every [$i]
           is within bounds. *)
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

optional_precedence:
  /* epsilon */
    { None }
| PREC symbol
    { Some $2 }

/* ------------------------------------------------------------------------- */
/* A production is a list of producers, optionally followed by a
   precedence declaration. Lists of productions are nonempty and
   separated with bars. */

productions:
  production
    { [ $1 ] }
| production bar_productions
    { $1 :: $2 }

bar_productions:
  BAR production
    { [ $2 ] }
| BAR production bar_productions
    { $2 :: $3 }

production:
  producers optional_precedence
    { List.rev $1,
      $2,
      ParserAux.new_production_level(),
      Positions.lex_join (symbol_start_pos()) (symbol_end_pos())
    }

producers:
  /* epsilon */
    { [] }
| producers producer
    { $2 :: $1 }

/* ------------------------------------------------------------------------- */
/* A producer is an actual parameter, possibly preceded by a
   binding. */

producer:
| actual
    { Positions.lex_join (symbol_start_pos()) (symbol_end_pos()), None, $1 }
| LID EQUAL actual
    { Positions.lex_join (symbol_start_pos()) (symbol_end_pos()), Some $1, $3 }

%%

