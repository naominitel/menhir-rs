(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Paris-Rocquencourt                            *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2015 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* This module drives the front-end. It opens and parses the input files,
   which yields a number of partial grammars. It joins these grammars, expands
   them to get rid of parameterized nonterminals, and performs reachability
   analysis. This yields a single unified grammar. It then performs type
   inference. This yields the grammar that the back-end works with (often
   through the interface provided by module [Grammar]). *)

val grammar: UnparameterizedSyntax.grammar

