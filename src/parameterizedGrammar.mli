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

(* This turns a grammar where nonterminal symbols can be parameterized
   into a grammar where nonterminal symbols are not parameterized. The
   transformation is a textual expansion process, whose termination is
   guaranteed by a simple type system.

   Expansion creates new nonterminal symbols whose names contain
   parentheses and commas. These names can be printed directly in
   informational messages (error messages, conflict reports,
   descriptions of the automaton, etc.). However, they must be
   sanitized via [Misc.normalize] when printed in a context where a
   valid identifier is expected. *)

val expand : InternalSyntax.grammar -> UnparameterizedSyntax.grammar


