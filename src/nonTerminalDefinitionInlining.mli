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

(** [inline g] traverses the rules of [g] and inlines the non terminal
    definitions that are marked with [%inline]. It returns a pair of the transformed
    grammar and a flag that tells whether any inlining was actually done. *)
val inline: UnparameterizedSyntax.grammar -> UnparameterizedSyntax.grammar * bool
