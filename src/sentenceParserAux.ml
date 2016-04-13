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

open Grammar

type terminals =
  Terminal.t list

type sentence =
  Nonterminal.t option * terminals

type located_sentence =
  Positions.positions * sentence

type comment =
  string

type 'a or_comment =
| Thing of 'a
| Comment of comment

let or_comment_iter f = function
  | Thing s ->
      f s
  | Comment _ ->
      ()

let or_comment_map f = function
  | Thing s ->
      Thing (f s)
  | Comment c ->
      Comment c

let unThing = function
  | Thing x ->
      [ x ]
  | Comment _ ->
      []
