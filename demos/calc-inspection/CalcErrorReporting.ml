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

open Parser
open Parser.MenhirInterpreter

(* In order to submit artificial tokens to the parser, we need a function that
   converts a terminal symbol to a (dummy) token. Unfortunately, we cannot (in
   general) auto-generate this code, because it requires making up semantic
   values of arbitrary OCaml types. *)

let terminal2token (type a) (symbol : a terminal) : token =
  match symbol with
  | T_TIMES ->
      TIMES
  | T_RPAREN ->
      RPAREN
  | T_PLUS ->
      PLUS
  | T_MINUS ->
      MINUS
  | T_LPAREN ->
      LPAREN
  | T_INT ->
      INT 0
  | T_EOL ->
      EOL
  | T_DIV ->
      DIV
  | T_error ->
      assert false

