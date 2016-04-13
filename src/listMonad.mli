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

(** Monad type which represents a list of results. *)
type 'a m = 'a list

(** [bind x f] applies [f] to a list of results, returning
    a list of results. *)
val bind: 'a m -> ('a -> 'b m) -> 'b m
val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m

(** [return x] is the left and right unit of [bind]. *)
val return: 'a -> 'a m


