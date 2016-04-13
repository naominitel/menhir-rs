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

(* The Boolean lattice. *)

type property =
    bool

let bottom =
  false

let equal (b1 : bool) (b2 : bool) =
  b1 = b2

let is_maximal b =
  b

let union (b1 : bool) (b2 : bool) =
  b1 || b2

