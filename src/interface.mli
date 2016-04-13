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

(* This module defines the interface of the generated parser. *)

(* This is the [Error] exception. *)

val excname: string
val excdef: IL.excdef

(* The type of the entry point for the start symbol [nt]. *)

val entrytypescheme: UnparameterizedSyntax.grammar -> string -> IL.typescheme

(* The name of the interpreter sub-module, when the table back-end
   is used. *)

val interpreter: string

(* The type ['a checkpoint], defined in the interpreter sub-module. *)

val checkpoint: IL.typ -> IL.typ

(* The name of the sub-module that contains the incremental entry points. *)

val incremental: string

(* The name of the sub-module that contains the inspection API. *)

val inspection: string

(* This writes the interface of the generated parser to the [.mli] file. *)

val write: UnparameterizedSyntax.grammar -> unit -> unit

