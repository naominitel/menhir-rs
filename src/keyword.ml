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

(* This module provides some type and function definitions
   that help deal with the keywords that we recognize within
   semantic actions. *)

(* ------------------------------------------------------------------------- *)
(* Types. *)

(* The user can request position information either at type
   [int] (a simple offset) or at type [Lexing.position]. *)

type flavor =
  | FlavorOffset
  | FlavorPosition

(* The user can request position information about the $start or $end
   of a symbol. Also, $symbolstart requests the computation of the
   start position of the first nonempty element in a production. *)

type where =
| WhereSymbolStart
| WhereStart
| WhereEnd

(* The user can request position information about a production's
   left-hand side or about one of the symbols in its right-hand
   side, which he can refer to by position or by name. *)

type subject =
  | Before
  | Left
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information. *)

type keyword =
  | Position of subject * where * flavor
  | SyntaxError

(* ------------------------------------------------------------------------- *)
(* These auxiliary functions help map a [Position] keyword to the
   name of the variable that the keyword is replaced with. *)

let where = function
  | WhereSymbolStart ->
      "symbolstart"
  | WhereStart ->
      "start"
  | WhereEnd ->
      "end"

let subject = function
  | Before ->
      "__0_"
  | Left ->
      ""
  | RightNamed id ->
      Printf.sprintf "_%s_" id

let flavor = function
  | FlavorPosition ->
      "pos"
  | FlavorOffset ->
      "ofs"

let posvar s w f =
  Printf.sprintf "_%s%s%s" (where w) (flavor f) (subject s)

(* ------------------------------------------------------------------------- *)
(* Sets of keywords. *)

module KeywordSet = struct

  include Set.Make (struct
    type t = keyword
    let compare = compare
  end)

  let map f keywords =
    fold (fun keyword accu ->
      add (f keyword) accu
    ) keywords empty

end
