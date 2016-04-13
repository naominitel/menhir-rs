(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Paris-Rocquencourt                            *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2015 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the GNU Library General Public License, with the   *)
(*  special exception on linking described in file LICENSE.               *)
(*                                                                        *)
(**************************************************************************)

(* This functor is invoked inside the generated parser, in [--table] mode. It
   produces no code! It simply constructs the types [symbol] and [xsymbol] on
   top of the generated types [terminal] and [nonterminal]. *)

module Symbols (T : sig

  type 'a terminal
  type 'a nonterminal

end)

: IncrementalEngine.SYMBOLS
  with type 'a terminal := 'a T.terminal
   and type 'a nonterminal := 'a T.nonterminal

(* This functor is invoked inside the generated parser, in [--table] mode. It
   constructs the inspection API on top of the inspection tables described in
   [InspectionTableFormat]. *)

module Make
  (B : TableFormat.TABLES)
  (T : InspectionTableFormat.TABLES
       with type 'a lr1state = int)

: IncrementalEngine.INSPECTION
  with type 'a terminal := 'a T.terminal
   and type 'a nonterminal := 'a T.nonterminal
   and type 'a lr1state := 'a T.lr1state
   and type production := int

