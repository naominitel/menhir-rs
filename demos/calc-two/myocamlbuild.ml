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

open Ocamlbuild_plugin
open Command


(* Define ocamlbuild flags [only_tokens] and [external_tokens(Foo)] and
   [unused_token(Bar)] which correspond to menhir's [--only-tokens] and
   [--external-tokens Foo] and [--unused-token Bar]. When they are used, these
   flags should be passed both to [menhir] and to [menhir --raw-depend]. *)

let menhir_flags() =
  List.iter (fun mode ->
    
    flag [ mode; "only_tokens" ] (S[A "--only-tokens"]);

    pflag [ mode ] "external_tokens" (fun name ->
      S[A "--external-tokens"; A name]
    );

    pflag [ mode ] "unused_token" (fun name ->
      S[A "--unused-token"; A name]
    )
      
  ) [ "menhir"; "menhir_ocamldep" ]

let () =
  dispatch (fun event ->
    match event with
    | After_rules ->
      menhir_flags()
    | _ -> ()
  )
