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

let menhir_flags() =
  (* Define two ocamlbuild flags [only_tokens] and [external_tokens(Foo)]
     which correspond to menhir's [--only-tokens] and [--external-tokens Foo].
     When they are used, these flags should be passed both to [menhir] and to
     [menhir --raw-depend]. *)
  List.iter (fun mode ->
    
    flag [ mode; "only_tokens" ] (S[A "--only-tokens"]);

    pflag [ mode ] "external_tokens" (fun name ->
      S[A "--external-tokens"; A name]
    )
  
  ) [ "menhir"; "menhir_ocamldep" ]

let () =
  dispatch (fun event ->
    match event with
    | After_rules ->
        menhir_flags()
    | _ -> ()
  )
