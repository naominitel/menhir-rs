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

(* Let's do floating-point evaluation, for a change. *)

module FloatSemantics = struct

  type number =
      float

  let inject =
    float_of_int

  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let (~- ) = (~-. )

end

(* Let us now specialize our parameterized parser. *)

module FloatParser =
  Parser.Make(FloatSemantics)

(* The rest is as usual. *)

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    (* Run the parser on this line of input. *)
    Printf.printf "%f\n%!" (FloatParser.main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | FloatParser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
let () =
  repeat (Lexing.from_channel stdin)

