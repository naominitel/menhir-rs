(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

(* This is the ocamlyacc-specific driver. There is nothing special
   to do. We handle syntax errors in a minimalistic manner. This
   error handling code will be exercised only if there is a syntax
   error in [fancy-parser.mly], during stage 2 of the bootstrap
   process. *)

let grammar lexer lexbuf =
  try
    Parser.grammar lexer lexbuf
  with Parsing.Parse_error ->
    Error.error (Positions.lexbuf lexbuf) "syntax error."

