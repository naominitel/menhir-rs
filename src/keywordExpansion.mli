open UnparameterizedSyntax

(* [expand_grammar] expands away the keywords [$startpos] and [$endpos], as well
   the entire [ofs] family of keywords. Doing this early simplifies some aspects
   later on, in particular %inlining. *)

val expand_grammar: grammar -> grammar

