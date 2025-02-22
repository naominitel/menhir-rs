{

open Lexing
open Parser
open Positions
open Keyword

(* ------------------------------------------------------------------------ *)

(* Short-hands. *)

let error1 pos =
  Error.error (Positions.one pos)

let error2 lexbuf =
  Error.error (Positions.two lexbuf.lex_start_p lexbuf.lex_curr_p)

(* ------------------------------------------------------------------------ *)

(* This wrapper saves the current lexeme start, invokes its argument,
   and restores it. This allows transmitting better positions to the
   parser. *)

let savestart lexbuf f =
  let startp = lexbuf.lex_start_p in
  let token = f lexbuf in
  lexbuf.lex_start_p <- startp;
  token

(* ------------------------------------------------------------------------ *)

(* Extracts a chunk out of the source file. *)

let chunk ofs1 ofs2 =
  let contents = Error.get_file_contents() in
  let len = ofs2 - ofs1 in
  String.sub contents ofs1 len

(* ------------------------------------------------------------------------ *)

(* Overwrites an old character with a new one at a specified
   offset in a [bytes] buffer. *)

let overwrite content offset c1 c2 =
  assert (Bytes.get content offset = c1);
  Bytes.set content offset c2

(* ------------------------------------------------------------------------ *)

(* Keyword recognition and construction. *)

(* A monster is a spot where we have identified a keyword in concrete syntax.
   We describe a monster as an object with the following methods: *)

type monster = {

  (* The position of the monster. *)
  pos: Positions.t;

  (* This method is passed an array of (optional) names for the producers,
     that is, the elements of the production's right-hand side. It may
     perform some checks and is allowed to fail. *)
  check: string option array -> unit;

  (* This method transforms the keyword (in place) into a conventional
     OCaml identifier. This is done by replacing '$', '(', and ')' with
     '_'. Bloody. The arguments are [ofs1] and [content]. [ofs1] is the
     offset where [content] begins in the source file. *)
  transform: int -> bytes -> unit;

  (* This is the keyword, in abstract syntax. *)
  keyword: keyword option;

}

(* ------------------------------------------------------------------------ *)

(* The [$syntaxerror] monster. *)

let syntaxerror pos : monster =
  let check _ = ()
  and transform ofs1 content =
    (* [$syntaxerror] is replaced with
       [(raise _eRR)]. Same length. *)
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    let source = "(raise _eRR)" in
    Bytes.blit_string source 0 content ofs (String.length source)
  and keyword =
    Some SyntaxError
  in
  { pos; check; transform; keyword }

(* ------------------------------------------------------------------------ *)

(* We check that every [$i] is within range. Also, we forbid using [$i]
   when a producer has been given a name; this is bad style and may be
   a mistake. (Plus, this simplies our life, as we rewrite [$i] to [_i],
   and we would have to rewrite it to a different identifier otherwise.) *)

let check_dollar pos i producers =
  if not (0 <= i - 1 && i - 1 < Array.length producers) then
    Error.error [pos] "$%d refers to a nonexistent symbol." i
  else
    producers.(i - 1) |> Option.iter (fun x ->
      Error.error [pos] "please do not say: $%d. Instead, say: %s." i x
    )

(* We check that every reference to a producer [x] in a position keyword,
   such as [$startpos(x)], exists. *)

let check_producer pos x producers =
  if not (List.mem (Some x) (Array.to_list producers)) then
    Error.error [pos] "%s refers to a nonexistent symbol." x

(* ------------------------------------------------------------------------ *)

(* The [$i] monster. *)

let dollar pos i : monster =
  let check = check_dollar pos i
  and transform ofs1 content =
    (* [$i] is replaced with [_i]. Thus, it is no longer a keyword. *)
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_'
  and keyword =
    None
  in
  { pos; check; transform; keyword }

(* ------------------------------------------------------------------------ *)

(* The position-keyword monster. The most horrible of all. *)

let position pos
  (where : string)
  (flavor : string)
  (i : string option) (x : string option)
=
  let none _ = () in
  let where, ofslpar (* offset of the opening parenthesis, if there is one *) =
    match where with
    | "symbolstart" -> WhereSymbolStart, 15
    | "start"       -> WhereStart,        9
    | "end"         -> WhereEnd,          7
    | _       -> assert false
  in
  let () =
    match where, i, x with
    | WhereSymbolStart, Some _, _
    | WhereSymbolStart, _, Some _ ->
        Error.error [pos] "$symbolstart%s does not take a parameter." flavor
    | _, _, _ ->
        ()
  in
  let flavor =
    match flavor with
    | "pos"   -> FlavorPosition
    | "ofs"   -> FlavorOffset
    | _       -> assert false
  in
  let subject, check =
    match i, x with
    | Some i, None ->
        let ii = int_of_string i in (* cannot fail *)
        if ii = 0 && where = WhereEnd then
          (* [$endpos($0)] *)
          Before, none
        else
          (* [$startpos($i)] is rewritten to [$startpos(_i)]. *)
          RightNamed ("_" ^ i), check_dollar pos ii
    | None, Some x ->
        (* [$startpos(x)] *)
        RightNamed x, check_producer pos x
    | None, None ->
        (* [$startpos] *)
        Left, none
    | Some _, Some _ ->
        assert false
  in
  let transform ofs1 content =
    let pos = start_of_position pos in
    let ofs = pos.pos_cnum - ofs1 in
    overwrite content ofs '$' '_';
    let ofslpar = ofs + ofslpar in
    match i, x with
    | None, Some x ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1 + String.length x) ')' '_'
    | Some i, None ->
        overwrite content ofslpar '(' '_';
        overwrite content (ofslpar + 1) '$' '_';
        overwrite content (ofslpar + 2 + String.length i) ')' '_'
    | _, _ ->
        ()
  in
  let keyword =
    Some (Position (subject, where, flavor))
  in
  { pos; check; transform; keyword }

(* ------------------------------------------------------------------------ *)

(* In an OCaml header, there should be no monsters. This is just a sanity
   check. *)

let no_monsters monsters =
  match monsters with
  | [] ->
      ()
  | monster :: _ ->
      Error.error [monster.pos]
        "a Menhir keyword cannot be used in an OCaml header."

(* ------------------------------------------------------------------------ *)

(* Creates a stretch. *)

let mk_stretch pos1 pos2 parenthesize monsters =
  (* Read the specified chunk of the file. *)
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let raw_content : string = chunk ofs1 ofs2 in
  (* Transform the monsters, if there are any. (This explicit test
     allows saving one string copy and keeping just one live copy.) *)
  let content : string =
    match monsters with
    | [] ->
        raw_content
    | _ :: _ ->
        let content : bytes = Bytes.of_string raw_content in
        List.iter (fun monster -> monster.transform ofs1 content) monsters;
        Bytes.unsafe_to_string content
  in
  (* Add whitespace so that the column numbers match those of the source file.
     If requested, add parentheses so that the semantic action can be inserted
     into other code without ambiguity. *)
  let content =
    if parenthesize then
      (String.make (pos1.pos_cnum - pos1.pos_bol - 1) ' ') ^ "(" ^ content ^ ")"
    else
      (String.make (pos1.pos_cnum - pos1.pos_bol) ' ') ^ content
  in
  Stretch.({
    stretch_filename = Error.get_filename();
    stretch_linenum = pos1.pos_lnum;
    stretch_linecount = pos2.pos_lnum - pos1.pos_lnum;
    stretch_content = content;
    stretch_raw_content = raw_content;
    stretch_keywords = Misc.map_opt (fun monster -> monster.keyword) monsters
  })

(* ------------------------------------------------------------------------ *)

(* OCaml's reserved words. *)

let reserved =
  let table = Hashtbl.create 149 in
  List.iter (fun word -> Hashtbl.add table word ()) [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "object";
    "of";
    "open";
    "or";
    "parser";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "mod";
    "land";
    "lor";
    "lxor";
    "lsl";
    "lsr";
    "asr";
  ];
  table

}

(* ------------------------------------------------------------------------ *)

(* Patterns. *)

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let poskeyword =
  '$'
  (("symbolstart" | "start" | "end") as where)
  (("pos" | "ofs") as flavor)
  ( '(' ( '$' (['0'-'9']+ as i) | ((lowercase identchar*) as x)) ')')?

let previouserror =
  "$previouserror"

let syntaxerror =
  "$syntaxerror"

(* ------------------------------------------------------------------------ *)

(* The lexer. *)

rule main = parse
| "%token"
    { TOKEN }
| "%type"
    { TYPE }
| "%left"
    { LEFT }
| "%right"
    { RIGHT }
| "%nonassoc"
    { NONASSOC }
| "%start"
    { START }
| "%prec"
    { PREC }
| "%public"
    { PUBLIC }
| "%parameter"
    { PARAMETER }
| "%inline"
    { INLINE }
| "%on_error_reduce"
    { ON_ERROR_REDUCE }
| "%%"
    { (* The token [PERCENTPERCENT] carries a stretch that contains
         everything that follows %% in the input file. This string
         must be created lazily. The parser decides (based on the
         context) whether this stretch is needed. If it is indeed
         needed, then constructing this stretch drives the lexer
         to the end of the file. *)
      PERCENTPERCENT (lazy (
        let openingpos = lexeme_end_p lexbuf in
        let closingpos = finish lexbuf in
        mk_stretch openingpos closingpos false []
      )) }
| ":"
    { COLON }
| ","
    { COMMA }
| "="
    { EQUAL }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| (lowercase identchar *) as id
    { if Hashtbl.mem reserved id then
        error2 lexbuf "this is an OCaml reserved word."
      else
        LID (with_pos (cpos lexbuf) id)
    }
| (uppercase identchar *) as id
    { UID (with_pos (cpos lexbuf) id) }
| "//" [^ '\010' '\013']* newline (* skip C++ style comment *)
| newline
    { new_line lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| "/*"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "<"
    { savestart lexbuf (ocamltype (lexeme_end_p lexbuf)) }
| "%{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_end_p lexbuf in
        let closingpos, monsters = action true openingpos [] lexbuf in
        no_monsters monsters;
        HEADER (mk_stretch openingpos closingpos false [])
      ) }
| "{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_end_p lexbuf in
        let closingpos, monsters = action false openingpos [] lexbuf in
        ACTION (
          fun (producers : string option array) ->
            List.iter (fun monster -> monster.check producers) monsters;
            let stretch = mk_stretch openingpos closingpos true monsters in
            Action.from_stretch stretch
        )
      ) }
| eof
    { EOF }
| _
    { error2 lexbuf "unexpected character(s)." }

(* ------------------------------------------------------------------------ *)

(* Skip C style comments. *)

and comment openingpos = parse
| newline
    { new_line lexbuf; comment openingpos lexbuf }
| "*/"
    { () }
| eof
    { error1 openingpos "unterminated comment." }
| _
    { comment openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect an O'Caml type delimited by angle brackets. Angle brackets can
   appear as part of O'Caml function types and variant types, so we must
   recognize them and *not* treat them as a closing bracket. *)

and ocamltype openingpos = parse
| "->"
| "[>"
    { ocamltype openingpos lexbuf }
| '>'
    { OCAMLTYPE (Stretch.Declared (mk_stretch openingpos (lexeme_start_p lexbuf) true [])) }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamltype openingpos lexbuf }
| newline
    { new_line lexbuf; ocamltype openingpos lexbuf }
| eof
    { error1 openingpos "unterminated OCaml type." }
| _
    { ocamltype openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Collect O'Caml code delimited by curly brackets. The monsters that are
   encountered along the way are accumulated in the list [monsters]. Nested
   curly brackets must be properly counted. Nested parentheses are also kept
   track of, so as to better report errors when they are not balanced. *)

and action percent openingpos monsters = parse
| '{'
    { let _, monsters = action false (lexeme_end_p lexbuf) monsters lexbuf in
      action percent openingpos monsters lexbuf }
| ("}" | "%}") as delimiter
    { match percent, delimiter with
      | true, "%}"
      | false, "}" ->
          (* This is the delimiter we were instructed to look for. *)
          lexeme_start_p lexbuf, monsters
      | _, _ ->
          (* This is not it. *)
          error1 openingpos "unbalanced opening brace."
    }
| '('
    { let _, monsters = parentheses (lexeme_end_p lexbuf) monsters lexbuf in
      action percent openingpos monsters lexbuf }
| '$' (['0'-'9']+ as i)
    { let monster = dollar (cpos lexbuf) (int_of_string i) in
      action percent openingpos (monster :: monsters) lexbuf }
| poskeyword
    { let monster = position (cpos lexbuf) where flavor i x in
      action percent openingpos (monster :: monsters) lexbuf }
| previouserror
    { error2 lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { let monster = syntaxerror (cpos lexbuf) in
      action percent openingpos (monster :: monsters) lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos monsters lexbuf }
| "'"
    { char lexbuf;
      action percent openingpos monsters lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos monsters lexbuf }
| newline
    { new_line lexbuf;
      action percent openingpos monsters lexbuf }
| ')'
| eof
    { error1 openingpos "unbalanced opening brace." }
| _
    { action percent openingpos monsters lexbuf }

(* ------------------------------------------------------------------------ *)

and parentheses openingpos monsters = parse
| '('
    { let _, monsters = parentheses (lexeme_end_p lexbuf) monsters lexbuf in
      parentheses openingpos monsters lexbuf }
| ')'
    { lexeme_start_p lexbuf, monsters }
| '{'
    { let _, monsters = action false (lexeme_end_p lexbuf) monsters lexbuf in
      parentheses openingpos monsters lexbuf }
| '$' (['0'-'9']+ as i)
    { let monster = dollar (cpos lexbuf) (int_of_string i) in
      parentheses openingpos (monster :: monsters) lexbuf }
| poskeyword
    { let monster = position (cpos lexbuf) where flavor i x in
      parentheses openingpos (monster :: monsters) lexbuf }
| previouserror
    { error2 lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { let monster = syntaxerror (cpos lexbuf) in
      parentheses openingpos (monster :: monsters) lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; parentheses openingpos monsters lexbuf }
| "'"
    { char lexbuf; parentheses openingpos monsters lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; parentheses openingpos monsters lexbuf }
| newline
    { new_line lexbuf; parentheses openingpos monsters lexbuf }
| '}'
| eof
    { error1 openingpos "unbalanced opening parenthesis." }
| _
    { parentheses openingpos monsters lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml comments. Comments can be nested and can contain
   strings or characters, which must be correctly analyzed. (A string
   could contain begin-of-comment or end-of-comment sequences, which
   must be ignored; a character could contain a begin-of-string
   sequence.) *)

and ocamlcomment openingpos = parse
| "*)"
    { () }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| "'"
    { char lexbuf; ocamlcomment openingpos lexbuf }
| newline
    { new_line lexbuf; ocamlcomment openingpos lexbuf }
| eof
    { error1 openingpos "unterminated OCaml comment." }
| _
    { ocamlcomment openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml strings. *)

and string openingpos = parse
| '"'
   { () }
| '\\' newline
| newline
   { new_line lexbuf; string openingpos lexbuf }
| '\\' _
   (* Upon finding a backslash, skip the character that follows,
      unless it is a newline. Pretty crude, but should work. *)
   { string openingpos lexbuf }
| eof
   { error1 openingpos "unterminated OCaml string." }
| _
   { string openingpos lexbuf }

(* ------------------------------------------------------------------------ *)

(* Skip O'Caml characters. A lone quote character is legal inside
   a comment, so if we don't recognize the matching closing quote,
   we simply abandon. *)

and char = parse
| '\\'? newline "'"
   { new_line lexbuf }
| [^ '\\' '\''] "'"
| '\\' _ "'"
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
| ""
   { () }

(* ------------------------------------------------------------------------ *)

(* Read until the end of the file. This is used after finding a %%
   that marks the end of the grammar specification. We update the
   current position as we go. This allows us to build a stretch
   for the trailer. *)

and finish = parse
| newline
    { new_line lexbuf; finish lexbuf }
| eof
    { lexeme_start_p lexbuf }
| _
    { finish lexbuf }

