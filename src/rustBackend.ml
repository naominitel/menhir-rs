open Format
open Grammar

type variant = {
    ctor: string ;
    types: string list
}

type enum = variant list

let rec pp_list printer sep ff = function
    | [] -> ()
    | [x] -> printer ff x
    | (x :: l) ->
        Format.fprintf ff "%a%s" printer x sep ;
        pp_list printer sep ff l

let pp_array printer sep ff arr =
    let rec iter idx =
        if idx == Array.length arr then ()
        else if idx == Array.length arr - 1 then printer ff arr.(idx)
        else begin
            Format.fprintf ff "%a%s" printer arr.(idx) sep ;
            iter (idx + 1)
        end
    in iter 0

let pp_use ff =
    Format.fprintf ff
        "extern crate menhir_runtime;\n\
         use std::ops::Index;
         use self::menhir_runtime::{Action, parse, SemAct};"

let pp_variant ff = function
    | { ctor ; types = [] } -> fprintf ff "%s" ctor
    | { ctor ; types } -> fprintf ff "%s(%a)" ctor (pp_list pp_print_string ", ") types

let pp_enum ff pub name variants =
    Format.fprintf ff
        "%senum %s {\n%a\n}\n\n"
        (if pub then "pub " else "")
        name (pp_list pp_variant ",\n") variants

let pp_terminals ff =
    pp_enum ff true "Token"

let pp_yytype ff =
    pp_enum ff false "YYType"

let variant name ocamltype =
    let tys = match ocamltype with
        | Some (Stretch.Declared t) -> [t.Stretch.stretch_content]
        | Some (Stretch.Inferred _) -> failwith "TODO: can we have inferred types?"
        | None -> []
    in { ctor = name ; types = tys }

let pp_parser_enums ff =
    let terminals =
        Terminal.(fold
            (fun t acc ->
                  if real t then (variant (print t) (ocamltype t)) :: acc
                  else acc)
            [])
    in

    let nt_ocamltype t =
        if Nonterminal.is_start t
        then Some (Nonterminal.ocamltype_of_start_symbol t)
        else Nonterminal.ocamltype t
    in

    let nonterminals =
        Nonterminal.(foldx
            (fun t acc -> (variant (print false t) (nt_ocamltype t)) :: acc) [])
    in

    pp_terminals ff terminals ;
    pp_yytype ff (terminals @ nonterminals)

type action =
    | Shift of int               (* state number *)
    | Reduce of Production.index (* (semantic) action number *)
    | Err                        (* error *)
    | Acc                        (* accepting state *)

let pp_semact ff prod =
    if Production.is_start prod then
        Format.fprintf ff "None"
    else
        Format.fprintf ff "Some(RULE_%d)" @@ Production.p2i prod

let pp_act ff = function
    | Shift x  -> Format.fprintf ff "Action::Shift(%d)" x
    | Reduce x -> Format.fprintf ff "Action::Reduce(%a)" pp_semact x
    | Err      -> Format.fprintf ff "Action::Err"
    | Acc      -> Format.fprintf ff "Action::Acc"

let pp_parse_table ff =
    (* Build parse table *)
    let goto_table  = Array.init Lr1.n (fun _ -> Array.make (Nonterminal.n) 0) in
    let parse_table = Array.init Lr1.n (fun _ -> Array.make (Terminal.n) Err)  in
    let default_table = Array.init Lr1.n (fun _ -> None) in

    Lr1.iter
        (fun node ->
             let st = Lr1.number node in

             match Invariant.has_default_reduction node with
                 | None ->
                     SymbolMap.iter
                         (fun sym dst ->
                              let dst = Lr1.number dst in match sym with
                                  | Symbol.N nt -> goto_table.(st).(Nonterminal.n2i nt) <- 1 + dst
                                  | Symbol.T t  -> parse_table.(st).(Terminal.t2i t) <- Shift dst)
                         (Lr1.transitions node) ;

                     TerminalMap.iter
                         (fun t rules ->
                              parse_table.(st).(Terminal.t2i t) <-
                                  Reduce (List.hd rules))
                         (Lr1.reductions node)
                 | Some (prod, _) ->
                     default_table.(st) <- Some(prod)) ;

    (* the error bitmap. *)
    let error = Array.make (Lr1.n * Terminal.n) false in
    Array.iteri
        (fun st a ->
             Array.iteri (fun t act -> error.(st * Terminal.n + t) <- act = Err) a)
        parse_table ;

    Format.fprintf ff "const ERROR_TABLE: [bool ; %d] = [\n%a\n];\n\n"
        (Lr1.n * Terminal.n)
        (pp_array
             (fun ff v -> fprintf ff "%s" @@ if v then "true" else "false") ", ")
        error ;

    (* compress the parse table *)
    let (displacement, parse_table) =
        MenhirLib.RowDisplacement.compress
            (=) (fun x -> x = Err) Err
            (Lr1.n) (Terminal.n)
            parse_table
    in

    (* FIXME: Menhir uses a special encoding for displacements to avoid
     * negative numbers because of the representation used for serializing
     * the tables into OCaml code. we don't need that so for now we just
     * fix it here but we should add an argument to the compress function
     * to be able to opt-out from this encoding... *)
    Array.iteri
        (fun i d ->
             displacement.(i) <- MenhirLib.RowDisplacement.decode displacement.(i))
        displacement ;

    Format.fprintf ff "const ACT_TABLE_DISP: [isize ; %d] = [\n%a\n];\n\n"
        (Array.length displacement) (pp_array (fun ff x -> fprintf ff "%d" x) ",\n")
        displacement ;

    Format.fprintf ff "const ACT_TABLE: [Action<YYType> ; %d] = [\n%a\n];\n\n"
        (Array.length parse_table)
        (pp_array (fun ff v -> Format.fprintf ff "%a" pp_act v) ",\n")
        parse_table ;

    (* compress the GOTO table *)
    let (displacement, goto_table) =
        MenhirLib.RowDisplacement.compress
            (=) (fun x -> x = 0) 0
            (Lr1.n) (Terminal.n)
            goto_table
    in

    (* FIXME: As above... *)
    Array.iteri
        (fun i d ->
             displacement.(i) <- MenhirLib.RowDisplacement.decode displacement.(i))
        displacement ;

    Format.fprintf ff "const GOTO_TABLE_DISP: [isize ; %d] = [\n%a\n];\n\n"
        (Array.length displacement) (pp_array (fun ff x -> fprintf ff "%d" x) ",\n")
        displacement ;

    Format.fprintf ff "const GOTO_TABLE: [usize ; %d] = [\n%a\n];\n\n"
        (Array.length goto_table)
        (pp_array (fun ff v -> Format.fprintf ff "%d" v) ",\n")
        goto_table ;

    Format.fprintf ff "const DEFAULT_REDUCTION: [%s ; %d] = [\n%a\n];\n\n"
        "Option<SemAct<YYType>>" (Array.length default_table)
        (pp_array
             (fun ff v -> match v with
                  | None -> fprintf ff "None"
                  | Some prod -> fprintf ff "Some(%a)" pp_semact prod) ",\n")
        default_table ;

    (* Print wrappers *)

    Format.fprintf ff
        "struct ErrTable([bool ; %d]);\n\n\
         impl Index<(usize, usize)> for ErrTable {\n\
         \    type Output = bool;\n\n\
         \    fn index(&self, (st, tok): (usize, usize)) -> &bool {\n\
         \        let &ErrTable(ref table) = self;\n\
         \        &table[st * %d + tok]\n\
         \    }
         }\n\n\
         static ERR_TABLE_WRAP: ErrTable = ErrTable(ERROR_TABLE);\n\n"
         (Lr1.n * Terminal.n) (Terminal.n) ;

    Format.fprintf ff
        "struct ActTable([isize ; %d], [Action<YYType> ; %d]);\n\n\
         impl Index<(usize, usize)> for ActTable {\n\
         \    type Output = Action<YYType>;\n\n\
         \    fn index(&self, (st, tok): (usize, usize)) -> &Action<YYType> {\n\
         \        let &ActTable(ref disp, ref table) = self;\n\
         \        &table[(disp[st] + tok as isize) as usize]\n\
         \    }
         }\n\n\
         static ACT_TABLE_WRAP: ActTable = ActTable(ACT_TABLE_DISP, ACT_TABLE);\n\n"
        Lr1.n (Array.length parse_table) ;

    Format.fprintf ff
        "struct GotoTable([isize ; %d], [usize ; %d]);\n\n\
         impl Index<(usize, usize)> for GotoTable {\n\
         \    type Output = usize;\n\n\
         \    fn index(&self, (st, nt): (usize, usize)) -> &usize {\n\
         \        let &GotoTable(ref disp, ref table) = self;\n\
         \        &table[(disp[st] + nt as isize) as usize]\n\
         \    }
         }\n\n\
         static GOTO_TABLE_WRAP: GotoTable = GotoTable(GOTO_TABLE_DISP, GOTO_TABLE);\n\n"
        Lr1.n (Array.length goto_table) ;

    Format.fprintf ff
        "struct DefTable([Option<SemAct<YYType>> ; %d]);\n\n\
         impl Index<usize> for DefTable {\n\
         \    type Output = Option<SemAct<YYType>>;\n\n\
         \    fn index(&self, st: usize) -> &Option<SemAct<YYType>> {\n\
         \        let &DefTable(ref table) = self;\n\
         \        &table[st]\n\
         \    }
         }\n\n\
         static DEF_TABLE_WRAP: DefTable = DefTable(DEFAULT_REDUCTION);\n\n"
        Lr1.n

let pp_action ff act = match Action.to_il_expr act with
    | IL.ETextual s -> Format.fprintf ff "%s" s.Stretch.stretch_content
    | _ -> failwith "all semantic actions should be textuals"

let pp_actions ff =
    Production.iterx
        (fun r ->
             let (_, stmts) =
                 Array.fold_left
                     (fun (i, stmts) id ->
                          let pat = match (Production.rhs r).(i) with
                              | Symbol.T t  -> Terminal.(ocamltype t, print t)
                              | Symbol.N nt -> Nonterminal.(ocamltype nt, print false nt)
                          in (i + 1, (id, pat) :: stmts))
                     (0, []) (Production.identifiers r)
             in

             Format.fprintf ff
                 "fn RULE_%d(state: usize, stack: &mut Vec<(usize, YYType)>) -> usize {\n\
                 \    %a\n\
                 \    stack.push((state, YYType::%s(%a)));\n\
                 \    GOTO_TABLE_WRAP[(state, %d)] - 1\n\
                  }\n\n"
                 (Production.p2i r)
                 (pp_list
                      (fun ff (id, (ty, name)) -> match ty with
                           | Some _ ->
                               Format.fprintf ff
                                   "let (state, %s) = match stack.pop().unwrap() {\n\
                                   \        (state, YYType::%s(data)) => (state, data),\n\
                                   \        _ => unreachable!()\n\
                                   \    };" id name
                           | None ->
                               Format.fprintf ff
                                   "let state = match stack.pop().unwrap() {\n\
                                   \        (state, YYType::%s) => state,\n\
                                   \        _ => unreachable!()\n\
                                   \    };" name)
                      "\n    ") stmts
                 (Nonterminal.print false @@ Production.nt r)
                 pp_action (Production.action r)
                 (Nonterminal.n2i @@ Production.nt r))

let pp_nexttoken ff =
    Format.fprintf ff
        "fn next_tok<Lexer>(lexer: &mut Lexer) -> (YYType, usize)\
        \    where Lexer: Iterator<Item = Token> {\n\
        \    let tok = match lexer.next() {\n\
        \        Some(t) => t,\n\
        \        None    => return (unsafe { ::std::mem::uninitialized() }, %d)\
        \    };\n\n\
        \    match tok {\n%a\n    }\n\
         }\n\n"
        (Terminal.t2i @@ Terminal.sharp)
        (pp_list
             (fun ff (ty, name, n) -> match ty with
                  | Some _ ->
                      Format.fprintf ff
                          "        Token::%s(data) => (YYType::%s(data), %d)"
                          name name n
                  | None   ->
                      Format.fprintf ff
                          "        Token::%s => (YYType::%s, %d)"
                          name name n)
             ",\n")
        (Terminal.map_real (fun t -> Terminal.(ocamltype t, print t, t2i t)))

let pp_start ff (nt, init_st, ty) =
    let Stretch.Declared(ty) = ty in
    let name = Nonterminal.print false nt in
    Format.fprintf ff
        "pub fn %s<Lexer>(lexer: &mut Lexer) -> Result<%s, ()>\n\
         \    where Lexer: Iterator<Item = Token> {\n\
         \    let mut stack = try!(\
         \        menhir_runtime::parse::<Token, Lexer, ActTable, DefTable, ErrTable, YYType>(\n\
         \            lexer, &ACT_TABLE_WRAP, &DEF_TABLE_WRAP, &ERR_TABLE_WRAP, next_tok, %d\n\
         \        )
         \    );\n\n\
         \    match stack.pop().unwrap() {\n\
         \        (_, YYType::%s(data)) => Ok(data),\n\
         \        _ => unreachable!()\n\
         \    }\n\
         }"
        name ty.Stretch.stretch_content
        (Lr1.number init_st)
        name

let pp_main ff =
    let nts = Lr1.(fold_entry (fun _ st nt ty acc -> (nt, st, ty) :: acc) []) in
    Format.fprintf ff "%a" (pp_list pp_start "\n\n") nts

let pp_grammar ff =
    pp_use ff ;
    pp_parser_enums ff ;
    pp_parse_table ff ;
    pp_actions ff ;
    pp_nexttoken ff ;
    pp_main ff ;
    ()

let write_all oc =
    let ff = Format.formatter_of_out_channel oc in
    List.iter
        (fun s -> Format.fprintf ff "%s\n" s.Stretch.stretch_content)
        Front.grammar.UnparameterizedSyntax.preludes ;
    pp_grammar ff ;
    Format.pp_print_flush ff () ;
    close_out oc
