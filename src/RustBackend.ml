open Grammar
open RustIL

let rust_type t = match t with
    | Some (Stretch.Declared t) -> [TTextual t]
    | Some (Stretch.Inferred _) -> failwith "Rust types cannot be inferred"
    | None -> []

(* We generate two enum types:
 * Token, the external representation of tokens to be used by the lexer
 * YYType, the internal representation of symbols (both terminals and
 * non-terminals). It's the type of semantic values that we put on the stack.
 * Since it's hard to compare Rust types here, we have one variant per symbol
 * instead of one variant per type but this doesn't change much...
 * Such a variant is not really useful since we know statically the type of
 * every semantic value is known when we push it or pop it from the stack and
 * every match has just a single reachable arm, the other cases being impossible
 * by construction. But it's not easy to encode unsafe C-like unions in Rust
 * and at least those dynamic checks will allow us to easily debug the generated
 * code in the first time... *)
let parser_enums () =
    (* Generate the Token type *)
    let terminals =
        Terminal.(fold
            (fun t acc ->
                  if real t then (print t, rust_type @@ ocamltype t) :: acc
                  else acc)
            [])
    in

    (* Generate the YYType type *)
    let nonterminals =
        Nonterminal.(foldx
            (fun t acc -> (print true t, rust_type @@ ocamltype t) :: acc)
            [])
    in

    [IEnum (true, "Token", terminals) ;
     IEnum (false, "YYType", terminals @ nonterminals)]

(* The type of actions (entries in the ACTION table)
 * There is no Accept action, we consider reduction by a start production as an
 * Accept action. *)
type action =
    | Shift of int               (* state number *)
    | Reduce of Production.index (* (semantic) action number *)
    | Err                        (* error *)

(* We do not have a production array.
 * Reduce entries directly contain a pointer to the semantic function.
 * Reduction on a start production (i.e. an Accept action), since there is no
 * semantic action for it, is encoded as None. rustc should optimize it as NULL
 * so the size of the Reduce variant should be just the size of a pointer. *)
let encode_semact prod =
    if Production.is_start prod then
        EVariant (["None"], [])
    else
        let prod_fun = Format.sprintf "RULE_%d" @@ Production.p2i prod in
        EVariant (["Some"], [EVar (prod_fun)])

(* Encode an action as the corresponding Rust variant.
 * The Action enum is defined in the Menhir runtime. *)
let encode_action = function
    | Shift i -> EVariant (["Action" ; "Shift"], [EInt i])
    | Reduce prod -> EVariant (["Action" ; "Reduce"], [encode_semact prod])
    | Err -> EVariant (["Action" ; "Err"], [])

(* Compression of 2-dimensional tables. *)
let compress insig elem_size table =
    let (disp, compressed) =
        MenhirLib.RowDisplacement.compress
            (=) (fun x -> x = insig) insig (Lr1.n) elem_size table
    in

    (* FIXME: Menhir uses a special encoding for displacements to avoid
     * negative numbers because of the representation used for serializing
     * the tables into OCaml code. we don't need that so for now we just
     * fix it here but we should add an argument to the compress function
     * to be able to opt-out from this encoding... *)
    Array.iteri
        (fun i d ->
             disp.(i) <- MenhirLib.RowDisplacement.decode disp.(i))
        disp ;

    (disp, compressed)

(* The parser tables.
 * We create 4 tables:
 * The ACTION and GOTO tables, the standard LR parse tables,
 * The ERROR table, which is a bitmap indicating wether the corresponding entry
 * in the ACTION table is an error. This allows us never to lookup the ACTION
 * table entry in error cases, and thus to consider those cases as irrelevant,
 * allowing much better compression results.
 * The DEFAULT table, indicating if there is a default reduction for a given
 * state (that is, if the only possible action is a reduction). This way we
 * avoid a lot of iterations and we can detect termination of parsing without
 * consuming an extra token, removing the need for a special EOF token. *)
let parser_tables () =
    (* Build parse table in a single pass. *)
    let goto_table  = Array.init Lr1.n (fun _ -> Array.make (Nonterminal.n) 0) in
    let parse_table = Array.init Lr1.n (fun _ -> Array.make (Terminal.n) Err)  in
    let default_table = Array.init Lr1.n (fun _ -> None) in

    Lr1.iter
        (fun node ->
             let st = Lr1.number node in

             match Invariant.has_default_reduction node with
                 | None ->
                     (* Shift and GOTO actions *)
                     SymbolMap.iter
                         (fun sym dst ->
                              let dst = Lr1.number dst in
                              match sym with
                                  | Symbol.N nt ->
                                      goto_table.(st).(Nonterminal.n2i nt) <- 1 + dst
                                  | Symbol.T t  ->
                                      parse_table.(st).(Terminal.t2i t) <- Shift dst)
                         (Lr1.transitions node) ;

                     (* Reduce actions *)
                     TerminalMap.iter
                         (fun t rules ->
                              parse_table.(st).(Terminal.t2i t) <-
                                  Reduce (List.hd rules))
                         (Lr1.reductions node)

                 | Some (prod, _) ->
                     (* If we have a default reduction, don't put anything
                      * in the parse table, it will never be looked up, and
                      * the compression will work better... *)
                     default_table.(st) <- Some(prod)) ;

    (* The error bitmap. *)
    let error = Array.make (Lr1.n * Terminal.n) false in
    Array.iteri
        (fun st a ->
             Array.iteri (fun t act -> error.(st * Terminal.n + t) <- act = Err) a)
        parse_table ;

    (* Compress the ACTION and GOTO tables. *)
    (compress Err (Terminal.n) parse_table,
     compress 0 (Nonterminal.n) goto_table,
     error, default_table)  

(* Create an IL const or static declaration from a table. *)
let encode_table name table ty encode_elem =
    let array = Array.map encode_elem table in
    let ty = TArray (ty, Array.length array) in
    IGlob (name, KConst, ty, EArray (array))

(* Create an implementation id Index<idx_ty> for ty *)
let index_impl ty idx_ty out_ty idx_pat self_pats ret_expr = IImpl (
    [], ("Index", [idx_ty]), TVar ty,
    [
        (* type Output = $ty; *)
        IType ("Output", out_ty) ;

        (* fn index(&self, (st, tok): (usize, usize)) -> &$ty *)
        IFn (false, "index", {
            generics = [] ;
            self = SelfRef ;
            args = [(idx_pat, idx_ty)] ;
            ret = TRef out_ty ;

            body = {
                (* let &$type_name(disp, table) = self; *)
                stmts = [SLet (PDeref (PVariant ([ty], self_pats)), None, EVar "self")] ;

                (* &table[(disp[st] + tok as isize) as usize] *)
                ret = Some (ERef (ret_expr))
            }
        })
    ]
)

(* Create a wrapper type. *)
let wrapper_type name tys exprs impls =
    let type_name  = Format.sprintf "%sType" name in
    [ INewtype (type_name, tys) ] @ impls @
    [ IGlob (name, KStatic, TVar (type_name), EVariant ([type_name], exprs)) ]

(* Take a compressed two-dimentional table and encode it as
 * - Two constant tables (the displacement table and the table)
 * - A newtype wrapper around those two tables
 * - An implementation of indexing by a tuple for this newtype. *)
let encode_compressed name (disp, table) ty encode_elem =
    let table_name = Format.sprintf "%s_TABLE" name in
    let disp_name  = Format.sprintf "%s_TABLE_DISP" name in
    let type_name  = Format.sprintf "%sType" name in

    let impl =
        index_impl
            type_name (TTup [TUsize ; TUsize]) ty
            (PTup [PVar "st" ; PVar "tok"])
            [PRef "disp" ; PRef "table"]
            (EIndex (EVar "table", EAs (EBinop ("+", EIndex (EVar "disp", EVar "st"),
                                                EAs (EVar "tok", TIsize)), TUsize))) ;
    in

    (encode_table disp_name disp TIsize (fun i -> EInt i)) ::
    (encode_table table_name table ty encode_elem) ::
    (wrapper_type name
         [TArray (TIsize, Array.length disp) ; TArray (ty, Array.length table)]
         [EVar (disp_name) ; EVar (table_name)] [impl])

let parser_tables_items () =
    let defred_ty = (TApp ("Option", [TApp ("SemAct", [TVar "YYType"])])) in
    let (act_table, goto_table, error_table, default_table) = parser_tables () in
    encode_table "ERROR_TABLE" error_table TBool (fun err -> EBool err) ::

    wrapper_type "ERROR" [TArray (TBool, Array.length error_table)] [EVar ("ERROR_TABLE")]
        [index_impl "ERRORType" (TTup [TUsize ; TUsize]) TBool
             (PTup [PVar "st" ; PVar "tok"]) [PRef "table"]
             (EIndex (EVar "table",
                      EBinop ("+", EBinop ("*", EVar "st", EInt (Terminal.n)),
                              EVar "tok")))] @

    encode_table "DEFAULT_TABLE" default_table defred_ty
        (function
            | Some act -> EVariant (["Some"], [encode_semact act])
            | None -> EVariant (["None"], [])) ::

    wrapper_type "DEFAULT" [TArray (defred_ty, Array.length default_table)]
        [EVar ("DEFAULT_TABLE")]
        [index_impl "DEFAULTType" TUsize defred_ty
             (PVar "st") [PRef "table"] (EIndex (EVar "table", EVar "st"))] @

    encode_compressed "ACT" act_table (TApp ("Action", [TVar "YYType"])) encode_action @
    encode_compressed "GOTO" goto_table TUsize (fun st  -> EInt st)

(* The semantic actions.
 * Actions are encoded as simple Rust functions that take as argument the
 * current state and the stack, pop everything they need from the stack (they
 * have builtin knowledge of the number and types of semantic values they can
 * expect), execute the user code then push back the resulting semantic value.
 * They are responsible of perorming the GOTO move after the reduction, since
 * the parser loop doesn't have access to it. They return the number of the state
 * after the GOTO move. *)
let semantic_actions () =
    let stack_pop = EMeth (EMeth (EVar "stack", "pop", []), "unwrap", []) in
    let unreach = (PWildcard, EMac ("unreachable", None)) in

    Production.mapx
        (fun r ->
             let (_, stmts) =
                 Array.fold_left
                     (fun (i, stmts) id ->
                          let (ty, nt) = match (Production.rhs r).(i) with
                              | Symbol.T t  -> Terminal.(ocamltype t, print t)
                              | Symbol.N nt -> Nonterminal.(ocamltype nt, print false nt)
                          in

                          let (bind, pats, expr) = match ty with
                              | None -> ((PVar "state"), [], EVar "state")
                              | Some ty ->
                                  ((PTup [PVar "state" ; PVar id]),
                                   [PVar "data"],
                                   ETup [EVar "state" ; EVar "data"])
                          in

                          let s =
                              SLet (
                                  bind, None,
                                  EMatch (stack_pop, [
                                      (PTup [
                                           PVar "state" ;
                                           PVariant (["YYType" ; nt], pats)
                                       ], expr) ;
                                      unreach
                                  ])
                              )
                          in

                          (i + 1, s :: stmts))
                     (0, []) (Production.identifiers r)
             in

             let nt = Nonterminal.print true @@ Production.nt r in
             let nt_no = Nonterminal.n2i @@ Production.nt r in
             let expr = match Action.to_il_expr @@ Production.action r with
                 | IL.ETextual s -> ETextual s
                 | _ -> failwith "all semantic actions should be textuals"
             in

             IFn (
                 false, (Format.sprintf "RULE_%d" (Production.p2i r)), {
                     generics = [] ;
                     self = SelfNone ;
                     args = [(PVar "state", TUsize) ;
                             (PVar "stack",
                              TRefMut (TApp ("Vec", [TTup [TVar "usize" ;
                                                           TVar "YYType"]])))] ;
                     ret = TUsize ;
                     body = {
                         stmts = stmts @ [SExpr (
                             EMeth (EVar "stack", "push", [
                                 ETup [EVar "state" ;
                                       EVariant (["YYType" ; nt], [expr])]
                             ])
                         )] ;

                         ret = Some (EBinop (
                             "-",
                             EIndex (EVar "GOTO", ETup [EVar "state" ; EInt nt_no]),
                             EInt 1
                         ))
                     }
                 }
             ))

(* The next_token function.
 * This function is the interface between the lexer and the parser. It translates
 * tokens from the external representation used by the lexer (the Token enum) to
 * the internal representation: a tuple of the terminal number and a semantic
 * value with the YYType type.
 * This has to be in the generated code instead of the runtime because we need
 * to know the name of the YYType variants for each token. *)
let next_token () =
    IFn (
        false, "next_tok", {
            generics = [("Lexer", ("Iterator", [TrPAssoc ("Item", TVar "Token")]))] ;
            self = SelfNone ;
            args = [(PVar "lexer", TRefMut (TVar "Lexer"))] ;
            ret = TTup [TVar "YYType" ; TUsize] ;
            body = {
                stmts = [
                    SLet (PVar "tok", None, EMatch (
                        EMeth (EVar "lexer", "next", []), [
                            (PVariant (["Some"], [PVar "t"]), EVar "t") ;
                            (PVariant (["None"], []), EReturn (ETup [
                                 EUnsafe (ECall (
                                     ["" ; "std" ; "mem" ; "uninitialized"],
                                     [], []
                                 )) ;
                                 EInt (Terminal.t2i @@ Terminal.sharp)
                             ]))
                        ]
                    ))
                ] ;

                ret = Some (EMatch (EVar "tok",
                    Terminal.map_real
                        (fun t -> Terminal.(
                             let (pats, exprs) = match ocamltype t with
                                 | None -> ([], [])
                                 | Some _ -> ([PVar "data"], [EVar "data"])
                             in

                             (PVariant (["Token" ; print t], pats),
                              ETup [EVariant (["YYType" ; print t], exprs) ;
                                    EInt (t2i t)])))
                ))
            }
        }
    )

let entry_points () =
    Lr1.fold_entry
        (fun _ init_st nt ty acc ->
             let Stretch.Declared(ty) = ty in
             let name = Nonterminal.print false nt in
             IFn (true, name, {
                 generics = [("Lexer", ("Iterator", [TrPAssoc ("Item", TVar "Token")]))] ;
                 self = SelfNone ;
                 args = [(PVar "lexer", TRefMut (TVar "Lexer"))] ;
                 ret = TApp ("Result", [TTextual ty ; TUnit]) ;
                 body = {
                     stmts = [
                         SLet (PMut "stack", None, EMac (
                             "try", Some (ECall (
                                 ["menhir_runtime" ; "parse"],
                                 [TVar "Token" ; TVar "Lexer" ; TVar "ACTType" ;
                                  TVar "DEFAULTType" ; TVar "ERRORType" ;
                                  TVar "YYType"],
                                 [EVar "lexer" ; ERef (EVar "ACT") ;
                                  ERef (EVar "DEFAULT") ; ERef (EVar "ERROR") ;
                                  EVar "next_tok" ; EInt (Lr1.number init_st)]
                             ))
                         ))
                     ] ;

                     ret = Some (EMatch (
                         EMeth (EMeth (EVar "stack", "pop", []), "unwrap", []),
                         [(PTup [PWildcard ;
                                 PVariant (["YYType" ; name], [PVar "data"])],
                           EVariant (["Ok"], [EVar "data"])) ;
                          (PWildcard, EMac ("unreachable", None))                            
                         ]
                     ))
                 }
             }) :: acc)
        []

let items () =
    IExtCrate "menhir_runtime" ::
    IUse ["std" ; "ops" ; "Index"] ::
    IUse ["self" ; "menhir_runtime" ; "Action"] ::
    IUse ["self" ; "menhir_runtime" ; "SemAct"] ::
    parser_enums () @ parser_tables_items () @
    semantic_actions () @ [next_token ()] @ entry_points ()

let write_all oc =
    let ff = Format.formatter_of_out_channel oc in
    RustIL.pp_program ff @@ items () ;
    List.iter
        (fun s -> Format.fprintf ff "%s\n" s.Stretch.stretch_content)
        Front.grammar.UnparameterizedSyntax.preludes ;
    Format.pp_print_flush ff () ;
    close_out oc
