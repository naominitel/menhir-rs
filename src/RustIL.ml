type ty =
    | TUnit
    | TIsize
    | TUsize
    | TBool
    | TVar of string
    | TApp of string * ty list
    | TArray of ty * int
    | TTup of ty list
    | TRef of ty
    | TRefMut of ty
    | TTextual of Stretch.t

type path = string list

type pattern =
    | PVar of string
    | PMut of string
    | PRef of string
    | PVariant of path * pattern list
    | PTup of pattern list
    | PDeref of pattern
    | PWildcard

type expr =
    | EInt of int
    | EBool of bool
    | EArray of expr array
    | ETup of expr list
    | EVar of string
    | EVariant of path * expr list
    | ERef of expr
    | EIndex of expr * expr
    | EBinop of string * expr * expr
    | EAs of expr * ty
    | EMatch of expr * (pattern * expr) list
    | EMeth of expr * string * expr list
    | EMac of string * expr option
    | ETextual of Stretch.t
    | EReturn of expr
    | EUnsafe of expr
    | ECall of path * ty list * expr list

type stmt =
    | SLet of pattern * ty option * expr
    | SExpr of expr

type block = {
    stmts: stmt list ;
    ret: expr option
}

type trait_bound_param =
    | TrPType of ty
    | TrPAssoc of string * ty

type trait_bound = string * trait_bound_param list

type trait = string * ty list

type self_arg =
    | SelfNone
    | SelfRef

type fn_def = {
    generics: (string * trait_bound) list ;
    self: self_arg ;
    args: (pattern * ty) list ;
    ret: ty ;
    body: block
}

type glob_kind =
    | KStatic
    | KConst

type item =
    | IExtCrate of string
    | IUse of path
    | IEnum of bool * string * (string * ty list) list
    | IGlob of string * glob_kind * ty * expr
    | IFn of bool * string * fn_def
    | INewtype of string * ty list
    | IImpl of (string * trait_bound) list * trait * ty * item list
    | IType of string * ty

open Format

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
            Format.fprintf ff "%a%a" printer arr.(idx) sep () ;
            iter (idx + 1)
        end
    in iter 0

let rec pp_ty ff = function
    | TUnit  -> fprintf ff "()"
    | TUsize -> fprintf ff "usize"
    | TIsize -> fprintf ff "isize"
    | TBool  -> fprintf ff "bool"
    | TVar v -> fprintf ff "%s" v
    | TApp (v, a) ->
        fprintf ff "%s<@[<hv>%a@]>" v
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ",@ "))
                 pp_ty) a
    | TArray (t, n) -> fprintf ff "[%a ; %d]" pp_ty t n
    | TTup tys -> fprintf ff "(%a)" (pp_list pp_ty ", ") tys
    | TRef ty -> fprintf ff "&%a" pp_ty ty
    | TRefMut ty -> fprintf ff "&mut %a" pp_ty ty
    | TTextual s -> fprintf ff "%s" (String.trim s.Stretch.stretch_content)

let pp_variant p_ctor p_args ff = function
    | (ctor, [])    -> fprintf ff "%a" p_ctor ctor
    | (ctor, args) -> fprintf ff "%a(%a)" p_ctor ctor (pp_list p_args ", ") args

let pp_path = pp_list pp_print_string "::"

let rec pp_pat ff = function
    | PVar v -> fprintf ff "%s" v
    | PMut v -> fprintf ff "mut %s" v
    | PRef v -> fprintf ff "ref %s" v
    | PVariant (v, a) -> pp_variant pp_path pp_pat ff (v, a)
    | PTup pats -> fprintf ff "(%a)" (pp_list pp_pat ", ") pats
    | PDeref p -> fprintf ff "&%a" pp_pat p
    | PWildcard -> fprintf ff "_"

let rec pp_expr ff = function
    | EInt i -> fprintf ff "%d" i
    | EBool b -> fprintf ff "%s" @@ if b then "true" else "false"
    | EArray arr ->
        fprintf ff "[@,@[<hov 4>    @,%a@]]"
            (pp_array pp_expr (fun ff () -> fprintf ff ",@ ")) arr
    | ETup exprs -> fprintf ff "(%a)" (pp_list pp_expr ", ") exprs
    | EVar v -> fprintf ff "%s" v
    | EVariant (v, a) -> pp_variant pp_path pp_expr ff (v, a)
    | ERef e -> fprintf ff "(&%a)" pp_expr e
    | EIndex (e, idx) -> fprintf ff "(%a[%a])" pp_expr e pp_expr idx
    | EBinop (op, x, y) -> fprintf ff "(%a@ %s@ %a)" pp_expr x op pp_expr y
    | EAs (e, t) -> fprintf ff "(%a@ as@ %a)" pp_expr e pp_ty t
    | EMatch (e, arms) ->
        fprintf ff "match %a {@,@[<v 4>    %a@]@,}" pp_expr e
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ",@,"))
                 (fun ff (p, e) -> fprintf ff "%a => %a" pp_pat p pp_expr e))
            arms
    | EMeth (e, m, a) ->
        fprintf ff "%a.%s(@[<hv>%a@])" pp_expr e m
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ",@ "))
                 pp_expr) a
    | EMac (m, None) -> fprintf ff "%s!()" m
    | EMac (m, Some a) -> fprintf ff "%s!(%a)" m pp_expr a
    | ETextual s -> fprintf ff "%s" (String.trim s.Stretch.stretch_content)
    | EReturn e -> fprintf ff "return %a" pp_expr e
    | EUnsafe e -> fprintf ff "unsafe {@[<hv>@ %a@ @]}" pp_expr e
    | ECall (p, t, a) ->
        fprintf ff "%a%a(@[<hov 4>@,%a@])" pp_path p
            (match t with
                | [] -> fun _ () -> ()
                | l -> fun ff () -> fprintf ff "::<%a>" (pp_list pp_ty ", ") l) ()
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ",@ ")) pp_expr) a

let pp_stmt ff = function
    | SLet (pat, None, expr) ->
        fprintf ff "@[<hv>let %a =@ @[<hv>%a@]@]" pp_pat pat pp_expr expr
    | SLet (pat, Some ty, expr) ->
        fprintf ff "@[<hv>let %a: %a =@ @[<hv>%a@]@]" pp_pat pat pp_ty ty pp_expr expr
    | SExpr e -> pp_expr ff e

let pp_block ff = function
    | { stmts ; ret = None } ->
        fprintf ff "{@,@[<v 4>    %a;@]@,}"
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ";@,")) pp_stmt) stmts
    | { stmts ; ret = Some e } ->
        fprintf ff "{@,@[<v 4>    %a;@,@[<hv>%a@]@]@,}"
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ";@,")) pp_stmt)
            stmts pp_expr e

let pp_bound ff (tr, params) =
    fprintf ff "%s<%a>" tr
        (pp_list
             (fun ff -> function
                 | TrPType t -> pp_ty ff t
                 | TrPAssoc (t, ty) -> fprintf ff "%s = %a" t pp_ty ty) ", ")
        params

let rec pp_item ff = function
    | IExtCrate s -> fprintf ff "extern crate %s;" s
    | IUse p -> fprintf ff "use %a;" pp_path p
    | IEnum (pub, name, variants) ->
        fprintf ff "%senum %s {@,@[<v 4>    %a@]@,}"
            (if pub then "pub " else "") name
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff ",@,"))
                 (pp_variant pp_print_string pp_ty))
            variants
    | IGlob (name, kind, ty, expr) ->
        fprintf ff "%s %s: %a = %a;"
            (match kind with KStatic -> "static" | KConst -> "const")
            name pp_ty ty pp_expr expr
    | IFn (pub, name, { generics ; self ; args ; ret ; body }) ->
        fprintf ff "%sfn %s%a(%s%a) -> %a%a %a"
            (if pub then "pub " else "") name
            (match generics with
                | [] -> fun _ () -> ()
                | l -> fun ff () ->
                    fprintf ff "<%a>"
                        (pp_list (fun ff (t, _) -> fprintf ff "%s" t) ", ") l) ()
            (match self with SelfRef -> "&self, " | SelfNone -> "")
            (pp_list (fun ff (p, t) -> fprintf ff "%a: %a" pp_pat p pp_ty t) ", ") args
            pp_ty ret
            (match generics with
                | [] -> fun _ () -> ()
                | l -> fun ff () ->
                    fprintf ff "@,@[<hv 4>@,where @[<v>%a@]@]"
                        (pp_list
                             (fun ff (t, b) -> fprintf ff "%s: %a" t pp_bound b)
                             ", ") l) ()
            pp_block body
    | INewtype (name, tys) ->
        fprintf ff "struct %a;" (pp_variant pp_print_string pp_ty) (name, tys)
    | IImpl (generics, (trait, args), ty, items) ->
        fprintf ff "impl%a %s<%a> for %a {@,@[<v 4>    %a@]@,}"
            (match generics with
                | [] -> fun _ () -> ()
                | l -> fun ff () ->
                    fprintf ff "<%a>"
                        (pp_list (fun ff (t, _) -> fprintf ff "%s" t) ", ") l) ()
            trait (pp_list pp_ty ", ") args pp_ty ty
            (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff "@,@,")) pp_item)
            items
    | IType (name, ty) -> fprintf ff "type %s = %a;" name pp_ty ty

let pp_program ff items =
    fprintf ff "@[<v>%a@]"
        (pp_print_list ?pp_sep:(Some (fun ff () -> fprintf ff "@,@,")) pp_item)
        items
