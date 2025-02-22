(* TEMPORARY clean up and write an .mli file *)

open Syntax
open Positions

let app p ps =
  match ps with
  | [] ->
      ParameterVar p
  | _ ->
      ParameterApp (p, ps)

let oapp1 o p =
  match o with
  | None ->
      p
  | Some var ->
      ParameterApp (var, [ p ])

let unapp = function
  | ParameterVar x ->
      (x, [])

  | ParameterApp (p, ps) ->
      (p, ps)

let rec map f = function
  | ParameterVar x ->
      ParameterVar (f x)

  | ParameterApp (p, ps) ->
      ParameterApp (f p, List.map (map f) ps)


let rec fold f init = function
  | ParameterVar x ->
      f init x

  | ParameterApp (p, ps) ->
      f (List.fold_left (fold f) init ps) p

let identifiers m p =
  fold (fun acu x -> StringMap.add x.value x.position acu) m p

type t = parameter

let rec equal x y =
  match x, y with
    | ParameterVar x, ParameterVar y when x.value = y.value ->
        true
    | ParameterApp (p1, p2), ParameterApp (p1', p2') ->
        p1.value = p1'.value && List.for_all2 equal p2 p2'
    | _ -> false

let hash = function
  | ParameterVar x
  | ParameterApp (x, _) ->
      Hashtbl.hash (Positions.value x)

let position = function
  | ParameterVar x
  | ParameterApp (x, _) ->
      Positions.position x

let with_pos p =
  Positions.with_pos (position p) p
