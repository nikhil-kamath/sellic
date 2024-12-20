open Solver
(** Preprocessing for parsing matrices *)
(*
  for now, we expect all matrices to be "tensors", that is, not rough:
  each dimension should be nested to an equal depth, with
  an equal number of elements at each depth.

  For example,

  [
    [1; 0; 0];
    [0; 1; 0];
    [0; 0; 1]
  ]

  is a valid matrix. For "irregular" matrices, we use the `*[` syntax:

  *[
    [[]; 1; 0; 0];
    [0; 1; [0]];
    [0; 0; 1; 3]
  ]
*)

open Core

exception ShapeError of string

let ( let* ) = Option.( >>= )
let ( = ) = Poly.( = )

type dims = size_expr list [@@deriving show]
type nested = Item of float | Nested of nested list [@@deriving show]
type sparsity = Sparse | Dense | Unknown [@@deriving show]

let is_nested = function Nested _ -> true | _ -> false
let is_item = function Item _ -> true | _ -> false

let rec flatten = function
  | Item i -> [ i ]
  | Nested ns -> List.concat_map ns ~f:flatten

let rec depths = function
  | Item _ -> [ 0 ]
  | Nested [] -> [ 1 ]
  | Nested ns -> List.concat_map ns ~f:depths |> List.map ~f:succ

(*
  only valid for depth-equal nesteds:
  a `Nested` will contain either ONLY Items, or ONLY Nesteds
*)
let rec lengths = function
  | Item _ -> []
  | Nested is when not (List.exists is ~f:is_nested) -> [ List.length is ]
  | Nested ns -> List.concat_map ns ~f:lengths

(* Returns None if the list is misshapen, otherwise Some shape *)
let rec shape : nested -> dims option = function
  | Item _ -> Some []
  | Nested [] -> Some [ CNum 0 ]
  | Nested ns ->
      let* shapes = List.map ~f:shape ns |> Option.all in
      let* shape = List.all_equal shapes ~equal:( = ) in
      Some (CNum (List.length ns) :: shape)

