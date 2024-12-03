open Matrix
open Core
open Owl_stats
open Py

module Sparsity = struct
  type t = { s : float; rlv : float } [@@deriving show]

  let nonzero x =
    let open Float in
    abs x >. 0.000001 (* zero tolerance*)

  let rec count_nonzero = function
    | Item x -> if nonzero x then 1 else 0
    | Nested xs -> List.fold ~f:( + ) ~init:0 (List.map ~f:count_nonzero xs)

  let count_nonzero_array =
    Array.fold ~init:0 ~f:(fun a x -> if nonzero x then 1 + a else a)

  let s n = float (count_nonzero n) /. float (size n)

  let rlv n =
    n |> Matrix.to_array
    |> Array.map ~f:count_nonzero_array
    |> Array.map ~f:float |> var

  let predict { s = s1; rlv } { s = s2; _ } =
    { s = Py.predict_sparsity s1 s2 rlv; rlv = Py.predict_rlv s1 s2 rlv }

  let multiply { s = s1; rlv } { s = s2; _ } = Py.predict_mult s1 s2 rlv
end
