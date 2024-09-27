open Mexpression
open Base.Result
open Base.List

let ( let* ) = Base.Result.( >>= )

type shape = Shape of int list
type res = (shape, string) result

(* Each of the `check` functions returns either the shape of the reduced expression, or an error *)
let rec check_matrix (t : mexpression) : res =
  match t with
  | Matrix { dims; shape; elements } ->
      if
        dims > 0
        && length shape = dims
        && length elements = reduce_exn shape ~f:( * )
      then Ok (Shape shape)
      else Error "Invalid matrix"
  | _ -> Error "Expected a matrix"

and check_unop (t : mexpression) : res =
  match t with
  | Unop (op, expr) -> (
      let* (Shape shape) = check expr in
      match op with
      | Inverse ->
          if length shape = 2 && hd_exn shape = hd_exn (tl_exn shape) then
            Ok (Shape shape)
          else Error "Trying to an invert a non-square matrix"
      | Transpose -> Ok (Shape (rev shape)))
  | _ -> Error "Expected unary operator"

and check_if (t : mexpression) : res =
  match t with
  | If (_, e1, e2) ->
      let* (Shape shape1) = check e1 in
      let* (Shape shape2) = check e2 in
      if shape1 = shape2 then Ok (Shape shape1)
      else Error "If expression yields two different shaped matrices"
  | _ -> Error "Expected if expression"

and check_binop (t : mexpression) : res =
  match t with
  | Binop (b, e1, e2) -> (
      let* (Shape shape1) = check e1 in
      let* (Shape shape2) = check e2 in
      match b with
      | Add | Subtract ->
          if shape1 = shape2 then Ok (Shape shape1)
          else Error "Trying to add/subtract two differently-sized matrices"
      | Multiply ->
          if last_exn shape1 = hd_exn shape2 then
            Ok (Shape (append (drop_last_exn shape1) (tl_exn shape2)))
          else Error "Multiplying two matrices with incompatible shapes")
  | _ -> Error "Expected binary operator"

and check_scale (t : mexpression) : res =
  match t with Scale (_, e) -> check e | _ -> Error "Expected scale"

and check (t : mexpression) : res =
  match t with
  | Matrix _ -> check_matrix t
  | Binop _ -> check_binop t
  | Unop _ -> check_unop t
  | If _ -> check_if t
  | Scale _ -> check_scale t
