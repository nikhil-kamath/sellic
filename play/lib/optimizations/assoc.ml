open Term
open Types
open Py
open Sparsity

(* describes how many of each multiplication algorithm we're doing *)
type mults = { ip : int; op : int; rw : int } [@@deriving show]

let zero = { ip = 0; op = 0; rw = 0 }

let incr ({ ip; op; rw } as m) w =
  match w with
  | 0 -> { ip = ip + 1; op; rw }
  | 1 -> { ip; op = op + 1; rw }
  | 2 -> { ip; op; rw = rw + 1 }
  | _ -> m

let ( ++ ) { ip = ip1; op = op1; rw = rw1 } { ip = ip2; op = op2; rw = rw2 } =
  { ip = ip1 + ip2; op = op1 + op2; rw = rw1 + rw2 }

let rec count = function
  | ABOp (Mult, e1, e2, _) -> (
      let ty1 = ty_of_annot e1 in
      let ty2 = ty_of_annot e2 in
      match (ty1, ty2) with
      | TMatrix { sparsity = s1; _ }, TMatrix { sparsity = s2; _ } ->
          let which = Sparsity.multiply s1 s2 in
          print_int which;
          incr (count e1 ++ count e2) which
      | _ -> count e1 ++ count e2)
  | AApp (a, b, _) -> count a ++ count b
  | AIf (a, b, c, _) -> count a ++ count b ++ count c
  | AAbs (_, _, a, _) -> count a
  | AUOp (_, a, _) -> count a
  | ABOp (_, a, b, _) -> count a ++ count b
  | ALet (_, a, b, _) -> count a ++ count b
  | _ -> zero
