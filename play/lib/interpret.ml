(** UNUSED *)

(* bootleg interpreter *)
open Core
open Term

let ( = ) = Poly.(=)
let ( <> ) = Poly.(<>)

type value =
  | VScalar of int
  | VBool of bool
  | VFun of string * term
  | VVar of string (* free variable? *)
  | VMatrix (* not making interpreter for this yet *)
  | VError


(* substitutes x for s in t *)
let rec subst (x: string) (s: term) (t: term) =
  let subst = subst x s in
  match t with
  | Matrix _ | Bool _ | Scalar _  -> t
  | Var y when x = y -> s
  | Var _ -> t
  | App (e1, e2) -> App (subst e1, subst e2)
  | Fold (e1, e2, e3) -> Fold (subst e1, subst e2, subst e3)
  | Map (e1, e2) -> Map (subst e1, subst e2)
  | If (e1, e2, e3) -> If (subst e1, subst e2, subst e3)
  | Abs (y, _, body) when x = y -> t
  | Abs (y, ty, body) -> Abs (y, ty, subst body)
  | UOp (op, e) -> UOp (op, subst e)
  | BOp (op, e1, e2) -> BOp (op, subst e1, subst e2)
  | Let (y, e1, e2) when x = y -> Let (y, subst e1, e2)
  | Let (y, e1, e2) -> Let (y, subst e1, subst e2)



let rec interpret = function
  | _ -> VMatrix
  (* | Matrix _ -> VMatrix
  | Bool b -> VBool b
  | Scalar s -> VScalar s
  | Var x -> VVar x
  | Abs (x, _, body) -> VFun (x, body)
  | App (Abs(x, _, body), arg) -> interpret (subst x arg body)
  | App _ -> VError
  | Fold (f, acc, xs) -> VMatrix
  | Map (f, xs) -> VMatrix
  | UOp (Not, e) -> let VBool(b) = interpret e in VBool (not b)
  | UOp (Negate, e) -> let VScalar(s) = interpret e in VScalar (-s)
  | BOp (Mult, e1, e2) -> let VScalar(s1) = interpret e1 in let VScalar(s2) = interpret e2 in VScalar(s1 * s2)
  | _ -> VError *)