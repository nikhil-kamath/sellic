open Term

(* Substitute occurrences of x with e in the term t *)
let rec subst x e t =
  let subst = subst x e in
  match t with
  | Matrix _ | Bool _ | Scalar _ -> t
  | If (p, t1, t2) -> If (subst p, subst t1, subst t2)
  | UOp (op, t) -> UOp (op, subst t)
  | BOp (op, t1, t2) -> BOp (op, subst t1, subst t2)
  | Fold _ | Map _ -> failwith "Folds and Maps cannot be substituted"
  | App (t1, t2) -> App (subst t1, subst t2)
  | Let (y, t1, t2) when x = y -> Let (y, subst t1, t2) (* shadowed *)
  | Let (y, t1, t2) -> Let (y, subst t1, subst t2)
  | Abs (y, _, _) when x = y -> t (* shadowed *)
  | Abs (y, ty, body) -> Abs (y, ty, subst body)
  | Var y when x = y -> e
  | Var y -> t

(* Inline function applications into let-expressions *)
let rec inline t =
  match t with
  | Matrix _ | Bool _ | Scalar _ | Var _ -> t
  | If (p, t1, t2) -> If (inline p, inline t1, inline t2)
  | UOp (op, t) -> UOp (op, inline t)
  | BOp (op, t1, t2) -> BOp (op, inline t1, inline t2)
  | Let (x, t1, t2) ->
      subst x (inline t1) t2 |> inline (* inline body after subst *)
  | Fold _ | Map _ -> failwith "Folds and Maps cannot be inlined"
  | App (t1, t2) -> (
      match inline t1 with
      | Abs (x, _ty, body) -> subst x (inline t2) body
      | _ -> failwith "Applying non-function")
  | Abs (x, ty, body) -> Abs (x, ty, inline body)
