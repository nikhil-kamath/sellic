open Term
open Base

(** Type Inference? *)
let ( = ) = Poly.( = )

let ( let* ) = Base.Result.( >>= )

type context = (string, typ list, String.comparator_witness) Map.t

let rec infer_context (c : context) (t : term) : (typ, string) Result.t =
  let open List in
  match t with
  | Matrix { shape; _ } -> Ok (TMatrix { shape; sparsity = Unknown })
  | Bool _ -> Ok TBool
  | Scalar _ -> Ok TScalar
  | If (p, e1, e2) ->
      let* p = infer_context c p in
      let* e1 = infer_context c e1 in
      let* e2 = infer_context c e2 in
      if p = TBool && e1 = e2 then Ok e1 else Error "Invalid If"
  | Var x -> (
      match Map.find c x with
      | Some ty -> Ok (hd_exn ty)
      | _ -> Error "Invalid variable")
  | App (e1, e2) -> (
      let* t1 = infer_context c e1 in
      let* t2 = infer_context c e2 in
      match t1 with
      | TFun (a, b) ->
          if t2 = a then Ok b else Error "Mismatch function application"
      | _ -> Error "Not a function")
  | Abs (x, tao, e) ->
      let* res = infer_context (Map.add_multi c ~key:x ~data:tao) e in
      Ok (TFun (tao, res))
  | UOp (op, e) -> infer_context_unop c op e
  | BOp (op, e1, e2) -> infer_context_binop c op e1 e2
  | Let (x, e1, e2) ->
      let* t1 = infer_context c e1 in
      infer_context (Map.add_multi c ~key:x ~data:t1) e2

and infer_context_unop (c : context) (op : op1) (t : term) :
    (typ, string) Result.t =
  let* ty = infer_context c t in
  match (op, ty) with
  | Inverse, TMatrix _ -> Ok ty (* inverse preserves the same type *)
  | Transpose, TMatrix { shape; sparsity } ->
      Ok (TMatrix { shape = List.rev shape; sparsity })
  | Not, TBool -> Ok TBool
  | Negate, TScalar -> Ok TScalar
  | _ -> Error "Unary operator type mismatch"

and infer_context_binop (c : context) (op : op2) (t1 : term) (t2 : term) :
    (typ, string) Result.t =
  let open List in
  let* ty1 = infer_context c t1 in
  let* ty2 = infer_context c t2 in
  match (op, ty1, ty2) with
  | Eq, ty1, ty2 when ty1 = ty2 -> Ok TBool
  | Eq, _, _ -> Error "Equality mismatch"
  | op, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ }
    when op = Add || op = Sub ->
      if shape1 = shape2 then
        Ok (TMatrix { shape = shape1; sparsity = Unknown })
      else Error "Matrix addition shape mismatch"
  | Mult, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
      if last_exn shape1 = hd_exn shape2 then
        Ok
          (TMatrix
             {
               shape = append (drop_last_exn shape1) (tl_exn shape2);
               sparsity = Unknown;
             })
      else Error "Matrix multiplication shape mismatch"
  | Add, TScalar, TScalar -> Ok TScalar
  | Sub, TScalar, TScalar -> Ok TScalar
  | Mult, TScalar, TScalar -> Ok TScalar
  | op, TScalar, TScalar when List.exists ~f:(( = ) op) [ Lt; Gt; Lte; Gte ] ->
      Ok TBool
  | Mult, TScalar, TMatrix _ -> Ok ty2
  | And, TBool, TBool -> Ok TBool
  | Or, TBool, TBool -> Ok TBool
  | _ -> Error "Binary operator type mismatch"

let infer = infer_context (Map.empty (module String))

let infer_program (Program p) =
  TypedProgram (List.zip_exn p (List.map ~f:infer p))
