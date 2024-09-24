(*** FULL LANGUAGE *)
open Base
open Base.List
open Base.Result

let ( = ) = Poly.( = )

type sparsity = Sparse | Dense | Unknown

type typ =
  | TScalar
  | TBool
  | TMatrix of { shape : int list; sparsity : sparsity }
  | TFun of typ * typ

type op1 = Inverse | Transpose | BNot | SNegate
type op2 = MAdd | MMult | SAdd | SMult | BAnd | BOr

type term =
  | Matrix of { shape : int list; elements : int list }
  | Bool of bool
  | Scalar of int
  | If of term * term * term
  | Var of string
  | App of term * term
  | Abs of string * typ * term
  | UOp of op1 * term
  | BOp of op2 * term * term

(** Substitution *)
let rec subst (x : string) (s : term) (t : term) : term =
  let ( = ) = String.( = ) in
  match t with
  | Matrix _ | Scalar _ | Bool _ -> t
  | If (p, e1, e2) -> If (subst x s p, subst x s e1, subst x s e2)
  | Var y -> if x = y then s else t
  | App (e1, e2) -> App (subst x s e1, subst x s e2)
  | Abs (y, tao, e) -> if x = y then t else Abs (y, tao, subst x s e)
  | UOp (o, e) -> UOp (o, subst x s e)
  | BOp (o, e1, e2) -> BOp (o, subst x s e1, subst x s e2)

(** Values *)
let is_value (t : term) : bool =
  match t with Matrix _ | Scalar _ | Bool _ | Abs _ -> true | _ -> false

(** Type Inference? *)
let ( let* ) = Base.Result.( >>= )

type context = (string, typ list, String.comparator_witness) Map.t

let rec infer_context (c : context) (t : term) : (typ, string) Result.t =
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

and infer_context_unop (c : context) (op : op1) (t : term) :
    (typ, string) Result.t =
  let* ty = infer_context c t in
  match (op, ty) with
  | Inverse, TMatrix _ -> Ok ty (* inverse preserves the same type *)
  | Transpose, TMatrix { shape; sparsity } ->
      Ok (TMatrix { shape = rev shape; sparsity })
  | BNot, TBool -> Ok TBool
  | SNegate, TScalar -> Ok TScalar
  | _ -> Error "Unary operator type mismatch"

and infer_context_binop (c : context) (op : op2) (t1 : term) (t2 : term) :
    (typ, string) Result.t =
  let* ty1 = infer_context c t1 in
  let* ty2 = infer_context c t2 in
  match (op, ty1, ty2) with
  | MAdd, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
      if shape1 = shape2 then
        Ok (TMatrix { shape = shape1; sparsity = Unknown })
      else Error "Matrix addition shape mismatch"
  | MMult, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
      if last_exn shape1 = hd_exn shape2 then
        Ok
          (TMatrix
             {
               shape = append (drop_last_exn shape1) (tl_exn shape2);
               sparsity = Unknown;
             })
      else Error "Matrix multiplication shape mismatch"
  | SAdd, TScalar, TScalar -> Ok TScalar
  | SMult, TScalar, TScalar -> Ok TScalar
  | SMult, TScalar, TMatrix _ -> Ok ty2
  | BAnd, TBool, TBool -> Ok TBool
  | BOr, TBool, TBool -> Ok TBool
  | _ -> Error "Binary operator type mismatch"

let infer = infer_context (Map.empty (module String))
