open Term
open Base
open Solver
open Sparsity

(** Type Inference? *)
let ( = ) = Poly.( = )

let ( let* ) = Base.Result.( >>= )

type context = (string, typ list, String.comparator_witness) Map.t

let rec infer_context (c : context) (r : restrictions) (t : term) :
    (typ * restrictions, string) Result.t =
  let open List in
  match t with
  | Matrix { shape; elements } ->
      Ok
        ( TMatrix
            {
              shape;
              sparsity =
                { s = Sparsity.s elements; rlv = Sparsity.rlv elements };
            },
          [] )
  | Bool _ -> Ok (TBool, [])
  | Scalar _ -> Ok (TScalar, [])
  | If (p, e1, e2) ->
      let* p, r = infer_context c r p in
      let* e1, r = infer_context c r e1 in
      let* e2, r = infer_context c r e2 in
      if p = TBool && e1 = e2 then Ok (e1, r) else Error "Invalid If"
  | Var x -> (
      match Map.find c x with
      | Some ty -> Ok (hd_exn ty, r)
      | _ -> Error "Invalid variable")
  | App (e1, e2) -> (
      let* t1, r = infer_context c r e1 in
      (* do we need to append the r's? *)
      let* t2, r = infer_context c r e2 in
      match t1 with
      | TFun (a, b) -> (
          match (t2, a) with
          | TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
              let r = append (zip_exn shape1 shape2) r in
              if solve r then
                Ok
                  ( TMatrix { shape = shape1; sparsity = { s = 0.; rlv = 0. } },
                    [] ) (* application concretizes the restrictions *)
              else
                Error
                  "Solver could not find a solution for our addition \
                   constraints"
          | _ ->
              if t2 = a then Ok (b, r)
              else Error "Mismatch function application")
      | _ -> Error "Not a function")
  | Abs (x, tao, e) ->
      let* res, r = infer_context (Map.add_multi c ~key:x ~data:tao) r e in
      Ok (TFun (tao, res), r)
  | UOp (op, e) -> infer_context_unop c r op e
  | BOp (op, e1, e2) -> infer_context_binop c r op e1 e2
  | Let (x, e1, e2) ->
      let* t1, _ = infer_context c r e1 in
      infer_context (Map.add_multi c ~key:x ~data:t1) r e2

and infer_context_unop (c : context) (r : restrictions) (op : op1) (t : term) :
    (typ * restrictions, string) Result.t =
  let* ty, r = infer_context c r t in
  match (op, ty) with
  | Not, TBool -> Ok (TBool, r)
  | Negate, TScalar -> Ok (TScalar, r)
  | _ -> Error "Unary operator type mismatch"

and infer_context_binop (c : context) (r : restrictions) (op : op2) (t1 : term)
    (t2 : term) : (typ * restrictions, string) Result.t =
  let open List in
  let* ty1, r2 = infer_context c r t1 in
  let* ty2, r3 = infer_context c (append r r2) t2 in
  let r = concat [ r; r2; r3 ] in
  match (op, ty1, ty2) with
  | Eq, ty1, ty2 when ty1 = ty2 -> Ok (TBool, r)
  | Eq, _, _ -> Error "Equality mismatch"
  (* Sparsit of the sum of matrices? *)
  | ( Add,
      TMatrix { shape = shape1; sparsity = sp1 },
      TMatrix { shape = shape2; sparsity = sp2 } )
  | ( Sub,
      TMatrix { shape = shape1; sparsity = sp1 },
      TMatrix { shape = shape2; sparsity = sp2 } ) ->
      let r = append (zip_exn shape1 shape2) r in
      if solve r then Ok (TMatrix { shape = shape1; sparsity = sp1 }, r)
      else Error "Solver could not find a solution for our addition constraints"
  | ( Mult,
      TMatrix { shape = shape1; sparsity = sp1 },
      TMatrix { shape = shape2; sparsity = sp2 } ) ->
      let r = (last_exn shape1, hd_exn shape2) :: r in
      if solve r then
        Ok
          ( TMatrix
              {
                shape = append (drop_last_exn shape1) (tl_exn shape2);
                sparsity = Sparsity.predict sp1 sp2; (* chaining rule *)
              },
            r )
      else
        Error
          "Solver could not find a solution for our multiplication constraints"
  | Add, TScalar, TScalar -> Ok (TScalar, r)
  | Sub, TScalar, TScalar -> Ok (TScalar, r)
  | Mult, TScalar, TScalar -> Ok (TScalar, r)
  | op, TScalar, TScalar when List.exists ~f:(( = ) op) [ Lt; Gt; Lte; Gte ] ->
      Ok (TBool, r)
  | Mult, TScalar, TMatrix _ -> Ok (ty2, r)
  | And, TBool, TBool -> Ok (TBool, r)
  | Or, TBool, TBool -> Ok (TBool, r)
  | _ -> Error "Binary operator type mismatch"

let infer = infer_context (Map.empty (module String)) []
let infer_with_term t = (t, Result.( >>= ) (infer t) (fun (ty, _) -> Ok ty))
let infer_program p = List.map p ~f:infer_with_term |> fun t -> TypedProgram t
