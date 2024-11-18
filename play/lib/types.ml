open Term
open Base
open Solver

(** Type Inference? *)
let ( = ) = Poly.( = )

let ( let* ) = Base.Result.( >>= )

type context = (string, typ list, String.comparator_witness) Map.t



let rec infer_context (c : context) (r : restrictions) (t : term) :
    (typ * restrictions, string) Result.t =
  let open List in
  match t with
  | Matrix { shape; _ } -> Ok (TMatrix { shape; sparsity = Unknown }, [])
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
                Ok (TMatrix { shape = shape1; sparsity = Unknown }, []) (* application concretizes the restrictions *)
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
  | Fold (f, init, xs) -> (
      let* f, r = infer_context c r f in
      let* init, r = infer_context c r init in
      let* xs, r = infer_context c r xs in
      match (f, xs) with
      (* lowest-level reduction (1-dim matrix) *)
      | TFun (acc, TFun (TScalar, res)), TMatrix { shape; _ }
        when acc = res && acc = init && length shape = 1 ->
          Ok (res, r)
      (* higher-level reduction (n-dim matrix) *)
      | ( TFun (acc, TFun (TMatrix { shape = ishape; _ }, res)),
          TMatrix { shape; _ } )
        when acc = res && acc = init && ishape = drop shape 1 ->
          Ok (res, r)
      | _, TMatrix _ -> Error "Invalid folding function"
      | _ -> Error "Trying to fold over a non-matrix")
  | Map (f, xs) -> (
      let* f, r = infer_context c r f in
      let* xs, r = infer_context c r xs in
      match (f, xs) with
      | TFun (TScalar, TScalar), TMatrix { shape; _ } ->
          Ok (xs, r) (* simplest map on a matrix, maintains shape *)
      | TFun (TScalar, TMatrix { shape = oshape; _ }), TMatrix { shape; _ } ->
          (* mapping scalar -> matrix appends the function output shape to the matrix shape *)
          Ok
            (TMatrix { shape = List.append oshape shape; sparsity = Unknown }, r)
      | TFun (TMatrix { shape = ishape; _ }, TScalar), TMatrix { shape; _ }
      (* mapping a suffix of the shape to a scalar *)
        when List.is_suffix shape ~suffix:ishape ~equal:( = ) ->
          Ok
            ( TMatrix
                {
                  shape = take shape (length shape - length ishape);
                  sparsity = Unknown;
                },
              r )
          (* mapping a suffix of the shape to a different shape *)
      | ( TFun (TMatrix { shape = ishape; _ }, TMatrix { shape = oshape; _ }),
          TMatrix { shape; _ } )
        when List.is_suffix shape ~suffix:ishape ~equal:( = ) ->
          Ok
            ( TMatrix
                {
                  shape =
                    append (take shape (length shape - length ishape)) oshape;
                  sparsity = Unknown;
                },
              r )
      | _, TMatrix _ -> Error "Invalid folding function"
      | _ -> Error "Trying to map over a non-matrix")

and infer_context_unop (c : context) (r : restrictions) (op : op1) (t : term) :
    (typ * restrictions, string) Result.t =
  let* ty, r = infer_context c r t in
  match (op, ty) with
  | Inverse, TMatrix _ -> Ok (ty, r) (* inverse preserves the same type *)
  | Transpose, TMatrix { shape; sparsity } ->
      Ok (TMatrix { shape = List.rev shape; sparsity }, r)
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
  | Add, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ }
  | Sub, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
      let r = append (zip_exn shape1 shape2) r in
      if solve r then Ok (TMatrix { shape = shape1; sparsity = Unknown }, r)
      else Error "Solver could not find a solution for our addition constraints"
  | Mult, TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
      let r = (last_exn shape1, hd_exn shape2) :: r in
      if solve r then
        Ok
          ( TMatrix
              {
                shape = append (drop_last_exn shape1) (tl_exn shape2);
                sparsity = Unknown;
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

let infer_program (Program (Toplevel top, p)) =
  let open List in
  let context = Map.empty (module String) in
  (* type the toplevel mappings in order, expanding the original context *)
  let context, toplevel_mappings =
    List.fold_left top ~init:(context, [])
      ~f:(fun (context, defs) (Def (x, t)) ->
        let result_ty = infer_context context [] t in
        let context =
          match result_ty with
          | Ok (ty, _) -> Map.add_multi context ~key:x ~data:ty
          (* if a top-level definition does not type check, can't add to context *)
          | _ -> context
        in
        (context, (t, result_ty) :: defs))
  in
  let fst = Result.map ~f:fst in
  TypedProgram
    (List.append
       (List.rev toplevel_mappings >>| fun (a, b) -> (a, fst b))
       (List.zip_exn p (p >>| infer_context context [] >>| fst)))
