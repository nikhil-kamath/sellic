open Term
open Inline
open Base
open Solver
open Sparsity

(** Type Inference *)
let ( = ) = Poly.( = )

let ( =? ) ty1 ty2 =
  match (ty1, ty2) with
  | TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
      shape1 = shape2
  | _ -> ty1 = ty2

let ( let* ) = Base.Result.( >>= )

type context = (string, typ list, String.comparator_witness) Map.t

let ty_of_annot a =
  match a with
  | AMatrix { typ; _ } -> typ
  | ABool _ -> TBool
  | AScalar _ -> TScalar
  | AVar (_, typ) -> typ
  | AApp (_, _, typ) -> typ
  | AIf (_, _, _, typ) -> typ
  | AAbs (_, _, _, typ) -> typ
  | AUOp (_, _, typ) -> typ
  | ABOp (_, _, _, typ) -> typ
  | ALet (_, _, _, typ) -> typ

let rec infer_context (c : context) (r : restrictions) (t : term) :
    (annotated * restrictions * typ, string) Result.t =
  let open List in
  match t with
  | Matrix { shape; elements } ->
      let typ =
        TMatrix
          {
            shape;
            sparsity = { s = Sparsity.s elements; rlv = Sparsity.rlv elements };
          }
      in
      Ok (AMatrix { shape; elements; typ }, [], typ)
  | Bool b -> Ok (ABool b, [], TBool)
  | Scalar f -> Ok (AScalar f, [], TScalar)
  | If (p, e1, e2) ->
      let* p, r, ty0 = infer_context c r p in
      let* e1, r, ty1 = infer_context c r e1 in
      let* e2, r, ty2 = infer_context c r e2 in
      (* IF EXPRESSIONS BRANCHING BETWEEN 2 POSSIBLE SPARSITIES, CHOOSES THE FIRST ONE *)
      if ty0 = TBool && ty1 =? ty2 then Ok (AIf (p, e1, e2, ty1), r, ty1)
      else Error "Invalid If"
  | Var x -> (
      match Map.find c x with
      | Some ty -> Ok (AVar (x, hd_exn ty), r, hd_exn ty)
      | _ -> Error "Invalid variable")
  | App (e1, e2) -> (
      let* e1, r, ty1 = infer_context c r e1 in
      (* do we need to append the r's? *)
      let* e2, r, ty2 = infer_context c r e2 in
      match ty1 with
      | TFun (a, b) -> (
          match (ty2, a) with
          | TMatrix { shape = shape1; _ }, TMatrix { shape = shape2; _ } ->
              let r = append (zip_exn shape1 shape2) r in
              if solve r then
                let typ =
                  TMatrix { shape = shape1; sparsity = { s = 0.; rlv = 0. } }
                in
                Ok (AApp (e1, e2, typ), [], typ)
              else
                Error
                  "Solver could not find a solution for our addition \
                   constraints"
          | _ ->
              if ty2 = a then Ok (AApp (e1, e2, b), r, b)
              else Error "Mismatch function application")
      | _ -> Error "Not a function")
  | Abs (x, tao, e) ->
      let* res, r, body =
        infer_context (Map.add_multi c ~key:x ~data:tao) r e
      in
      let typ = TFun (tao, body) in
      Ok (AAbs (x, tao, res, typ), r, typ)
  | UOp (op, e) -> infer_context_unop c r op e
  | BOp (op, e1, e2) -> infer_context_binop c r op e1 e2
  | Let (x, e1, e2) ->
      let* e1, _, ty1 = infer_context c r e1 in
      let* e2, r, ty2 = infer_context (Map.add_multi c ~key:x ~data:ty1) r e2 in
      Ok (ALet (x, e1, e2, ty2), r, ty2)

and infer_context_unop (c : context) (r : restrictions) (op : op1) (t : term) :
    (annotated * restrictions * typ, string) Result.t =
  let* e, r, typ = infer_context c r t in
  match (op, typ) with
  | Not, TBool -> Ok (e, r, TBool)
  | Negate, TScalar -> Ok (e, r, TScalar)
  | _ -> Error "Unary operator type mismatch"

and infer_context_binop (c : context) (r : restrictions) (op : op2) (t1 : term)
    (t2 : term) : (annotated * restrictions * typ, string) Result.t =
  let open List in
  let* e1, r2, ty1 = infer_context c r t1 in
  let* e2, r3, ty2 = infer_context c (append r r2) t2 in
  let r = concat [ r; r2; r3 ] in
  match (op, ty1, ty2) with
  | Eq, ty1, ty2 when ty1 = ty2 -> Ok (ABOp (Eq, e1, e2, TBool), r, TBool)
  | Eq, _, _ -> Error "Equality mismatch"
  (* Sparsit of the sum of matrices? *)
  | ( Add,
      TMatrix { shape = shape1; sparsity = sp1 },
      TMatrix { shape = shape2; sparsity = sp2 } )
  | ( Sub,
      TMatrix { shape = shape1; sparsity = sp1 },
      TMatrix { shape = shape2; sparsity = sp2 } ) ->
      let r = append (zip_exn shape1 shape2) r in
      let typ = TMatrix { shape = shape1; sparsity = sp1 } in
      if solve r then Ok (ABOp (op, e1, e2, typ), r, typ)
      else Error "Solver could not find a solution for our addition constraints"
  | ( Mult,
      TMatrix { shape = shape1; sparsity = sp1 },
      TMatrix { shape = shape2; sparsity = sp2 } ) ->
      let r = (last_exn shape1, hd_exn shape2) :: r in
      let typ =
        TMatrix
          {
            shape = append (drop_last_exn shape1) (tl_exn shape2);
            sparsity = Sparsity.predict sp1 sp2;
            (* chaining rule *)
          }
      in
      if solve r then Ok (ABOp (op, e1, e2, typ), r, typ)
      else Error "Unsolvable multiplication constraints"
  | Add, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TScalar), r, TScalar)
  | Sub, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TScalar), r, TScalar)
  | Mult, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TScalar), r, TScalar)
  | Lt, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TBool), r, TBool)
  | Gt, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TBool), r, TBool)
  | Lte, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TBool), r, TBool)
  | Gte, TScalar, TScalar -> Ok (ABOp (op, e1, e2, TBool), r, TBool)
  | And, TBool, TBool -> Ok (ABOp (op, e1, e2, TBool), r, TBool)
  | Or, TBool, TBool -> Ok (ABOp (op, e1, e2, TBool), r, TBool)
  | _ -> Error "Binary operator type mismatch"

let infer t =
  match infer_context (Map.empty (module String)) [] t with
  | Ok (_a, r, ty) -> Ok ty
  | Error s -> Error s

let annotate t =
  let t = inline t in
  match infer_context (Map.empty (module String)) [] t with
  | Ok (a, _, _) -> Ok a
  | Error s -> Error s

let infer_with_term t = (t, Result.( >>= ) (infer t) (fun ty -> Ok ty))
let infer_program p = List.map p ~f:infer_with_term |> fun t -> TypedProgram t
let annotate_program p = List.map p ~f:annotate |> fun a -> AnnotatedProgram a
