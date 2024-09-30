(*** FULL LANGUAGE *)
open Base
open Matrix

let ( = ) = Poly.( = )

type sparsity = Sparse | Dense | Unknown [@@deriving show]

type typ =
  | TScalar
  | TBool
  | TMatrix of { shape : int list; sparsity : sparsity }
  | TFun of typ * typ
  | TPoly
  | TNoType
[@@deriving show]

type op1 = Inverse | Transpose | Not | Negate [@@deriving show]

type op2 = Mult | Add | Sub | And | Or | Lt | Gt | Lte | Gte | Eq
[@@deriving show]

type term =
  | Matrix of { shape : int list; elements : nested } (* primitive matrices *)
  | Bool of bool
  | Scalar of int
  | Var of string
  | App of term * term (* basic function application *)
  | Fold of
      term
      * term
      * term (* matrix-"polymorphic" fold [(acc -> a -> acc), acc, [a]) *)
  | Map of term * term (* matrix-"polymorphic" map [(a -> b), [a]]*)
  | If of term * term * term
  | Abs of string * typ * term
  | UOp of op1 * term
  | BOp of op2 * term * term
  | Let of string * term * term
[@@deriving show]

type program = Program of term list [@@deriving show]
type typed_program = TypedProgram of (term * (typ, string) Core.Result.t) list

let show_typed_program (TypedProgram tp) =
  String.concat ~sep:"\n\n========\n\n"
    (List.map tp ~f:(fun (t, ty) ->
         show_term t ^ "\n"
         ^
         match ty with
         | Error s -> ">> " ^ s
         | Ok ty ->
             ">> "
             ^ String.substr_replace_all ~pattern:"\n" ~with_:"\n>> "
                 (show_typ ty)))
