(*** FULL LANGUAGE *)
open Base
open Matrix

let ( = ) = Poly.( = )
let ( >>| ) = List.( >>| )

type typ =
  | TScalar
  | TBool
  | TMatrix of { shape : dims; sparsity : sparsity }
  | TFun of typ * typ
  | TPoly
  | TNoType
[@@deriving show]

type op1 = Inverse | Transpose | Not | Negate [@@deriving show]

type op2 = Mult | Add | Sub | And | Or | Lt | Gt | Lte | Gte | Eq
[@@deriving show]

type term =
  | Matrix of { shape : dims; elements : nested } (* primitive matrices *)
  | Bool of bool
  | Scalar of float
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

(* toplevel mappings of name -> term *)
type toplevel_def = Def of string * term [@@deriving show]
type toplevel = Toplevel of toplevel_def list [@@deriving show]
type program = Program of toplevel * term list [@@deriving show]
type typed_program = TypedProgram of (term * (typ, string) Core.Result.t) list

