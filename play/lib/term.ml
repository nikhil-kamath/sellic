(*** FULL LANGUAGE *)
open Base
open Matrix
open Sparsity

let ( = ) = Poly.( = )
let ( >>| ) = List.( >>| )

type typ =
  | TScalar
  | TBool
  | TMatrix of { shape : dims; sparsity : Sparsity.t }
  | TFun of typ * typ
  | TPoly
  | TNoType
[@@deriving show]

type op1 = Not | Negate [@@deriving show]

type op2 = Mult | Add | Sub | And | Or | Lt | Gt | Lte | Gte | Eq
[@@deriving show]

type term =
  | Matrix of { shape : dims; elements : nested } (* primitive matrices *)
  | Bool of bool
  | Scalar of float
  | Var of string
  | App of term * term (* basic function application *)
  | If of term * term * term
  | Abs of string * typ * term
  | UOp of op1 * term
  | BOp of op2 * term * term
  | Let of string * term * term
[@@deriving show]

type annotated =
  | AMatrix of { shape : dims; elements : nested; typ : typ }
  | ABool of bool
  | AScalar of float
  | AVar of string * typ
  | AApp of annotated * annotated * typ
  | AIf of annotated * annotated * annotated * typ
  | AAbs of string * typ * annotated * typ
  | AUOp of op1 * annotated * typ
  | ABOp of op2 * annotated * annotated * typ
  | ALet of string * annotated * annotated * typ
[@@deriving show]

(* toplevel mappings of name -> term *)
(* SIMPLIFIED: unused *)
(* type toplevel_def = Def of string * term [@@deriving show] *)
(* type toplevel = Toplevel of toplevel_def list [@@deriving show] *)
(* type program = Program of toplevel * term list [@@deriving show] *)
type program = term list [@@deriving show]
type typed_program = TypedProgram of (term * (typ, string) Core.Result.t) list

type annotated_program =
  | AnnotatedProgram of (annotated, string) Core.Result.t list
