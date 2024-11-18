open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector

(* Matrix type constrints to a satisfiability solution *)
type size_expr =
  | CNum of int
  | CVar of string
  | CAdd of size_expr * size_expr
  | CSub of size_expr * size_expr
  | CTimes of size_expr * size_expr [@@deriving show]

type restriction = size_expr * size_expr [@@deriving show]
type restrictions = restriction list [@@deriving show]

let rec uniq x =
  let rec uniq_help l n =
    match l with
    | [] -> []
    | h :: t -> if n = h then uniq_help t n else h :: uniq_help t n
  in
  match x with [] -> [] | h :: t -> h :: uniq_help (uniq t) h

let rec repeat n x = match n with 0 -> [] | n -> x :: repeat (n - 1) x

let names rs =
  let rec names_e = function
    | CNum _ -> []
    | CVar x -> [ x ]
    | CAdd (a, b) | CSub (a, b) | CTimes (a, b) ->
        List.append (names_e a) (names_e b)
  in
  Core.List.concat_map rs ~f:(fun (l, r) -> List.append (names_e l) (names_e r))
  |> uniq

let rec expr_of_size (ctx : context) (s : size_expr) =
  match s with
  | CNum i -> Integer.mk_numeral_i ctx i
  | CVar x -> Integer.mk_const ctx (Symbol.mk_string ctx x)
  | CAdd (a, b) ->
      let a = expr_of_size ctx a in
      let b = expr_of_size ctx b in
      Arithmetic.mk_add ctx [ a; b ]
  | CSub (a, b) ->
      let a = expr_of_size ctx a in
      let b = expr_of_size ctx b in
      Arithmetic.mk_sub ctx [ a; b ]
  | CTimes (a, b) ->
      let a = expr_of_size ctx a in
      let b = expr_of_size ctx b in
      Arithmetic.mk_mul ctx [ a; b ]

let solve (rs : restrictions) =
  let open Core.List in
  let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = (mk_context cfg) in
  let ns = List.map (Symbol.mk_string ctx) (names rs) in
  let types = repeat (List.length ns) (Integer.mk_sort ctx) in
  let r = rs
  |> map ~f:(fun (l, r) -> mk_eq ctx (expr_of_size ctx l) (expr_of_size ctx r))
  |> Boolean.mk_and ctx
  in
  let x = Quantifier.mk_exists ctx types ns r (Some 1) [] [] (Some (Symbol.mk_string ctx "hello1")) (Some (Symbol.mk_string ctx "hello")) in
  let solver = (mk_solver ctx None) in
  (* (Printf.printf "c4: %s\n" (Expr.to_string r)) ; *)
  check solver [r] = SATISFIABLE


let rs : restrictions = [
  (CNum 5, CVar "j");
  (CVar "l", CNum 3);
  (CVar "m", CVar "j");
]