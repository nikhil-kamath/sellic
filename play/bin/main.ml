(*
   Copyright (C) 2012 Microsoft Corporation
   Author: CM Wintersteiger (cwinter) 2012-12-17
*)

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

exception TestFailedException of string

(**
   A basic example of how to use quantifiers.
**)
let  quantifier_example1 ( ctx : context ) =
  Printf.printf "QuantifierExample\n" ;
  let is = (Integer.mk_sort ctx) in
  let types = [ is; is; is ] in
  let names = [ (Symbol.mk_string ctx "x_0");
		(Symbol.mk_string ctx "x_1");
		(Symbol.mk_string ctx "x_2") ] in
  let vars = [ (Quantifier.mk_bound ctx 2 (List.nth types 0));
	       (Quantifier.mk_bound ctx 2 (List.nth types 1));
	       (Quantifier.mk_bound ctx 2 (List.nth types 2)) ] in
  let xs = [ (Integer.mk_const ctx (List.nth names 0));
	     (Integer.mk_const ctx (List.nth names 1));
	     (Integer.mk_const ctx (List.nth names 2)) ] in

  let body_vars = (Boolean.mk_and ctx
		     [ (mk_eq ctx
			  (Arithmetic.mk_add ctx [ (List.nth vars 0) ; (Integer.mk_numeral_i ctx 1)])
			  (Integer.mk_numeral_i ctx 2)) ;
		       (mk_eq ctx
			  (Arithmetic.mk_add ctx [ (List.nth vars 1); (Integer.mk_numeral_i ctx 2)])
			  (Arithmetic.mk_add ctx [ (List.nth vars 2); (Integer.mk_numeral_i ctx 3)])) ]) in
  let body_const = (Boolean.mk_and ctx
		      [ (mk_eq ctx
			   (Arithmetic.mk_add ctx [ (List.nth xs 0); (Integer.mk_numeral_i ctx 1)])
			   (Integer.mk_numeral_i ctx 2)) ;
			(mk_eq ctx
			   (Arithmetic.mk_add ctx [ (List.nth xs 1); (Integer.mk_numeral_i ctx 2)])
			   (Arithmetic.mk_add ctx [ (List.nth xs 2); (Integer.mk_numeral_i ctx 3)])) ]) in

  let x = (Quantifier.mk_forall ctx types names body_vars (Some 1) [] [] (Some (Symbol.mk_string ctx "Q1")) (Some (Symbol.mk_string ctx "skid1"))) in
  Printf.printf "Quantifier X: %s\n" (Quantifier.to_string x) ;
  let y = (Quantifier.mk_forall_const ctx xs body_const (Some 1) [] [] (Some (Symbol.mk_string ctx "Q2")) (Some (Symbol.mk_string ctx "skid2"))) in
  Printf.printf "Quantifier Y: %s\n" (Quantifier.to_string y) ;
  print_endline ("nikhil: " ^ (string_of_bool (is_true (Quantifier.expr_of_quantifier x))));
  if (is_true (Quantifier.expr_of_quantifier x)) then
    raise (TestFailedException "JLKFAJKFJELK") (* unreachable *)
  else if (is_false (Quantifier.expr_of_quantifier x)) then
    raise (TestFailedException "") (* unreachable *)
  else if (is_const (Quantifier.expr_of_quantifier x)) then
    raise (TestFailedException "") (* unreachable *)



let _ =
  try (
    if not (Log.open_ "z3.log") then
      raise (TestFailedException "Log couldn't be opened.")
    else
      (
	Printf.printf "Running Z3 version %s\n" Version.to_string ;
	Printf.printf "Z3 full version string: %s\n" Version.full_version ;
	let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = (mk_context cfg) in
	quantifier_example1 ctx ;
	Printf.printf "Disposing...\n";
	Gc.full_major ()
      );
    Printf.printf "Exiting.\n" ;
    exit 0
  ) with Error(msg) -> (
    Printf.printf "Z3 EXCEPTION: %s\n" msg ;
    exit 1
  )
;;
