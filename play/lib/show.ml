open Term
open Core
open Inline
open Types

type typed_term = term * (typ, string) result

let show_terms (tt : typed_term) = fst tt |> show_term

let show_types (tt : typed_term) =
  let ok_show f x = match x with Ok x -> f x | Error s -> s in
  snd tt |> ok_show show_typ

let show_inlined (tt : typed_term) = fst tt |> inline |> show_term

let show_inlined_types (tt : typed_term) =
  fst tt |> infer |> Result.ok_or_failwith |> fst |> show_typ

let indent s sep =
  let sep = sep ^ " " in
  sep ^ String.substr_replace_all ~pattern:"\n" ~with_:("\n" ^ sep) s

let rec nest d ss =
  match ss with
  | [] -> ""
  | s :: ss ->
      let sep = String.make d '>' in
      indent s sep ^ "\n\n" ^ nest (d + 1) ss

let show_tt
    ?(whats : [< `Terms | `Types | `Inlined | `InlinedTypes ] list = [ `Terms ])
    (tt : typed_term) =
  List.map
    ~f:(function
      | `Terms -> show_terms tt
      | `Types -> show_types tt
      | `Inlined -> show_inlined tt
      | `InlinedTypes -> show_inlined_types tt)
    whats
  |> nest 0

let show
    ?(whats : [ `Terms | `Types | `Inlined | `InlinedTypes ] list = [ `Terms ])
    (TypedProgram tp) =
  List.map ~f:(show_tt ~whats) tp
  |> String.concat ~sep:"\n--------------------\n\n"

let show_typed_program t = show
