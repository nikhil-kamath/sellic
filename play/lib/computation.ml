(** UNUSED *)

(* Storing computation graphs for matrix calculations *)
open Core
open Matrix
open Term
open Types

type nid = Id of int [@@deriving show]
type node = { nid : nid; output_t : typ; inputs : nid list } [@@deriving show]

let nid n = n.nid
(*
   each node has:
   - a unique `nid`
   - one specific output type
   - a list of inputs: can be empty or have duplicates *)

let fresh =
  let counter = ref 0 in
  let _fresh () =
    counter := !counter + 1;
    Id !counter
  in
  _fresh

type var_bindings = (string, nid) Hashtbl.t (* variable bindings / placeholders for function application *)
type node_bindings = (nid, node) Hashtbl.t (* node id -> node bindings*)

(* let rec graph (c : context) (xb: var_bindings) (nb: node_bindings)  (t : term) : node =
  let open Result in
  (* add and return *)
  let aar n = Hashtbl.set nb ~key:n.nid ~data:n; n in
  match t with
  | Matrix { shape; _ } ->
      aar {
        nid = fresh ();
        output_t = TMatrix { shape; sparsity = Unknown };
        inputs = [];
      }
  | Bool _ -> aar { nid = fresh (); output_t = TBool; inputs = [] }
  | Scalar _ -> aar { nid = fresh (); output_t = TScalar; inputs = [] }
  | If (p, e1, e2) ->
      let p = graph c xb nb p |> nid in
      let e1 = graph c xb nb e1 in
      let e2 = graph c xb nb e2 |> nid in
      aar {
        nid = fresh ();
        output_t = e1.output_t;
        inputs = [ p; e1.nid; e2 ];
      }
  | Var x ->
      let output_t = List.hd_exn (Map.find_multi c x) in
      let nid = if Hashtbl.mem b x then Hashtbl.find_exn b x else fresh () in
      Hashtbl.set b ~key:x ~data:nid;
      { nid; output_t; inputs = [] }
  | App (e1, e2) ->
    let e1 = graph c b e1 in
    let e2 = graph c b e2 |> nid in
    { nid=e1.nid; output_t=e1.output_t; inputs=[e2] }
  | *)
