open Base.Result
open Play
open Play.Reduce
open Play.Term

let _ =
  let open Play.Mexpression in
  let m1 =
    { dims = 2; shape = [ 5; 3 ]; elements = Base.List.range 0 (5 * 3) }
  in
  let m2 =
    { dims = 2; shape = [ 3; 4 ]; elements = Base.List.range 0 (3 * 4) }
  in
  let m3 =
    { dims = 2; shape = [ 3; 4 ]; elements = Base.List.range 0 (3 * 4) }
  in
  let m1_e = Matrix m1 in
  let m2_e = Matrix m2 in
  let m3_e = Matrix m3 in

  assert (check m1_e = Ok (Shape m1.shape));
  assert (check m2_e = Ok (Shape m2.shape));

  let mx_e = Binop (Multiply, m1_e, m2_e) in
  assert (check mx_e = Ok (Shape [ 5; 4 ]));

  let mx_e2 = Scale (Scalar 2, mx_e) in
  assert (check mx_e2 = Ok (Shape [ 5; 4 ]));

  let mp_e = Binop (Add, m2_e, m3_e) in
  assert (check mp_e = Ok (Shape m3.shape));

  let mb_e = If (true, m2_e, m3_e) in
  assert (check mb_e = Ok (Shape m3.shape))

let _ =
  let open Play.Term in
  let sparsity = Unknown in
  let e1 = Matrix { shape = [ 3; 4 ]; elements = Base.List.range 0 12 } in
  let e2 = Matrix { shape = [4; 5]; elements = Base.List.range 0 20 } in
  let e3 = BOp (MMult, e1, e2) in
  assert (infer e1 = Ok (TMatrix { shape = [ 3; 4 ]; sparsity }));
  assert (infer e3 = Ok (TMatrix { shape = [ 3; 5 ]; sparsity }));

  let e4 = Abs ("x", TBool, e2) in
  let e5 = App (e4, Bool true) in
  assert (infer e4 = Ok (TFun (TBool, TMatrix {shape = [4; 5]; sparsity})));
  assert (infer e5 = Ok (TMatrix {shape = [4; 5]; sparsity}))

