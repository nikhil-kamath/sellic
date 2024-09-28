open Base.Result
open Play
open Play.Term

let _ =
  let open Play.Term in
  let sparsity = Unknown in
  let e1 = Matrix { shape = [ 3; 4 ]; elements = Nested [] } in
  let e2 = Matrix { shape = [ 4; 5 ]; elements = Nested [] } in
  let e3 = BOp (Mult, e1, e2) in
  assert (infer e1 = Ok (TMatrix { shape = [ 3; 4 ]; sparsity }));
  assert (infer e3 = Ok (TMatrix { shape = [ 3; 5 ]; sparsity }));

  let e4 = Abs ("x", TBool, e2) in
  let e5 = App (e4, Bool true) in
  assert (infer e4 = Ok (TFun (TBool, TMatrix { shape = [ 4; 5 ]; sparsity })));
  assert (infer e5 = Ok (TMatrix { shape = [ 4; 5 ]; sparsity }));

  let e6 = Abs ("x", TBool, Var "x") in
  assert (infer e6 = Ok (TFun (TBool, TBool)));

  let e7 = App (e6, Scalar 2) in
  assert (infer e7 = Error "Mismatch function application");

  let e8 = Let ("x", Scalar 5, Scalar 5) in
  assert (infer e8 = Ok TScalar)
