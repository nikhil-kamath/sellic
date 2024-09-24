type scalar = Scalar of int

type matrix = {
  (* will include other data later *)
  dims : int;
  shape : int list;
  elements : int list (* flattened representation for now *);
}

type binop = Add | Multiply | Subtract
type unop = Inverse | Transpose

type mexpression =
  (* every mexpression can be reduced to a matrix *)
  | Matrix of matrix
  | Binop of binop * mexpression * mexpression
  | Unop of unop * mexpression
  | If of bool * mexpression * mexpression
  | Scale of scalar * mexpression
