let m1 = [[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];];;
let m2 = [[1; 2; 3; 4];[1; 2; 3; 4];[1; 2; 3; 4];];;
let m3 = [[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];[1; 2; 3; 4; 5];];;

let add = fun (a: scalar) (b: scalar) -> a + b;;

let sum_5 = fun (m : M<5>) -> add / 0 \ m;;

let sum_5x5 = fun (m : M<5*5>) ->
    (let z = sum_5 ' m in sum_5 z);;

let mult = fun (m1 : M<3*4, sparse>) (m2 : M<4*5, dense>) -> m1 * m2;;

sum_5x5 m1;;
mult m2 m3;;
