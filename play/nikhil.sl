(let x = (3) in (fun x : scalar -> if (-x) || true then true else 19)) 5;;

fun m : M<3*3, unknown> -> [
    [1; 0; 0;];
    [0; 1; 0];
    [0; 0; 1]
];;

[[0]; [0]; [0]];;
[[0]; [0]; [0;]];;
[[0;]; [0;]; [0;];];;

let f = fun x : scalar -> x in f 3;;
let f = fun x : scalar -> x in f true;;

let x = [
    [1; 0; 0];
    [0; 1; 0];
    [0; 0; 1]
] in
let y = [
    [0; 0; 1];
    [0; 1; 0];
    [1; 0; 0];
] in
let f = (fun x: M<3*3, unknown> -> y) in
if true then x else f x ;;

fun m2 : M<1*3, dense> -> m2;;

fun m1 : M<3*20, dense> -> fun m2 : M <20 * 1, dense> -> m1 * m2;;

fun t1:M<3*4>  t2:M<4*2> -> t1 * t2;;

fun m1 : M<3*4, unknown> -> fun m2 : M<4*5, unknown> -> fun m3 : M<3*5, unknown> ->
    if (m1 * m2) = m3 then 5 > 3 else 5 <= 3;;

[
    [1; 0; 0];
    [0; 1; 0];
    [0; 0; 1];
];;


3 3 3;;

-3 - 3;;
