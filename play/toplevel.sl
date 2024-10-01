// fun (x : scalar) -> 3;;
let add = (fun ( a : scalar) ( b : scalar) -> a + b);;
let succ = add 1;;
let pred = add -1;;

let compose = (fun (f : scalar -> scalar) (g: scalar -> scalar) (x: scalar) -> f (g x));;

let id = compose succ pred;;

let sum3 = (fun (m: M<3>) -> add / 0 \ m);;

let sum5x3 = (fun (m: M<5*3>) -> add / 0 \ (sum3 ' m) );;

let m = [
    [1; 2; 3];
    [1; 2; 3];
    [1; 2; 3];
    [1; 2; 3];
    [1; 2; 3];
] in sum5x3 m;;

fun (x: scalar) (y: bool) -> 3;;

compose succ pred 12;;
