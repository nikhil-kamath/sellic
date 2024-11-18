fun (m1: <A*A>) (m2: <B*B>) -> m1 * m2;;

// does not type check
fun (m1: <A*A>) (m2: <B*B>) -> m1 * m2 + [[1; 2]];;

fun (a: <3*4>) (b: <X*Y>) -> a * b;;

(fun (a: <N*N>) -> a) [[1]];;
(fun (a: <N*N>) -> a) [[1; 2]; [3; 4]];;

// does not type check
let f = fun (a: <N*N>) -> a * a in (f [[1]]) + (f [[1; 2]; [3; 4]]);;


let f = fun (a: <A*B>) -> a in (f [[1; 2]]) * (f [[1]; [2]]);;