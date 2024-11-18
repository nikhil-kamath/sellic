// super simple feed-forward neural network that sums the first, 5th, and last elements of a 9d input
//            v           v           v
let input = [[1; 2; 3; 4; 5; 6; 7; 8; 9]] in

let layer1 = [
    [1; 0; 0];
    [0; 0; 0];
    [0; 0; 0];
    [0; 0; 0];
    [0; 1; 0];
    [0; 0; 0];
    [0; 0; 0];
    [0; 0; 0];
    [0; 0; 1];
] in

let layer2 = [[1]; [1]; [1]] in

input * layer1 * layer2;;

[
    [1; 0; 0; 0; 0];
    [1; 0; 0; 0; 0];
    [1; 0; 0; 0; 0];
    [1; 0; 0; 0; 0];
    [1; 1; 0; 0; 0];
];;

[[1]; [1]; [1]];;