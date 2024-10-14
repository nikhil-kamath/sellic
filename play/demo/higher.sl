/* FOLDS */

(fun (x: scalar) -> x + 2) ' [1; 2; 3];;
(fun (x: scalar) -> x + 2) ' [1; 2; 3; 4];;
(fun (x: scalar) -> [1; 2; 3]) ' [0; 0; 0];;
(fun (x: M<3>) -> 1) ' [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]];;

// wrong
(fun (x: M<2>) -> 1) ' [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]];;

(fun (x: M<3>) -> [[0; 1]; [2; 3]]) ' [[1; 2; 3]; [1; 2; 3]; [1; 2; 3]; [1; 2; 3]];;

(fun (x: M<3*3>) -> [[0; 1]]) ' [
    [
        [1; 0; 0];
        [0; 1; 0];
        [0; 0; 1];
    ];

    [
        [2; 0; 0];
        [0; 2; 0];
        [0; 0; 2]
    ]
];;

/* REDUCTIONS */

// sum function
(fun (a: scalar) (x: scalar) -> a + x) / 0 \ [1; 2; 3];;

// length function
(fun (a: scalar) (x: scalar) -> a + 1) / 0 \ [1; 2; 3; 4];;

// Nx5 sum
(fun (a : scalar) (x : M<5>) -> a + ((fun (a: scalar) (x: scalar) -> a + x) / 0 \ x)) / 0 \ [
// THE ISSUE HERE IS ^^^ HAS TO BE SPECIFIED, EVEN THOUGH THE FUNCION DOESN'T DEPEND ON IT
    [1; 2; 3; 4; 5];
    [6; 7; 8; 9; 10];
    [1; 2; 3; 4; 5];
];;

// add a matrix of 3x3 submatrices
let init = [
    [0; 0; 0];
    [0; 0; 0];
    [0; 0; 0];
] in
(fun (a: M<3*3>) (x: M<3*3>) -> x + a) / init \ [
    [
        [1; 1; 1];
        [1; 1; 1];
        [1; 1; 1];
    ];
    [
        [2; 2; 2];
        [2; 2; 2];
        [2; 2; 2];
    ];
    [
        [3; 3; 3];
        [3; 3; 3];
        [3; 3; 3];
    ];
];;
