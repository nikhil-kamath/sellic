# sellic
Program sparse matrix hardware accelerators more easily through compilation into Hardcaml


## Setup
The language frontend is called `play`. You can interact with the compiler and the hardware consumption estimator by calling `dune exec play -- <filename.sl> [-show [terms | types | inlined]*]` within the `play` directory. 

For now, all external machine learning calls are made through Python subscripts, with saved models in `play/py`. The code to retrain the models is currently closed-source, but I will be uploading new approaches to perform this in my new project, [Anvia](https://github.com/nikhil-kamath/anvia). 

Basic hardware generation is done through Harcaml in the folder `hardcamltest`. This is not integrated with `play`--the actual lowering into optimized hardware is a complex direction I have not fully connected with the compiler toplevel at the moment. We may attempt different lowerings, such as Calyx instead.

## Examples

Examples of Sellic (.sl) programs are found in `play/demo`, which includes a variety of matrix-based benchmarks.
