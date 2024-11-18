#!/usr/bin/env python

import sys
import argparse
import warnings
import pickle
import numpy as np
import pandas as pd
from sklearn.exceptions import InconsistentVersionWarning

warnings.simplefilter("ignore", InconsistentVersionWarning)


class Prediction:

    def __init__(self):
        self.dt_filename = "/Users/nikhil/Code/sellic/play/py/dt.pkl"
        self.gb_s_filename = "/Users/nikhil/Code/sellic/play/py/chain_s.pkl"
        self.gb_rlv_filename = "/Users/nikhil/Code/sellic/play/py/chain_rlv.pkl"

        with open(self.dt_filename, 'rb') as file:
            self.dt = pickle.load(file)

        with open(self.gb_s_filename, 'rb') as file:
            self.gb_s = pickle.load(file)

        with open(self.gb_rlv_filename, 'rb') as file:
            self.gb_rlv = pickle.load(file)

    def make_input(self, sparsityA, sparsityB, rlvA):
        return pd.DataFrame({"sparsityA": [sparsityA], "sparsityB": [
                            sparsityB], "avg_row_lengthA_var": [rlvA]})

    def predict_mult(self, sparsityA, sparsityB, rlvA):
        inp = self.make_input(sparsityA, sparsityB, rlvA)
        return self.dt.predict(inp)[0]

    def predict_sparsity(self, sparsityA, sparsityB, rlvA):
        inp = self.make_input(sparsityA, sparsityB, rlvA)
        return np.clip(self.gb_s.predict(inp)[0], 0, 1)

    def predict_rlv(self, sparsityA, sparsityB, rlvA):
        inp = self.make_input(sparsityA, sparsityB, rlvA)
        return np.clip(self.gb_rlv.predict(inp)[0], 0, 1)


def main():
    parser = argparse.ArgumentParser(
        description="Run prediction functions with three float arguments.")
    parser.add_argument("function", choices=["predict_mult", "predict_sparsity", "predict_rlv"],
                        help="Specify the function to run")
    parser.add_argument("sparsityA", type=float, help="Sparsity of matrix A")
    parser.add_argument("sparsityB", type=float, help="Sparsity of matrix B")
    parser.add_argument("rlvA", type=float, help="Row length var of matrix A")
    args = parser.parse_args()
    sparsityA = args.sparsityA
    sparsityB = args.sparsityB
    rlvA = args.rlvA

    pred = Prediction()

    # Call the chosen function with arguments
    if args.function == "predict_mult":
        result = pred.predict_mult(sparsityA, sparsityB, rlvA)
    elif args.function == "predict_sparsity":
        result = pred.predict_sparsity(sparsityA, sparsityB, rlvA)
    elif args.function == "predict_rlv":
        result = pred.predict_rlv(sparsityA, sparsityB, rlvA)
    else:
        print("Invalid function name.")
        sys.exit(1)

    print(result)


if __name__ == "__main__":
    main()
