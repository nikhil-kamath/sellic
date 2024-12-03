# Generate matrices!
import numpy as np
import random
import sys
import pyperclip


np.set_printoptions(threshold=sys.maxsize)


def main():
    number = int(input("Enter number of matrices: "))
    out = ""
    var = 'a'
    for _ in range(number):
        shape = input("Enter dimensions (space-separated): ")
        shape = shape.split()
        shape = map(int, shape)
        shape = list(shape)

        sparsity = input("Enter sparsity (float): ")
        sparsity = float(sparsity)

        arr = np.zeros(tuple(shape))
        size = arr.size
        nonzero = int(size * sparsity)

        while nonzero:
            index = tuple([random.choice(list(range(dim))) for dim in shape])
            if abs(arr[index]) < 0.00001:
                element = random.random() * 20 - 10
                nonzero -= 1
                arr[index] = element

        s = np.array2string(arr, separator=';', suppress_small=True)
        # pyperclip.copy(s)
        print(s[:1000] + "..." if len(s) > 1000 else s)
        out += f"\nlet {var} = {s} in\n"
        var = chr(ord(var) + 1)

    pyperclip.copy(out)
    print("copied to clipboard!")


if __name__ == "__main__":
    main()
