from typing import List
from itertools import count
import numpy as np

# TODO(acsaba): figure out typing with numpy.
def main():
    with open('d10/input.txt') as f:
        l = [int(line) for line in f.readlines()]
    l.sort()
    diff = np.append(l, [max(l)+3]) - np.append([0], l)
    hist = dict(zip(*np.unique(diff, return_counts=True)))
    print(hist[1]*hist[3])

    diff = [3] +  list(diff)
    ways = [1]
    for i in range(len(diff)):
        length = 0
        current = 0
        for j in range(i+1):
            length += diff[i-j]
            if 3 < length:
                break
            current += ways[-1-j]
        ways.append(current)
    print(ways[-1])

if __name__ == '__main__':
    main()
