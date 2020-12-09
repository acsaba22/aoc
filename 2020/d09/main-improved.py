
from typing import List
from itertools import accumulate, combinations

n = 25
fname = 'd09/input.txt'

def p1(v: List[int]) -> int:
    for i in range(n, len(v)):
        if v[i] not in [x + y for x, y in combinations(v[i-n:i], 2)]:
            return v[i]
    return -1

def p2(v: List[int]):
    target = p1(v)
    for i in range(len(v)):
        for ij, s in enumerate(accumulate(v[i:])):
            if s == target:
                res = sorted(v[i:i+ij+1])
                print(res[0]+res[-1])
                return

def main():
    with open(fname) as f:
        v = [int(line) for line in f.readlines()]
    print(p1(v))
    p2(v)

if __name__ == '__main__':
    main()
