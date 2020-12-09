
from typing import List

# 11 min: You got rank 692 on this star's leaderboard.
n = 25
fname = 'd09/input.txt'

def p1(v: List[int]) -> int:
    for i in range(n, len(v)):
        has = False
        for j in range(i-n, i):
            for k in range(j+1, i):
                if v[i] == v[j]+v[k]:
                    has = True
        if not has:
            return v[i]
    return -1

def p2(v: List[int]):
    target = p1(v)
    for i in range(len(v)):
        s = v[i]
        for j in range(i+1, len(v)):
            s += v[j]
            if s == target:
                res = sorted(v[i:j+1])
                print(res[0]+res[-1])
            if target < s:
                break

def main():
    with open(fname) as f:
        v = [int(line) for line in f.readlines()]
    print(p1(v))
    p2(v)

if __name__ == '__main__':
    main()
