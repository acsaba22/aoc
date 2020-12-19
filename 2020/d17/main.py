from __future__ import annotations
from typing import List, Tuple
from collections import defaultdict

Coor = Tuple[int, ...]

def Plus(c1: Coor, c2: Coor) -> Coor:
        return tuple((x1+x2 for x1, x2 in zip(c1, c2)))

def FullCube3(dim:int) -> List[Coor]:
    if dim == 0:
        return [()]
    sub=FullCube3(dim-1)
    return [(d,) + c1 for c1 in sub for d in [0, -1, 1]]

def solve(lines: List[str], dim: int):
    field: set[Coor] = set()
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == '#':
                field.add((x, y) + (0,)*(dim-2))
    neighs = FullCube3(dim)[1:]
    for _ in range(6):
        votes : defaultdict[Coor, int] = defaultdict(lambda:0)
        next : set[Coor] = set()
        for pix in field:
            for neigh in neighs:
                votes[Plus(pix, neigh)] += 1
        for p, v in votes.items():
            if v == 3 or (v == 2 and p in field):
                next.add(p)
        field = next
    print(len(field))

def main():
    with open('d17/input.txt') as f:
        lines = f.readlines()
    solve(lines, 3)
    solve(lines, 4)

if __name__ == '__main__':
    main()
