from __future__ import annotations
from typing import Callable, List
from copy import deepcopy
from itertools import chain

class Img:
    data: list[str]
    nx: int
    ny: int
    n: int
    neigh: List[int]

    @staticmethod
    def Parse(lines: List[str]) -> Img:
        ret = Img()
        ret.ny = len(lines) + 2
        ret.nx = len(lines[0].strip()) +2
        ret.n = ret.ny*ret.nx

        nx = ret.nx
        ret.neigh = [-nx-1, -nx, -nx+1, -1, +1, nx-1, nx, nx+1]

        ret.data = [' '] * ret.nx
        for line in lines:
            line = ' ' + line.strip() + ' '
            assert(len(line) == ret.nx)
            ret.data += list(line)
        ret.data += [' '] * ret.nx
        return ret

    def __repr__(self) -> str:
        return ''.join(chain(*[self.data[y*self.nx:(y+1)*self.nx] + ['|\n'] for y in range(self.ny)]))

def neighCount1(img: Img, pos: int) -> int:
    return sum(1 for d in img.neigh if img.data[pos+d] == '#')

def neighCount2(img: Img, pos: int) -> int:
    ret = 0
    for d in img.neigh:
        p2 = pos
        while True:
            p2 += d
            c = img.data[p2]
            if c == ' ' or c == 'L':
                break
            if c == '#':
                ret += 1
                break
    return ret

def solve(img: Img, neighCount: Callable[[Img, int], int], limit: int):
    same = False
    while not same:
        img2 = deepcopy(img)
        for p in range(img2.n):
            if img.data[p] not in ['L', '#']:
                continue
            nc = neighCount(img, p)
            # No string character replacement, ouch
            if nc == 0:
                img2.data[p] = '#'
            if limit <= nc:
                img2.data[p] = 'L'
        if img.data == img2.data:
            same = True
        img = img2
    print(img.data.count('#'))

def main():
    with open('d11/input.txt') as f:
        lines = f.readlines()
        img = Img.Parse(lines)
        solve(deepcopy(img), neighCount1, 4)
        solve(deepcopy(img), neighCount2, 5)

if __name__ == '__main__':
    main()
