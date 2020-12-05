
from typing import Callable, List


def toId(line: str) -> int:
    s = line.strip().replace('F', '0').replace('B', '1').replace('L', '0').replace('R', '1')
    return int(s, 2)

def p1(lines: List[str]):
    print(max([toId(s) for s in lines]))

def p2(lines: List[str]):
    ids = [toId(s) for s in lines]
    nsum : Callable[[int], int] = lambda n: n*(n+1)//2
    want = nsum(max(ids)) - nsum(min(ids)-1)
    got = sum(ids)
    print(want - got)

def main():
    with open('d05/input.txt') as f:
        lines = f.readlines()
        p1(lines)
        p2(lines)

if __name__ == '__main__':
    main()
