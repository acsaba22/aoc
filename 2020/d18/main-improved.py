from typing import Callable, Deque, Dict, List, Optional, Tuple
from collections import deque

OP = Callable[[int, int], int]
OPMap : Dict[str, OP] = {
    '+': lambda x, y: x+y,
    '*': lambda x, y: x*y,
}

# parses until ')', returns val and remainder str.
def Parse(line: Deque[str], precedence: bool) -> Tuple[int, Deque[str]]:
    ret : int = 0
    op : Optional[OP] = None
    while True:
        v = -1
        c = line.popleft()
        if c.isdigit():
            v = int(c)
        elif c == ')':
            return ret, line
        elif c == '(':
            v, line = Parse(line, precedence)
        else:
            op = OPMap[c]
            if precedence and c == '*':
                v2, rest = Parse(line, precedence)
                return  op(ret, v2), rest
        if 0 <= v:
            if op is None:
                ret = v
            else:
                ret = op(ret, v)

def Solve(lines: List[str], precedence: bool):
    print(sum(
        Parse(deque(line.strip().replace(' ', '') +')'), precedence)[0]
        for line in lines))

def main():
    with open('d18/input.txt') as f:
        lines = f.readlines()
    Solve(lines, False)
    Solve(lines, True)

if __name__ == '__main__':
    main()
