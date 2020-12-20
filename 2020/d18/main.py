from typing import Callable, Dict, List, Optional, Tuple
import re

OP = Callable[[int, int], int]
OPMap : Dict[str, OP] = {
    '+': lambda x, y: x+y,
    '*': lambda x, y: x*y,
}

numPref = re.compile(r"(\d+)(.*)$")

# parses until ')', returns val and remainder str.
def P2(line: str, precedence: bool) -> Tuple[int, str]:
    ret : int = 0
    op : Optional[OP] = None
    while line:
        v = -1
        m = numPref.match(line)
        if m:
            v = int(m.group(1))
            line = m.group(2)
        else:
            c = line[0]
            line = line[1:]
            if c == ')':
                return (ret, line)
            elif c == '(':
                v, line = P2(line, precedence)
            else:
                op = OPMap[c]
                if precedence and c == '*':
                    v2, rest = P2(line, precedence)
                    return  op(ret, v2), rest
        if 0 <= v:
            if op is None:
                ret = v
            else:
                ret = op(ret, v)
    return (0, '')

def Solve(lines: List[str], precedence: bool):
    print(
        sum(
            P2(line.strip().replace(' ', '') +')', precedence)[0]
            for line in lines)
    )

def main():
    with open('d18/input.txt') as f:
        lines = f.readlines()
    Solve(lines, False)
    Solve(lines, True)

if __name__ == '__main__':
    main()
