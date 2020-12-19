from typing import Callable, Dict, Optional, Tuple
import re

OP = Callable[[int, int], int]

OPMap : Dict[str, OP] = {
    '+': lambda x, y: x+y,
    '*': lambda x, y: x*y,
}

numPref = re.compile(r"(\d+)(.*)$")

# parses until ')', returns val and remainder str.
def Parse(line: str) -> Tuple[int, str]:
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
                v, line = Parse(line)
            else:
                op = OPMap[c]
        if 0 <= v:
            if op is None:
                ret = v
            else:
                ret = op(ret, v)
    return (0, '')

def main():
    with open('d18/input.txt') as f:
        lines = f.readlines()
    p1 = 0
    for line in lines:
        v, _ = Parse(line.strip().replace(' ', '') +')')
        p1 += v
    print(p1)


if __name__ == '__main__':
    main()
