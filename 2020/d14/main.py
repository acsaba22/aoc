import re
from typing import Dict, List
from functools import reduce
from operator import or_

def main():
    with open('d14/input.txt') as f:
        ones = 0
        zeros = 0
        floating : List[int] = []
        mem1 : Dict[int, int] = {}
        mem2 : Dict[int, int] = {}
        for s in f.readlines():
            m1 = re.match(r"mask = (\w+)", s)
            if m1:
                ones = 0
                zeros = 0
                floating = []
                for c in m1.group(1):
                    ones <<= 1
                    zeros = zeros << 1 | 1
                    floating = [x << 1 for x in floating]
                    if c == '1':
                        ones |= 1
                    if c == '0':
                        zeros &= ~1
                    if c == 'X':
                        floating.append(1)
            else:
                m2 = re.match(r"mem\[(\d+)\] = (\d+)", s)
                assert(m2 is not None)
                pos = int(m2.group(1))
                val = int(m2.group(2))
                mem1[pos] = val & zeros | ones
                fAll = reduce(or_, [floating[x] for x in range(len(floating))], 0)
                for i in range(1 << len(floating)):
                    fMask = reduce(or_, [floating[-x] for x in range(len(floating)) if i&(1<<x)], 0)
                    pos2 = (pos&~fAll)|ones|fMask
                    mem2[pos2] = val
        print(sum(mem1.values()))
        print(sum(mem2.values()))

if __name__ == '__main__':
    main()
