from typing import DefaultDict, Dict, Set
from collections import defaultdict
import math

d : DefaultDict[str, Set[int]] = defaultdict(set)

def add1(s: str, id: int):
    d[s].add(id)
    d[s[-1::-1]].add(id)

def main():
    with open('d20/input.txt') as f:
        for title in f:
            id = int(title.split()[1][:-1])
            first = ''
            last = ''
            left = ''
            right = ''
            for l in f:
                l = l.strip()
                if l == '':
                    break
                if first == '':
                    first=l
                last = l
                left += l[0]
                right += l[-1]
            add1(first, id*4+0)
            add1(last, id*4+1)
            add1(left, id*4+2)
            add1(right, id*4+3)
    noneigh : Dict[int, int] = defaultdict(lambda:0)
    for s in d.values():
        if 1 == len(s):
            for id in s:
                noneigh[id//4] += 1
    print(math.prod([id for id, k in noneigh.items() if k == 4]))
if __name__ == '__main__':
    main()