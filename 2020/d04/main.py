from typing import Callable, Dict, List, Set
import re

Doc = Dict[str, str]
Docs = List[Doc]

validFields : Set[str] = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid', 'cid'}

def p1(docs: Docs):
    res = 0
    for doc in docs:
        missing = validFields.copy()
        for k in doc:
            missing.remove(k)
        if missing == set() or missing == {'cid'}:
            res += 1
    print(res)


def inRange(v: int, minV: int, maxV: int):
    return minV <= v and v <= maxV

checker : Dict[str, Callable[[str], bool]] = {
    'byr': lambda x: inRange(int(x), 1920, 2002),
    'iyr': lambda x: inRange(int(x), 2010, 2020),
    'eyr': lambda x: inRange(int(x), 2020, 2030),
    'hgt': lambda x: (
        inRange(int(x[:-2]), 150, 193) if x[-2:] == 'cm' else
        inRange(int(x[:-2]), 59, 76) if x[-2:] == 'in' else
        False
    ),
    'hcl': lambda x: x[0] == '#' and bool(re.match('[0-9a-f]{6}$', x[1:])),
    'ecl': lambda x: x in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'},
    'pid': lambda x: bool(re.match('[0-9]{9}$', x)),
    'cid': lambda _: True
    }

def p2(docs: Docs):
    res = 0
    for doc in docs:
        missing = validFields.copy()
        ok = True
        for k, v in doc.items():
            if not checker[k](v):
                ok = False
            missing.remove(k)
        if ok and (missing == set() or missing == {'cid'}):
            res += 1
    print(res)

def main():
    with open('d04/input.txt') as f:
        docs : Docs = []
        doc : Doc = {}
        for line in f.readlines() + ['\n']:
            if line == '\n':
                docs.append(doc)
                doc = {}
            for term in line.split():
                k, v = term.split(':')
                doc[k]=v
        p1(docs)
        p2(docs)

if __name__ == '__main__':
    main()
