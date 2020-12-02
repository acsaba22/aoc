from typing import List, NamedTuple

class PassWd(NamedTuple):
    n1: int
    n2: int
    char: str
    passwd: str

def Parse(line: str) -> PassWd:
    interval, charcol, passwd = line.split()
    n1Str, n2Str = interval.split('-')
    ret = PassWd(int(n1Str), int(n2Str), charcol[0], passwd)
    return ret

def p1(pss : List[PassWd]):
    okNum = 0
    for ps in pss:
        count = ps.passwd.count(ps.char)
        if ps.n1 <= count and count <= ps.n2:
            okNum += 1
    print(okNum)

def p2(pss : List[PassWd]):
    okNum = 0
    for ps in pss:
        if (ps.passwd[ps.n1-1] == ps.char) != (ps.passwd[ps.n2-1] == ps.char):
            okNum += 1
    print(okNum)

def main():
    with open('d02/input.txt') as f:
        pss = [Parse(line) for line in f.readlines()]
        p1(pss)
        p2(pss)

if __name__ == '__main__':
    main()
