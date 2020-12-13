from typing import List, Tuple

Pairs = List[Tuple[int, int]]

def p1(n: int, buses: Pairs):
    waits : List[int] = [k-(n%k) for k, _ in buses]
    wait = min(waits)
    print(wait*buses[waits.index(wait)][0])


# solves b = x*a % n
def modDiv(a:int, b:int, n:int) -> int:
    for x in range(n):
        if (a*x) % n == b % n:
            return x
    assert(False)

def p2(buses: Pairs):
    k1 = 1
    b1 = 0
    for k2, b2 in buses:
        x = modDiv(k1, b2-b1%k1, k2)
        b1 = x*k1 + b1
        k1 *= k2
    print(b1)

def main():
    with open('d13/input.txt') as f:
        n = int(f.readline())
        buses : List[Tuple[int, int]] = []
        for i, v in enumerate(f.readline().strip().split(',')):
            if v != 'x':
                vv = int(v)
                buses.append((vv, (-i)%vv))
    p1(n, buses)
    p2(buses)

if __name__ == '__main__':
    main()
