
from typing import List


def p1(lines: List[str]):
    yn = len(lines)
    xn = len(lines[0].strip())
    res = 0
    for y in range(yn):
        # print(lines[y][:(3*y)%xn], lines[y][(3*y)%xn], lines[y][(3*y)%xn+1:])
        if lines[y][(3*y)%xn] == '#':
            res += 1
    print(res)

def p20(lines: List[str], dx: int, dy: int) -> int:
    yn = len(lines)
    xn = len(lines[0].strip())
    res = 0
    for y in range(0, yn, dy):
        if lines[y][(dx * (y // dy)) % xn] == '#':
            res += 1
    return res

def p2(lines: List[str]):
    res : int = 1
    res *= p20(lines, 1, 1)
    res *= p20(lines, 3, 1)
    res *= p20(lines, 5, 1)
    res *= p20(lines, 7, 1)
    res *= p20(lines, 1, 2)
    print(res)

def main():
    with open('d03/input.txt') as f:
        lines = f.readlines()
        p1(lines)
        p2(lines)

if __name__ == '__main__':
    main()
