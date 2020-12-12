from typing import Callable, Dict, List, NamedTuple

class Ship(NamedTuple):
    pos: complex
    d: complex # dirrection for 1 ;  waypoint for 2

FDict = Dict[str, Callable[[Ship, int], Ship]]

op1 : FDict = {
    'N' : lambda s, n: Ship(s.pos + 1j*n, s.d),
    'S' : lambda s, n: Ship(s.pos - 1j*n, s.d),
    'E' : lambda s, n: Ship(s.pos + n, s.d),
    'W' : lambda s, n: Ship(s.pos - n, s.d),
    'L' : lambda s, n: Ship(s.pos, 1j**(n/90) * s.d),
    'R' : lambda s, n: Ship(s.pos, (-1j)**(n/90) * s.d),
    'F' : lambda s, n: Ship(s.pos + n*s.d, s.d),
}

op2 : FDict = op1.copy()
op2['N'] = lambda s, n: Ship(s.pos, s.d + 1j*n)
op2['S'] = lambda s, n: Ship(s.pos, s.d - 1j*n)
op2['E'] = lambda s, n: Ship(s.pos, s.d + n)
op2['W'] = lambda s, n: Ship(s.pos, s.d - n)

def solve(ls: List[str], ship: Ship, op: FDict):
    for line in ls:
        ship = op[line[0]](ship, int(line[1:].strip()))
        # print(line.strip(), ship)
    print(int(abs(ship.pos.real) + abs(ship.pos.imag)))

def main():
    with open('d12/input.txt') as f:
        lines = f.readlines()
    solve(lines, Ship(0j, 1+0j), op1)
    solve(lines, Ship(0j, 10+1j), op2)

if __name__ == '__main__':
    main()
