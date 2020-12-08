from typing import Callable, Dict, List, Set, Tuple
from dataclasses import dataclass

@dataclass
class Comp:
    ax: int
    ip: int

    def Nop(self, _: int):
        self.ip += 1

    def Acc(self, x: int):
        self.ax += x
        self.ip += 1

    def Jmp(self, x: int):
        self.ip += x

Prog = List[Tuple[str, int]]

inst : Dict[str, Callable[[Comp, int], None]] = {
    'nop': lambda c, x: c.Nop(x),
    'acc': lambda c, x: c.Acc(x),
    'jmp': lambda c, x: c.Jmp(x)
}

def run(prog: Prog, c: Comp):
    seen : Set[int] = set()
    while c.ip < len(prog) and c.ip not in seen:
        seen.add(c.ip)
        i, x = prog[c.ip]
        inst[i](c, x)

def p1(prog: Prog):
    c = Comp(0, 0)
    run(prog, c)
    print(c.ax)

def p2(prog: Prog):
    for i, (inst, x) in enumerate(prog):
        newInst = set(['nop', 'jmp']) - set([inst])
        if len(newInst) == 1:
            prog2 : Prog = prog[:i] + [(newInst.pop(), x)] + prog[i+1:]
            c = Comp(0, 0)
            run(prog2, c)
            if c.ip == len(prog):
                print(c.ax)

def main():
    with open('d08/input.txt') as f:
        prog : Prog = []
        for line in f.readlines():
            inst, x = line.strip().split()
            prog.append((inst, int(x)))
        p1(prog)
        p2(prog)

if __name__ == '__main__':
    main()
