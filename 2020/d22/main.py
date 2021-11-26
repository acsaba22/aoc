import collections, copy
from typing import Deque, Tuple
import sys
sys.setrecursionlimit(10000)

Deck = Deque[int]

cache = {}
calcing = {}

def state(l1: Deck, l2: Deck) -> Tuple[int, ...]:
    return (len(l2),)+tuple(l2)+tuple(l1)

cache2 = {'all': 0, 'hit': 0}

def p2(r: int, l1: Deck, l2:Deck) -> Tuple[bool, Deck]:
    if r < 4:
        print(r)
    s0 = state(l1, l2)
    cache2['all'] += 1
    if cache2['all'] %100000 == 0:
        print('count:', cache2['all'], 'ratio:', cache2['hit'] / cache2['all'])
    if s0 in cache:
        cache2['hit'] += 1
        # print('hit', r)
        return cache[s0]
    seen = set()
    while l1 and l2:
        s = state(l1, l2)
        if s in seen:
            return True, Deck()
        seen.add(s)
        a, b = l1.popleft(), l2.popleft()
        win1 = False
        if a <= len(l1) and b <= len(l2):
            win1, _ = p2(r+1, copy.copy(l1), copy.copy(l2))
        else:
            win1 = b < a
        if win1:
            l1.extend([a, b])
        else:
            l2.extend([b, a])
        # print(l1, l2)
    a, b = False, l1
    if l1:
        a, b = True, l1
    else:
        a, b = False, l2
    cache[s0] = (a, b)
    return a, b

def p22(r: int, l1: Deck, l2:Deck) -> Tuple[bool, Deck]:
    if r > 6000:
        print('HHHHHHHHHHHHH', r)
    cache2['all'] += 1
    if cache2['all'] %1000000 == 0:
        print('count:', cache2['all'], 'size:', len(cache),  'ratio:', cache2['hit'] / cache2['all'])
    s0 = state(l1, l2)
    # if s0 in cache:
    #     cache2['hit'] += 1
    #     # print('hit', r)
    #     return cache[s0]
    if s0 in calcing:
        return True, Deck()
    calcing[s0] = True
    if l1 and l2:
        a, b = l1.popleft(), l2.popleft()
        win1 = False
        if a <= len(l1) and b <= len(l2):
            win1, _ = p22(r+1, copy.copy(l1), copy.copy(l2))
        else:
            win1 = b < a
        if win1:
            l1.extend([a, b])
        else:
            l2.extend([b, a])
    a, b = False, l1
    if not (l1 and l2):
        if l1:
            a, b = True, l1
        else:
            a, b = False, l2
    else:
        a, b = p22(r+1, l1, l2)
    # cache[s0] = (a, b)
    del calcing[s0]
    return a, b


l1g = collections.deque(maxlen=1000)
l2g = collections.deque(maxlen=1000)

stack = []

finish = None

def p23(g: int, r2: int) -> bool:
    # print('p23', g+1, r2+1, l1g, l2g)
    if r2 > 2000:
        # assert False
        return True
    if not l1g or not l2g:
        if l1g:
            if g == 0:
                print('result1:',l1g)
                print(sum((i+1)*x for i, x in enumerate(reversed(l1g))))
            return True
        else:
            if g == 0:
                print('result2:',l2g)
                print(sum((i+1)*x for i, x in enumerate(reversed(l2g))))
            return False
    a, b = l1g.popleft(), l2g.popleft()
    win1 = False
    if a <= len(l1g) and b <= len(l2g):
        s1 = len(l1g)
        s2 = len(l2g)
        # print("PRE", l1g, l2g)
        while a < len(l1g):
            stack.append(l1g.pop())
        while b < len(l2g):
            stack.append(l2g.pop())
        win1 = p23(g+1, 0)
        while len(l2g) < s2:
            l2g.append(stack.pop())
        while len(l1g) < s1:
            l1g.append(stack.pop())
        # print("POST", l1g, l2g)
    else:
        win1 = b < a
    if win1:
        l1g.append(a)
        l1g.append(b)
    else:
        l2g.append(b)
        l2g.append(a)
    res = p23(g, r2+1)
    if win1:
        l1g.pop()
        l1g.pop()
    else:
        l2g.pop()
        l2g.pop()
    l1g.appendleft(a)
    l2g.appendleft(b)
    return res

# def p23(r: int) -> Tuple[bool, Deck]:
#     if r < 50:
#         print(r)
#     cache2['all'] += 1
#     if cache2['all'] %10000000 == 0:
#         print('count:', cache2['all'], 'size:', len(cache))
#     s0 = state(l1, l2)
#     if s0 in cache:
#         cache2['hit'] += 1
#         # print('hit', r)
#         return cache[s0]
#     if s0 in calcing:
#         return True, Deck()
#     calcing[s0] = True
#     if l1 and l2:
#         a, b = l1.popleft(), l2.popleft()
#         win1 = False
#         if a <= len(l1) and b <= len(l2):
#             win1, _ = p22(r+1, copy.copy(l1), copy.copy(l2))
#         else:
#             win1 = b < a
#         if win1:
#             l1.extend([a, b])
#         else:
#             l2.extend([b, a])
#     a, b = False, l1
#     if not (l1 and l2):
#         if l1:
#             a, b = True, l1
#         else:
#             a, b = False, l2
#     else:
#         a, b = p22(r+1, l1, l2)
#     cache[s0] = (a, b)
#     del calcing[s0]
#     return a, b

def main():
    fname = 'd22/input.txt'
    l1 = collections.deque()
    l2 = collections.deque()
    with open(fname) as f:
        f.readline()
        for line in f:
            if line == '\n':
                break
            l1.append(int(line))
        f.readline()
        for line in f:
            l2.append(int(line))
    l1Orig = copy.copy(l1)
    l2Orig = copy.copy(l2)
    while l2 and l1:
        a, b = l1.popleft(), l2.popleft()
        if a < b:
            l2.extend([b, a])
        else:
            l1.extend([a, b])
        print(l1, l2)
    winner = l1 if l1 else l2
    print(winner)
    print(sum((i+1)*x for i, x in enumerate(reversed(winner))))
    print(l1Orig, l2Orig)
    # who, winner2 = p22(0, l1Orig, l2Orig)
    # print(who)
    # print(sum((i+1)*x for i, x in enumerate(reversed(winner2))))
    global l1g
    global l2g
    l1g = l1Orig
    l2g = l2Orig
    print('hello', p23(0, 0))
    print('hello2')


if __name__ == '__main__':
    main()
