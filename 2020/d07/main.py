from typing import Dict, List, Tuple


Inverse = Dict[str, List[str]]
Graph = Dict[str, List[Tuple[int, str]]]

graph : Graph = {}

def p1(inv: Inverse):
    bfs : List[str] = ['shiny gold']
    i = 0
    while i < len(bfs):
        b = bfs[i]
        i += 1
        if b not in inv:
            continue
        for b2 in inv[b]:
            if b2 not in bfs:
                bfs.append(b2)
    print(len(bfs)-1)

cache : Dict[str, int] = {}

def backTrack(node: str) -> int:
    if node in cache:
        return cache[node]
    ret = 1
    for bs in graph[node]:
        ret += bs[0]*backTrack(bs[1])
    return ret

def p2():
    print(backTrack('shiny gold')-1)

def trim(s: str) -> str:
    s = s.strip(' \n.')
    return s

def main():
    with open('d07/input.txt') as f:
        inv : Inverse = {}
        for line in f.readlines():
            a, bs = line.replace('bags', ''). replace('bag', '').split('contain')
            a = trim(a)
            inv[a] = []
            graph[a] = []
            for b in bs.split(','):
                b = trim(b)
                if b == 'no other':
                    continue
                nums, n1, n2 = b.split()
                graph[a].append((int(nums), n1+' '+n2))
        for a, bs in graph.items():
            for b in bs:
                inv[b[1]].append(a)
        p1(inv)
        p2()

if __name__ == '__main__':
    main()
