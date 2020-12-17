from typing import List, Set, Tuple
from collections import defaultdict
import re
from functools import reduce
from operator import and_, mul


Pair = Tuple[int, int]
Rules = List[Tuple[Pair, Pair]]
Ticket = List[int]

def main():
    rules: Rules = []
    ruleOk: defaultdict[int, Set[int]] = defaultdict(set)
    tickets : List[Ticket] = []
    p1 = 0
    with open('d16/input.txt') as f:
        for line in f:
            m = re.match(r"(.+): (\d+)-(\d+) or (\d+)-(\d+)", line)
            if m is None:
                break
            a = [int(m.group(i)) for i in range(2, 6)]
            for x in list(range(a[0], a[1]+1)) + list(range(a[2], a[3]+1)):
                ruleOk[x].add(len(rules))
            rules.append((((a[0], a[1]), (a[2], a[3]))))
        for l in f:
            if re.match(r"\d", l):
                ticket = [int(x) for x in l.strip().split(',')]
                bads = [x for x in ticket if x not in ruleOk]
                p1 += sum(bads)
                if not bads:
                    tickets.append(ticket)
    print(p1)

    graphList = [reduce(and_, [ruleOk[t[i]] for t in tickets]) for i in range(len(rules))]
    graph = dict(enumerate(graphList))
    mapping = {}
    while graph:
        exacts = [field for field, rules in graph.items() if len(rules)==1]
        assert(exacts)
        field = exacts[0]
        rule = graph[field].pop()
        del graph[field]
        mapping[rule] = field
        for rs in graph.values():
            rs.remove(rule)
    print(reduce(mul, [tickets[0][mapping[i]] for i in range(min(6, len(rules)))]))

if __name__ == '__main__':
    main()
