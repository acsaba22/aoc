from typing import Dict, Set, Tuple
from functools import lru_cache

Pattern = Tuple[int, ...]
Patterns = Set[Pattern]

class Solver:
    ruleMap : Dict[int, Patterns]
    endValues: Dict[int, str]

    def __init__(self, fname:str):
        self.ruleMap = {}
        self.endValues = {}
        with open(fname) as f:
            for line in f:
                if line == '\n':
                    break
                self.AddRule(line)
            print(sum(self.Check(ex.strip()) for ex in f))


    def AddRule(self, line: str):
        ids, s = line.split(':')
        id = int(ids)
        s = s.strip()
        if s[0]=='"':
            self.endValues[id] = s[1]
        else:
            self.ruleMap[id] = set(tuple(int(x) for x in xs.split()) for xs in s.split('|'))

    def Check(self, s: str) -> bool:
        patterns : Patterns = set([(0,)])
        for c in s:
            patterns = self.EatFromPatterns(patterns, c)
        return () in patterns

    def EatFromPatterns(self, patterns: Patterns, c: str) -> Patterns:
        ret : Patterns = set([])
        for pattern in patterns:
            if pattern == ():
                continue
            leftovers = self.EatFromRule(pattern[0], c)
            ret |= set(p0+pattern[1:] for p0 in leftovers)
        return ret

    @lru_cache(maxsize=None)
    def EatFromRule(self, ruleId: int, c: str) -> Patterns:
        if ruleId in self.endValues:
            return set([()]) if self.endValues[ruleId] == c else set()
        return self.EatFromPatterns(self.ruleMap[ruleId], c)

def main():
    Solver('d19/input.txt')
    Solver('d19/input2.txt')

if __name__ == '__main__':
    main()
