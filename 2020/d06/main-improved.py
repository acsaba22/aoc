from typing import List
from operator import and_
from functools import reduce

Groups = List[str]

def p1(groups: Groups):
    print(sum(len(set(group)-set('\n')) for group in groups))

def p2(groups: Groups):
    print(sum(len(
        reduce(and_, map(set, group.split('\n'))))
        for group in groups))

def main():
    with open('d06/input.txt') as f:
        groups = f.read().strip().split('\n\n')
        p1(groups)
        p2(groups)

if __name__ == '__main__':
    main()
