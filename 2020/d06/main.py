from typing import List


Group = List[str]
Groups = List[Group]

def p1(groups: Groups):
    res = 0
    for group in groups:
        res += len(set("".join(group)))
    print(res)

def p2(groups: Groups):
    res = 0
    for group in groups:
        common = set(group[0])
        for answer in group[1:]:
            common &= set(answer)
        res += len(common)
    print(res)


def main():
    with open('d06/input.txt') as f:
        groups : Groups = []
        group : Group = []
        line : str
        for line in f.readlines() + ['\n']:
            if line == '\n':
                groups.append(group)
                group = []
            else:
                group.append(line.strip())
        p1(groups)
        p2(groups)

if __name__ == '__main__':
    main()
