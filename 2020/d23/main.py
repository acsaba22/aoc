from typing import List

# python3: 5.95s
# pypy3:   0.83s

def solve(input: List[int], n: int, stepCount: int) -> List[int]:
    m = len(input)
    input = [x-1 for x in input] + list(range(m, n))
    next = [0]*n
    for i in range(n):
        next[input[i]] = input[(i+1)%n]
    current = input[0]
    for _ in range(stepCount):
        a = next[current]
        b = next[a]
        c = next[b]
        nextCurrent = next[c]
        insert = (current-1)%n
        while insert == a or insert == b or insert == c:
            insert = (insert-1)%n
        insert2 = next[insert]
        next[current]= nextCurrent
        next[insert] = a
        next[c] = insert2
        current = nextCurrent
    c = 0
    ret = []
    while len(ret)<m-1:
        c = next[c]
        ret.append(c+1)
    return ret

def main():
    # inputS = '389125467'
    intputS = '318946572'
    input = [int(c) for c in intputS]
    print(''.join(map(str, solve(input, len(input), 100))))
    p2 = solve(input, 1000000, 10000000)
    print(p2[0]*p2[1])

if __name__ == '__main__':
    main()
