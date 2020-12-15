from typing import Dict, List


def solve(N: int, input: List[int]):
    seen : Dict[int, int] = {}
    n = 0
    for i in input[:-1]:
        seen[i]=n
        # print(i, n)
        n += 1

    num = input[-1]
    while n < N-1:
        next = n - seen[num] if num in seen else 0
        seen[num] = n
        # print(num, n, seen)
        n += 1
        num = next
    print(num)

# python3 11.2s
# pypy3 4.8s
def main():
    # input = [0,3,6]
    input = [0,14,1,3,7,9]
    solve(2020, input)
    solve(30000000, input)

if __name__ == '__main__':
    main()
