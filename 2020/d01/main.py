from typing import List

# TODO(acsaba): find pypy launch bug
def p1(nums: List[int]):
    n = len(nums)
    for i in range(n):
        for j in range(i+1, n):
            if nums[i] + nums[j] == 2020:
                print(nums[i] * nums[j])


def p2(nums: List[int]):
    n = len(nums)
    for i in range(n):
        for j in range(i+1, n):
            for k in range(j+1, n):
                if nums[i] + nums[j] + nums[k] == 2020:
                    print(nums[i] * nums[j] * nums[k])

def main():
    with open('input.txt') as f:
        nums = [int(l) for l in f.readlines()]
        p1(nums)
        p2(nums)

if __name__ == '__main__':
    main()
