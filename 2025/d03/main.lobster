import std

let fname = "input.txt"

let lines = read_file(fname).check("No file").tokenize("\n", " ")
print(lines)

var p1 = 0
var p2 = 0

for(lines) line:
    print(line)
    let pos1 = find_best<int>(line.substring(0, line.length()-1)): _
    let s2 = line.substring(pos1+1, -1)
    let pos2 = find_best<int>(s2): _
    let p1part = (line[pos1]-'0')*10 + s2[pos2]-'0'
    print("P1: {p1part}")
    p1 += p1part

    let nums = map(line): _-'0'
    
    let N  = nums.length()
    let K = 12
    let cache = []

    // TODO initialize better an NxK matrix with -2
    for(N+1) i:
        cache.push([])
        for K+1:
            cache[i].push(-2)

    let pow10 = [1]
    for K:
        pow10.push(pow10[_]*10)

    // TODO cache decorator?
    def solve(i, k) -> int:
        if cache[i][k] != -2:
            return cache[i][k]
        def save(val):
            // print("ret {i} {k} : {val}")
            cache[i][k] = val
            return val

        if k == 0:
            return save(0)
        if N <= i:
            return save(-1)
        
        var ret = solve(i+1,k)

        let tail = solve(i+1, k-1)
        if tail == -1:
            return save(ret)
        let retIfUsed = nums[i]*pow10[k-1] + tail
        if ret < retIfUsed:
            ret = retIfUsed
        return save(ret)

    let p2part = solve(0, K)
    p2 += p2part
    print("P2: {p2part}")

// TODO print type of a
// let a = [1, 2, 3]

print("!!!! P1 {p1}")
print("!!!! P2 {p2}")
