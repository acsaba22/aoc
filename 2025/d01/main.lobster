import std

let fname = "input.txt"

// TODO no named return :(
def divMod(a, b) -> int, int:
    var div = a / b
    var mod = a - div * b
    if mod < 0:
        div--
        mod += b
    return div, mod

var pos = 50
var p1 = 0
var p2 = 0

let lines = check(read_file(fname), "No file")
for(lines.tokenize("\n", " ")) line:
    print(line)
    let dirc = line[0]
    check(dirc == 'R' or dirc == 'L', "error: L or R")
    
    let dir = if dirc == 'R': 1 else: -1

    let x = line.substring(1, -1).string_to_int()


    let rounds, newP = divMod(pos + dir*x, 100)
    // print(rounds)
    // print(newP)

    p2 += abs(rounds)
    if newP == 0:
        p1++
        p2++

    if pos == 0 and rounds < 0:
        p2--
    if newP == 0 and 0 < rounds:
        p2 --

    pos = newP

print("!!!!!!!!!!!!!!! P1: " + string(p1))
print("!!!!!!!!!!!!!!! P2: " + string(p2))
