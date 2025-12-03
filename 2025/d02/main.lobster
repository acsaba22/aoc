import std

let fname = "input.txt"

let fcontent = check(read_file(fname), "No file")

print(fcontent)

let rangess = fcontent.tokenize(",", "\n ")
print(rangess)

def isRepeat(x, repeatNum = 2):
    let xs = string(x)
    let len = xs.length()
    if len % repeatNum != 0:
        return false
    let l1 = len / repeatNum
    let pattern = xs.substring(0, l1)
    var i = 1
    while i < repeatNum:
        let ss = xs.substring(i*l1, l1)
        if pattern != ss:
            return false
        i++
    return true

def isRepeatAny(x):
    let len = string(x).length()
    var i = 2
    while i <= len:
        if isRepeat(x, i):
            return true
        i++
    return false

var p1 = 0
var p2 = 0

for(rangess):
    let endss = _.tokenize("-","")
    let as, bs = endss[0], endss[1]
    let a, b = string_to_int(as), string_to_int(bs)
    print(a, b)
    
    // TODO for loop range a..b?
    var x = a
    while x <= b:
        if isRepeat(x, 2):
            p1 += x
            print("FOUND: " + string(x))
        
        if isRepeatAny(x):
            p2 += x
            print("FOUND2: " + string(x))

        x++

    // // TODO call just map(endss, string_to_int)
    // let ends = map(endss) x: string_to_int(x)
    // print(ends)

print("!!!!!!!!!!!!!!! P1: {p1}") // 30608905813
print("!!!!!!!!!!!!!!! P2: {p2}") // 31898925685
