import std

// run with $ lobster d01/main.lobster

// DONE int mekkora? overflow vagy grow? float?
// int64, dobule



// DONE current dir? ["elso.lobster", "d01"]
// - ez van
print(scan_folder("."))
print(exists_file("d01/example.txt")) // no
print(exists_file("example.txt")) // yes
print(exists_file("./example.txt")) // yes

// TODO Hogy csinalok ?string-bol stringet (throw error)
// let lines = read_file("example.txt")
// for(lines) a:
// main.lobster(10): error: `for` can only iterate over int / string / vector, not `string?`

// TODO f2 rename doesn't work
var pos = 50
var p1 = 0
var p2 = 0

print("------ TEST: ")
print((-1)%100) // remains negative

if let lines = read_file("input.txt"):
  for(lines.tokenize("\n", " ")) line:
    // DONE format string print?
    // print(a[0] char, a[1..])
    // - didn't find
    print(line)

    // TODO assert a[0] == R or L?
    check(line[0] == 'R' or line[0] == 'L', "error: L or R")

    // value if
    let dir = if line[0] == 'R': 1 else: -1
    print(dir)
    let x = line.substring(1, -1).string_to_int()
    print(x)

    let prev = pos
    pos += dir*x

    if prev != 0 and ((pos <= 0) or (100 <= pos)):
      p2++
    print(pos)
 
    pos += 100
    pos %= 100
    
    if pos == 0:
      p1++

  print("!!!!!!!!!!!!!!! P1: " + string(p1))
  print("!!!!!!!!!!!!!!! P2: " + string(p2))
  // 2448 too low

// DONE space 4?
else:
  print("error!")



