package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
)

// MAXMEM is
const MAXMEM = 1000000
const BUFFSIZE = 100000

func acsLog(level int, p ...interface{}) {
	if level <= 1 {
		fmt.Println(p...)
	}
}

func assert(b bool, s string) {
	if !b {
		log.Fatal(s)
	}
}

func assertErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func runForTest(code string, input string) []int {
	comp := computerFromString(code, input)
	comp.run()
	return comp.mem
}

func test() {
	e := map[string]string{
		"1,0,0,0,99":                    "2,0,0,0,99",
		"2,3,0,3,99":                    "2,3,0,6,99",
		"2,4,4,5,99,0":                  "2,4,4,5,99,9801",
		"1,1,1,4,99,5,6,0,99":           "30,1,1,4,2,5,6,0,99",
		"1,9,10,3,2,3,11,0,99,30,40,50": "3500,9,10,70,2,3,11,0,99,30,40,50",
		"1002,4,3,4,33":                 "1002,4,3,4,99",
	}
	for i, oe := range e {
		if o := memToString(runForTest(i, "")); oe != o {
			log.Fatalln(fmt.Sprintf("actual: %v expected: %v input: %v", o, oe, i))
		}
	}

	ioe := map[string]string{
		"3,0,4,0,99|42": "42",
		"109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99|": "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
		"1102,34915192,34915192,7,4,7,99,0|":                         "1219070632396864",
		"104,1125899906842624,99|":                                   "1125899906842624",
	}
	for i, oe := range ioe {
		iv := strings.Split(i, "|")
		comp := computerFromString(iv[0], iv[1])
		comp.run()
		if o := memToString(comp.outputToMem()); oe != o {
			log.Fatalln(fmt.Sprintf("actual: %v expected: %v input: %v", o, oe, i))
		}
	}
	acsLog(1, "Tests ok")
}

func toMem(input string) []int {
	if input == "" {
		return []int{}
	}
	strs := strings.Split(input, ",")
	code := make([]int, len(strs))
	for i, str := range strs {
		var err error
		code[i], err = strconv.Atoi(str)
		if err != nil {
			log.Fatal(err)
		}
	}
	return code
}

func memToString(mem []int) string {
	return strings.Trim(strings.Replace(fmt.Sprint(mem), " ", ",", -1), "[]")
}

type computer struct {
	mem     []int
	ip      int
	input   chan int
	output  chan int
	modes   int
	id      int
	rBase   int // relative base
	reading bool
	killed  bool
}

func computerFromString(code string, input string) computer {
	return newComputer(toMem(code), toMem(input))
}

func newComputer(mem []int, input []int) computer {
	comp := computer{mem: mem, input: make(chan int, BUFFSIZE), output: make(chan int, BUFFSIZE)}
	comp.typeInput(input)
	return comp
}

func (comp *computer) clone() computer {
	if !comp.reading {
		log.Fatal("Can clone only reading computer")
	}
	ret := *comp
	ret.mem = make([]int, len(comp.mem))
	copy(ret.mem, comp.mem)
	ret.input = make(chan int, BUFFSIZE)
	ret.output = make(chan int, BUFFSIZE)
	ret.ip--
	return ret
}

func (comp *computer) outputToMem() []int {
	om := []int{}
	for 0 < len(comp.output) {
		om = append(om, <-comp.output)
	}
	return om
}

func (comp *computer) typeInput(input []int) {
	for _, v := range input {
		comp.input <- v
	}
}

func (comp *computer) get(p int) int {
	if p < len(comp.mem) {
		return comp.mem[p]
	}
	return 0
}

func (comp *computer) put(p, v int) {
	if MAXMEM < p {
		log.Fatal("Too much memory used", p)
	}
	if len(comp.mem) <= p {
		comp.mem = append(comp.mem, make([]int, p-len(comp.mem)+100)...)
	}
	comp.mem[p] = v
}

func (comp *computer) loadParam() int {
	mode := comp.modes % 10
	comp.modes /= 10
	ret := 0
	switch mode {
	case 0:
		ret = comp.get(comp.get(comp.ip))
	case 1:
		ret = comp.get(comp.ip)
	case 2:
		ret = comp.get(comp.get(comp.ip) + comp.rBase)
	default:
		log.Fatalln("Unkown read mode", mode)
	}
	comp.ip++
	return ret
}

func (comp *computer) writeParam(v int) {
	mode := comp.modes % 10
	comp.modes /= 10
	switch mode {
	case 0:
		comp.put(comp.get(comp.ip), v)
		// case 1: // direct parameter write not allowed
		// 	ret = comp.mem[comp.ip]
	case 2:
		comp.put(comp.get(comp.ip)+comp.rBase, v)
	default:
		log.Fatalln("Unkown write mode", mode)
	}
	comp.ip++
}

type instruction = struct {
	f func(*computer)
}

var instructions = map[int]instruction{
	1: {
		// add
		func(comp *computer) {
			a := comp.loadParam()
			b := comp.loadParam()
			comp.writeParam(a + b)
		},
	},
	2: {
		// mul
		func(comp *computer) {
			a := comp.loadParam()
			b := comp.loadParam()
			comp.writeParam(a * b)
		},
	},
	3: {
		// read input
		func(comp *computer) {
			comp.reading = true
			a := <-comp.input
			if comp.killed {
				return
			}
			comp.reading = false
			acsLog(2, "comp", comp.id, "reading", a)
			comp.writeParam(a)
		},
	},
	4: {
		// write output
		func(comp *computer) {
			a := comp.loadParam()
			acsLog(2, "comp", comp.id, "writing", a)
			comp.output <- a
		},
	},
	5: {
		// jump if true
		func(comp *computer) {
			c := comp.loadParam()
			p := comp.loadParam()
			if c != 0 {
				comp.ip = p
			}
		},
	},
	6: {
		// jump if false
		func(comp *computer) {
			c := comp.loadParam()
			p := comp.loadParam()
			if c == 0 {
				comp.ip = p
			}
		},
	},
	7: {
		// less then
		func(comp *computer) {
			a := comp.loadParam()
			b := comp.loadParam()
			r := 0
			if a < b {
				r = 1
			}
			comp.writeParam(r)
		},
	},
	8: {
		// equals
		func(comp *computer) {
			a := comp.loadParam()
			b := comp.loadParam()
			r := 0
			if a == b {
				r = 1
			}
			comp.writeParam(r)
		},
	},
	9: {
		// adjust relative base
		func(comp *computer) {
			comp.rBase += comp.loadParam()
		},
	},
}

// TODO add usage
func (comp *computer) kill() {
	comp.killed = true
	comp.input <- 1
}

func (comp *computer) step() bool {
	op := comp.mem[comp.ip]
	comp.ip++
	opCode := op % 100
	comp.modes = op / 100
	acsLog(3, "comp", comp.id, "op", opCode)
	if opCode == 99 {
		return false
	}
	i, err := instructions[opCode]
	if !err {
		log.Fatal("No such instruction:", opCode)
	}
	i.f(comp)
	return !comp.killed
}

func (comp *computer) run() {
	n := 0
	for {
		n++
		if !comp.step() {
			acsLog(5, "Steps for ", n)
			return
		}
	}
}

func (comp *computer) runSignal(done chan struct{}) {
	comp.run()
	done <- struct{}{}
}

type point struct {
	p0, p1 int
}

func (p *point) add(p2 point) {
	p.p0 += p2.p0
	p.p1 += p2.p1
}

type image struct {
	m          map[point]rune
	minp, maxp point
}

func (i *image) clone() image {
	ret := *i
	ret.m = map[point]rune{}
	for k, v := range i.m {
		ret.m[k] = v
	}
	return ret
}

func newImage() image {
	return image{m: make(map[point]rune)}
}

func (i *image) paint(p point, r rune) {
	i.m[p] = r
	if p.p0 < i.minp.p0 {
		i.minp.p0 = p.p0
	}
	if p.p1 < i.minp.p1 {
		i.minp.p1 = p.p1
	}
	if i.maxp.p0 < p.p0 {
		i.maxp.p0 = p.p0
	}
	if i.maxp.p1 < p.p1 {
		i.maxp.p1 = p.p1
	}
}

func (i *image) get(p point, def rune) rune {
	ret, exists := i.m[p]
	if !exists {
		return def
	}
	return ret
}

func (i image) str(defaultR rune) string {
	// h := i.maxp.p0 - i.minp.p0
	// w := i.maxp.p0 - i.minp.p0
	ret := make([]rune, 0)
	acsLog(0, i.minp, i.maxp)
	for q := i.minp.p0; q <= i.maxp.p0; q++ {
		for w := i.minp.p1; w <= i.maxp.p1; w++ {
			c, ok := i.m[point{q, w}]
			if !ok {
				c = defaultR
				// acsLog(1, q, w, "-", string(c), "-")
				// } else {
				// 	acsLog(1, q, w, "+", string(c), "+")
			}
			ret = append(ret, c)
		}
		ret = append(ret, '\n')
	}
	return string(ret[:len(ret)-1])
}

const prg string = "3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1002,1034,1,1039,101,0,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1106,0,124,1001,1034,-1,1039,1008,1036,0,1041,101,0,1035,1040,1001,1038,0,1043,1001,1037,0,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,1002,1035,1,1040,102,1,1038,1043,101,0,1037,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,37,1032,1006,1032,165,1008,1040,37,1032,1006,1032,165,1102,1,2,1044,1106,0,224,2,1041,1043,1032,1006,1032,179,1101,1,0,1044,1105,1,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,73,1044,1105,1,224,1102,1,0,1044,1105,1,224,1006,1044,247,1002,1039,1,1034,1001,1040,0,1035,101,0,1041,1036,101,0,1043,1038,101,0,1042,1037,4,1044,1105,1,0,58,87,52,69,28,16,88,43,75,16,91,2,94,51,62,80,96,46,64,98,72,8,54,71,47,84,88,44,81,7,90,13,80,42,62,68,85,27,34,2,13,89,87,79,63,76,9,82,58,60,93,63,78,79,43,32,84,25,34,80,87,15,89,96,1,50,75,25,67,82,27,3,89,48,99,33,36,77,86,62,99,19,86,92,6,56,24,96,2,79,9,3,84,41,94,79,76,91,66,50,82,88,85,13,88,18,93,79,12,98,46,75,52,99,95,11,16,25,17,77,55,87,17,74,76,81,41,77,80,92,46,20,99,22,16,41,90,64,89,53,3,61,88,97,14,2,33,79,62,79,90,80,77,71,45,40,51,62,67,82,42,27,97,17,72,77,12,38,97,85,85,35,92,82,3,84,96,40,27,93,96,18,45,98,16,49,82,52,90,43,81,10,88,94,15,42,77,67,84,88,51,35,84,20,99,7,9,79,65,86,39,93,52,98,11,19,83,75,92,27,72,77,77,78,99,18,53,35,75,14,23,90,15,83,15,98,74,14,75,67,98,93,64,97,97,58,77,88,28,19,1,82,96,69,92,34,1,90,45,79,27,25,85,59,89,88,13,91,93,38,95,55,24,61,79,56,63,61,80,10,76,84,24,80,41,83,37,86,81,93,53,33,75,78,6,81,66,84,98,3,37,84,48,89,88,70,93,96,17,94,38,82,39,74,65,90,9,77,55,53,78,10,98,27,96,11,18,86,54,98,53,86,66,19,93,52,99,44,85,79,19,7,53,86,13,90,46,33,86,19,52,79,60,92,94,97,4,99,83,67,84,58,10,96,5,91,75,47,74,93,68,76,74,50,45,99,15,85,13,99,96,30,99,84,59,81,51,64,74,9,27,2,99,34,49,76,61,28,87,56,84,81,32,6,88,48,57,89,43,76,77,15,80,91,45,9,6,52,93,84,77,17,82,32,67,97,92,74,54,46,99,80,5,83,74,85,64,89,36,41,77,47,94,24,86,45,23,99,59,90,43,61,95,98,91,90,33,91,15,19,88,49,54,86,75,42,67,43,54,97,10,10,42,85,10,11,60,76,17,90,43,80,80,34,90,85,71,70,40,80,97,31,55,80,3,58,99,31,31,99,31,90,90,57,29,85,76,22,14,77,76,87,21,88,77,85,33,81,77,94,57,56,18,83,54,90,90,2,89,87,36,13,85,36,85,70,96,20,85,82,43,34,97,93,27,40,44,80,97,2,81,16,44,12,91,35,90,24,49,75,71,96,5,29,65,80,87,35,51,92,43,94,30,84,88,10,99,4,71,76,65,77,71,1,89,90,58,28,77,42,57,81,87,13,16,72,74,32,98,83,8,75,79,10,96,11,92,34,84,13,1,77,78,71,21,63,78,37,98,86,53,84,75,1,60,75,66,86,22,78,32,31,78,97,97,89,23,88,78,4,75,59,99,65,13,85,70,74,77,83,39,62,76,81,33,98,87,25,41,90,48,42,33,24,94,86,15,94,89,21,23,81,29,36,99,93,60,20,90,19,66,52,90,80,97,95,21,86,45,80,78,7,37,80,84,22,6,97,79,34,87,27,43,52,97,84,72,9,89,93,2,75,82,60,92,12,87,89,59,74,64,90,38,71,89,12,26,81,6,53,78,96,8,81,91,69,68,89,76,79,50,77,19,83,14,75,26,76,34,78,1,83,70,80,39,99,62,95,89,99,6,79,93,80,10,83,50,79,80,92,41,78,20,86,9,84,53,87,13,74,0,0,21,21,1,10,1,0,0,0,0,0,0"

func p1() {
	c := computerFromString(prg, "")
	done := make(chan struct{}, 10)
	go c.runSignal(done)
	dir := []point{{-1, 0}, {1, 0}, {0, 1}, {0, -1}}
	img := newImage()
	loc := point{0, 0}
	img.paint(loc, 'X')

	var dfs func(loc point, depth int)
	ox := point{1000, 1000}
	maxdepth := 10000
	dfs = func(loc point, depth int) {
		if maxdepth < depth {
			return
		}
		acsLog(1, "dfs:", depth, "loc", loc)
		acsLog(2, img.str(' '))
		dorder := []int{0, 1, 2, 3}
		for _, s := range dorder {
			d := dir[s]
			p2 := loc
			p2.add(d)
			v2 := img.get(p2, '?')
			if v2 == '?' {
				c.input <- s + 1
				resp := <-c.output
				if resp == 0 {
					img.paint(p2, '#')
				} else {
					if resp == 2 {
						img.paint(p2, 'O')
						ox = p2
					} else {
						assert(resp == 1, "Something else: "+strconv.Itoa(resp))
						img.paint(p2, '.')
					}
					dfs(p2, depth+1)
					c.input <- (s ^ 1) + 1
					ok := <-c.output
					assert(ok == 1, "Not ok")
				}
			}
		}
	}
	dfs(point{0, 0}, 0)
	acsLog(1, "ox", ox)
	acsLog(1, img.str(' '))

	type dist struct {
		p point
		n int
	}
	visited := map[point]bool{}
	ds := []dist{{point{0, 0}, 0}}
	for i := 0; i < len(ds); i++ {
		loc := ds[i].p
		for s := 0; s < 4; s++ {
			p2 := loc
			c := img.get(p2, '?')
			assert(c != '?', "its ?")
			if c == '#' {
				continue
			}
			p2.add(dir[s])
			if !visited[p2] {
				visited[p2] = true
				next := dist{p2, ds[i].n + 1}
				if p2 == ox {
					acsLog(0, "Oxygen in:", next.n)
					// 32 nem jo
					return
				}
				ds = append(ds, next)
			}
		}
	}

	// step := []int{0, 1, 2, 3, 0, 1, 3, 1, 3, 0, 3, 3}
	// for _, s := range step {
	// 	d := dir[s]
	// 	c.input <- s + 1
	// 	v := <-c.output
	// 	p2 := loc
	// 	p2.add(d)
	// 	switch v {
	// 	case 0:
	// 		img.paint(p2, '#')
	// 	case 1:
	// 		img.paint(loc, '.')
	// 		img.paint(p2, 'D')
	// 		loc = p2
	// 	case 2:
	// 		log.Fatal("Found")
	// 	}
	// 	acsLog(1, img.str(' '))
	// }

	// #D##
	// #...#
	//  ###

}

func p2() {
	c := computerFromString(prg, "")
	done := make(chan struct{}, 10)
	go c.runSignal(done)
	dir := []point{{-1, 0}, {1, 0}, {0, 1}, {0, -1}}
	img := newImage()
	loc := point{0, 0}
	img.paint(loc, 'X')

	var dfs func(loc point, depth int)
	ox := point{1000, 1000}
	maxdepth := 10000
	dfs = func(loc point, depth int) {
		if maxdepth < depth {
			return
		}
		acsLog(1, "dfs:", depth, "loc", loc)
		acsLog(2, img.str(' '))
		dorder := []int{0, 1, 2, 3}
		for _, s := range dorder {
			d := dir[s]
			p2 := loc
			p2.add(d)
			v2 := img.get(p2, '?')
			if v2 == '?' {
				c.input <- s + 1
				resp := <-c.output
				if resp == 0 {
					img.paint(p2, '#')
				} else {
					if resp == 2 {
						img.paint(p2, 'O')
						ox = p2
					} else {
						assert(resp == 1, "Something else: "+strconv.Itoa(resp))
						img.paint(p2, '.')
					}
					dfs(p2, depth+1)
					c.input <- (s ^ 1) + 1
					ok := <-c.output
					assert(ok == 1, "Not ok")
				}
			}
		}
	}
	dfs(point{0, 0}, 0)
	acsLog(1, img.str(' '))
	acsLog(1, "ox", ox)

	type dist struct {
		p point
		n int
	}
	visited := map[point]bool{}
	visited[ox] = true
	ds := []dist{{ox, 0}}
	for i := 0; i < len(ds); i++ {
		loc := ds[i].p
		acsLog(1, fmt.Sprintf("loc:%v %c", loc, rune(img.get(loc, '?'))))
		for s := 0; s < 4; s++ {
			p2 := loc
			p2.add(dir[s])
			c := img.get(p2, '?')
			// acsLog(1, fmt.Sprintf("loc:%p2 %c", loc, rune(c)))
			assert(c != '?', "its ?")
			if c == '#' {

				continue
			}
			if !visited[p2] {
				visited[p2] = true
				next := dist{p2, ds[i].n + 1}
				// img.paint(p2, 'H') // rune('0'+(next.n%10)))
				ds = append(ds, next)
			}
		}
	}
	acsLog(1, img.str(' '))
	acsLog(0, "ds:", ds)
	acsLog(0, "Fill time:", ds[len(ds)-1].n)
	// 447 not

	// step := []int{0, 1, 2, 3, 0, 1, 3, 1, 3, 0, 3, 3}
	// for _, s := range step {
	// 	d := dir[s]
	// 	c.input <- s + 1
	// 	v := <-c.output
	// 	p2 := loc
	// 	p2.add(d)
	// 	switch v {
	// 	case 0:
	// 		img.paint(p2, '#')
	// 	case 1:
	// 		img.paint(loc, '.')
	// 		img.paint(p2, 'D')
	// 		loc = p2
	// 	case 2:
	// 		log.Fatal("Found")
	// 	}
	// 	acsLog(1, img.str(' '))
	// }

	// #D##
	// #...#
	//  ###

}

func main() {
	p2()
}
