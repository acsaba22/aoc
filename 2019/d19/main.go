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

const LOGLEVEL = 1

func acsLog(level int, p ...interface{}) {
	if level <= LOGLEVEL {
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
			close(comp.output)
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

const prg string = "109,424,203,1,21101,11,0,0,1105,1,282,21102,18,1,0,1106,0,259,1201,1,0,221,203,1,21102,1,31,0,1105,1,282,21101,38,0,0,1106,0,259,20102,1,23,2,21201,1,0,3,21101,1,0,1,21102,57,1,0,1105,1,303,1201,1,0,222,21001,221,0,3,20101,0,221,2,21102,1,259,1,21101,0,80,0,1105,1,225,21101,76,0,2,21102,1,91,0,1106,0,303,2102,1,1,223,21002,222,1,4,21102,1,259,3,21101,0,225,2,21102,225,1,1,21102,1,118,0,1105,1,225,21001,222,0,3,21102,1,54,2,21102,1,133,0,1106,0,303,21202,1,-1,1,22001,223,1,1,21101,148,0,0,1106,0,259,1202,1,1,223,21001,221,0,4,20101,0,222,3,21101,14,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,0,195,0,106,0,108,20207,1,223,2,20101,0,23,1,21101,0,-1,3,21102,1,214,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1202,-4,1,249,22102,1,-3,1,21201,-2,0,2,21202,-1,1,3,21101,0,250,0,1106,0,225,22101,0,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2105,1,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,21201,-2,0,-2,109,-3,2105,1,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,22101,0,-2,3,21102,1,343,0,1106,0,303,1106,0,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22102,1,-4,1,21101,0,384,0,1105,1,303,1106,0,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21202,1,1,-4,109,-5,2106,0,0"

// 143 too low

func p1() {
	acsLog(2, "Hello")
	n := 50
	sum := 0
	field := newImage()
	for x := 0; x < n; x++ {
		for y := 0; y < n; y++ {
			c := computerFromString(prg, "")
			c.input <- x
			c.input <- y
			c.run()
			o, more := <-c.output
			assert(more, "empty output")
			acsLog(3, "o", o)
			r := '.'
			if o == 1 {
				r = '#'
				sum++
			}
			field.paint(point{y, x}, r)
			_, more = <-c.output
			assert(!more, "more than 1")

		}
	}
	acsLog(1, "Field:\n"+field.str('?'))
	acsLog(0, "Solution:", sum)
}

func testP(p point) bool {
	c := computerFromString(prg, "")
	c.input <- p.p0
	c.input <- p.p1
	c.run()
	o, more := <-c.output
	assert(more, "empty output")
	return o == 1
}

func p2() {
	acsLog(2, "Hello")
	L := 100
	L1 := L - 1
	p := point{0, 0}
	fits := false
	for !fits {
		acsLog(1, "p:", p)
		fmt.Printf("Down")
		for !testP(point{p.p0 + L1, p.p1}) {
			p.p1++
			fmt.Printf(".")
		}
		fmt.Println()
		fmt.Printf("Right")
		for !testP(point{p.p0, p.p1 + L1}) {
			p.p0++
			fmt.Printf(".")
		}
		fmt.Println()
		fits = testP(point{p.p0 + L1, p.p1})
	}
	acsLog(1, "p", p)
	acsLog(1, "solution", p.p0*10000+p.p1)
}

func main() {
	// p1() // 226
	p2()
}
