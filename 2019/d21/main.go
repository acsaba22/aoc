package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	"time"
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

const prg string = "109,2050,21102,966,1,1,21102,13,1,0,1106,0,1378,21101,20,0,0,1106,0,1337,21101,0,27,0,1106,0,1279,1208,1,65,748,1005,748,73,1208,1,79,748,1005,748,110,1208,1,78,748,1005,748,132,1208,1,87,748,1005,748,169,1208,1,82,748,1005,748,239,21101,0,1041,1,21101,73,0,0,1105,1,1421,21101,0,78,1,21102,1041,1,2,21102,1,88,0,1105,1,1301,21101,68,0,1,21102,1041,1,2,21102,1,103,0,1106,0,1301,1102,1,1,750,1106,0,298,21101,0,82,1,21102,1,1041,2,21101,0,125,0,1105,1,1301,1102,1,2,750,1105,1,298,21101,79,0,1,21101,1041,0,2,21101,0,147,0,1106,0,1301,21102,84,1,1,21102,1041,1,2,21101,0,162,0,1105,1,1301,1101,0,3,750,1105,1,298,21101,0,65,1,21101,0,1041,2,21101,184,0,0,1105,1,1301,21101,76,0,1,21101,0,1041,2,21102,1,199,0,1106,0,1301,21101,75,0,1,21101,0,1041,2,21102,214,1,0,1106,0,1301,21101,221,0,0,1106,0,1337,21101,0,10,1,21102,1,1041,2,21102,1,236,0,1105,1,1301,1105,1,553,21101,0,85,1,21101,0,1041,2,21102,1,254,0,1105,1,1301,21102,1,78,1,21101,0,1041,2,21102,269,1,0,1106,0,1301,21102,1,276,0,1105,1,1337,21102,10,1,1,21102,1,1041,2,21102,1,291,0,1106,0,1301,1101,0,1,755,1105,1,553,21102,1,32,1,21102,1,1041,2,21101,313,0,0,1106,0,1301,21101,0,320,0,1106,0,1337,21101,327,0,0,1105,1,1279,2102,1,1,749,21102,1,65,2,21101,0,73,3,21101,346,0,0,1106,0,1889,1206,1,367,1007,749,69,748,1005,748,360,1101,0,1,756,1001,749,-64,751,1106,0,406,1008,749,74,748,1006,748,381,1101,0,-1,751,1106,0,406,1008,749,84,748,1006,748,395,1101,0,-2,751,1105,1,406,21101,0,1100,1,21101,0,406,0,1105,1,1421,21101,32,0,1,21102,1100,1,2,21101,0,421,0,1106,0,1301,21102,428,1,0,1105,1,1337,21101,435,0,0,1105,1,1279,1202,1,1,749,1008,749,74,748,1006,748,453,1102,1,-1,752,1106,0,478,1008,749,84,748,1006,748,467,1101,0,-2,752,1106,0,478,21101,0,1168,1,21101,478,0,0,1106,0,1421,21101,485,0,0,1106,0,1337,21101,10,0,1,21102,1,1168,2,21102,1,500,0,1106,0,1301,1007,920,15,748,1005,748,518,21102,1209,1,1,21101,518,0,0,1106,0,1421,1002,920,3,529,1001,529,921,529,102,1,750,0,1001,529,1,537,102,1,751,0,1001,537,1,545,1002,752,1,0,1001,920,1,920,1105,1,13,1005,755,577,1006,756,570,21102,1100,1,1,21102,570,1,0,1106,0,1421,21101,0,987,1,1105,1,581,21102,1,1001,1,21102,588,1,0,1105,1,1378,1102,1,758,593,1002,0,1,753,1006,753,654,21001,753,0,1,21102,610,1,0,1106,0,667,21102,0,1,1,21102,1,621,0,1106,0,1463,1205,1,647,21101,1015,0,1,21102,635,1,0,1105,1,1378,21101,0,1,1,21101,646,0,0,1105,1,1463,99,1001,593,1,593,1106,0,592,1006,755,664,1101,0,0,755,1105,1,647,4,754,99,109,2,1101,0,726,757,21202,-1,1,1,21101,9,0,2,21101,0,697,3,21101,692,0,0,1105,1,1913,109,-2,2105,1,0,109,2,1002,757,1,706,1201,-1,0,0,1001,757,1,757,109,-2,2106,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,255,63,191,127,223,159,95,0,196,92,163,76,201,122,109,220,53,55,58,117,204,142,57,233,78,254,229,87,248,155,113,85,140,213,69,125,110,136,252,172,238,38,250,206,231,219,158,166,189,120,247,214,205,198,54,174,93,153,215,99,139,178,39,202,243,183,169,106,34,86,190,101,156,50,253,175,115,79,35,124,244,226,239,126,232,234,177,43,212,217,59,154,246,242,181,222,141,187,152,56,47,62,157,249,230,162,218,70,107,138,173,199,186,197,221,84,51,108,123,179,94,188,184,114,227,119,245,118,241,68,71,49,46,251,102,116,237,167,200,170,121,143,60,182,228,168,216,171,98,235,236,42,137,100,207,203,185,103,77,61,111,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20,73,110,112,117,116,32,105,110,115,116,114,117,99,116,105,111,110,115,58,10,13,10,87,97,108,107,105,110,103,46,46,46,10,10,13,10,82,117,110,110,105,110,103,46,46,46,10,10,25,10,68,105,100,110,39,116,32,109,97,107,101,32,105,116,32,97,99,114,111,115,115,58,10,10,58,73,110,118,97,108,105,100,32,111,112,101,114,97,116,105,111,110,59,32,101,120,112,101,99,116,101,100,32,115,111,109,101,116,104,105,110,103,32,108,105,107,101,32,65,78,68,44,32,79,82,44,32,111,114,32,78,79,84,67,73,110,118,97,108,105,100,32,102,105,114,115,116,32,97,114,103,117,109,101,110,116,59,32,101,120,112,101,99,116,101,100,32,115,111,109,101,116,104,105,110,103,32,108,105,107,101,32,65,44,32,66,44,32,67,44,32,68,44,32,74,44,32,111,114,32,84,40,73,110,118,97,108,105,100,32,115,101,99,111,110,100,32,97,114,103,117,109,101,110,116,59,32,101,120,112,101,99,116,101,100,32,74,32,111,114,32,84,52,79,117,116,32,111,102,32,109,101,109,111,114,121,59,32,97,116,32,109,111,115,116,32,49,53,32,105,110,115,116,114,117,99,116,105,111,110,115,32,99,97,110,32,98,101,32,115,116,111,114,101,100,0,109,1,1005,1262,1270,3,1262,20102,1,1262,0,109,-1,2105,1,0,109,1,21101,0,1288,0,1105,1,1263,20101,0,1262,0,1101,0,0,1262,109,-1,2106,0,0,109,5,21102,1310,1,0,1106,0,1279,22102,1,1,-2,22208,-2,-4,-1,1205,-1,1332,21202,-3,1,1,21101,0,1332,0,1105,1,1421,109,-5,2105,1,0,109,2,21101,1346,0,0,1105,1,1263,21208,1,32,-1,1205,-1,1363,21208,1,9,-1,1205,-1,1363,1106,0,1373,21102,1370,1,0,1105,1,1279,1105,1,1339,109,-2,2105,1,0,109,5,2102,1,-4,1386,20101,0,0,-2,22101,1,-4,-4,21101,0,0,-3,22208,-3,-2,-1,1205,-1,1416,2201,-4,-3,1408,4,0,21201,-3,1,-3,1106,0,1396,109,-5,2105,1,0,109,2,104,10,22102,1,-1,1,21102,1,1436,0,1106,0,1378,104,10,99,109,-2,2105,1,0,109,3,20002,593,753,-1,22202,-1,-2,-1,201,-1,754,754,109,-3,2106,0,0,109,10,21101,0,5,-5,21102,1,1,-4,21101,0,0,-3,1206,-9,1555,21102,3,1,-6,21101,0,5,-7,22208,-7,-5,-8,1206,-8,1507,22208,-6,-4,-8,1206,-8,1507,104,64,1106,0,1529,1205,-6,1527,1201,-7,716,1515,21002,0,-11,-8,21201,-8,46,-8,204,-8,1106,0,1529,104,46,21201,-7,1,-7,21207,-7,22,-8,1205,-8,1488,104,10,21201,-6,-1,-6,21207,-6,0,-8,1206,-8,1484,104,10,21207,-4,1,-8,1206,-8,1569,21102,0,1,-9,1105,1,1689,21208,-5,21,-8,1206,-8,1583,21101,0,1,-9,1106,0,1689,1201,-5,716,1589,20101,0,0,-2,21208,-4,1,-1,22202,-2,-1,-1,1205,-2,1613,21202,-5,1,1,21101,0,1613,0,1106,0,1444,1206,-1,1634,22101,0,-5,1,21102,1,1627,0,1105,1,1694,1206,1,1634,21101,2,0,-3,22107,1,-4,-8,22201,-1,-8,-8,1206,-8,1649,21201,-5,1,-5,1206,-3,1663,21201,-3,-1,-3,21201,-4,1,-4,1106,0,1667,21201,-4,-1,-4,21208,-4,0,-1,1201,-5,716,1676,22002,0,-1,-1,1206,-1,1686,21101,0,1,-4,1105,1,1477,109,-10,2105,1,0,109,11,21101,0,0,-6,21101,0,0,-8,21101,0,0,-7,20208,-6,920,-9,1205,-9,1880,21202,-6,3,-9,1201,-9,921,1725,20102,1,0,-5,1001,1725,1,1733,20102,1,0,-4,21202,-4,1,1,21102,1,1,2,21102,1,9,3,21101,0,1754,0,1106,0,1889,1206,1,1772,2201,-10,-4,1767,1001,1767,716,1767,20102,1,0,-3,1105,1,1790,21208,-4,-1,-9,1206,-9,1786,21201,-8,0,-3,1105,1,1790,21202,-7,1,-3,1001,1733,1,1796,20102,1,0,-2,21208,-2,-1,-9,1206,-9,1812,21202,-8,1,-1,1106,0,1816,21202,-7,1,-1,21208,-5,1,-9,1205,-9,1837,21208,-5,2,-9,1205,-9,1844,21208,-3,0,-1,1105,1,1855,22202,-3,-1,-1,1105,1,1855,22201,-3,-1,-1,22107,0,-1,-1,1105,1,1855,21208,-2,-1,-9,1206,-9,1869,21201,-1,0,-8,1105,1,1873,22102,1,-1,-7,21201,-6,1,-6,1105,1,1708,21202,-8,1,-10,109,-11,2106,0,0,109,7,22207,-6,-5,-3,22207,-4,-6,-2,22201,-3,-2,-1,21208,-1,0,-6,109,-7,2106,0,0,0,109,5,1201,-2,0,1912,21207,-4,0,-1,1206,-1,1930,21102,1,0,-4,22101,0,-4,1,22102,1,-3,2,21102,1,1,3,21102,1,1949,0,1106,0,1954,109,-5,2106,0,0,109,6,21207,-4,1,-1,1206,-1,1977,22207,-5,-3,-1,1206,-1,1977,21201,-5,0,-5,1105,1,2045,21201,-5,0,1,21201,-4,-1,2,21202,-3,2,3,21102,1996,1,0,1106,0,1954,22101,0,1,-5,21102,1,1,-2,22207,-5,-3,-1,1206,-1,2015,21101,0,0,-2,22202,-3,-2,-3,22107,0,-4,-1,1206,-1,2037,21202,-2,1,1,21102,2037,1,0,105,1,1912,21202,-3,-1,-3,22201,-5,-3,-5,109,-6,2105,1,0"

const rprg1 string = `OR D J
NOT C T
AND T J
NOT A T
OR T J
WALK
`

func p1() {
	acsLog(1, "Hello")
	c := computerFromString(prg, "")
	for _, v := range rprg1 {
		c.input <- int(v)
	}
	c.run()
	var o int
	for more := true; more; {
		oprev := o
		o, more = <-c.output
		if o < 128 {
			fmt.Printf("%c", o)
		} else {
			fmt.Println("Solution", o)
			break
		}
		if o == 10 && oprev == 10 {
			time.Sleep(200 * time.Millisecond)
		}
	}

}

const rprg2 string = `NOT B J
NOT C T
OR T J
AND D J
OR T T
AND E T
AND I T
OR H T
AND T J
NOT A T
OR T J
RUN
`

// const rprg2 string = `OR D J
// NOT C T
// AND T J
// AND H T
// OR I T
// AND T J
// NOT A T
// OR T J
// RUN
// `

// `
// NOT C T
// OR T J
// AND H J
// AND D J
// NOT A T
// OR T J
// RUN
// `

func p2() {
	acsLog(1, "Hello")
	c := computerFromString(prg, "")
	for _, v := range rprg2 {
		c.input <- int(v)
	}
	c.run()
	var o int
	for more := true; more; {
		oprev := o
		o, more = <-c.output
		if o < 128 {
			fmt.Printf("%c", o)
		} else {
			fmt.Println("Solution", o)
			break
		}
		if o == 10 && oprev == 10 {
			time.Sleep(200 * time.Millisecond)
		}
	}

}

func main() {
	// p1() // 226
	p2()
}
