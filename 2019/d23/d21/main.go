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

const prg string = "3,62,1001,62,11,10,109,2263,105,1,0,1037,2127,1368,1115,862,1331,600,1201,2022,1599,1793,2232,724,693,1300,1956,2199,1667,1857,1078,1434,1399,1754,631,1987,1630,1002,1725,831,765,1502,1236,1824,1152,660,571,893,1696,959,1469,796,2162,2086,928,1568,1535,1269,1923,2053,1888,0,0,0,0,0,0,0,0,0,0,0,0,3,64,1008,64,-1,62,1006,62,88,1006,61,170,1105,1,73,3,65,20102,1,64,1,20101,0,66,2,21102,1,105,0,1105,1,436,1201,1,-1,64,1007,64,0,62,1005,62,73,7,64,67,62,1006,62,73,1002,64,2,133,1,133,68,133,102,1,0,62,1001,133,1,140,8,0,65,63,2,63,62,62,1005,62,73,1002,64,2,161,1,161,68,161,1102,1,1,0,1001,161,1,169,1001,65,0,0,1102,1,1,61,1101,0,0,63,7,63,67,62,1006,62,203,1002,63,2,194,1,68,194,194,1006,0,73,1001,63,1,63,1105,1,178,21102,210,1,0,105,1,69,1202,1,1,70,1101,0,0,63,7,63,71,62,1006,62,250,1002,63,2,234,1,72,234,234,4,0,101,1,234,240,4,0,4,70,1001,63,1,63,1106,0,218,1105,1,73,109,4,21101,0,0,-3,21102,0,1,-2,20207,-2,67,-1,1206,-1,293,1202,-2,2,283,101,1,283,283,1,68,283,283,22001,0,-3,-3,21201,-2,1,-2,1105,1,263,22101,0,-3,-3,109,-4,2106,0,0,109,4,21101,0,1,-3,21101,0,0,-2,20207,-2,67,-1,1206,-1,342,1202,-2,2,332,101,1,332,332,1,68,332,332,22002,0,-3,-3,21201,-2,1,-2,1106,0,312,21201,-3,0,-3,109,-4,2106,0,0,109,1,101,1,68,359,20101,0,0,1,101,3,68,366,21001,0,0,2,21101,376,0,0,1106,0,436,22102,1,1,0,109,-1,2106,0,0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,109,8,21202,-6,10,-5,22207,-7,-5,-5,1205,-5,521,21102,0,1,-4,21101,0,0,-3,21102,1,51,-2,21201,-2,-1,-2,1201,-2,385,471,20101,0,0,-1,21202,-3,2,-3,22207,-7,-1,-5,1205,-5,496,21201,-3,1,-3,22102,-1,-1,-5,22201,-7,-5,-7,22207,-3,-6,-5,1205,-5,515,22102,-1,-6,-5,22201,-3,-5,-3,22201,-1,-4,-4,1205,-2,461,1106,0,547,21101,-1,0,-4,21202,-6,-1,-6,21207,-7,0,-5,1205,-5,547,22201,-7,-6,-7,21201,-4,1,-4,1105,1,529,22102,1,-4,-7,109,-8,2106,0,0,109,1,101,1,68,564,20102,1,0,0,109,-1,2105,1,0,1101,0,15271,66,1102,1,1,67,1102,1,598,68,1101,0,556,69,1101,0,0,71,1102,1,600,72,1106,0,73,1,1520,1101,16987,0,66,1101,1,0,67,1102,627,1,68,1102,1,556,69,1102,1,1,71,1101,629,0,72,1105,1,73,1,150,5,45389,1101,0,39397,66,1102,1,1,67,1102,658,1,68,1101,0,556,69,1102,0,1,71,1102,1,660,72,1105,1,73,1,1717,1101,35977,0,66,1102,2,1,67,1101,0,687,68,1101,0,302,69,1102,1,1,71,1102,691,1,72,1105,1,73,0,0,0,0,42,92154,1102,1,53197,66,1102,1,1,67,1102,1,720,68,1101,556,0,69,1101,0,1,71,1101,722,0,72,1105,1,73,1,1051,26,124071,1102,1,94379,66,1101,0,6,67,1101,751,0,68,1102,1,302,69,1101,1,0,71,1102,763,1,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,32,205586,1101,25357,0,66,1101,0,1,67,1102,792,1,68,1101,556,0,69,1101,1,0,71,1102,794,1,72,1105,1,73,1,125,41,73354,1101,0,73751,66,1102,3,1,67,1102,1,823,68,1101,0,302,69,1101,1,0,71,1101,0,829,72,1106,0,73,0,0,0,0,0,0,22,172495,1102,1,47857,66,1102,1,1,67,1102,1,858,68,1101,0,556,69,1101,0,1,71,1101,0,860,72,1105,1,73,1,24877,30,14753,1101,72269,0,66,1102,1,1,67,1101,0,889,68,1101,556,0,69,1102,1,1,71,1102,1,891,72,1106,0,73,1,-150,19,54581,1102,1,92173,66,1102,1,3,67,1102,1,920,68,1102,1,302,69,1101,0,1,71,1102,926,1,72,1106,0,73,0,0,0,0,0,0,22,103497,1102,1,27779,66,1102,1,1,67,1101,955,0,68,1102,1,556,69,1102,1,1,71,1101,0,957,72,1106,0,73,1,3083,49,292749,1102,81283,1,66,1102,1,1,67,1101,0,986,68,1102,1,556,69,1102,1,7,71,1102,1,988,72,1106,0,73,1,5,5,90778,5,181556,42,61436,21,95267,41,36677,41,110031,12,188758,1101,0,41357,66,1102,1,3,67,1102,1,1029,68,1101,0,302,69,1102,1,1,71,1101,0,1035,72,1105,1,73,0,0,0,0,0,0,22,137996,1101,0,9257,66,1101,0,1,67,1102,1064,1,68,1102,1,556,69,1101,6,0,71,1102,1066,1,72,1105,1,73,1,17740,45,129566,7,7451,7,22353,1,50153,1,100306,1,150459,1102,54581,1,66,1102,4,1,67,1101,1105,0,68,1102,302,1,69,1101,0,1,71,1102,1,1113,72,1106,0,73,0,0,0,0,0,0,0,0,7,14902,1102,5273,1,66,1102,1,1,67,1101,1142,0,68,1102,556,1,69,1102,4,1,71,1102,1,1144,72,1106,0,73,1,2,42,15359,21,285801,12,94379,12,377516,1102,1,41959,66,1101,1,0,67,1101,0,1179,68,1102,1,556,69,1102,1,10,71,1102,1181,1,72,1106,0,73,1,1,30,29506,31,99754,47,20782,34,71954,49,97583,26,82714,20,74869,36,92173,40,147502,19,218324,1102,7451,1,66,1101,3,0,67,1102,1228,1,68,1102,1,302,69,1101,1,0,71,1102,1234,1,72,1106,0,73,0,0,0,0,0,0,25,7673,1102,1,49877,66,1101,2,0,67,1101,1263,0,68,1101,0,302,69,1102,1,1,71,1102,1267,1,72,1105,1,73,0,0,0,0,47,10391,1101,41081,0,66,1101,0,1,67,1102,1296,1,68,1101,0,556,69,1101,0,1,71,1101,1298,0,72,1106,0,73,1,337,36,276519,1102,76651,1,66,1102,1,1,67,1102,1,1327,68,1101,0,556,69,1101,0,1,71,1101,0,1329,72,1105,1,73,1,2659,40,73751,1101,45389,0,66,1101,0,4,67,1101,0,1358,68,1102,1,302,69,1102,1,1,71,1102,1,1366,72,1105,1,73,0,0,0,0,0,0,0,0,42,76795,1101,99859,0,66,1102,1,1,67,1101,1395,0,68,1102,1,556,69,1101,1,0,71,1102,1,1397,72,1105,1,73,1,28,49,195166,1101,0,95267,66,1102,1,3,67,1101,0,1426,68,1102,1,302,69,1102,1,1,71,1101,0,1432,72,1105,1,73,0,0,0,0,0,0,45,64783,1102,74869,1,66,1102,1,3,67,1101,1461,0,68,1102,302,1,69,1102,1,1,71,1102,1,1467,72,1106,0,73,0,0,0,0,0,0,22,34499,1101,31771,0,66,1101,0,1,67,1101,1496,0,68,1102,556,1,69,1102,2,1,71,1102,1498,1,72,1106,0,73,1,3,42,30718,19,109162,1101,0,14753,66,1101,0,2,67,1102,1529,1,68,1101,302,0,69,1102,1,1,71,1102,1533,1,72,1105,1,73,0,0,0,0,31,49877,1101,64783,0,66,1101,0,2,67,1102,1562,1,68,1102,1,302,69,1102,1,1,71,1102,1,1566,72,1106,0,73,0,0,0,0,25,15346,1101,0,27197,66,1101,1,0,67,1101,0,1595,68,1101,556,0,69,1102,1,1,71,1101,0,1597,72,1106,0,73,1,160,12,471895,1101,0,33857,66,1101,0,1,67,1101,0,1626,68,1101,0,556,69,1102,1,1,71,1101,0,1628,72,1105,1,73,1,17,20,224607,1101,7673,0,66,1101,4,0,67,1101,1657,0,68,1101,0,253,69,1101,1,0,71,1101,1665,0,72,1105,1,73,0,0,0,0,0,0,0,0,32,102793,1101,5987,0,66,1101,1,0,67,1101,0,1694,68,1101,556,0,69,1101,0,0,71,1102,1,1696,72,1106,0,73,1,1790,1102,1,50891,66,1102,1,1,67,1102,1723,1,68,1102,556,1,69,1102,0,1,71,1101,1725,0,72,1105,1,73,1,1961,1102,13159,1,66,1101,1,0,67,1102,1752,1,68,1102,1,556,69,1101,0,0,71,1102,1754,1,72,1106,0,73,1,1741,1101,0,34499,66,1102,5,1,67,1101,0,1781,68,1101,253,0,69,1102,1,1,71,1102,1,1791,72,1105,1,73,0,0,0,0,0,0,0,0,0,0,48,82997,1101,44687,0,66,1102,1,1,67,1102,1820,1,68,1102,556,1,69,1102,1,1,71,1102,1,1822,72,1106,0,73,1,53,40,221253,1102,102793,1,66,1102,2,1,67,1102,1851,1,68,1102,1,351,69,1101,1,0,71,1102,1855,1,72,1105,1,73,0,0,0,0,255,9257,1101,88873,0,66,1101,1,0,67,1102,1884,1,68,1101,0,556,69,1102,1,1,71,1102,1,1886,72,1105,1,73,1,3469,20,149738,1101,97583,0,66,1102,1,3,67,1101,1915,0,68,1101,0,302,69,1102,1,1,71,1101,0,1921,72,1105,1,73,0,0,0,0,0,0,22,68998,1102,10391,1,66,1102,1,2,67,1102,1950,1,68,1101,0,302,69,1102,1,1,71,1102,1954,1,72,1105,1,73,0,0,0,0,34,35977,1101,30089,0,66,1102,1,1,67,1102,1983,1,68,1102,556,1,69,1101,1,0,71,1102,1,1985,72,1106,0,73,1,250,48,165994,1102,1,7253,66,1101,0,1,67,1101,2014,0,68,1102,1,556,69,1101,0,3,71,1101,2016,0,72,1106,0,73,1,10,5,136167,41,146708,12,566274,1102,72467,1,66,1101,1,0,67,1101,0,2049,68,1101,556,0,69,1101,1,0,71,1102,1,2051,72,1106,0,73,1,54,26,41357,1102,82997,1,66,1101,2,0,67,1102,1,2080,68,1102,302,1,69,1102,1,1,71,1101,2084,0,72,1106,0,73,0,0,0,0,21,190534,1101,0,15359,66,1102,1,6,67,1101,0,2113,68,1101,302,0,69,1101,1,0,71,1101,0,2125,72,1105,1,73,0,0,0,0,0,0,0,0,0,0,0,0,25,23019,1101,50153,0,66,1101,0,3,67,1102,1,2154,68,1102,302,1,69,1101,1,0,71,1102,1,2160,72,1106,0,73,0,0,0,0,0,0,25,30692,1102,1,36677,66,1101,0,4,67,1102,2189,1,68,1101,302,0,69,1101,0,1,71,1101,0,2197,72,1105,1,73,0,0,0,0,0,0,0,0,12,283137,1101,293,0,66,1102,1,1,67,1101,2226,0,68,1102,556,1,69,1102,1,2,71,1102,1,2228,72,1106,0,73,1,71,42,46077,19,163743,1102,49297,1,66,1101,1,0,67,1101,2259,0,68,1101,0,556,69,1101,0,1,71,1102,1,2261,72,1105,1,73,1,217,36,184346"

func read1(ch chan int) (int, bool) {
	select {
	case x := <-ch:
		return x, true
	default:
		return -1, false
	}

}

func p1() {
	acsLog(1, "Hello p1")
	n := 50
	cs := make([]*computer, n)
	for i := range cs {
		c := computerFromString(prg, "")
		cs[i] = &c
		cs[i].input <- i
		go c.run()
	}
	for {
		acsLog(1, "loop")
		for i, c := range cs {
			outs := []int{}
			acsLog(1, "comp", i)
			for o, more := read1(c.output); more || len(outs)%3 != 0; o, more = read1(c.output) {
				acsLog(1, "output[", i, "]: ", o)
				outs = append(outs, o)
			}
			acsLog(1, i, "outs", outs)
			for len(outs) != 0 {
				assert(3 <= len(outs), "not 3")
				to := outs[0]
				x := outs[1]
				y := outs[2]
				outs = outs[3:]
				if n <= to {
					acsLog(0, "Sending out[", to, x, y, "]")
					acsLog(0, "Solution", y)

					return
				}
				cs[to].input <- x
				cs[to].input <- y
			}
			c.input <- -1
		}
	}
}

func p2() {
	acsLog(1, "Hello p1")
	n := 50
	cs := make([]*computer, n)
	for i := range cs {
		c := computerFromString(prg, "")
		cs[i] = &c
		cs[i].input <- i
		go c.run()
	}
	natx, naty := -1, -1
	idle := 0
	prevnaty0 := -1
	for {
		acsLog(1, "loop")
		time.Sleep(10 * time.Millisecond)
		idle++
		for i, c := range cs {
			outs := []int{}
			acsLog(1, "comp", i)
			for o, more := read1(c.output); more || len(outs)%3 != 0; o, more = read1(c.output) {
				acsLog(1, "output[", i, "]: ", o)
				outs = append(outs, o)
			}
			acsLog(1, i, "outs", outs)
			for len(outs) != 0 {
				idle = 0
				assert(3 <= len(outs), "not 3")
				to := outs[0]
				x := outs[1]
				y := outs[2]
				if n <= to {
					if to != 255 {
						acsLog(0, outs)
						assert(to == 255, fmt.Sprint("not nat: ", to))
					}
					natx, naty = x, y
					acsLog(0, "Nat", x, y)
				} else {
					cs[to].input <- x
					cs[to].input <- y
				}
				outs = outs[3:]
			}
			c.input <- -1
		}
		if idle == 20 {
			cs[0].input <- natx
			cs[0].input <- naty
			acsLog(-1, "Natsend", natx, naty)
			if prevnaty0 == naty {
				acsLog(-1, "Repeat", naty)
			}
			prevnaty0 = naty
		}
	}
}

const LOGLEVEL = 0

func main() {
	// p1() // 15969
	p2()
}
