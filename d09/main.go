package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
)

func acsLog(level int, p ...interface{}) {
	if level < 2 {
		fmt.Println(p...)
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
	mem    []int
	ip     int
	input  chan int
	output chan int
	modes  int
	id     int
	rBase  int // relative base
}

func computerFromString(code string, input string) computer {
	return newComputer(toMem(code), toMem(input))
}

func newComputer(mem []int, input []int) computer {
	buffsize := 1000
	comp := computer{mem: mem, input: make(chan int, buffsize), output: make(chan int, buffsize)}
	comp.typeInput(input)
	return comp
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

// MAXMEM is
const MAXMEM = 1000000

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
			a := <-comp.input
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
	return true
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

var s = "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,0,38,1019,1102,1,37,1008,1101,252,0,1023,1102,24,1,1004,1102,35,1,1017,1101,0,28,1011,1101,0,36,1003,1102,30,1,1013,1101,0,0,1020,1102,1,1,1021,1102,897,1,1028,1101,20,0,1000,1101,0,22,1005,1102,29,1,1007,1101,0,34,1009,1102,1,259,1022,1101,310,0,1025,1102,892,1,1029,1101,21,0,1014,1102,1,315,1024,1101,0,33,1002,1102,31,1,1015,1102,190,1,1027,1102,1,39,1001,1101,26,0,1010,1101,27,0,1016,1102,1,23,1018,1101,0,32,1012,1101,0,25,1006,1102,1,197,1026,109,34,2106,0,-7,1001,64,1,64,1106,0,199,4,187,1002,64,2,64,109,-22,2108,34,-3,63,1005,63,221,4,205,1001,64,1,64,1106,0,221,1002,64,2,64,109,-10,1208,-1,42,63,1005,63,237,1106,0,243,4,227,1001,64,1,64,1002,64,2,64,109,20,2105,1,1,1001,64,1,64,1105,1,261,4,249,1002,64,2,64,109,1,21108,40,40,-6,1005,1017,283,4,267,1001,64,1,64,1105,1,283,1002,64,2,64,109,7,1205,-9,301,4,289,1001,64,1,64,1105,1,301,1002,64,2,64,109,-1,2105,1,-5,4,307,1106,0,319,1001,64,1,64,1002,64,2,64,109,-8,1206,0,331,1105,1,337,4,325,1001,64,1,64,1002,64,2,64,109,-6,21108,41,38,0,1005,1015,353,1105,1,359,4,343,1001,64,1,64,1002,64,2,64,109,11,1206,-6,377,4,365,1001,64,1,64,1106,0,377,1002,64,2,64,109,1,21101,42,0,-8,1008,1019,42,63,1005,63,399,4,383,1105,1,403,1001,64,1,64,1002,64,2,64,109,-29,1202,6,1,63,1008,63,24,63,1005,63,425,4,409,1106,0,429,1001,64,1,64,1002,64,2,64,109,14,1201,-3,0,63,1008,63,34,63,1005,63,451,4,435,1105,1,455,1001,64,1,64,1002,64,2,64,109,10,21101,43,0,-9,1008,1013,41,63,1005,63,475,1106,0,481,4,461,1001,64,1,64,1002,64,2,64,109,-17,2101,0,0,63,1008,63,21,63,1005,63,501,1106,0,507,4,487,1001,64,1,64,1002,64,2,64,109,-5,2107,21,5,63,1005,63,525,4,513,1105,1,529,1001,64,1,64,1002,64,2,64,109,13,1202,-7,1,63,1008,63,26,63,1005,63,553,1001,64,1,64,1106,0,555,4,535,1002,64,2,64,109,5,21107,44,45,-8,1005,1010,573,4,561,1105,1,577,1001,64,1,64,1002,64,2,64,109,-6,21102,45,1,7,1008,1019,45,63,1005,63,603,4,583,1001,64,1,64,1105,1,603,1002,64,2,64,109,-15,1207,10,28,63,1005,63,623,1001,64,1,64,1106,0,625,4,609,1002,64,2,64,109,8,2108,37,-4,63,1005,63,645,1001,64,1,64,1105,1,647,4,631,1002,64,2,64,109,6,21102,46,1,1,1008,1012,44,63,1005,63,671,1001,64,1,64,1106,0,673,4,653,1002,64,2,64,109,4,1207,-6,35,63,1005,63,695,4,679,1001,64,1,64,1106,0,695,1002,64,2,64,109,1,2107,38,-8,63,1005,63,715,1001,64,1,64,1105,1,717,4,701,1002,64,2,64,109,-23,1208,10,36,63,1005,63,739,4,723,1001,64,1,64,1105,1,739,1002,64,2,64,109,4,2102,1,7,63,1008,63,24,63,1005,63,765,4,745,1001,64,1,64,1105,1,765,1002,64,2,64,109,13,2102,1,-4,63,1008,63,22,63,1005,63,789,1001,64,1,64,1105,1,791,4,771,1002,64,2,64,109,-8,1201,5,0,63,1008,63,32,63,1005,63,811,1106,0,817,4,797,1001,64,1,64,1002,64,2,64,109,11,1205,7,829,1105,1,835,4,823,1001,64,1,64,1002,64,2,64,109,-1,2101,0,-6,63,1008,63,25,63,1005,63,857,4,841,1106,0,861,1001,64,1,64,1002,64,2,64,109,8,21107,47,46,-9,1005,1011,877,1106,0,883,4,867,1001,64,1,64,1002,64,2,64,109,9,2106,0,-1,4,889,1106,0,901,1001,64,1,64,4,64,99,21101,0,27,1,21102,915,1,0,1105,1,922,21201,1,59500,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1105,1,922,21201,1,0,-1,21201,-2,-3,1,21101,0,957,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0"

func p1() {
	comp := computerFromString(s, "1")
	comp.run()
	acsLog(1, "p1: ", comp.outputToMem())
}

func p2() {
	comp := computerFromString(s, "2")
	comp.run()
	acsLog(1, "p1: ", comp.outputToMem())
}

func main() {
	test()
	p1()
	p2()
}

// That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 437860.)
