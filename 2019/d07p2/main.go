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
func (comp *computer) typeInput(input []int) {
	for _, v := range input {
		comp.input <- v
	}
}

func (comp *computer) loadParam() int {
	mode := comp.modes % 10
	comp.modes /= 10
	ret := 0
	switch mode {
	case 0:
		ret = comp.mem[comp.mem[comp.ip]]
	case 1:
		ret = comp.mem[comp.ip]
	default:
		log.Fatalln("Unkown mode", mode)
	}
	comp.ip++
	return ret
}

func (comp *computer) writeParam(v int) {
	comp.mem[comp.mem[comp.ip]] = v
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
	for {
		if !comp.step() {
			return
		}
	}
}

func (comp *computer) runSignal(done chan struct{}) {
	comp.run()
	done <- struct{}{}
}

func run1(code string, input string) []int {
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
		if o := memToString(run1(i, "")); oe != o {
			log.Fatalln(fmt.Sprintf("actual: %v expected: %v input: %v", o, oe, i))
		}
	}
	fmt.Println("OK")
}

func test2() {
	comp := computerFromString("3,0,4,0,99", "42")
	comp.run()
	fmt.Println(comp.input)
	fmt.Println(comp.output)
}

func findPerm(prog []int, input int, left []int) int {
	var nextLeft []int
	if 0 < len(left) {
		nextLeft = make([]int, len(left)-1)
	}
	result := -1
	for i, current := range left {
		copy(nextLeft, left[:i])
		copy(nextLeft[i:], left[i+1:])
		comp := newComputer(prog, []int{current, input})
		comp.run()
		if len(nextLeft) == 0 {
			return <-comp.output
		}
		if res := findPerm(prog, <-comp.output, nextLeft); result < res {
			result = res
		}
	}
	return result
}

func perms(values []int, prefix []int, f func([]int)) {
	if len(values) == 0 {
		f(prefix)
	} else {
		level := len(prefix)
		nextValues := make([]int, len(values)-1)
		nextPrefix := make([]int, level+1)
		copy(nextPrefix, prefix)
		for i, current := range values {
			copy(nextValues, values[:i])
			copy(nextValues[i:], values[i+1:])
			nextPrefix[level] = current
			perms(nextValues, nextPrefix, f)
		}
	}
}

func permN(n int, f func([]int)) {
	values := make([]int, n)
	for i := 0; i < n; i++ {
		values[i] = i
	}
	perms(values, []int{}, f)
}

func connect(progs string, loop bool) {
	fmt.Println("Run")
	d := 0
	if loop {
		d = 5
	}
	prog := toMem(progs)
	res := -1
	permN(5, func(p []int) {
		// func(p []int) {
		n := len(p)
		comps := make([]computer, n)
		input := make(chan int, 1000)
		input0 := input
		for i, v := range p {
			comps[i] = computer{mem: append([]int(nil), prog...)}
			comps[i].id = i + d
			comps[i].input = input
			comps[i].input <- v + d
			if loop && (i == n-1) {
				comps[i].output = input0
			} else {
				comps[i].output = make(chan int, 1000)
			}
			input = comps[i].output
		}
		comps[0].input <- 0
		done := make(chan struct{})
		for i := range p {
			go comps[i].runSignal(done)
		}
		for range p {
			<-done
		}
		output := <-comps[n-1].output
		if res < output {
			res = output
		}
		// fmt.Println(p, output)
	})
	// }([]int{4, 3, 2, 1, 0})

	fmt.Println("final:", res)
}

func p1(progs string) {
	connect(progs, false)
}

func p2(progs string) {
	connect(progs, true)
}

func p11(progs string) {
	fmt.Println("Run")
	prog := toMem(progs)
	res := -1
	permN(5, func(p []int) {
		output := 0
		for _, v := range p {
			comp := newComputer(prog, []int{v, output})
			comp.run()
			output = <-comp.output
		}
		if res < output {
			res = output
		}
		// fmt.Println(p, output)
	})
	fmt.Println("final:", res)
}

var s = "3,8,1001,8,10,8,105,1,0,0,21,46,55,72,85,110,191,272,353,434,99999,3,9,1002,9,5,9,1001,9,2,9,102,3,9,9,101,2,9,9,102,4,9,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1002,9,2,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,4,9,99,3,9,1002,9,3,9,101,5,9,9,1002,9,3,9,101,3,9,9,1002,9,5,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99"

func main() {
	test()
	test2()
	p1("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
	p1("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
	p1("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
	p1(s)

	p2("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
	p2("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
	p2(s)
}

// That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 437860.)
