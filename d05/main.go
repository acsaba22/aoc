package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
)

type memory []int

func toMem(input string) memory {
	if input == "" {
		return memory{}
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

func (mem memory) toString() string {
	return strings.Trim(strings.Replace(fmt.Sprint(mem), " ", ",", -1), "[]")
}

type computer struct {
	mem    memory
	ip     int
	input  []int
	output []int
	modes  int
}

func newComputer(code string, input string) computer {
	return computer{toMem(code), 0, toMem(input), []int{}, 0}
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
			a := comp.input[0]
			comp.input = comp.input[1:]
			comp.writeParam(a)
		},
	},
	4: {
		// write output
		func(comp *computer) {
			a := comp.loadParam()
			comp.output = append(comp.output, a)
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

func run1(code string, input string) memory {
	comp := newComputer(code, input)
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
		if o := run1(i, "").toString(); oe != o {
			log.Fatalln(fmt.Sprintf("actual: %v expected: %v input: %v", o, oe, i))
		}
	}
	fmt.Println("OK")
}

func test2() {
	comp := newComputer("3,0,4,0,99", "42")
	comp.run()
	fmt.Println(comp.input)
	fmt.Println(comp.output)
}

var s = `3,225,1,225,6,6,1100,1,238,225,104,0,1101,48,82,225,102,59,84,224,1001,224,-944,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,92,58,224,101,-150,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,10,89,224,101,-890,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,29,16,225,101,23,110,224,1001,224,-95,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1102,75,72,225,1102,51,8,225,1102,26,16,225,1102,8,49,225,1001,122,64,224,1001,224,-113,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,55,72,225,1002,174,28,224,101,-896,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,57,32,225,2,113,117,224,101,-1326,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1,148,13,224,101,-120,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,102,2,223,223,1006,224,329,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,404,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,419,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,434,1001,223,1,223,1008,677,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,677,224,102,2,223,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226`

func p1() {
	comp := newComputer(s, "1")
	comp.run()
	fmt.Println(comp.input)
	fmt.Println(comp.output)
}

func p2() {
	comp := newComputer(s, "5")
	comp.run()
	fmt.Println(comp.input)
	fmt.Println(comp.output)
}

func main() {
	test()
	test2()
	p1()
	p2()
}
