package computer

import (
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/acsaba22/advent/clean/logutil"
)

const MAXMEM = 1000000  // max mem size
const BUFFSIZE = 100000 // max io buf size

type computer struct {
	mem      []int
	ip       int
	Input    chan int
	Output   chan int
	modes    int // parameter reading/writing modes. (e.g. local or dereference).
	id       int
	rBase    int // relative base, will ofset memory reading and writing in mode 2.
	Reading  bool
	killed   bool
	Finished bool
	n        int // number of steps (operations done)
}

// Both code and input is a comma separated list of integers.
func ComputerFromString(code string, input string) computer {
	return NewComputer(toMem(code), toMem(input))
}

func NewComputer(mem []int, input []int) computer {
	comp := computer{mem: mem, Input: make(chan int, BUFFSIZE), Output: make(chan int, BUFFSIZE)}
	comp.typeInput(input)
	return comp
}

func (comp *computer) Run() {
	comp.n = 0
	for {
		comp.n++
		if !comp.step() {
			logutil.Log(5, "Steps for ", comp.n)
			close(comp.Output)
			comp.Finished = true
			return
		}
	}
}

func (comp *computer) runSignal(done chan struct{}) {
	comp.Run()
	done <- struct{}{}
}

// Read one instruction and execute it.
func (comp *computer) step() bool {
	op := comp.mem[comp.ip]
	comp.ip++
	opCode := op % 100
	comp.modes = op / 100
	logutil.Log(3, "comp", comp.id, "op", opCode)
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

// Every opcode has a function to do.
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
			comp.Reading = true
			a := <-comp.Input
			if comp.killed {
				return
			}
			comp.Reading = false
			logutil.Log(2, "comp", comp.id, "reading", a)
			comp.writeParam(a)
		},
	},
	4: {
		// write output
		func(comp *computer) {
			a := comp.loadParam()
			logutil.Log(2, "comp", comp.id, "writing", a)
			comp.Output <- a
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

// Write v into memory at location p
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

// Often you need to kill while the computer is waiting for reading:
// c.Kill()
// c.Input <- 1
func (comp *computer) Kill() {
	comp.killed = true
	comp.Input <- 1
}

/////////////////////////////////////////////////////
// Not so importan and unused functions from here on.

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

func (comp *computer) clone() computer {
	if !comp.Reading {
		log.Fatal("Can clone only reading computer")
	}
	ret := *comp
	ret.mem = make([]int, len(comp.mem))
	copy(ret.mem, comp.mem)
	ret.Input = make(chan int, BUFFSIZE)
	ret.Output = make(chan int, BUFFSIZE)
	ret.ip--
	return ret
}

func (comp *computer) outputToMem() []int {
	om := []int{}
	for 0 < len(comp.Output) {
		om = append(om, <-comp.Output)
	}
	return om
}

func (comp *computer) typeInput(input []int) {
	for _, v := range input {
		comp.Input <- v
	}
}

func (comp *computer) get(p int) int {
	if p < len(comp.mem) {
		return comp.mem[p]
	}
	return 0
}
