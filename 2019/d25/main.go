package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
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
	mem      []int
	ip       int
	input    chan int
	output   chan int
	modes    int
	id       int
	rBase    int // relative base
	reading  bool
	killed   bool
	finished bool
	n        int // number of steps
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
	comp.n = 0
	for {
		comp.n++
		if !comp.step() {
			acsLog(5, "Steps for ", comp.n)
			close(comp.output)
			comp.finished = true
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

func readPrg() string {
	f, err := os.Open("prg.txt")
	assertErr(err)
	bs, err := ioutil.ReadAll(f)
	assertErr(err)
	return string(bs)
}

func read1(ch chan int) (int, bool) {
	select {
	case x := <-ch:
		return x, true
	default:
		return -1, false
	}

}

const stepsS = ``

const stepsSOld = `north
south
east
take antenna
north
north
take asterisk
south
west
south
take hologram
north
west
take astronaut ice cream
east
east
south
east
take ornament
north
west
take fixed point
east
south
west
west
south
south
south
east
west
take dark matter
north
west
north
take monolith
north
north
inv
east`

// south
// inv
// take photons`

const invS = `antenna
astronaut ice cream
hologram
ornament
asterisk
fixed point
dark matter
monolith`

func p1a(prg string, drop []string, live bool) bool {
	acsLog(1, "Hello p1")
	c := computerFromString(prg, "")
	var err error
	var log *os.File
	if live {
		now := time.Now()
		log, err = os.Create(fmt.Sprintf("log-%v-%02v.txt", now.Hour(), now.Minute()))
		assertErr(err)
		defer log.Close()
	}
	go c.run()
	reader := bufio.NewReader(os.Stdin)
	steps := strings.Split(stepsS, "\n")
	if !live {
		last := make([]string, 2)
		copy(last, steps[len(steps)-2:])
		steps = append(steps[:len(steps)-1], drop...)
		steps = append(steps, last...)
	}

	output := bytes.Buffer{}
	// prevLines := []string{}
	for {
		time.Sleep(time.Millisecond)
		select {
		case r := <-c.output:
			if r == 10 {
				line := output.String()
				fmt.Printf("%s\n", output.String())
				output.Reset()
				if strings.Contains(line, "Alert!") {
					c.kill()
					return false
				}
			} else {
				output.WriteRune(rune(r))
			}
		default:
			if c.reading || c.finished {
				text := ""
				if 0 < len(steps) {
					text = steps[0] + "\n"
					steps = steps[1:]
					fmt.Printf(text)
				} else {
					var err error
					text, err = reader.ReadString('\n')
					assertErr(err)

				}
				if strings.TrimSpace(text) == "quit" {
					return true
				}
				for _, r := range text {
					c.input <- int(r)
				}
				if live {
					log.WriteString(text)
				}

			}
		}
	}
}

func p1tryall() {
	prg := readPrg()
	inv := strings.Split(invS, "\n")
	pow2 := 1 << len(inv)
	// p1a(prg, []string{})
	for i := 86; i < pow2; i++ {
		drop := []string{}
		for b, j := 1, 0; b <= i; b <<= 1 {
			if i&b != 0 {
				drop = append(drop, "drop "+inv[j])
			}
			j++
		}
		fmt.Println("Trying: ", strings.Join(drop, "|"))
		start := time.Now()
		ok := p1a(prg, drop, false)
		fmt.Println("Tried", i, "[", time.Since(start), "]: ", strings.Join(drop, "|"))
		time.Sleep(3 * time.Second)
		if ok {
			fmt.Println("Horray!")
			return
		}
	}
	acsLog(0, "Tried all")
}

func p1() {
	p1a(readPrg(), nil, true)
}

const LOGLEVEL = 2

func main() {
	// p1tryall() // 15969
	p1()
}
