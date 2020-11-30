package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/eiannone/keyboard"
)

// MAXMEM is
const MAXMEM = 1000000
const BUFFSIZE = 100000

func acsLog(level int, p ...interface{}) {
	if level <= 1 {
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

func p1() {
	c := computerFromString(s, "")
	done := make(chan struct{}, 10)
	go c.runSignal(done)
	for len(done) == 0 {
	}
	count := 0
	for 0 < len(c.output) {
		<-c.output
		<-c.output
		t := <-c.output
		if t == 2 {
			count++
		}

	}
	acsLog(1, "Block tiles: ", count)
}

type Stack struct {
	c     computer
	img   image
	score int
}

func p2() {
	readKeys := true
	debKeys := []keyboard.Key{
		keyboard.KeyArrowRight,
		keyboard.KeyArrowLeft,
		keyboard.KeyArrowUp,
		keyboard.KeyArrowUp,
		keyboard.KeyArrowUp,
		keyboard.KeyArrowUp,
		keyboard.KeyArrowUp,
		keyboard.KeyBackspace2,
		keyboard.KeyBackspace2,
		keyboard.KeyBackspace2,
		keyboard.KeyArrowRight,
		keyboard.KeyBackspace2}
	if readKeys {
		err := keyboard.Open()
		if err != nil {
			panic(err)
		}
		defer keyboard.Close()
	}
	c := computerFromString("2"+s[1:], "")
	done := make(chan struct{}, 10)
	go c.runSignal(done)
	img := newImage()
	// for 0 < len(c.output) {
	score := -1
	// barPos := -1
	// ballPos := -1
	// ballPosPrev := -1
	stack := []Stack{}
	backed := false
	finished := false
	for {
		read1 := false
		for !backed && (!c.reading || 0 < len(c.output) || !read1) {
			if 0 < len(done) {
				acsLog(0, "Finsihed score:", score)
				finished = true
				break
			}
			read1 = true
			x := <-c.output
			y := <-c.output
			rn := <-c.output
			if x == -1 && y == 0 {
				score = rn
				continue
			}
			r := ' '
			switch rn {
			case 0:
				r = ' '
			case 1:
				r = '#'
			case 2:
				r = 'O'
			case 3:
				r = '-'
				// barPos = x
			case 4:
				r = 'X'
				// ballPosPrev = ballPos
				// ballPos = x
			default:
				log.Fatal("unknown tile type", r)
			}
			img.paint(point{y, x}, r)
			acsLog(1, "paint", point{y, x}, string(r))
		}
		if finished {
			acsLog(0, "You finished, going back, score:", score)
			c.kill()
			<-done

			last := stack[len(stack)-2]
			c, img, score = last.c, last.img, last.score
			stack = stack[:len(stack)-1]
			backed = true
			go c.runSignal(done)
			finished = false
		}
		time.Sleep(time.Millisecond * 1)
		if 0 < len(c.output) || 0 < len(c.input) {
			log.Fatal("Not empty queue", len(c.output), len(c.input))
		}
		acsLog(0, img.str(' '))
		acsLog(0, "Score:", score)
		if backed {
			backed = false
		} else {
			stack = append(stack, Stack{c.clone(), img.clone(), score})
		}
		// time.Sleep(time.Millisecond * 1000)
		// // joystick := 0
		// // ballPosNext := 2*ballPos - ballPosPrev
		// // if ballPosNext < barPos {
		// // 	joystick = -1
		// // } else if barPos < ballPosNext {
		// // 	joystick = 1
		// // }
		// // c.input <- joystick
		// // acsLog(0, "Joystick:", joystick)
		acsLog(0, "Stack size:", len(stack))
		acsLog(0, "Press ESC to quit, arrows to play")
		var (
			char rune
			key  keyboard.Key
			err  error
		)

		if readKeys {
			char, key, err = keyboard.GetKey()
		} else {
			if 0 < len(debKeys) {
				char, key, err = ' ', debKeys[0], nil
				debKeys = debKeys[1:]
			} else {
				err = nil
			}
		}

		if err != nil {
			panic(err)
		} else if key == keyboard.KeyEsc {
			break
		}
		fmt.Printf("You pressed: %v   %q \n", key, char)
		if key == keyboard.KeyArrowLeft {
			c.input <- -1
		} else if key == keyboard.KeyArrowRight {
			c.input <- 1
		} else if key == keyboard.KeyBackspace || key == keyboard.KeyBackspace2 {
			acsLog(1, "You pressed backspace")
			c.kill()
			<-done

			last := stack[len(stack)-2]
			c, img, score = last.c, last.img, last.score
			stack = stack[:len(stack)-1]
			backed = true
			go c.runSignal(done)
		} else {
			c.input <- 0
		}
	}
}

var s = "1,380,379,385,1008,2487,361412,381,1005,381,12,99,109,2488,1101,0,0,383,1102,0,1,382,20101,0,382,1,20101,0,383,2,21101,0,37,0,1105,1,578,4,382,4,383,204,1,1001,382,1,382,1007,382,44,381,1005,381,22,1001,383,1,383,1007,383,21,381,1005,381,18,1006,385,69,99,104,-1,104,0,4,386,3,384,1007,384,0,381,1005,381,94,107,0,384,381,1005,381,108,1105,1,161,107,1,392,381,1006,381,161,1102,-1,1,384,1105,1,119,1007,392,42,381,1006,381,161,1102,1,1,384,20101,0,392,1,21101,19,0,2,21102,0,1,3,21102,1,138,0,1106,0,549,1,392,384,392,21002,392,1,1,21102,19,1,2,21101,0,3,3,21101,161,0,0,1106,0,549,1101,0,0,384,20001,388,390,1,21001,389,0,2,21101,0,180,0,1106,0,578,1206,1,213,1208,1,2,381,1006,381,205,20001,388,390,1,21001,389,0,2,21101,0,205,0,1105,1,393,1002,390,-1,390,1101,1,0,384,21002,388,1,1,20001,389,391,2,21101,228,0,0,1106,0,578,1206,1,261,1208,1,2,381,1006,381,253,21001,388,0,1,20001,389,391,2,21102,253,1,0,1106,0,393,1002,391,-1,391,1101,1,0,384,1005,384,161,20001,388,390,1,20001,389,391,2,21101,279,0,0,1105,1,578,1206,1,316,1208,1,2,381,1006,381,304,20001,388,390,1,20001,389,391,2,21102,304,1,0,1105,1,393,1002,390,-1,390,1002,391,-1,391,1102,1,1,384,1005,384,161,20102,1,388,1,20102,1,389,2,21102,1,0,3,21102,1,338,0,1106,0,549,1,388,390,388,1,389,391,389,21002,388,1,1,21001,389,0,2,21102,4,1,3,21101,0,365,0,1105,1,549,1007,389,20,381,1005,381,75,104,-1,104,0,104,0,99,0,1,0,0,0,0,0,0,326,20,16,1,1,22,109,3,21201,-2,0,1,21201,-1,0,2,21101,0,0,3,21102,1,414,0,1105,1,549,21201,-2,0,1,22101,0,-1,2,21101,429,0,0,1105,1,601,1201,1,0,435,1,386,0,386,104,-1,104,0,4,386,1001,387,-1,387,1005,387,451,99,109,-3,2105,1,0,109,8,22202,-7,-6,-3,22201,-3,-5,-3,21202,-4,64,-2,2207,-3,-2,381,1005,381,492,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,481,21202,-4,8,-2,2207,-3,-2,381,1005,381,518,21202,-2,-1,-1,22201,-3,-1,-3,2207,-3,-2,381,1006,381,507,2207,-3,-4,381,1005,381,540,21202,-4,-1,-1,22201,-3,-1,-3,2207,-3,-4,381,1006,381,529,21202,-3,1,-7,109,-8,2106,0,0,109,4,1202,-2,44,566,201,-3,566,566,101,639,566,566,1202,-1,1,0,204,-3,204,-2,204,-1,109,-4,2106,0,0,109,3,1202,-1,44,594,201,-2,594,594,101,639,594,594,20101,0,0,-2,109,-3,2106,0,0,109,3,22102,21,-2,1,22201,1,-1,1,21101,0,467,2,21102,1,497,3,21101,0,924,4,21101,0,630,0,1106,0,456,21201,1,1563,-2,109,-3,2106,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,2,2,2,2,2,0,2,2,2,2,0,2,2,2,2,0,2,2,2,2,0,0,2,2,0,0,2,2,0,0,2,2,2,0,2,0,0,2,0,0,1,1,0,2,2,0,0,2,0,0,2,2,0,0,2,2,2,2,0,0,0,0,0,0,0,2,0,2,2,2,2,0,2,0,2,2,2,0,2,0,0,0,0,0,1,1,0,2,0,0,2,0,2,0,0,2,0,2,0,0,0,0,2,2,2,2,2,2,0,2,2,2,0,0,2,0,2,2,0,2,0,0,2,2,2,0,0,0,1,1,0,2,2,2,0,2,2,2,2,2,0,2,2,2,2,0,2,2,0,0,2,0,2,0,0,2,2,2,2,2,2,0,2,2,0,0,0,2,2,2,0,0,1,1,0,0,0,2,2,2,2,2,2,0,0,0,2,2,0,0,2,2,2,0,2,0,0,2,0,0,2,2,2,2,0,2,2,0,2,0,0,2,2,0,2,0,1,1,0,2,2,0,2,2,0,2,2,2,0,2,2,2,2,2,2,2,2,2,0,2,2,0,0,2,2,2,0,0,2,0,2,0,0,2,2,2,2,2,2,0,1,1,0,0,2,2,2,0,2,0,2,2,2,0,0,2,0,0,2,2,0,0,2,2,0,2,2,0,0,0,0,0,2,0,0,2,2,2,2,0,0,0,2,0,1,1,0,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,2,0,2,2,2,2,0,2,0,0,2,2,2,0,2,2,0,0,2,2,2,0,2,0,2,0,1,1,0,2,0,2,2,2,0,2,2,0,0,2,2,2,2,0,0,2,2,2,2,0,2,0,2,0,0,2,2,2,0,0,2,2,2,0,0,2,2,0,2,0,1,1,0,2,0,0,2,2,2,0,0,0,2,0,0,2,2,0,2,2,2,2,0,2,2,2,2,0,0,2,2,0,0,2,2,2,0,2,2,0,2,2,2,0,1,1,0,2,2,2,0,2,2,2,0,2,2,0,2,2,2,2,0,0,2,0,2,0,0,2,2,2,2,2,2,2,2,0,2,0,2,2,0,2,2,0,0,0,1,1,0,0,2,2,2,2,2,2,2,2,0,2,0,0,2,2,2,2,0,0,2,0,0,0,2,0,2,2,2,2,0,2,0,2,2,0,2,2,2,0,2,0,1,1,0,2,2,2,2,2,2,2,2,0,0,2,2,0,2,2,2,2,2,2,2,0,0,0,0,2,2,2,2,2,2,2,0,0,2,2,2,0,0,2,2,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,76,15,12,20,17,35,34,85,94,21,61,77,95,65,26,54,97,28,64,78,13,60,7,20,13,39,26,86,92,16,31,89,45,57,59,94,4,79,83,27,94,86,44,96,79,56,56,68,94,84,79,17,16,43,86,35,76,47,16,20,71,88,82,7,12,17,8,63,61,88,40,93,14,85,9,34,70,27,27,58,71,69,88,14,72,51,33,43,15,44,84,29,35,18,82,14,45,98,10,35,62,74,16,44,7,51,38,50,31,82,72,21,94,21,53,73,40,24,93,96,6,64,19,57,51,56,53,57,58,68,78,9,79,87,52,62,36,17,80,30,42,65,96,3,55,56,95,89,42,33,23,30,90,47,18,68,94,51,26,52,23,32,13,3,93,91,44,1,30,86,93,8,69,72,2,53,33,23,58,48,69,74,24,6,33,85,96,38,83,51,61,96,79,25,14,78,83,41,85,32,94,95,67,87,53,47,81,14,56,88,37,95,54,83,84,41,35,75,33,77,24,32,62,10,5,91,82,63,21,81,71,5,89,4,64,87,32,59,22,3,98,79,70,79,5,52,26,70,19,95,23,45,77,79,60,89,89,88,45,5,50,31,47,14,76,22,9,48,71,4,15,38,82,61,62,59,68,50,81,71,57,47,41,9,63,77,49,91,25,2,14,88,60,43,44,7,14,51,93,44,45,75,19,49,34,41,19,48,25,34,32,88,29,51,88,71,79,76,7,81,73,90,42,78,43,50,40,18,8,56,51,86,58,74,47,27,5,80,83,96,36,23,93,56,94,40,67,49,83,10,62,45,82,94,8,51,16,43,7,16,37,92,12,9,77,51,75,95,46,95,91,46,35,49,97,85,28,17,39,78,42,37,17,20,9,22,3,41,43,78,34,70,60,28,92,60,8,81,70,86,6,61,57,37,85,23,55,69,89,25,5,20,49,31,98,11,27,56,9,74,9,66,5,44,60,97,13,37,74,54,27,38,4,19,72,90,89,23,8,35,54,35,79,82,72,43,14,67,64,89,12,4,52,30,6,7,11,7,72,81,73,49,76,23,44,8,17,35,21,26,88,4,46,71,96,89,34,71,79,59,24,49,87,24,57,89,32,97,71,95,41,29,27,70,70,33,94,62,84,21,26,23,82,5,35,58,48,71,60,97,89,84,69,82,85,90,54,30,81,87,51,59,76,20,95,8,11,46,18,59,36,76,69,71,42,29,69,69,24,54,84,96,88,79,40,38,73,10,89,66,32,25,96,16,41,78,53,76,28,37,92,33,33,77,22,51,50,7,47,50,4,59,70,14,42,20,97,95,54,10,47,94,89,68,69,10,85,9,57,7,32,44,3,75,54,52,89,39,38,88,10,23,95,5,66,68,72,42,94,94,51,73,3,5,35,8,28,4,20,74,32,40,20,51,17,3,62,28,59,43,10,7,52,70,82,8,52,70,50,45,79,98,65,78,20,73,64,64,87,2,11,69,28,70,37,73,3,29,57,32,15,87,42,66,1,57,26,31,23,56,51,45,50,31,10,8,74,29,73,70,72,18,74,97,88,80,46,10,20,8,97,80,54,47,64,12,48,87,14,94,49,52,30,20,21,9,98,30,51,11,30,32,78,21,72,55,38,79,74,35,93,31,40,66,86,27,12,34,80,45,44,23,4,35,35,58,6,17,47,57,30,82,65,16,82,76,63,75,76,85,86,69,29,92,79,9,14,46,76,37,66,61,15,97,7,4,23,91,8,81,81,15,59,3,29,47,24,81,85,63,10,87,9,10,87,15,25,25,62,17,30,21,87,38,92,65,88,13,23,21,75,44,89,9,86,58,81,25,75,93,46,52,44,13,70,32,71,82,7,11,54,71,11,69,3,31,7,26,23,65,10,15,10,82,33,28,67,33,65,7,70,23,92,83,53,6,35,44,65,95,23,53,13,25,90,69,7,89,34,26,91,81,84,45,61,78,87,51,98,38,5,59,29,12,5,53,78,88,4,93,56,97,65,37,22,3,52,80,1,18,43,93,20,97,65,81,84,35,6,58,16,31,86,72,47,18,27,22,97,85,52,43,95,16,12,10,44,49,24,86,28,55,19,22,361412"

func main() {
	// acsLog(1, "Hello World", -1%4)
	// test()
	// p1()
	p2()
}
