package main

import (
	"fmt"
	"log"
	"regexp"
	"strconv"
)

func acsLog(level int, p ...interface{}) {
	if level <= 1 {
		fmt.Println(p...)
	}
}

func gcd(a, b int) int {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	if a == 0 && b == 0 {
		return 1
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int, integers ...int) int {
	result := a * b / gcd(a, b)

	for i := 0; i < len(integers); i++ {
		result = lcm(result, integers[i])
	}

	return result
}

// func lcm(a, b int, integers ...int) int {
// 	aa := big.NewInt(a)
// 	bb := big.NewInt(b)
// 	t := big.NewInt(0)
// 	result := aa.a * b / gcd(a, b)

// 	for i := 0; i < len(integers); i++ {
// 		result = lcm(result, integers[i])
// 	}

// 	return result
// }

func acsAssert(b bool, s ...interface{}) {
	if !b {
		log.Fatal(s...)
	}
}

const s1 = `
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>`

const s2 = `
<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>`

const final = `
<x=5, y=4, z=4>
<x=-11, y=-11, z=-3>
<x=0, y=7, z=0>
<x=-13, y=2, z=10>`

// D dimensions
const D = 3

// N number of moons
const N = 4

type vect [D]int

func (v vect) add(v2 vect) vect {
	ret := vect{}
	for i := range v {
		ret[i] = v[i] + v2[i]
	}
	return ret
}

func (v vect) sub(v2 vect) vect {
	ret := vect{}
	for i := range v {
		ret[i] = v[i] - v2[i]
	}
	return ret
}

func (v vect) dir() vect {
	ret := vect{}
	for i, v := range v {
		ret[i] = 0
		if v < 0 {
			ret[i] = -1
		}
		if v > 0 {
			ret[i] = +1
		}
	}
	return ret
}

func (v vect) sum() int {
	ret := 0
	for _, v := range v {
		if v < 0 {
			v = -v
		}
		ret += v
	}
	return ret
}

type moon struct {
	c vect
	v vect
}

func (m *moon) updateV(m2 moon) {
	m.v = m.v.add(m2.c.sub(m.c).dir())
}

func (m *moon) updateC() {
	m.c = m.c.add(m.v)
}

type system [N]moon

func (m *moon) read(i string) {
	r := regexp.MustCompile(`x=(.*), y=(.*), z=(.*)`)
	matches := r.FindStringSubmatch(i)
	acsLog(3, matches, fmt.Sprintf("%T", matches))
	acsAssert(len(matches) == D+1, "Bad number of coordinates", i)
	for q, mm := range matches[1:] {
		cc, err := strconv.Atoi(mm)
		acsAssert(err == nil, "number failed", matches[q], "Err:", err)
		m.c[q] = cc
	}

}

func (s *system) read(i string) {
	r := regexp.MustCompile(`<(.*)>`)
	matches := r.FindAllStringSubmatch(i, -1)
	acsAssert(len(matches) == N, "Bad number of moons", i)
	acsLog(3, matches, fmt.Sprintf("%T", matches))
	for q, mm := range matches {
		s[q].read(mm[1])
	}
}

func (s *system) updateV() {
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			s[i].updateV(s[j])
		}

	}
}

func (s *system) updateC() {
	for i := 0; i < N; i++ {
		s[i].updateC()
	}
}

func (s *system) energy() int {
	ret := 0
	for i := 0; i < N; i++ {
		ret += s[i].c.sum() * s[i].v.sum()
	}
	return ret
}

func p1(i string, steps int) {
	s := system{}
	s.read(i)
	acsLog(2, s)
	for i := 0; i < steps; i++ {
		s.updateV()
		s.updateC()
		acsLog(2, i, s, s.energy())
	}
}

func p2(i string, steps int) {
	s := system{}
	s.read(i)
	s0 := s
	acsLog(2, s)
	for i := 0; i < steps; i++ {
		if i%1000000 == 0 {
			acsLog(1, "Step:", i, float64(i)/float64(steps))
		}
		s.updateV()
		s.updateC()
		if s == s0 {
			acsLog(1, "They are the same!", i+1)
			return
		}
	}
	acsLog(1, "Not yet")
}

func f(x, y int, a ...int) {

}

func p22(i string, steps int) {
	s := system{}
	s.read(i)
	s0 := s
	acsLog(2, s)
	have := 0
	counts := [D]int{-1, -1, -1}
	for i := 1; i < steps && have != D; i++ {
		if i%1000000 == 0 {
			acsLog(1, "Step:", i, float64(i)/float64(steps))
		}
		s.updateV()
		s.updateC()
		for j := 0; j < D; j++ {
			same := true
			for k := 0; k < N; k++ {
				if s[k].c[j] != s0[k].c[j] || s[k].v[j] != s0[k].v[j] {
					same = false
				}
			}
			if same {
				acsLog(1, "Dim ", j, "is the same:", i)
				if counts[j] < 0 {
					counts[j] = i
					have++
				}
			}

		}
	}
	if have == D {
		// f(counts[:]...)
		acsLog(1, counts, lcm(counts[0], counts[1], counts[2:]...))
	} else {
		acsLog(1, "Not yet")
	}
}

func main() {
	acsLog(1, "Hello World")
	// p1(s1, 10)
	// p1(s2, 100)
	// p1(final, 1000)
	p2(s1, 10000)
	// p2(s2, 100000000)
	// p2(final, 100000000)
	p22(s1, 100000000)
	p22(s2, 100000000)
	p22(final, 100000000)
}
