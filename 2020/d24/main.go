package main

import (
	"bytes"
	"fmt"
	"log"
	"strings"
)

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

type point struct {
	p0, p1 int
}

func (p *point) add(p2 point) {
	p.p0 += p2.p0
	p.p1 += p2.p1
}

type rectangle struct {
	d point
	n int
	v []rune
}

func newRectangle(d point) rectangle {
	n := d.p0 * d.p1
	return rectangle{v: make([]rune, n), d: d, n: n}
}

func newRectangleFromString1(s string) rectangle {
	lines := strings.FieldsFunc(s, func(r rune) bool {
		return r == 10
	})
	rect := newRectangle(point{len(lines) + 2, len(lines[0]) + 2})
	for i, line := range lines {
		for j, r := range " " + line + " " {
			rect.paint(point{i + 1, j}, r)
		}
	}
	return rect
}

func newRectangleFromString(s string) rectangle {
	lines := strings.FieldsFunc(s, func(r rune) bool {
		return r == 10
	})
	rect := newRectangle(point{len(lines), len(lines[0])})
	for i, line := range lines {
		for j, r := range line {
			rect.paint(point{i, j}, r)
		}
	}
	return rect
}

func (r *rectangle) paint(p point, ru rune) {
	r.v[p.p0*r.d.p1+p.p1] = ru
}

func (r *rectangle) get(p point) rune {
	return r.v[p.p0*r.d.p1+p.p1]
}

func (r *rectangle) get1(p point) rune {
	return r.get(point{p.p0 + 1, p.p1 + 1})
}

func (r *rectangle) paint1(p point, ru rune) {
	r.paint(point{p.p0 + 1, p.p1 + 1}, ru)
}

func (r *rectangle) translate(i int) point {
	return point{i / r.d.p1, i % r.d.p1}
}

func isSmall(r rune) bool {
	return 'a' <= r && r <= 'z'
}

func isBig(r rune) bool {
	return 'A' <= r && r <= 'Z'
}

func toSmall(r rune) rune {
	return r - 'A' + 'a'
}

func (r rectangle) str() string {
	ret := make([]rune, 0)
	for q := 0; q < r.d.p0; q++ {
		ret = append(ret, r.v[q*r.d.p1:(q+1)*r.d.p1]...)
		ret = append(ret, '\n')

	}
	return string(ret[:len(ret)-1])
}

func p1(input string) {
	r1o := newRectangleFromString1(input)
	r2o := newRectangleFromString1(input)
	r1 := &r1o
	r2 := &r2o
	n := r1.d.p0 - 2
	acsLog(3, "n", n)
	acsLog(3, r1.str())
	seen := map[int]bool{}
	for step := 0; ; step++ {
		pow2 := 1
		rating := 0
		for y := 0; y < n; y++ {
			for x := 0; x < n; x++ {
				p := point{y, x}
				current := r1.get1(p)

				if current == '#' {
					rating |= pow2
				}
				pow2 <<= 1

				neigh := []point{point{y - 1, x}, point{y + 1, x}, point{y, x - 1}, point{y, x + 1}}
				nl := 0
				for _, n := range neigh {
					if r1.get1(n) == '#' {
						nl++
					}
				}
				if current == '#' {
					if nl != 1 {
						current = '.'
					}
				} else {
					if nl == 1 || nl == 2 {
						current = '#'
					}
				}
				r2.paint1(p, current)
			}
		}
		if seen[rating] {
			acsLog(1, "Final:\n", r1.str())
			acsLog(0, "Step", step)
			acsLog(0, "Solution", rating)
			return
		}
		seen[rating] = true
		acsLog(3, r2.str())
		r2, r1 = r1, r2
	}
}

type manylevels struct {
	minlevel, maxlevel int
	levels             map[int]*rectangle
}

func nml() *manylevels {
	return &manylevels{0, 0, map[int]*rectangle{}}
}

func (mls *manylevels) str() string {
	buf := bytes.Buffer{}
	for i := mls.minlevel; i <= mls.maxlevel; i++ {
		fmt.Fprintln(&buf, "Depth ", i, ":")
		fmt.Fprintln(&buf, mls.levels[i].str())
		fmt.Fprintln(&buf)
	}
	return buf.String()
}

const N = 5

func (mls *manylevels) get(level, y, x int) rune {
	if level < mls.minlevel || mls.maxlevel < level {
		return 'X'
	}
	if y < 0 || N <= y || x < 0 || N <= x {
		return 'Y'
	}
	return mls.levels[level].get(point{y, x})
}

func newLevel() *rectangle {
	r := newRectangle(point{5, 5})
	r.paint(point{2, 2}, '?')
	return &r
}

func p2(input string, steps int) {
	const n = 5
	manylevels := nml()
	orig := newRectangleFromString(input)
	manylevels.levels[0] = &orig

	v := [n * n]int{}
	for i := range v {
		v[i] = i
	}

	// up := []int{20 - n, 21 - n, 22 - n, 23 - n, 24 - n}

	// neigh := map[point][]int{
	// 	// 		for y := 0; y < n; y++ {
	// 	// 			for x := 0; x < n; x++ {
	// 	// 				if x != 2 || y != 2 {
	// 	//0
	// 	append([]int{1, 5}, up...)}

	for step := 0; step < steps; step++ {
		next := nml()
		for level := manylevels.minlevel - 1; level <= manylevels.maxlevel+1; level++ {
			next.levels[level] = newLevel()
			for y := 0; y < n; y++ {
				for x := 0; x < n; x++ {
					if x != 2 || y != 2 {
						neigh := [][3]int{{level, y - 1, x}, {level, y + 1, x}, {level, y, x - 1}, {level, y, x + 1}}
						if x == 2 {
							innery := -1
							if y == 1 {
								innery = 0
							} else if y == 3 {
								innery = 4
							}
							if innery != -1 {
								for k := 0; k < n; k++ {
									neigh = append(neigh, [3]int{level + 1, innery, k})
								}
							}
						}
						if y == 2 {
							innerx := -1
							if x == 1 {
								innerx = 0
							} else if x == 3 {
								innerx = 4
							}
							if innerx != -1 {
								for k := 0; k < n; k++ {
									neigh = append(neigh, [3]int{level + 1, k, innerx})
								}
							}
						}
						if y == 0 {
							neigh = append(neigh, [3]int{level - 1, 1, 2})
						}
						if y == 4 {
							neigh = append(neigh, [3]int{level - 1, 3, 2})
						}
						if x == 0 {
							neigh = append(neigh, [3]int{level - 1, 2, 1})
						}
						if x == 4 {
							neigh = append(neigh, [3]int{level - 1, 2, 3})
						}
						num := 0
						for _, n := range neigh {
							if manylevels.get(n[0], n[1], n[2]) == '#' {
								num++
							}
						}
						current := manylevels.get(level, y, x)
						if current == '#' {
							if num != 1 {
								current = '.'
							}
						} else {
							if num == 1 || num == 2 {
								current = '#'
							}
						}
						if current == '#' {
							if level < next.minlevel {
								next.minlevel = level
							}
							if next.maxlevel < level {
								next.maxlevel = level
							}
						} else {
							current = '.'
						}
						next.levels[level].paint(point{y, x}, current)
					}
				}
			}
		}
		manylevels = next
	}
	acsLog(3, "=====================")
	acsLog(3, manylevels.str())
	sum := 0
	for level := manylevels.minlevel; level <= manylevels.maxlevel; level++ {
		for y := 0; y < n; y++ {
			for x := 0; x < n; x++ {
				if manylevels.get(level, y, x) == '#' {
					sum++
				}
			}
		}
	}
	acsLog(3, "Bugs:", sum)

	// 	pow2 := 1
	// 	rating := 0
	// 			p := point{y, x}
	// 			current := r1.get1(p)

	// 			if current == '#' {
	// 				rating |= pow2
	// 			}
	// 			pow2 <<= 1

	// 			neigh := []point{point{y - 1, x}, point{y + 1, x}, point{y, x - 1}, point{y, x + 1}}
	// 			nl := 0
	// 			for _, n := range neigh {
	// 				if r1.get1(n) == '#' {
	// 					nl++
	// 				}
	// 			}
	// 			if current == '#' {
	// 				if nl != 1 {
	// 					current = '.'
	// 				}
	// 			} else {
	// 				if nl == 1 || nl == 2 {
	// 					current = '#'
	// 				}
	// 			}
	// 			r2.paint1(p, current)
	// 		}
	// 	}
	// 	if seen[rating] {
	// 		acsLog(1, "Final:\n", r1.str())
	// 		acsLog(0, "Step", step)
	// 		acsLog(0, "Solution", rating)
	// 		return
	// 	}
	// 	seen[rating] = true
	// 	acsLog(3, r2.str())
	// 	r2, r1 = r1, r2
	// }
}

const LOGLEVEL = 3

func main() {
	// p1(input) // 1151290
	// p2(known2, 10)
	p2(input, 200)

}

const known1 = `....#
#..#.
#..##
..#..
#....`

const known2 = `....#
#..#.
#.?##
..#..
#....`

const input = `.##.#
###..
#...#
##.#.
.###.`
