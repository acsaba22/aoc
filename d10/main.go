package main

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

func acsLog(level int, p ...interface{}) {
	if level <= 2 {
		fmt.Println(p...)
	}
}

type void struct{}

var member void

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

func solve1(ss []string, x0, y0 int) int {
	m := map[[2]int]int{}
	for x, line := range ss {
		for y, c := range line {
			if c == '#' {
				dx := x - x0
				dy := y - y0
				d := gcd(dx, dy)
				pair := [2]int{dx / d, dy / d}
				v, ok := m[pair]
				if !ok {
					v = 0
				}
				m[pair] = v + 1
				acsLog(4, "Adding asteroid at", x, y, dx/d, dy/d, v+1)
			}
		}
	}
	m2 := map[int]int{}
	nines := 0
	dx0, dy0 := 0, 0
	for i, v := range m {
		m2[v]++
		if v == 9 {
			acsLog(3, "direction: ", i)
			nines++
			dx0 = i[0]
			dy0 = i[1]
		}
	}
	if nines == 1 {
		acsLog(3, "Have one", dx0, dy0)
		acsLog(3, "origo", x0, y0)
		c := 0
		for i := 0; i < x0; i++ {
			if ss[y0][i] == '#' {
				c++
			}
		}
		acsLog(3, "origo", x0, y0, c)
	}
	acsLog(3, "visible count:", m2)
	acsLog(3, len(m))
	return len(m) - 1
}

func p1(s string) int {
	ss := strings.Fields(s)
	max := -1
	maxx, maxy := -1, -1
	for x, line := range ss {
		for y, c := range line {
			if c == '#' {
				cur := solve1(ss, x, y)
				if max < cur {
					acsLog(1, "better solution", cur, x, y)
					max = cur
					maxx = x
					maxy = y
				} else if max == cur {
					acsLog(1, "same solution at", cur, x, y)
				}
			}
		}
	}
	acsLog(1, "maxval", max, "maxx", maxx, "maxy", maxy)
	return max
}

func p2(s string, x0, y0 int) int {
	ss := strings.Fields(s)
	solve1(ss, x0, y0)
	return 0
}

func p22(s string, x0, y0 int) {
	ss := strings.Fields(s)
	m := map[[2]int][][2]int{}
	for x, line := range ss {
		for y, c := range line {
			if (x != x0 || y != y0) && c == '#' {
				dx := x - x0
				dy := y - y0
				d := gcd(dx, dy)
				p := [2]int{dx, dy}
				key := [2]int{dx / d, dy / d}
				v := m[key]
				m[key] = append(v, p)
				acsLog(4, "Adding asteroid at", x, y, dx/d, dy/d)
			}
		}
	}
	type line struct {
		d  [2]int
		vs [][2]int
	}
	rs := []line{}
	for d, vs := range m {
		rs = append(rs, line{d: d, vs: vs})
	}
	sort.Slice(rs, func(i, j int) bool {
		// a1 = [i]
		return alpha(rs[i].d) < alpha(rs[j].d)
	})
	shot := 0
	for 0 < len(rs) {
		for i := 0; i < len(rs); {
			l := rs[i]
			difLen := 10000
			jj := -1
			for j, d := range l.vs {
				dl := d[0]*d[0] + d[1]*d[1]
				if dl < difLen {
					difLen = dl
					jj = j
				}
			}
			acsLog(4, rs)
			shot++
			acsLog(1, "shooting", shot, "absolute:", x0+l.vs[jj][0], y0+l.vs[jj][1], "relative:", l.vs[jj][0], l.vs[jj][1])
			// if 2 < shot {
			// 	return
			// }
			copy(l.vs[jj:], l.vs[jj+1:])
			l.vs = l.vs[:len(l.vs)-1]
			rs[i] = l

			if len(l.vs) == 0 {
				copy(rs[i:], rs[i+1:])
				rs = rs[:len(rs)-1]
			} else {
				i++
			}
			// time.Sleep(2 * time.Second)
		}
	}
	acsLog(3, m)
	acsLog(3, rs)
}

func alpha(p [2]int) float64 {
	a := (math.Atan2(float64(p[0]), float64(p[1])) / math.Pi) + 0.5
	if a < 0 {
		a += 2
	}
	return a
}

func vis(s string, x0, y0 int) {
	ss := strings.Fields(s)
	for x, line := range ss {
		acsLog(1, line, x)
	}
}

func main() {
	// p1(t1)
	// p1(t2)
	// p1(t4)
	// p1(final)

	// p2(t4, 13, 11) // y=0 x=11 vagy y=1 11
	// p2(final, 11, 11)
	// acsLog(0, math.Atan2(0, -1)/math.Pi)
	// var s []int

	// p22(t1, 4, 3)
	// p22(t4, 13, 11)
	p22(final, 11, 11)
	vis(final, 11, 11)
}

var t1 = `
.#..#
.....
#####
....#
...##`

var t2 = `......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####`

var t4 = `
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##`

// origo 13 11, direction -1 0
// result 1, 11

var final =
// `
// ###..#########.#####.
// .####.#####..####.#.#
// .###.#.#.#####.##..##
// ##.####.#.###########
// ###...#.####.#.#.####
// #.##..###.########...
// #.#######.##.#######.
// .#..#.#..###...####.#
// #######.##.##.###..##
// #.#......#....#.#.#..
// ######.###.#.#.##...#
// ####.#...#.#######.#.
// .######.#####.#######
// ##.##.##.#####.##.#.#
// ###.#######..##.#....
// ###.##.##..##.#####.#
// ##.########.#.#.#####
// .##....##..###.#...#.
// #..#.####.######..###
// ..#.####.############
// ..##...###..#########`
`
###..#########.#####.
.####.#####..####.#.#
.###.#.#.#####.##..##
##.####.#.###########
###...#.####.#.#.####
#.##..###.########...
#.#######.##.#######.
.#..#.#..###...####.#
#######.##.##.###..##
#.#......#....#.#.#..
######.###.#.#.##...#
####.#...#.#######.#.
.######.#####.#######
##.##.##.#####.##.#.#
###.#######..##.#....
###.##.##..##.#####.#
##.########.#.#.#####
.##....##..###.#...#.
#..#.####.######..###
..#.####.############
..##...###..#########
`

// y = 0 x =11
// 1100 That's not the right answer; your answer is too high.
// y = 0 x= 11

// 2020 too high
