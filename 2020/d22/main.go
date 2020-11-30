package main

import (
	"fmt"
	"log"
	"math/big"
	"strconv"
	"strings"
)

const LOGLEVEL = 1

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

func p0(input string, n int, deck []int) []int {

	lines := strings.FieldsFunc(input, func(r rune) bool { return r == 10 })
	acsLog(2, strings.Join(lines, "|"))
	incS := "deal with increment "
	reverseS := "deal into new stack"
	cutS := "cut "
	next := make([]int, n)
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, incS) {
			k, err := strconv.Atoi(line[len(incS):])
			assertErr(err)
			acsLog(2, "increment:", k)
			for i, v := range deck {
				next[i*k%n] = v
			}
		} else if strings.HasPrefix(line, cutS) {
			k, err := strconv.Atoi(line[len(cutS):])
			assertErr(err)
			acsLog(2, "cut:", k)
			if k < 0 {
				k += n
			}

			copy(next[n-k:], deck)
			copy(next, deck[k:])
		} else if strings.HasPrefix(line, reverseS) {
			acsLog(2, "reverse")
			for i, v := range deck {
				next[n-i-1] = v
			}

		} else {
			log.Fatal("Couldn't understand: " + line)
		}
		deck, next = next, deck
	}
	return deck
}

func p1b(input string, n, times, lookFor int) int {
	deck := make([]int, n)
	for i := range deck {
		deck[i] = i
	}
	for i := 0; i < times; i++ {
		deck = p0(input, n, deck)
	}
	acsLog(1, "P1ALL: ", deck)
	for i, v := range deck {
		if v == lookFor {
			acsLog(1, "P1Solution: ", i)
			return i
		}
	}
	return -1
}

func p1(input string, n int) []int {
	deck := make([]int, n)
	for i := range deck {
		deck[i] = i
	}
	deck = p0(input, n, deck)
	acsLog(1, "Finished", "steps")
	if 2019 < n {
		for i, v := range deck {
			if v == 2019 {
				acsLog(1, "Solution: ", i)
			}
		}
	} else {
		acsLog(2, deck)
	}
	return deck
}

var tests1 = [...]string{
	`deal into new stack`,
	`cut 3`,
	`cut -4`,
	`deal into new stack
	cut 3
	deal into new stack
	cut -2`,
	`deal with increment 7`,
}

func test1() {
	t := func(testS string, n int) {
		acsLog(0, "running test", testS)
		deck := p1(testS, n)
		for i, v := range deck {
			bn := big.NewInt(int64(n))
			v2 := p2a(testS, bn, v)
			if v2.Int64() != int64(i) {
				acsLog(0, "Error : ", i, "(", v2, "expected", i, ")")
				return
			}
		}
	}
	for _, testS := range tests1 {
		t(testS, 10)
	}
	t(input, 10007)
	acsLog(0, "Tests OK")
}

func trans(input string, n *big.Int) (a, b *big.Int) {
	lines := strings.FieldsFunc(input, func(r rune) bool { return r == 10 })
	acsLog(2, strings.Join(lines, "|"))
	incS := "deal with increment "
	reverseS := "deal into new stack"
	cutS := "cut "
	// needed := big.NewInt(int64(finalPosi64))
	tmp1 := &big.Int{}
	tmp2 := &big.Int{}
	c1 := big.NewInt(1)
	// n1 := &big.Int{}
	// n1.Sub(n, one)
	a = big.NewInt(1)
	b = big.NewInt(0)
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, reverseS) {
			tmp1.Neg(a)
			a, tmp1 = tmp1, a
			tmp2.Neg(b)
			b.Sub(tmp2, c1)
		} else if strings.HasPrefix(line, cutS) {
			kk, err := strconv.Atoi(line[len(cutS):])
			assertErr(err)
			// acsLog(2, "cut:", kk)
			// if kk < 0 {
			k := big.NewInt(int64(kk))
			tmp1.Sub(b, k)
			b, tmp1 = tmp1, b
			// }
		} else if strings.HasPrefix(line, incS) {
			kk, err := strconv.Atoi(line[len(incS):])
			assertErr(err)
			// acsLog(2, "increment:", k)
			k := big.NewInt(int64(kk))
			tmp1.Mul(a, k)
			a, tmp1 = tmp1, a
			tmp1.Mul(b, k)
			b, tmp1 = tmp1, b
		} else {
			log.Fatal("Couldn't understand: " + line)
		}
		tmp1.Mod(a, n)
		a, tmp1 = tmp1, a
		tmp1.Mod(b, n)
		b, tmp1 = tmp1, b
	}
	// acsLog(1, "Finished", len(lines), "steps")
	return
}

func p2a(input string, n *big.Int, startPos int) (final *big.Int) {
	a, b := trans(input, n)
	// acsLog(1, "a,b", a, b)
	t1 := &big.Int{}
	start := big.NewInt(int64(startPos))
	t1.Mul(a, start)
	t2 := &big.Int{}
	t2.Add(t1, b)
	final = &big.Int{}
	final.Mod(t2, n)
	return
}

func repeat(a1, b1, n *big.Int, times *big.Int) (a, b *big.Int) {
	acsLog(1, "repeat", a1, b1, n, times)
	a = big.NewInt(1)
	b = big.NewInt(0)

	null := big.NewInt(0)
	tmp := nb()
	tmp2 := nb()
	tmpa := nb()
	for i := 0; times.Cmp(null) != 0; i++ {
		if times.Bit(i) != 0 {
			acsLog(1, "adding", a1, b1, n, times)
			tmp.SetBit(times, i, 0)
			times, tmp = tmp, times
			tmp.Mul(a, a1)
			a.Mod(tmp, n)

			tmp2.Add(tmp.Mul(a1, b), b1)
			b.Mod(tmp2, n)
			acsLog(1, "added", a, b)

		}
		tmp.Mul(a1, a1)
		tmpa.Mod(tmp, n)
		tmp2.Add(tmp.Mul(a1, b1), b1)
		a1, tmpa = tmpa, a1
		b1.Mod(tmp2, n)

		acsLog(1, "newpow", a1, b1)

	}
	acsLog(1, "finalrepeat", a, b)
	return a, b

}

func nb() *big.Int {
	return &big.Int{}
}

// 97165407724701 too high
func p2(input string, nS, timesS, finalPos string) {
	n, ok := nb().SetString(nS, 10)
	assert(ok, "parse")
	a1, b1 := trans(input, n)
	times, ok := nb().SetString(timesS, 10)
	assert(ok, "parse")
	a, b := repeat(a1, b1, n, times)

	y, ok := nb().SetString(finalPos, 10)
	assert(ok, "parse")
	x := divBig(nb().Sub(y, b), a, n)
	acsLog(0, "Final answer: ", x)
	// tmp
}

// y/a (mod n)
// y = x*a (mod n)
func divBig(y, a, n *big.Int) (x *big.Int) {
	acsLog(2, "divBig", y, a, n)
	if a.Cmp(big.NewInt(1)) == 0 {
		x = nb()
		x.Set(y)
		return
	}
	x = nb()
	x.Div(y, a)
	y = nb().Mod(y, a)
	nma := nb()
	nma.Mod(n, a)
	nda := nb()
	nda.Div(n, a)
	sub := divBig(y, nma, a)
	x = nb().Add(x, nb().Sub(n, nb().Add(nb().Mul(sub, nda), nb().Div(nb().Mul(sub, nma), a))))
	x = nb().Mod(x, n)
	return
}

// y/a (mod n)
// y = x*a (mod n)
func div(y, a, n int) (x int) {
	acsLog(2, "div", y, a, n)
	if a == 1 {
		return y
	}
	x = y / a
	y = y % a
	acsLog(3, y)
	nma := n % a
	nda := n / a
	acsLog(3, nma)
	sub := div(y, nma, a)
	acsLog(3, "sub: ", sub)
	x += n - (sub*nda + (sub*nma)/a)
	x %= n
	return
}

func main() {
	acsLog(0, "all", p1(short1, 10))
	// p1(short2, 10)
	// p1(short3, 10)
	// p1(input, 10007) //8502

	endPos := strconv.Itoa(p1b(short2, 10, 3, 3))
	p2(short2, "10", "3", endPos)

	// p2(input, "10007", "2", "3285")
	p2(input, "119315717514047", "101741582076661", "2020")

	// test1()

	// a := big.NewInt(-9)
	// b := big.NewInt(4)
	// tmp := big.Int{}
	// acsLog(0, "bigtest: ", tmp.Mod(a, b))
	// acsLog(0, "bigtest: ", tmp.Div(a, b))

	// acsLog(0, 97/29, 97%29)
	// acsLog(0, (69*29)%97)

	// acsLog(0, 97/31, 97%31)
	// acsLog(0, 31%4)
	// acsLog(0, 54%31, 54/31)
	// acsLog(0, ((7+1)*31)%97)
	// acsLog(0, (72*31)%97)
	// acsLog(0, (5*10)%23)
	// acsLog(0, b%a, a%(b%a))
	// acsLog(0, "y=", (63*a)%b)
	// acsLog(0, div(27, 23, 79))
	// acsLog(0, "0:", div(0, a, b))

	// a := 23
	// b := 79
	// x := 0
	// // assert(x == div((x*a)%b, a, b), "Problem:"+strconv.Itoa(x))
	// acsLog(0, "0small:", div((x*a)%b, a, b))
	// acsLog(0, "0big:", divBig(big.NewInt(int64((x*a)%b)), big.NewInt(int64(a)), big.NewInt(int64(b))))

	// for x := 0; x < b; x++ {
	// 	acsLog(0, "Testing:", x)
	// 	assert(x == div((x*a)%b, a, b), "Problem:"+strconv.Itoa(x))
	// 	assert(int64(x) == divBig(big.NewInt(int64((x*a)%b)), big.NewInt(int64(a)), big.NewInt(int64(b))).Int64(), "ProblemBig:"+strconv.Itoa(x))

	// }
	// acsLog(0, "All ok")

	// acsLog(0, (69*29)%97)
	// acsLog(0, 61/29)
	// acsLog(0, (4*29)%97)
	// acsLog(0, 31/7, 31%7)
	// acsLog(0, (27*7)%31)
}

const short1 = `
deal with increment 7
deal into new stack
deal into new stack
` // Result: 0 3 6 9 2 5 8 1 4 7

const short2 = `
cut 6
deal with increment 7
deal into new stack
` // Result: 3 0 7 4 1 8 5 2 9 6

const short3 = `
deal with increment 7
deal with increment 9
cut -2
` // Result: 6 3 0 7 4 1 8 5 2 9

const input = `deal with increment 64
deal into new stack
cut 1004
deal with increment 31
cut 5258
deal into new stack
deal with increment 5
cut -517
deal with increment 67
deal into new stack
cut -4095
deal with increment 27
cut 4167
deal with increment 30
cut -5968
deal into new stack
deal with increment 40
deal into new stack
deal with increment 57
cut -5128
deal with increment 75
deal into new stack
deal with increment 75
cut -1399
deal with increment 12
cut -2107
deal with increment 9
cut -7110
deal into new stack
deal with increment 14
cut 3318
deal into new stack
deal with increment 57
cut -8250
deal with increment 5
deal into new stack
cut 903
deal with increment 28
deal into new stack
cut 2546
deal with increment 68
cut 9343
deal with increment 67
cut -6004
deal with increment 24
deal into new stack
cut -816
deal with increment 66
deal into new stack
deal with increment 13
cut 5894
deal with increment 43
deal into new stack
cut 4550
deal with increment 67
cut -3053
deal with increment 42
deal into new stack
deal with increment 32
cut -5985
deal with increment 18
cut -2808
deal with increment 44
cut -1586
deal with increment 16
cut 2173
deal with increment 53
cut 5338
deal with increment 48
cut -2640
deal with increment 36
deal into new stack
deal with increment 13
cut -5520
deal with increment 61
cut -3199
deal into new stack
cut 4535
deal with increment 17
cut -4277
deal with increment 72
cut -7377
deal into new stack
deal with increment 37
cut 6665
deal into new stack
cut 908
deal into new stack
cut 9957
deal with increment 31
cut 9108
deal with increment 44
cut -7565
deal with increment 33
cut -7563
deal with increment 23
cut -3424
deal with increment 63
cut -3513
deal with increment 74`
