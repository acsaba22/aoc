package main

import (
	"io/ioutil"
	"math/big"
	"strconv"
	"strings"

	"github.com/acsaba22/advent/clean/logutil"
)

func main() {
	shuffleSteps := readShuffle()

	// 8502
	logutil.Log(0, "Problem A: ", shuffleFollow(shuffleSteps, 10007, 2019, 1))

	// 41685581334351
	logutil.Log(0, "Problem B: ", tracebackBig(shuffleSteps, "119315717514047", "2020", "101741582076661"))
}

const (
	Cut = iota
	Reverse
	Interlace
)

//////////////////////////////////////////////////////
// Shuffle by simulating.

// finds where card ends up
func shuffleFollow(steps []string, n int, card int, times int) int {
	deck := newDeck(n)
	for i := 0; i < times; i++ {
		deck = shuffleDeck(steps, deck)
	}
	return findInDeck(deck, card)
}

func splitShuffleString(steps string, separator rune) []string {
	return strings.FieldsFunc(steps, func(r rune) bool { return r == separator })
}

func readShuffle() []string {
	fcontent, err := ioutil.ReadFile("data/shuffle.txt")
	logutil.AssertErr(err)
	return splitShuffleString(string(fcontent), '\n')
}

func newDeck(n int) []int {
	deck := make([]int, n)
	for i := range deck {
		deck[i] = i
	}
	return deck
}

func parse(step string) (action, k int) {
	trimmed := strings.TrimSpace(step)

	reverseS := "deal into new stack"
	if strings.HasPrefix(trimmed, reverseS) {
		return Reverse, 0
	}

	incS := "deal with increment "
	if strings.HasPrefix(trimmed, incS) {
		k, err := strconv.Atoi(trimmed[len(incS):])
		logutil.AssertErr(err)
		return Interlace, k
	}

	cutS := "cut "
	if strings.HasPrefix(trimmed, cutS) {
		k, err := strconv.Atoi(trimmed[len(cutS):])
		logutil.AssertErr(err)
		return Cut, k
	}

	logutil.Assert(false, "Bad action: "+step)
	return -1, 0
}

func shuffle(steps []string, n int) []int {
	return shuffleDeck(steps, newDeck(n))
}

func shuffleDeck(steps []string, deck []int) []int {
	n := len(deck)
	next := make([]int, n)
	for _, step := range steps {
		action, k := parse(step)
		if action == Reverse {
			for i, v := range deck {
				next[n-i-1] = v
			}
		} else if action == Cut {
			if k < 0 {
				k += n
			}
			copy(next[n-k:], deck)
			copy(next, deck[k:])
		} else if action == Interlace {
			for i, v := range deck {
				next[(i*k)%n] = v
			}
		}
		deck, next = next, deck
	}
	return deck
}

func findInDeck(deck []int, card int) int {
	for i, v := range deck {
		if v == card {
			return i
		}
	}
	return -1
}

//////////////////////////////////////////////////////
// Shuffle with linear transformation.

func traceback(steps []string, n int, finalPos int, times int) int {
	a, b := transform(steps, n)
	a, b = repeat(a, b, n, times)
	return div(mod(finalPos-b, n), a, n)
}

func mod(x, n int) int {
	if 0 <= x {
		return x % n
	} else {
		return (x % n) + n
	}
}

func transform(steps []string, n int) (a, b int) {
	a, b = 1, 0
	for _, step := range steps {
		action, k := parse(step)
		aa, bb := 0, 0
		if action == Reverse {
			aa = -1
			bb = -1
		} else if action == Cut {
			aa = 1
			bb = -k
		} else if action == Interlace {
			aa = k
			bb = 0
		}
		a, b = aa*a, aa*b+bb
		a = mod(a, n)
		b = mod(b, n)
	}
	return
}

func repeat(a1, b1, n, times int) (a, b int) {
	a = 1
	b = 0

	for i := 0; times != 0; i++ {
		bit := 1 << i
		if times&bit != 0 {
			times &= ^bit
			a *= a1
			a = mod(a, n)

			b = mod(a1*b+b1, n)

		}
		b1 = mod(a1*b1+b1, n)
		a1 = mod(a1*a1, n)

	}
	return a, b
}

func transformFollow(steps []string, n int, card int, times int) int {
	a, b := transform(steps, n)
	a, b = repeat(a, b, n, times)
	return mod(a*card+b, n)
}

// y/a (mod n)
// y = x*a (mod n)
func div(y, a, n int) (x int) {
	if a == 1 {
		return y
	}

	x = y / a
	y = y % a

	nma := n % a
	nda := n / a

	sub := div(y, nma, a)

	x += n - (sub*nda + (sub*nma)/a)
	x %= n
	return x
}

//////////////////////////////////////////////////////
// Shuffle with big numbers

func tracebackBig(steps []string, nS, finalPosS, timesS string) *big.Int {
	n := toBigS(nS)
	finalPos := toBigS(finalPosS)
	times := toBigS(timesS)

	a, b := transformBig(steps, n)
	a, b = repeatBig(a, b, n, times)

	tmp := zero()
	tmp.Sub(finalPos, b)

	tmp2 := zero()
	tmp2.Mod(tmp, n)
	return divBig(tmp2, a, n)
}

func toBig(x int) *big.Int {
	return big.NewInt(int64(x))
}

func toBigS(s string) *big.Int {
	x, ok := zero().SetString(s, 10)
	logutil.Assert(ok, "parse error")
	return x
}

func transformBig(steps []string, n *big.Int) (a, b *big.Int) {
	a, b = one(), zero()
	for _, step := range steps {
		action, k := parse(step)
		var aa, bb *big.Int
		// aa, bb := zero(), zero()
		if action == Reverse {
			aa = toBig(-1)
			bb = toBig(-1)
		} else if action == Cut {
			aa = toBig(1)
			bb = toBig(-k)
		} else if action == Interlace {
			aa = toBig(k)
			bb = toBig(0)
		}
		tmp := zero()
		tmp2 := zero()

		tmp.Mul(aa, b)
		tmp2.Add(tmp, bb)
		b.Mod(tmp2, n)

		tmp.Mul(aa, a)
		a.Mod(tmp, n)
	}
	return
}

func one() *big.Int {
	return big.NewInt(1)
}

func zero() *big.Int {
	return big.NewInt(0)
}

func repeatBig(a1, b1, n *big.Int, times *big.Int) (a, b *big.Int) {
	a = one()
	b = zero()

	null := big.NewInt(0)
	tmp := zero()
	tmp2 := zero()
	tmpa := zero()
	for i := 0; times.Cmp(null) != 0; i++ {
		if times.Bit(i) != 0 {
			tmp.SetBit(times, i, 0)
			times, tmp = tmp, times
			tmp.Mul(a, a1)
			a.Mod(tmp, n)

			tmp2.Add(tmp.Mul(a1, b), b1)
			b.Mod(tmp2, n)

		}
		tmp.Mul(a1, a1)
		tmpa.Mod(tmp, n)
		tmp2.Add(tmp.Mul(a1, b1), b1)
		a1, tmpa = tmpa, a1
		b1.Mod(tmp2, n)
	}
	return a, b

}

// y/a (mod n)
// y = x*a (mod n)
func divBig(y, a, n *big.Int) (x *big.Int) {
	if a.Cmp(big.NewInt(1)) == 0 {
		x = zero()
		x.Set(y)
		return
	}
	x = zero()
	x.Div(y, a)
	y = zero().Mod(y, a)
	nma := zero()
	nma.Mod(n, a)
	nda := zero()
	nda.Div(n, a)
	sub := divBig(y, nma, a)
	x = zero().Add(x, zero().Sub(n, zero().Add(zero().Mul(sub, nda), zero().Div(zero().Mul(sub, nma), a))))
	x = zero().Mod(x, n)
	return
}
