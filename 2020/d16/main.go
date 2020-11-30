package main

import (
	"bytes"
	"fmt"
	"log"
	"strconv"
)

const input = "59782619540402316074783022180346847593683757122943307667976220344797950034514416918778776585040527955353805734321825495534399127207245390950629733658814914072657145711801385002282630494752854444244301169223921275844497892361271504096167480707096198155369207586705067956112600088460634830206233130995298022405587358756907593027694240400890003211841796487770173357003673931768403098808243977129249867076581200289745279553289300165042557391962340424462139799923966162395369050372874851854914571896058891964384077773019120993386024960845623120768409036628948085303152029722788889436708810209513982988162590896085150414396795104755977641352501522955134675"

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a < b {
		return b
	}
	return a
}

func assert(b bool, s string) {
	if !b {
		log.Fatal(s)
	}
}

func p1(s string, phases int) {
	n := len(s)
	v0 := make([]int, n)
	v1 := make([]int, n)
	for i, c := range s {
		v0[i] = int(c - '0')
	}
	pattern := []int{0, 1, 0, -1}
	for i := 0; i < phases; i++ {
		fmt.Println(v0)
		for j := 0; j < n; j++ {
			res := 0
			for k := 0; k < n; k++ {
				res += v0[k] * pattern[((k+1)/(j+1))%4]
			}

			v1[j] = abs(res % 10)
		}
		v0, v1 = v1, v0
	}
	fmt.Println(v0)
	for i := 0; i < 8; i++ {
		fmt.Print(v0[i])
	}
	fmt.Println()
}

func p2a(s string, phases int) {
	n := len(s)
	v0 := make([]byte, n)
	v1 := make([]byte, n)
	for i, c := range s {
		v0[i] = byte(c - '0')
	}
	pattern := []int{0, 1, 0, -1}
	for i := 0; i < phases; i++ {
		fmt.Println(v0)
		for j := 0; j < n; j++ {
			res := 0
			for k := 0; k < n; k++ {
				res += int(v0[k]) * pattern[((k+1)/(j+1))%4]
			}

			v1[j] = byte(abs(res % 10))
		}
		v0, v1 = v1, v0
	}
	fmt.Println(v0)
	for i := 0; i < 8; i++ {
		fmt.Print(v0[i])
	}
	fmt.Println()
}

func p2b(s string, phases int) {
	n := len(s)
	v0 := make([]byte, n)
	v1 := make([]byte, n)
	sum := make([]int32, n+1)
	for i, c := range s {
		v0[i] = byte(c - '0')
	}
	pattern := []int32{0, 1, 0, -1}
	for i := 0; i < phases; i++ {
		// fmt.Println(v0)
		fmt.Println(i)
		s := int32(0)
		for j := 0; j < n; j++ {
			s += int32(v0[j])
			sum[j+1] = s
		}
		for j := 0; j < n; j++ {
			res := int32(0)
			for ppos, vpos := 0, 0; vpos < n; ppos++ {
				next := min(n, (ppos+1)*(j+1)-1)
				s := int32(0)
				assert(vpos <= next, "backgo")
				s = sum[next] - sum[vpos]
				res += pattern[ppos%4] * s
				vpos = next
			}
			v1[j] = byte(abs(int(res % 10)))
		}
		v0, v1 = v1, v0
	}
	// fmt.Println(v0)
	loc, err := strconv.Atoi(s[:7])
	assert(err == nil, "error in number")
	// fmt.Println(loc)
	for i := 0; i < 8; i++ {
		fmt.Print(v0[i+loc])
	}
	fmt.Println()
}

func p2c(s string, phases int) {
	n := len(s)
	v0 := make([]byte, n)
	v1 := make([]byte, n)
	sum := make([]int32, n+1)
	for i, c := range s {
		v0[i] = byte(c - '0')
	}
	pattern := []int32{0, 1, 0, -1}
	for i := 0; i < phases; i++ {
		// fmt.Println(v0)
		fmt.Println(i)
		s := int32(0)
		for j := 0; j < n; j++ {
			s += int32(v0[j])
			sum[j+1] = s
		}
		c := make(chan struct{}, 3)
		// c <- struct{}{}
		// c <- struct{}{}
		// c <- struct{}{}
		calc1 := func(j int) {
			// fmt.Println("start", j)
			res := int32(0)
			for ppos, vpos := 0, 0; vpos < n; ppos++ {
				next := min(n, (ppos+1)*(j+1)-1)
				s := int32(0)
				assert(vpos <= next, "backgo")
				s = sum[next] - sum[vpos]
				res += pattern[ppos%4] * s
				vpos = next
			}
			v1[j] = byte(abs(int(res % 10)))
			// fmt.Println("finish", j)
		}
		for j := 0; j < n; j++ {
			c <- struct{}{}
			go calc1(j)
			<-c
		}
		v0, v1 = v1, v0
	}
	loc, err := strconv.Atoi(s[:7])
	assert(err == nil, "error in number")
	// fmt.Println(loc)
	for i := 0; i < 8; i++ {
		fmt.Print(v0[i+loc])
	}
	fmt.Println()
}

func p2d(s string, phases int) {
	n := len(s)
	v0 := make([]byte, n)
	v1 := make([]byte, n)
	sum := make([]int32, n+1)
	for i, c := range s {
		v0[i] = byte(c - '0')
	}
	pattern := []int32{0, 1, 0, -1}
	for i := 0; i < phases; i++ {
		// fmt.Println(v0)
		fmt.Println(i)
		s := int32(0)
		for j := 0; j < n; j++ {
			s += int32(v0[j])
			sum[j+1] = s
		}
		P := 4
		c := make(chan int, 1000)
		d := make(chan struct{})
		calc1 := func() {
			for j := <-c; j != -1; j = <-c {
				res := int32(0)
				for ppos, vpos := 0, 0; vpos < n; ppos++ {
					next := min(n, (ppos+1)*(j+1)-1)
					s := int32(0)
					assert(vpos <= next, "backgo")
					s = sum[next] - sum[vpos]
					res += pattern[ppos%4] * s
					vpos = next
				}
				v1[j] = byte(abs(int(res % 10)))
			}
			d <- struct{}{}
		}
		for p := 0; p < P; p++ {
			go calc1()
		}
		for j := 0; j < n; j++ {
			c <- j
		}
		for p := 0; p < P; p++ {
			c <- -1
		}

		for p := 0; p < P; p++ {
			<-d
		}
		v0, v1 = v1, v0
	}
	loc, err := strconv.Atoi(s[:7])
	assert(err == nil, "error in number")
	// fmt.Println(loc)
	for i := 0; i < 8; i++ {
		fmt.Print(v0[i+loc])
	}
	fmt.Println()
}

func p2(s string, phases int) {
	n := len(s)
	v0 := make([]byte, n)
	v1 := make([]byte, n)
	sum := make([]int32, n+1)
	for i, c := range s {
		v0[i] = byte(c - '0')
	}
	pattern := []int32{0, 1, 0, -1}
	for i := 0; i < phases; i++ {
		// fmt.Println(v0)
		fmt.Println(i)
		s := int32(0)
		for j := 0; j < n; j++ {
			s += int32(v0[j])
			sum[j+1] = s
		}
		P := 7
		type pair struct {
			from, to int
		}
		c := make(chan pair, 1000)
		d := make(chan struct{})
		calc1 := func() {
			for interval := <-c; interval.from != -1; interval = <-c {
				for j := interval.from; j < interval.to; j++ {
					res := int32(0)
					for ppos, vpos := 0, 0; vpos < n; ppos++ {
						next := min(n, (ppos+1)*(j+1)-1)
						s := int32(0)
						assert(vpos <= next, "backgo")
						s = sum[next] - sum[vpos]
						res += pattern[ppos%4] * s
						vpos = next
					}
					v1[j] = byte(abs(int(res % 10)))
				}
			}
			d <- struct{}{}
		}
		for p := 0; p < P; p++ {
			go calc1()
		}
		for j := 0; j < n; {
			jj := (j + 1) + j/P
			if n < jj {
				jj = n
			}
			c <- pair{j, jj}
			j = jj
		}
		for p := 0; p < P; p++ {
			c <- pair{-1, -1}
		}

		for p := 0; p < P; p++ {
			<-d
		}
		v0, v1 = v1, v0
	}
	loc, err := strconv.Atoi(s[:7])
	assert(err == nil, "error in number")
	// fmt.Println(loc)
	for i := 0; i < 8; i++ {
		fmt.Print(v0[i+loc])
	}
	fmt.Println()
}

func multiply(s string, c int) string {
	b := bytes.Buffer{}
	for i := 0; i < c; i++ {
		b.WriteString(s)
	}
	return b.String()
}

func main() {
	// p1("12345678", 4)
	// p2("12345678", 4)
	// p1("80871224585914546619083218645595", 100)
	// p1("19617804207202209144916044189917", 100)
	// p2("80871224585914546619083218645595", 100)
	// p2b("00000002577212", 1)
	// p2c("00000002577212", 1)
	// p2("00000002577212", 1)
	// p2b(multiply("03036732577212944063491565474664", 10000), 1)
	// p2c(multiply("03036732577212944063491565474664", 10000), 1)
	// p2(multiply("03036732577212944063491565474664", 10000), 1)
	// p2(multiply("03036732577212944063491565474664", 10000), 100)
	// p2(multiply("02935109699940807407585447034323", 10000), 100)
	// p2(multiply("03081770884921959731165446850517", 10000), 100)

	// 86416960
	// p2b(multiply(input, 10000), 10) // 11.6s 100%
	// p2c(multiply(input, 10000), 10) // 32.9s 500%
	// p2d(multiply(input, 10000), 10) // 14.2s 400%
	// p2(multiply(input, 10000), 10) // 6.6s 400%

	// p1(input, 100) // 27229269
	p2(multiply(input, 10000), 100) // 49583619
	//P=8 1:05s 700%
}
