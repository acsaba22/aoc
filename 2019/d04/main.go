package main

import "fmt"

func isOk(n int) bool {
	last := 10
	hasDupe := false
	for n != 0 {
		current := n % 10
		if last < current {
			return false
		}
		if last == current {
			hasDupe = true
		}
		last = current
		n /= 10
	}
	return hasDupe
}

func p1(f func(int) bool) {
	counter := 0
	for i := 193651; i <= 649729; i++ {
		if f(i) {
			counter++
		}
	}
	fmt.Println(counter)
}

func isOk2(n int) bool {
	if !isOk(n) {
		return false
	}
	last := 10
	streak := 0
	for n != 0 {
		current := n % 10
		if last == current {
			streak++
		} else {
			if streak == 1 {
				return true
			}
			streak = 0
		}
		last = current
		n /= 10
	}
	return streak == 1
}

func main() {
	fmt.Println(isOk(111111))
	fmt.Println(isOk(223450))
	fmt.Println(isOk(123789))
	p1(isOk)
	fmt.Println(isOk2(112233))
	fmt.Println(isOk2(123444))
	fmt.Println(isOk2(111122))
	p1(isOk2)
}
