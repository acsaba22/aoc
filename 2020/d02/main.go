package main

import (
	"fmt"
	"log"
	"strconv"
	"strings"
)

func toCode(input string) []int {
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

func run(code []int) []int {
	cp := 0
	for {
		switch code[cp] {
		case 1:
			v1p := code[cp+1]
			v2p := code[cp+2]
			resp := code[cp+3]
			code[resp] = code[v1p] + code[v2p]
			cp += 4
		case 2:
			v1p := code[cp+1]
			v2p := code[cp+2]
			resp := code[cp+3]
			code[resp] = code[v1p] * code[v2p]
			cp += 4
		case 99:
			return code
		default:
			log.Fatal("Unkown code: ", strconv.Itoa(code[cp]))
		}
	}
}

func p1(input string) {
	fmt.Println("input: ", input)
	code := toCode(input)

	run(code)
	fmt.Println("result:", code)
}

func p2() {
	origCode := toCode("1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0")
	code := make([]int, len(origCode))
	N := 100
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			copy(code, origCode)
			code[1] = i
			code[2] = j
			run(code)
			fmt.Println(i, j, code[0])
			if code[0] == 19690720 {
				log.Fatal("FOUND!")
			}
		}
	}

}

func main() {
	p1("1,9,10,3,2,3,11,0,99,30,40,50")
	p1("1,0,0,0,99")          // becomes 2,0,0,0,99 (1 + 1 = 2).
	p1("2,3,0,3,99")          // 2,3,0,6,99 (3 * 2 = 6).
	p1("2,4,4,5,99,0")        // 2,4,4,5,99,9801 (99 * 99 = 9801).
	p1("1,1,1,4,99,5,6,0,99") // 30,1,1,4,2,5,6,0,99.
	p1("1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0")
	p2()
}
