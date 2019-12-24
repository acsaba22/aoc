package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

func acsLog(level int, p ...interface{}) {
	if level < 3 {
		fmt.Println(p...)
	}
}

func p1() {
	f, err := ioutil.ReadFile("/home/acsaba/data/work/advent/d08/in.txt")
	if err != nil {
		log.Fatal("couldn't open file", err)
	}
	msg := string(f)
	msg = strings.TrimSpace(msg)
	acsLog(3, "|"+msg+"|")
	// data := make([]int, len(msg))
	// for i, v := range msg {
	// 	data[i] = int(v - '0')
	// }
	w, h := 25, 6
	pageSize := w * h
	n := len(msg) / pageSize
	if len(msg)%pageSize != 0 {
		log.Fatal("Bad size")
	}
	acsLog(2, "str size", len(msg), n)
	min := pageSize
	loc := -1
	for i := 0; i < n; i++ {
		current := strings.Count(msg[i*pageSize:(i+1)*pageSize], "0")
		// fmt.Println(i, current)
		if current < min {
			loc = i
			min = current
		}
	}
	acsLog(2, "minimal location", loc)
	ones := strings.Count(msg[loc*pageSize:(loc+1)*pageSize], "1")
	twos := strings.Count(msg[loc*pageSize:(loc+1)*pageSize], "2")
	acsLog(1, ones*twos)
	img := []rune(msg[:pageSize])
	for i := 1; i < n; i++ {
		for j := 9; j < pageSize; j++ {
			if img[j] == '2' {
				img[j] = rune(msg[i*pageSize+j])
			}
		}
	}
	acsLog(1, string(img))
	for i := 0; i < h; i++ {
		acsLog(1, string(img[i*w:(i+1)*w]))
	}
	// layers := make([][]int, n)
	// for i := 0; i < n; i++ {
	// 	layers[i] = data[i*n : (i+1)*n]
	// 	fmt.Println("------------------------------------------")
	// 	count := 0
	// 	for j := 0; j < h; j++ {
	// 		fmt.Println(layers[i][j*h : (j+1)*h])
	// 		for k := 0; k < w; k++ {
	// 			// if layers[i][j*h+`k]
	// 		}
	// }
}

func main() {
	p1()
}
