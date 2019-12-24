package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"strconv"
)

func fuelNeeded(n int) int {
	fuel := n
	sum := 0
	for 0 < fuel {
		fuel = fuel/3 - 2
		if 0 < fuel {
			sum += fuel
		}
	}
	return sum
}

func readInput(s string) {
	file, err := os.Open(s)
	defer file.Close()
	//file, err := ioutil.ReadFile(s)
	if err != nil {
		fmt.Println(err)
		panic("File not found")
	}
	reader := bufio.NewScanner(file)
	sum := 0
	for reader.Scan() {
		n, err := strconv.Atoi(reader.Text())
		if err != nil {
			log.Fatal(err)
		}
		needed := fuelNeeded(n)
		sum += needed
		fmt.Printf("N = %v, needed = %v\n", n, needed)
	}
	// fmt.Println("sum=", sum)
	// for fuel := sum; 0 < fuel; {
	// 	fuel = fuel/3 - 2
	// 	fmt.Println("Fuel = ", fuel)
	// 	if 0 < fuel {
	// 		sum += fuel
	// 	}
	// }
	fmt.Println("sum=", sum)

}

func readInput2(s string) {
	file, err := os.Open(s)
	defer file.Close()
	//file, err := ioutil.ReadFile(s)
	if err != nil {
		fmt.Println(err)
		panic("File not found")
	}
	reader := bufio.NewScanner(file)
	sum := 0
	for reader.Scan() {
		n, err := strconv.Atoi(reader.Text())
		if err != nil {
			log.Fatal(err)
		}
		needed := n/3 - 2
		sum += needed
		fmt.Printf("N = %v, needed = %v\n", n, needed)
	}
	fmt.Println("sum=", sum)
	for fuel := sum; 0 < fuel; {
		fuel = fuel/3 - 2
		fmt.Println("Fuel = ", fuel)
		if 0 < fuel {
			sum += fuel
		}
	}
	fmt.Println("sum=", sum)

}

func main() {
	fname := flag.String("file", "main", ".in and .out will be appended")
	flag.Parse()
	fmt.Printf("%T\n", fname)
	fmt.Println("Hello World:", *fname)
	readInput(*fname + ".in")
}
