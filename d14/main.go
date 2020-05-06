package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func acsLog(level int, p ...interface{}) {
	if level <= 1 {
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

type Ingredient struct {
	name  string
	value int
}

type Recepie struct {
	to   Ingredient
	from []Ingredient
}

func parseIngredient(s string) Ingredient {
	ss := strings.Fields(s)
	assert(len(ss) == 2, "not two fields")

	i, err := strconv.Atoi(ss[0])
	assertErr(err)
	return Ingredient{ss[1], i}
	// acsLog(3, )
}

type Recepies map[string]Recepie

func read(filename string) Recepies {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(file)

	recepies := Recepies{}
	for scanner.Scan() {
		line := scanner.Text()
		acsLog(2, line)
		eq := strings.Split(line, "=>")
		assert(len(eq) == 2, "Not two fields.")
		to := parseIngredient(eq[1])
		fromstrs := strings.Split(eq[0], ",")
		from := []Ingredient{}
		for _, f := range fromstrs {
			from = append(from, parseIngredient(f))
		}
		_, exists := recepies[to.name]
		assert(!exists, "already exists: "+to.name)
		recepies[to.name] = Recepie{to, from}
	}
	return recepies
}

func p1() {
	recepies := read("in4.txt")
	acsLog(1, recepies)
	goal := map[string]int{"FUEL": 1, "ORE": 0}
	for 1 < len(goal) {
		acsLog(1, "Goal:", goal)
		neededForSomething := map[string]bool{}
		for _, v := range recepies {
			for _, n := range v.from {
				neededForSomething[n.name] = true
			}
		}
		for to, neededAmount := range goal {
			if !neededForSomething[to] && to != "ORE" {
				rec := recepies[to]
				delete(recepies, to)
				times := (neededAmount + rec.to.value - 1) / rec.to.value
				acsLog(1, "Choosing:", to, "amount:", neededAmount, "recepie:", rec, "times:", times)
				delete(goal, to)
				for _, i := range rec.from {
					goal[i.name] += i.value * times
					acsLog(1, "added:", i, "goal:", goal)
				}
				break
			}
		}
	}
	acsLog(0, "Needed", goal)
}

func solve(recepies Recepies, order []string, famount int) int {
	goal := map[string]int{"FUEL": famount}
	for i := len(order) - 1; 0 < i; i-- {
		acsLog(2, "Goal:", goal)
		to := order[i]
		neededAmount := goal[to]
		if neededAmount == 0 {
			continue
		}
		rec := recepies[to]
		times := (neededAmount + rec.to.value - 1) / rec.to.value
		acsLog(2, "Choosing:", to, "amount:", neededAmount, "recepie:", rec, "times:", times)
		delete(goal, to)
		for _, i := range rec.from {
			goal[i.name] += i.value * times
			acsLog(2, "added:", i, "goal:", goal)
		}
	}
	assert(len(goal) == 1, "goal not 1")
	return goal["ORE"]
}

func p2() {
	recepies := read("infinal.txt")
	acsLog(1, recepies)
	added := map[string]bool{}
	var recAdd func(s string, v []string) []string
	recAdd = func(s string, v []string) []string {
		if !added[s] {
			for _, f := range recepies[s].from {
				v = recAdd(f.name, v)
			}
			v = append(v, s)
			added[s] = true
		}
		return v
	}
	order := recAdd("FUEL", []string{})
	acsLog(1, "Order:", order)
	assert(order[0] == "ORE", "first not ore")
	_ = solve(recepies, order, 2)

	_ = solve(recepies, order, 2)
	a := 1
	b := a
	ore := 1
	N := 1000000000000
	for ore < N {
		ore = solve(recepies, order, b)
		acsLog(0, "abore: ", a, b, ore)
		if ore < N {
			a = b
			b *= 2
		}
	}
	for a < b {
		c := (a + b) / 2
		ore = solve(recepies, order, c)
		if ore <= N {
			a = c + 1
		}
		if N <= ore {
			b = c - 1
		}
	}

	acsLog(0, "a", a, "b", b)
	acsLog(0, "Final: ", a)
}

// P1 1585836: That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. (You guessed 1585836.) [Return to Day 14]

// 2267485 That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. (You guessed 2267485.) [Return to Day 14]

func main() {
	// p1()
	p2()
}
