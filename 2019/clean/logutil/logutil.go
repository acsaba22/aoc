package logutil

import (
	"fmt"
	"log"
)

var loglevel = 1

func SetLogLevel(level int) {
	loglevel = level
}

func Log(level int, p ...interface{}) {
	if level <= loglevel {
		fmt.Println(p...)
	}
}

func Assert(b bool, s string) {
	if !b {
		log.Fatal(s)
	}
}

func AssertErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
