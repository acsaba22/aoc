// a) Reads commands from stdin.
// b) logs commands under data/logs
// c) if data/default-commands.txt is available reads and executes those first

package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/acsaba22/advent/clean/logutil"
)

type commandReader struct {
	defaultCommands []string
	reader          *bufio.Reader
	logFile         *os.File
}

func readDefaultCommands() []string {
	logutil.Log(-1, "reading commands")
	buf, err := ioutil.ReadFile("data/default-commands.txt")
	if err != nil {
		return []string{}
	}
	fcontent := strings.TrimSpace(string(buf))
	if fcontent == "" {
		return []string{}
	}
	logutil.Log(-1, "commands: ", strings.Split(fcontent, "\n"))
	return strings.Split(fcontent, "\n")
}

func newCommandReader() commandReader {
	ret := commandReader{}
	ret.defaultCommands = readDefaultCommands()
	ret.reader = bufio.NewReader(os.Stdin)

	now := time.Now()
	filename := fmt.Sprintf("data/logs/log-%02v-%02v-%02v.txt", now.YearDay(), now.Hour(), now.Minute())
	var err error
	ret.logFile, err = os.Create(filename)
	logutil.AssertErr(err)

	return ret
}

// Includes the '\n'
func (sr *commandReader) nextStep() string {
	var nextStep string
	if 0 < len(sr.defaultCommands) {
		nextStep = sr.defaultCommands[0] + "\n"
		sr.defaultCommands = sr.defaultCommands[1:]
		fmt.Print(nextStep)
	} else {
		var err error
		nextStep, err = sr.reader.ReadString('\n')
		logutil.AssertErr(err)

	}
	sr.logFile.WriteString(nextStep)
	return nextStep
}

func (sr *commandReader) Close() {
	sr.logFile.Close()
}
