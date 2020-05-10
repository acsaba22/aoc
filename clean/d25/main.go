package main

import (
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/acsaba22/advent/clean/computer"
	"github.com/acsaba22/advent/clean/logutil"
)

func init() {
	logutil.SetLogLevel(1)
}

func main() {
	console() // result: 134227456
}

func console() {
	c := computer.ComputerFromString(readPrg(), "")
	go c.Run()

	commReader := newCommandReader()
	defer commReader.Close()

	aPlayer := newAutoPlayer()
	time.Sleep(time.Millisecond)
	for {
		select {
		case r := <-c.Output:
			os.Stdout.Write([]byte{byte(r)})
			aPlayer.write(r)
		default:
			if c.Reading || c.Finished {
				var nextStep string
				if aPlayer.isActive {
					nextStep = aPlayer.nextStep()
					// To wait for enter:
					// bufio.NewReader(os.Stdin).ReadString('\n')
				} else {
					nextStep = commReader.nextStep()
				}
				trimmed := strings.TrimSpace(nextStep)
				switch trimmed {
				case "":
				case "quit":
					return
				case "autoplay":
					aPlayer.start()
				default:
					for _, r := range nextStep {
						c.Input <- int(r)
					}
					time.Sleep(time.Millisecond)
				}
			}
		}
	}
}

func readPrg() string {
	buf, err := ioutil.ReadFile("data/prg.txt")
	logutil.AssertErr(err)
	return string(buf)
}
