// automatically generates commands.
// drop items in different combinations and goes east repeatedly
// until no "Alert!" is seen.

package main

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/acsaba22/advent/clean/logutil"
)

const INVENTORY = `antenna
astronaut ice cream
hologram
ornament
asterisk
fixed point
dark matter
monolith`

const SPOTTED = "Alert!"

type autoPlayer struct {
	inventory        []string
	isActive         bool
	currentInventory int
	nextTry          int
	planedSteps      []string
	output           bytes.Buffer
}

func newAutoPlayer() autoPlayer {
	ap := autoPlayer{}
	ap.isActive = false
	ap.inventory = strings.Split(INVENTORY, "\n")
	return ap
}

func (ap *autoPlayer) start() {
	logutil.Log(-1, "Autoplayer: starting")
	ap.isActive = true
	ap.currentInventory = (1 << len(ap.inventory)) - 1
	ap.nextTry = ap.currentInventory // For just a few use ap.nextTry = 3
	ap.output.WriteString(SPOTTED)
}

func (ap *autoPlayer) nextStep() string {
	logutil.Assert(ap.isActive, "Autoplayer not active")

	if 0 < len(ap.planedSteps) {
		return ap.popAndPrintCmd()
	}
	if strings.Contains(ap.output.String(), SPOTTED) {
		ap.output.Reset()
	} else {
		ap.isActive = false
		return ""
	}

	if ap.nextTry < 0 {
		logutil.Log(-1, "Autoplayer: tried all")
		ap.isActive = false
		return ""
	}

	for b, i := 1, 0; i < len(ap.inventory); b, i = b<<1, i+1 {
		has := (ap.currentInventory & b) != 0
		need := (ap.nextTry & b) != 0
		if has != need {
			if need {
				ap.addCmd("take " + ap.inventory[i])
			} else {
				ap.addCmd("drop " + ap.inventory[i])
			}
		}
	}
	ap.currentInventory = ap.nextTry
	ap.nextTry--
	ap.addCmd("inv")
	ap.addCmd("east")
	return ap.popAndPrintCmd()
}

func (ap *autoPlayer) write(r int) {
	ap.output.Write([]byte{byte(r)})
}

func (ap *autoPlayer) popAndPrintCmd() string {
	ret := ap.planedSteps[0]
	ap.planedSteps = ap.planedSteps[1:]
	fmt.Print(ret)
	return ret
}

func (ap *autoPlayer) addCmd(cmd string) {
	ap.planedSteps = append(ap.planedSteps, cmd+"\n")
}
