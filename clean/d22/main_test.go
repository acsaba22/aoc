package main

import (
	"reflect"
	"testing"
)

func TestSimpleExamples(t *testing.T) {
	n := 10
	cases := []struct {
		steps       string
		expectation []int
	}{
		{"deal into new stack", []int{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}},
		{"cut 3", []int{3, 4, 5, 6, 7, 8, 9, 0, 1, 2}},
		{"deal with increment 3", []int{0, 7, 4, 1, 8, 5, 2, 9, 6, 3}},
		{"cut 6|deal with increment 7|deal into new stack", []int{3, 0, 7, 4, 1, 8, 5, 2, 9, 6}},
	}
	for _, c := range cases {
		steps := splitShuffleString(c.steps, '|')
		actual := shuffle(steps, n)
		if !reflect.DeepEqual(actual, c.expectation) {
			t.Errorf("Bad simple shuffle. Actual %v (expected %v)", actual, c.expectation)
		}

		// test with transformation
		testpos := 3
		if loc := transformFollow(steps, 10, c.expectation[testpos], 1); loc != testpos {
			t.Errorf("transform error: %v when following %v. Expected: 3 acutal: %v ", steps, c.expectation[testpos], loc)
		}
	}
}
func TestMod(t *testing.T) {
	if !(mod(13, 10) == 3 && mod(-12, 10) == 8) {
		t.Error("Mod problem")
	}
}

func TestProblem1(t *testing.T) {
	shuffleSteps := readShuffle()
	expected := 8502
	actual := shuffleFollow(shuffleSteps, 10007, 2019, 1)
	if expected != actual {
		t.Errorf("Bad solution to first problem. Actual: %v (Expected %v)", actual, expected)
	}
	// test with transform
	actual = transformFollow(shuffleSteps, 10007, 2019, 1)
	if expected != actual {
		t.Errorf("Bad solution with transformation. Actual: %v (Expected %v)", actual, expected)
	}
}

func TestRepeated(t *testing.T) {
	shuffleSteps := readShuffle()
	times := 100
	expected := shuffleFollow(shuffleSteps, 10007, 2019, times)
	// test with transform
	actual := transformFollow(shuffleSteps, 10007, 2019, times)
	if expected != actual {
		t.Errorf("Bad solution with transformation. Actual: %v (Expected %v)", actual, expected)
	}
}

func TestDiv(t *testing.T) {
	if v := div((63*23)%79, 23, 79); v != 63 {
		t.Errorf("Div error %v (expeced 63)", v)
	}
	if v := div((12*20)%83, 20, 83); v != 12 {
		t.Errorf("Div error %v (expeced 12)", v)
	}
}

func TestTraceback(t *testing.T) {
	shuffleSteps := readShuffle()
	times := 100
	from := 2019
	n := 10007
	to := shuffleFollow(shuffleSteps, n, from, times)
	actualFrom := traceback(shuffleSteps, n, to, times)

	if actualFrom != from {
		t.Errorf("Bad trace back. Actual: %v (Expected %v)", actualFrom, from)
	}
}

func TestDivBig(t *testing.T) {
	v := divBig(toBig((63*23)%79), toBig(23), toBig(79))
	if v.Cmp(toBig(63)) != 0 {
		t.Errorf("Div error %v (expeced 63)", v)
	}
}

func TestTracebackBig(t *testing.T) {
	shuffleSteps := readShuffle()
	expectedFrom := traceback(shuffleSteps, 10007, 1234, 100)
	actualFrom := tracebackBig(shuffleSteps, "10007", "1234", "100")

	if actualFrom.Cmp(toBig(expectedFrom)) != 0 {
		t.Errorf("Bad trace back. Actual: %v (Expected %v)", actualFrom, expectedFrom)
	}
}
