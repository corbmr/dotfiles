package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"os/signal"
	"strings"
	"time"
)

var (
	errLog = log.New(os.Stderr, "\u001B[31mERROR\u001B[0m ", log.LstdFlags)
)

func main() {

	actions := make(actions)
	actions["clickDate"] = func(updates chan<- module) {
		updates <- module{"update", "HelloWorld"}
	}

	updates := make(chan module, 10)
	bar := bar{
		Sep:           " ",
		Prefix:        " ",
		ModulesLeft:   []modID{"date"},
		ModulesCenter: []modID{"update"},
	}

	go dateModule(updates)
	go actions.listenActions(os.Stdin, updates)
	go bar.listenUpdates(os.Stdout, updates)

	sig := make(chan os.Signal, 1)
	signal.Notify(sig, os.Interrupt, os.Kill)
	<-sig
}

func dateModule(updates chan<- module) {
	ticker := time.NewTicker(2 * time.Second)
	var buf strings.Builder
	for range ticker.C {
		c := component{
			Text:  time.Now().Format(time.Kitchen),
			Under: true,
			U:     "#059",
			Left:  "clickDate",
		}
		buf.Reset()
		c.render(&buf)

		updates <- module{"date", buf.String()}
	}
}

type modID string

type module struct {
	ID       modID
	Contents string
}

type bar struct {
	Prefix, Suffix, Sep string

	ModulesLeft, ModulesCenter, ModulesRight []modID
}

func (bar *bar) listenUpdates(out io.Writer, updates <-chan module) {
	state := make(map[modID]string)
	var buf strings.Builder
	for mod := range updates {
		state[mod.ID] = mod.Contents

		buf.Reset()
		bar.render(&buf, state)
		fmt.Fprintln(out, buf.String())
	}
}

func (bar *bar) render(out io.Writer, state map[modID]string) {
	renderModules := func(pos string, mods []modID) {
		for i, m := range mods {
			if c, ok := state[m]; ok {
				if i > 0 {
					fmt.Fprint(out, bar.Sep)
				} else {
					fmt.Fprint(out, pos)
				}
				fmt.Fprint(out, c)
			} else {
				errLog.Println("No state for: " + m)
			}
		}
	}

	fmt.Fprint(out, bar.Prefix)
	renderModules("", bar.ModulesLeft)
	renderModules("%{c}", bar.ModulesCenter)
	renderModules("%{r}", bar.ModulesRight)
	fmt.Fprint(out, bar.Suffix)
}

type component struct {
	Text        string
	Under, Over bool
	F, B, U     string

	Left, Right, Middle  string
	ScrollUp, ScrollDown string
}

func (c component) render(out io.Writer) {
	if c.Under {
		fmt.Fprint(out, "%{+u}")
		defer fmt.Fprint(out, "%{-u}")
	}
	if c.Over {
		fmt.Fprint(out, "%{+o}")
		defer fmt.Fprint(out, "%{-o}")
	}
	if len(c.F) > 0 {
		fmt.Fprintf(out, "%%{F%s}", c.F)
		defer fmt.Fprint(out, "%{F-}")
	}
	if len(c.B) > 0 {
		fmt.Fprintf(out, "%%{B%s}", c.B)
		defer fmt.Fprint(out, "%{B-}")
	}
	if len(c.U) > 0 {
		fmt.Fprintf(out, "%%{U%s}", c.U)
		defer fmt.Fprint(out, "%{U-}")
	}
	if len(c.Left) > 0 {
		fmt.Fprintf(out, "%%{A1:%s:}", c.Left)
		defer fmt.Fprint(out, "%{A}")
	}
	if len(c.Middle) > 0 {
		fmt.Fprintf(out, "%%{A2:%s:}", c.Middle)
		defer fmt.Fprint(out, "%{A}")
	}
	if len(c.Right) > 0 {
		fmt.Fprintf(out, "%%{A3:%s:}", c.Right)
		defer fmt.Fprint(out, "%{A}")
	}
	if len(c.ScrollUp) > 0 {
		fmt.Fprintf(out, "%%{A4:%s:}", c.ScrollUp)
		defer fmt.Fprint(out, "%{A}")
	}
	if len(c.ScrollDown) > 0 {
		fmt.Fprintf(out, "%%{A5:%s:}", c.ScrollDown)
		defer fmt.Fprint(out, "%{A}")
	}
	fmt.Fprint(out, c.Text)
}

type actions map[string]func(chan<- module)

func (a actions) listenActions(inp io.Reader, updates chan<- module) {
	scanner := bufio.NewScanner(inp)
	for scanner.Scan() {
		action := scanner.Text()
		if f, ok := a[action]; ok {
			f(updates)
		} else {
			errLog.Printf("Action not found: %s", action)
		}
	}
}
