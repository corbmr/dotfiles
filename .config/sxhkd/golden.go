package main

import (
	"encoding/json"
	"fmt"
	"os/exec"
)

const (
	ratio     = 0.61803
	ratio_inv = 1 - ratio
)

type desktop struct {
	FocusedNodeId uint64
	Root          *node
}

type node struct {
	Id          uint64
	SplitRatio  float64
	FirstChild  *node
	SecondChild *node
}

func main() {
	desktop, err := currentDesktop()
	if err != nil {
		panic(err)
	}

	walk(desktop.FocusedNodeId, desktop.Root)
}

func currentDesktop() (*desktop, error) {
	out, err := sh(`bspc query -T -d focused`).Output()
	if err != nil {
		return nil, err
	}

	var d desktop
	err = json.Unmarshal(out, &d)
	if err != nil {
		return nil, err
	}

	return &d, nil
}

func walk(focusedId uint64, node *node) bool {
	if node.Id == focusedId {
		return true
	}

	if node.FirstChild != nil && walk(focusedId, node.FirstChild) {
		changeRatio(node, ratio)
		return true
	}

	if node.SecondChild != nil && walk(focusedId, node.SecondChild) {
		changeRatio(node, ratio_inv)
		return true
	}

	return false
}

func changeRatio(node *node, rat float64) {
	if node.SplitRatio != rat {
		if err := sh(`bspc node %d -r %f`, node.Id, rat).Run(); err != nil {
			panic(err)
		}
	}
}

func sh(cmd string, fmtArgs ...interface{}) *exec.Cmd {
	return exec.Command("sh", "-c", fmt.Sprintf(cmd, fmtArgs...))
}
