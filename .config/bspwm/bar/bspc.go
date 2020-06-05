package main

import (
	"encoding/json"
	"os/exec"
)

func main() {

}

func getMonitor() (*monitor, error) {
	var monitor monitor
	err := runCmd(&monitor, "bspc", "query", "-T", "-m")
	if err != nil {
		return nil, err
	}
	return &monitor, nil
}

func getDump() (*state, error) {
	var state state
	err := runCmd(&state, "bspc", "wm", "-d")
	if err != nil {
		return nil, err
	}
	return &state, nil
}

func runCmd(v interface{}, cmd string, args ...string) error {
	c := exec.Command(cmd, args...)
	out, err := c.StdoutPipe()
	if err != nil {
		return err
	}
	dec := json.NewDecoder(out)
	err = c.Start()
	if err != nil {
		return nil
	}
	err = dec.Decode(v)
	if err != nil {
		return err
	}
	return c.Wait()
}

type splitType string

const (
	vertical   splitType = "vertical"
	horizontal           = "horizontal"
)

type node struct {
	ID uint64

	SplitType  splitType
	SplitRatio float64

	Vacant  bool
	Hidden  bool
	Sticky  bool
	Private bool
	Locked  bool
	Marked  bool

	Presel interface{}

	Rectangle   rectangle
	Constraints interface{}

	FirstChild  *node
	SecondChild *node

	Client *client
}

type rectangle struct {
	X      int32
	Y      int32
	Width  int32
	Height int32
}

type client struct {
	ClassName         string
	InstanceName      string
	BorderWidth       int32
	State             string
	LastState         string
	LastLayer         string
	Urgent            bool
	Shown             bool
	TiledRectangle    rectangle
	FloatingRectangle rectangle
}

type padding struct{ Top, Right, Bottom, Left int32 }

type desktop struct {
	Name string
	ID   uint64

	Layout        string
	UserLayout    string
	WindowGap     int32
	BorderWidth   int32
	FocusedNodeID uint64
	Padding       padding
	Root          *node
}

type monitor struct {
	Name             string
	ID               uint64
	RandrID          uint64
	Wired            bool
	StickyCount      int32
	WindowGap        int32
	BorderWidth      int32
	FocusedDesktopID uint64
	Padding          padding
	Rectangle        rectangle
	Desktops         []desktop
}

type state struct {
	FocusedMonitorID uint64
	ClientsCount     int32
	Monitors         []monitor
	FocusHistory     []struct{ MonitorID, DesktopID, NodeID uint64 }
	StackingList     []uint64
}
