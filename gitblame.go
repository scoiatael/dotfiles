package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
	"time"

	"github.com/fatih/color"
	"github.com/pkg/errors"
)

var (
	SEPARATOR = []byte("<<SEP")
)

type Change struct {
	Name    string
	File    string
	Details []string
}

type CommitDetails struct {
	Author string
	Commit string
	Date   time.Time
}

type Commit struct {
	Changes []Change
	CommitDetails
}

var (
	ErrBadGitCommitStatus = fmt.Errorf("Bad git commit status")
)

func Parse(out []byte) ([]Commit, error) {
	commits := []Commit{}
	lines := bytes.Split(out, []byte("\n"))
	for _, l := range lines {
		if bytes.HasPrefix(l, SEPARATOR) {
			var commit Commit
			raw := bytes.TrimPrefix(l, SEPARATOR)
			err := json.Unmarshal(raw, &commit)
			if err != nil {
				return commits, errors.Wrap(err, fmt.Sprintf("while parsing %s", raw))
			}
			commits = append(commits, commit)
		} else {
			changeStr := string(l)
			if len(changeStr) > 0 {
				changeDesc := strings.Fields(changeStr)
				if len(changeDesc) < 2 {
					return commits, errors.Wrap(ErrBadGitCommitStatus, fmt.Sprintf("while parsing %s", changeStr))
				}
				change := Change{
					Name:    changeDesc[0],
					File:    changeDesc[1],
					Details: changeDesc[1:],
				}
				last := len(commits) - 1
				commit := commits[last]
				commits[last].Changes = append(commit.Changes, change)
			}
		}

	}
	return commits, nil
}

type FileChange struct {
	Commit CommitDetails
	Change Change
}

type FileGroup map[string]FileChange

func GroupByFile(commits []Commit) (FileGroup, error) {
	fileGroup := make(FileGroup)
	for _, commit := range commits {
		for _, change := range commit.Changes {
			if _, ok := fileGroup[change.File]; !ok {
				fileGroup[change.File] = FileChange{
					Commit: commit.CommitDetails,
					Change: change,
				}
			}
		}
	}
	return fileGroup, nil
}

func FilterCurrentDir(fileGroup FileGroup) ([]string, error) {
	out := make([]string, 0, len(fileGroup))
	for name, _ := range fileGroup {
		if !strings.Contains(name, "/") {
			out = append(out, name)
		}
	}
	return out, nil
}

var (
	red    = color.New(color.FgRed).SprintFunc()
	green  = color.New(color.FgGreen).SprintFunc()
	yellow = color.New(color.FgYellow).SprintFunc()

	commitColor = color.New(color.FgCyan).SprintFunc()
	authorColor = color.New(color.FgMagenta).SprintFunc()
	dateColor   = color.New(color.FgBlue).SprintFunc()
)

func Print(file FileChange) string {
	fileName := file.Change.File
	changeName := file.Change.Name
	change := fmt.Sprintln(yellow(fileName))
	if strings.HasPrefix(changeName, "A") {
		change = fmt.Sprintln(green(fileName))
	}
	if strings.HasPrefix(changeName, "D") {
		change = fmt.Sprintln(red(fileName))
	}
	return fmt.Sprintf("%s %s %s %s %s",
		commitColor(file.Commit.Commit),
		authorColor(file.Commit.Author),
		dateColor(file.Commit.Date.UTC().Format("2006-01-02")),
		changeName[:1],
		change)
}

func main() {
	cmd := exec.Command("git", "log",
		"--name-status",
		fmt.Sprintf("--format=%s{ \"Author\": \"%%an\", \"Commit\": \"%%h\", \"Date\": \"%%aI\"}", SEPARATOR),
		"-p",
		".")
	out, err := cmd.Output()
	if err != nil {
		panic(err)
	}
	commits, err := Parse(out)
	if err != nil {
		panic(err)
	}
	fileHist, err := GroupByFile(commits)
	if err != nil {
		panic(err)
	}
	files, err := FilterCurrentDir(fileHist)
	if err != nil {
		panic(err)
	}
	for _, file := range files {
		str := Print(fileHist[file])
		fmt.Print(str)
	}
}
