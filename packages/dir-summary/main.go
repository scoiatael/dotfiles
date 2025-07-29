package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/charmbracelet/lipgloss"
	"golang.org/x/term"
)

var panelVerticalPadding = 2

var panelStyle = lipgloss.NewStyle().
	Border(lipgloss.RoundedBorder()).
	BorderForeground(lipgloss.Color("#555555")).
	Padding(0, panelVerticalPadding)

var gitLogWidth = 60

var warningStyle = lipgloss.NewStyle().
	Bold(true).
	Background(lipgloss.Color("#0000AA")).
	Padding(0, 1)

var columnStyle = lipgloss.NewStyle().
	Padding(0, 1)

func runCmd(cmd *exec.Cmd) (stdout string, stderr string, err error) {
	var stdoutb bytes.Buffer
	var stderrb bytes.Buffer
	cmd.Stdout = &stdoutb
	cmd.Stderr = &stderrb
	err = cmd.Run()
	stdout = strings.TrimRight(stdoutb.String(), "\n")
	stderr = strings.TrimRight(stderrb.String(), "\n")
	return
}

func gitLog() (string, error) {
	stdout, stderr, err := runCmd(exec.Command("git", "log", "--oneline", "-n10", "--color=always", "--abbrev-commit"))
	if err != nil {
		return stderr, err
	}

	commits := strings.Split(stdout, "\n")
	var table strings.Builder
	for _, commit := range commits {
		if commit != "" {
			table.WriteString(commit + "\n")
		}
	}
	return table.String(), nil
}

func gitStatus() (string, error) {
	stdout, stderr, err := runCmd(exec.Command("env", "GIT_STATUS_COLOR=always", "git", "--config-env=status.color=GIT_STATUS_COLOR", "status", "--short"))
	if err != nil {
		return stderr, err
	}
	return stdout, nil
}

func lsFiles(flags ...string) (string, error) {
	cmd := exec.Command("eza", append(flags, ".", "--color=always", "--icons=always")...)
	stdout, stderr, err := runCmd(cmd)
	if err != nil {
		return stderr, err
	}
	return stdout, nil
}

func main() {
	flag.Parse()

	var dirname string
	if flag.NArg() > 0 {
		dirname = flag.Arg(0)
	} else {
		dirname = "."
	}

	// Change to specified directory
	err := os.Chdir(dirname)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error changing to directory %s: %v\n", dirname, err)
		os.Exit(1)
	}

	physicalWidth, _, _ := term.GetSize(int(os.Stdout.Fd()))
	doc := strings.Builder{}

	// Get current directory and display it
	currentDir, err := os.Getwd()
	if err != nil {
		currentDir = dirname
	}
	titlePanel := panelStyle.Width(physicalWidth - panelVerticalPadding*3).Render(currentDir)
	doc.WriteString(titlePanel + "\n")

	// Check if .git directory exists
	_, err = os.Stat(".git")
	isGitRepo := err == nil

	if isGitRepo {
		// Display git status
		if status, err := gitStatus(); err != nil {
			fmt.Println(warningStyle.Render(err.Error()))
		} else {
			if status != "" {
				status := panelStyle.Width(physicalWidth - panelVerticalPadding*3).Render(status)
				doc.WriteString(status + "\n")
			}
		}

		var gitLogOutput string
		if log, err := gitLog(); err != nil {
			fmt.Println(warningStyle.Render(err.Error()))
		} else {
			// Create side-by-side layout for git log and ls
			gitLogOutput = columnStyle.Width(gitLogWidth - panelVerticalPadding).Render(log)
		}

		var filesOutput string
		if files, err := lsFiles("--tree", "--level=1"); err != nil {
			fmt.Println(warningStyle.Render(err.Error()))
		} else {
			filesOutput = columnStyle.Width(physicalWidth - gitLogWidth - panelVerticalPadding*3).Render(files)
		}

		// Join them side by side
		row := panelStyle.Render(lipgloss.JoinHorizontal(lipgloss.Top, gitLogOutput, filesOutput))
		doc.WriteString(row + "\n")
	} else {
		// Just display files
		var filesOutput string
		if files, err := lsFiles("--grid", fmt.Sprintf("--width=%d", physicalWidth-5)); err != nil {
			fmt.Println(warningStyle.Render(err.Error()))
		} else {
			filesOutput = panelStyle.Width(physicalWidth - panelVerticalPadding*3).Render(files)
		}
		doc.WriteString(filesOutput + "\n")
	}

	docStyle := lipgloss.NewStyle().Padding(1, 2, 1, 2)

	if physicalWidth > 0 {
		docStyle = docStyle.MaxWidth(physicalWidth)
	}

	// Okay, let's print it
	fmt.Println(docStyle.Render(doc.String()))
}
