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

var titleStyle = lipgloss.NewStyle().
	Bold(true).
	Foreground(lipgloss.Color("#FFFFFF")).
	Background(lipgloss.Color("#0000AA")).
	Padding(0, 1)

var panelVerticalPadding = 2

var panelStyle = lipgloss.NewStyle().
	Border(lipgloss.RoundedBorder()).
	BorderForeground(lipgloss.Color("#555555")).
	Padding(0, panelVerticalPadding)

var gitLogWidth = 60

func runCmd(cmd *exec.Cmd) (stdout string, stderr string, err error) {
	var stdoutb bytes.Buffer
	var stderrb bytes.Buffer
	cmd.Stdout = &stdoutb
	cmd.Stderr = &stderrb
	err = cmd.Run()
	stdout = stdoutb.String()
	stderr = stderrb.String()
	return
}

func gitLog() string {
	stdout, stderr, err := runCmd(exec.Command("git", "log", "--oneline", "-n10", "--color=always", "--abbrev-commit"))
	if err != nil {
		panic(fmt.Errorf("Error running git log: %v; %s", err, stderr))
	}

	commits := strings.Split(stdout, "\n")
	var table strings.Builder
	for _, commit := range commits {
		if commit != "" {
			table.WriteString(commit + "\n")
		}
	}
	return table.String()
}

func gitStatus() string {
	stdout, stderr, err := runCmd(exec.Command("env", "GIT_STATUS_COLOR=always", "git", "--config-env=status.color=GIT_STATUS_COLOR", "status", "--short"))
	if err != nil {
		panic(fmt.Errorf("Error running git status: %v; %s", err, stderr))
	}
	return stdout
}

func lsFiles(flags ...string) string {
	flags = append(flags, "--color=always", "--icons=always")
	cmd := exec.Command("eza", flags...)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return ""
	}

	return out.String()
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
		if status := gitStatus(); status != "" {
			status := panelStyle.Width(physicalWidth - panelVerticalPadding*3).Render(status)
			doc.WriteString(status + "\n")
		}

		// Create side-by-side layout for git log and ls
		gitLogOutput := panelStyle.Width(gitLogWidth - panelVerticalPadding).Render(gitLog())
		filesOutput := panelStyle.Width(physicalWidth - gitLogWidth - panelVerticalPadding*3).Render(lsFiles("--tree", "--level=1"))

		// Join them side by side
		row := lipgloss.JoinHorizontal(lipgloss.Top, gitLogOutput, filesOutput)
		doc.WriteString(row + "\n")
	} else {
		// Just display files
		files := panelStyle.Width(physicalWidth - panelVerticalPadding*3).Render(lsFiles("--grid", fmt.Sprintf("--width=%d", physicalWidth-5)))
		doc.WriteString(files + "\n")
	}

	docStyle := lipgloss.NewStyle().Padding(1, 2, 1, 2)

	if physicalWidth > 0 {
		docStyle = docStyle.MaxWidth(physicalWidth)
	}

	// Okay, let's print it
	fmt.Println(docStyle.Render(doc.String()))
}
