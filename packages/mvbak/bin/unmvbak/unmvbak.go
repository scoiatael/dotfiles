package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func UnMvBak(currentDir string, file string) (src string, dst string, err error) {
	if !filepath.IsAbs(file) {
		file = filepath.Join(currentDir, file)
	}

	dir, base := filepath.Split(file)
	path := filepath.Join(dir, base)

	if _, err := os.Stat(path); err == nil {
		src = path
		parts := strings.Split(src, ".bak")
		if len(parts) > 0 {
			dst = parts[0]
		} else {
			return "", "", fmt.Errorf("src is not mvbak file")
		}
	} else {
		pattern := filepath.Join(dir, base+".bak.*")
		matches, err := filepath.Glob(pattern)
		if err != nil || len(matches) == 0 {
			return "", "", fmt.Errorf("no backup found for %s\n", base)
		}

		// Use the first match (sorted alphabetically, which works for timestamp format)
		src = matches[0]
		dst = filepath.Join(dir, base)
	}

	err = os.Rename(src, dst)
	return
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <file>\n", os.Args[0])
		os.Exit(1)
	}

	pwd, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting cwd: %v\n", err)
		os.Exit(1)
	}

	if _, _, err := UnMvBak(pwd, os.Args[1]); err != nil {
		fmt.Fprintf(os.Stderr, "Error renaming: %v\n", err)
		os.Exit(1)
	}
}
