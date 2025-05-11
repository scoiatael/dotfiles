package main

import (
	"fmt"
	"os"
	"path/filepath"
	"time"
)

func MvBak(currentDir string, file string, suffix string) (dst string, err error) {
	// TBD: dotfiles
	// TBD: add leading dot?
	// TBD: path relative from current dir
	if !filepath.IsAbs(file) {
		file = filepath.Join(currentDir, file)
	}

	dir, base := filepath.Split(file)
	dst = filepath.Join(dir, base+".bak."+suffix)
	original := dst

	for c := range 10 {
		if _, err := os.Stat(dst); os.IsNotExist(err) {
			break
		}
		dst = fmt.Sprintf("%s_%02d", original, c)
	}

	err = os.Rename(file, dst)
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

	ts := time.Now().Format("20060102T1504")

	if _, err := MvBak(pwd, os.Args[1], ts); err != nil {
		fmt.Fprintf(os.Stderr, "Error renaming: %v\n", err)
		os.Exit(1)
	}
}
