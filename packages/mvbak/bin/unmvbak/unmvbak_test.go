package main

import (
	"os"
	"path/filepath"
	"testing"

	tests "git.sr.ht/~scoiatael/dotfiles/packages/mvbak/_tests"
)

func TestUnmvbakByBackup(t *testing.T) {
	tests.MkTestDir(t)
	defer tests.CleanTestDir(t)

	testFile, content := tests.CreateTestFile(t, "testfile.txt.bak.")

	// Run unmvbak
	_, _, err := UnMvBak(tests.TempDir, testFile)
	if err != nil {
		t.Fatalf("Failed to run unmvbak; %v", err)
	}

	// Check that backup file is gone
	if _, err := os.Stat(testFile); !os.IsNotExist(err) {
		t.Errorf("Backup file still exists")
	}

	// Check that original file exists with correct content
	restoredContent, err := os.ReadFile(filepath.Join(tests.TempDir, "testfile.txt"))
	if err != nil {
		t.Fatalf("Failed to read restored file: %v", err)
	}

	if string(restoredContent) != string(content) {
		t.Errorf("Restored content doesn't match original")
	}
}

func TestUnmvbakByOriginal(t *testing.T) {
	tests.MkTestDir(t)
	defer tests.CleanTestDir(t)

	testFile, content := tests.CreateTestFile(t, "testfile.txt.bak.")

	// Run unmvbak
	_, _, err := UnMvBak(tests.TempDir, "testfile.txt")
	if err != nil {
		t.Fatalf("Failed to run unmvbak; %v", err)
	}

	// Check that backup file is gone
	if _, err := os.Stat(testFile); !os.IsNotExist(err) {
		t.Errorf("Backup file still exists")
	}

	// Check that original file exists with correct content
	restoredContent, err := os.ReadFile(filepath.Join(tests.TempDir, "testfile.txt"))
	if err != nil {
		t.Fatalf("Failed to read restored file: %v", err)
	}

	if string(restoredContent) != string(content) {
		t.Errorf("Restored content doesn't match original")
	}
}
