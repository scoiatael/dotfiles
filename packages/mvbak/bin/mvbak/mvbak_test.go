package main

import (
	"os"
	"strings"
	"testing"

	tests "git.sr.ht/~scoiatael/dotfiles/packages/mvbak/_tests"
)

func TestMvbak(t *testing.T) {
	tests.MkTestDir(t)
	defer tests.CleanTestDir(t)

	testFile, content := tests.CreateTestFile(t, "testfile.txt")

	// Run mvbak
	backupFile, err := MvBak(tests.TempDir, testFile, "test_1")
	if err != nil {
		t.Fatalf("Failed to mvbak: %v", err)
	}

	// Check that original file is gone
	if _, err := os.Stat(testFile); !os.IsNotExist(err) {
		t.Errorf("Original file still exists")
	}

	if !strings.Contains(backupFile, ".bak.test_1") {
		t.Errorf("Backup filename doesn't contain .bak.<suffix>.: %s", backupFile)
	}

	backupContent, err := os.ReadFile(backupFile)
	if err != nil {
		t.Fatalf("Failed to read backup file: %v", err)
	}

	if string(backupContent) != string(content) {
		t.Errorf("Backup content doesn't match original: got %s, want %s",
			string(backupContent), string(content))
	}
}

func TestMvbakConflict(t *testing.T) {
	tests.MkTestDir(t)
	defer tests.CleanTestDir(t)

	testFile, content := tests.CreateTestFile(t, "testfile.txt")
	testFileConflict, contentConflict := tests.CreateTestFile(t, "testfile.txt.bak.test_1")

	// Run mvbak
	backupFile, err := MvBak(tests.TempDir, testFile, "test_1")
	if err != nil {
		t.Fatalf("Failed to mvbak: %v", err)
	}

	// Check that original file is gone
	if _, err := os.Stat(testFile); !os.IsNotExist(err) {
		t.Errorf("Original file still exists")
	}

	backupContent, err := os.ReadFile(backupFile)
	if err != nil {
		t.Fatalf("Failed to read backup file: %v", err)
	}

	if string(backupContent) != string(content) {
		t.Errorf("Backup content doesn't match original: got %s, want %s",
			string(backupContent), string(content))
	}

	// Check that conflict file is untouched
	oldContent, err := os.ReadFile(testFileConflict)
	if err != nil {
		t.Fatalf("Failed to read file with conflicting filename: %v", err)
	}

	if string(oldContent) != string(contentConflict) {
		t.Errorf("Backup overwrote old content in conflicting file: got %s, want %s",
			string(oldContent), string(contentConflict))
	}
}

func TestMvbakDotfile(t *testing.T) {
	tests.MkTestDir(t)
	defer tests.CleanTestDir(t)

	testFile, content := tests.CreateTestFile(t, ".testfile.txt")

	// Run mvbak
	backupFile, err := MvBak(tests.TempDir, testFile, "test_1")
	if err != nil {
		t.Fatalf("Failed to mvbak: %v", err)
	}

	// Check that original file is gone
	if _, err := os.Stat(testFile); !os.IsNotExist(err) {
		t.Errorf("Original file still exists")
	}

	if !strings.Contains(backupFile, ".bak.test_1") {
		t.Errorf("Backup filename doesn't contain .bak.<suffix>.: %s", backupFile)
	}

	backupContent, err := os.ReadFile(backupFile)
	if err != nil {
		t.Fatalf("Failed to read backup file: %v", err)
	}

	if string(backupContent) != string(content) {
		t.Errorf("Backup content doesn't match original: got %s, want %s",
			string(backupContent), string(content))
	}
}
