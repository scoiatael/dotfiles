package testing

import (
	"math/rand"
	"os"
	"path/filepath"
	"testing"
)

var TempDir string

func MkTestDir(t *testing.T) {
	var err error
	// Create temporary test directory
	TempDir, err = os.MkdirTemp("", "mvbak-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
}

func CleanTestDir(t *testing.T) {
	os.RemoveAll(TempDir)
}

func CreateTestFile(t *testing.T, name string) (string, []byte) {
	// Create a test file
	testFile := filepath.Join(TempDir, name)
	content := make([]byte, 4)
	rand.Read(content)
	if err := os.WriteFile(testFile, content, 0644); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}
	return testFile, content
}
