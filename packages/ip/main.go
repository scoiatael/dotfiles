package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
)

type Config struct {
	Port  int
	Proxy bool
}

func loadConfig() Config {
	config := Config{
		Port:  3000,
		Proxy: false,
	}

	// Load from environment variables if present
	if portStr := os.Getenv("PORT"); portStr != "" {
		if port, err := strconv.Atoi(portStr); err == nil {
			config.Port = port
		}
	}

	if proxyStr := os.Getenv("PROXY"); proxyStr != "" {
		config.Proxy = proxyStr == "true" || proxyStr == "1"
	}

	return config
}

func getRemoteAddr(r *http.Request, useProxy bool) string {
	if !useProxy {
		// Extract just the IP without port
		addr := r.RemoteAddr
		if idx := strings.LastIndex(addr, ":"); idx != -1 {
			return addr[:idx]
		}
		return addr
	}

	// Check proxy headers in order of preference
	if xff := r.Header.Get("X-Forwarded-For"); xff != "" {
		// X-Forwarded-For can contain multiple IPs, take the first one
		if idx := strings.Index(xff, ","); idx != -1 {
			return strings.TrimSpace(xff[:idx])
		}
		return strings.TrimSpace(xff)
	}

	if xri := r.Header.Get("X-Real-IP"); xri != "" {
		return strings.TrimSpace(xri)
	}

	// Fall back to remote address
	addr := r.RemoteAddr
	if idx := strings.LastIndex(addr, ":"); idx != -1 {
		return addr[:idx]
	}
	return addr
}

func handler(config Config) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		remoteAddr := getRemoteAddr(r, config.Proxy)
		w.Header().Set("Content-Type", "text/plain")
		fmt.Fprint(w, remoteAddr)
	}
}

func main() {
	config := loadConfig()

	http.HandleFunc("/", handler(config))

	addr := fmt.Sprintf(":%d", config.Port)
	log.Printf("Starting server on port %d (proxy mode: %v)", config.Port, config.Proxy)

	if err := http.ListenAndServe(addr, nil); err != nil {
		log.Fatal(err)
	}
}
