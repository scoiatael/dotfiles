# Amp.el Development Sessions

## Session 1: Security Review (Completed)

### Issues Fixed
1. ✅ **CRITICAL: Token Validation** - Authentication handler now validates tokens (lines 213-218)
2. ✅ **CRITICAL: File Permissions** - Lockfile has restricted permissions (600 file, 700 directory)

### Remaining Security Issues
3. **HIGH: Insecure token generation** - Uses basic PRNG instead of cryptographically secure random
4. **MEDIUM: Plain-text token storage** - Token stored unencrypted in JSON file
5. **LOW: Missing access controls** - No rate limiting or error handling for auth failures
6. **LOW: No token expiration/rotation** - Tokens persist indefinitely until server restart

## Session 2: IDE Launcher Features (Completed)

### Features Implemented
1. ✅ **amp-launch-ide command** - Launch amp --ide in terminal
   - Auto-detects terminal: vterm → term → compile mode
   - Lines: 636-695
   - Interactive command: `M-x amp-launch-ide`

2. ✅ **project.el integration** - Use project root as workspace
   - Functions: `amp--get-project-root`, `amp--get-workspace-folders`
   - Lines: 127-139
   - Lockfile now uses project root in workspaceFolders

3. ✅ **Connection info display** - Show launch command on server start
   - Lines: 593-603
   - Displays in message and *amp-log* buffer
   - Shows workspace, port, and full command

### New Configuration Options
- `amp-executable` - Path to amp executable (default: "amp")
- `amp-ide-terminal-function` - Custom terminal launcher (default: auto-detect)

### Usage
```elisp
;; Start server
M-x amp-start

;; Launch IDE in terminal
M-x amp-launch-ide

;; Customize amp executable path
(setq amp-executable "/usr/local/bin/amp")

;; Use custom terminal function
(setq amp-ide-terminal-function #'my-custom-launcher)
```

## Key Code Locations
- Configuration: lines 45-66
- Project integration: lines 127-139
- Token generation: lines 158-164
- Lockfile creation: lines 166-182
- Authentication handler: lines 213-218
- IDE launcher: lines 636-695
- WebSocket request handling: lines 204-287

## Implementation Notes
- Auth token stored in global var `amp--auth-token`
- Lockfile path: `~/.local/share/amp/ide/{port}.json`
- WebSocket server handles IDE protocol requests
- Uses websocket.el package for WebSocket functionality
- Requires project.el for workspace detection
