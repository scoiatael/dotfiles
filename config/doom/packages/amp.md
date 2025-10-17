# Amp.el Security Review Session

## Context
- Reviewed authentication implementation in amp.el (Emacs integration for Amp IDE)
- File: /Users/lukas/dotfiles/config/doom/packages/amp.el
- Focus: Authentication security vulnerabilities

## Issues Identified (Priority Order)
1. **CRITICAL: No token validation** - Authentication handler auto-approved without checking token
2. **CRITICAL: Insecure file permissions** - Lockfile readable by all users with disk access  
3. **HIGH: Insecure token generation** - Uses basic PRNG instead of cryptographically secure random
4. **MEDIUM: Plain-text token storage** - Token stored unencrypted in JSON file
5. **LOW: Missing access controls** - No rate limiting or error handling for auth failures
6. **LOW: No token expiration/rotation** - Tokens persist indefinitely until server restart

## Fixes Applied
### Issue #1: Token Validation (COMPLETED)
- Location: Lines 210-216 
- Changed: Authentication handler now validates provided token against amp--auth-token
- Before: Auto-approved all auth requests
- After: Validates token, returns error -32603 for invalid/missing tokens

### Issue #2: File Permissions (COMPLETED)  
- Location: Lines 148-154
- Changed: Set restrictive permissions on lockfile and directory
- Directory: 700 (owner only)
- File: 600 (owner read/write only)
- Always enforces permissions even if directory exists

## Current Todo State
```json
[
  {"id":"fix-auth-validation","content":"Fix authentication handler to validate tokens instead of auto-approving","status":"completed","priority":"high"},
  {"id":"fix-file-permissions","content":"Set restrictive permissions on lockfile and directory (600 for file, 700 for directory)","status":"completed","priority":"high"},
  {"id":"ensure-dir-permissions","content":"Always set directory permissions even if directory already exists","status":"completed","priority":"medium"}
]
```

## Remaining Issues to Address
3. **Insecure token generation** (amp--generate-auth-token, lines 130-136)
4. **Plain-text token storage** (lockfile creation, lines 138-152)  
5. **Missing access controls** (request handling, lines 202-267)
6. **No token expiration/rotation** (server lifecycle)

## Key Code Locations
- Token generation: lines 130-136
- Lockfile creation: lines 138-152  
- Authentication handler: lines 210-216
- WebSocket request handling: lines 190-267
- Auth token storage: line 63 (amp--auth-token variable)

## Implementation Notes
- Auth token stored in global var amp--auth-token
- Lockfile path: ~/.local/share/amp/ide/{port}.json
- WebSocket server handles IDE protocol requests
- Uses websocket.el package for WebSocket functionality
