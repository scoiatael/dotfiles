---
name: filesystem-search
description: Search this machine with fd and rg, scoped to known directories rather than / or $HOME. Use when locating files or content outside the current project — a package's source, an emacs straight build, a config under some dotfile tree — where an unscoped find/grep would stall on macOS permission-protected paths.
---

# Searching the filesystem

Use `fd` for paths and `rg` for content. Never `find`, never `grep`.

```
fd projectile.el ~/.emacs.local/straight    # path search, scoped
rg -n 'defcustom projectile-enable' FILE    # content search
```

## Never scan / or ~

An unscoped `fd . /` or `fd . ~` walks macOS's TCC-protected directories and
returns a wall of permission errors around whatever it actually found — slow,
noisy, and easy to misread as "not present".

Discover the top level first, then descend into one known directory:

```
ls -la ~                    # see what's there
fd -t d 'skills' ~/dotfiles # then search inside it
```

The same constraint is already encoded in this config's affe command
(`config/doom/modules/scoiatael/editor/config.el`), which excludes
`Application Support`, `Library`, `Pictures`, and `Music` — that list is the
shape of the problem.

## Known roots on this machine

| What | Where |
| --- | --- |
| dotfiles (flake, all configs) | `~/dotfiles` |
| emacs packages, built | `~/.emacs.local/straight/build-<emacs-version>/` |
| emacs packages, sources | `~/.emacs.local/straight/repos/` |
| claude config | `~/.claude` (skills, CLAUDE.md symlinked from dotfiles) |
| `~/.config/*` | mostly symlinks into `~/dotfiles/config/` |

Reading a package's real source beats recalling its API — variable names,
defaults, and function arities drift between releases:

```
rg -n 'defun projectile-invalidate-cache' \
  ~/.emacs.local/straight/build-30.2.50/projectile/projectile.el
```

## fd and rg defaults worth remembering

Both skip hidden files and honour `.gitignore`. When looking for dotfiles or
build output, pass the flags — otherwise a real hit reads as an absent one:

```
fd -H -I '^\.envrc$' ~/dotfiles   # -H hidden, -I ignore .gitignore
fd -t f -d 2 . ~/dotfiles/config  # -t type, -d max depth
rg -n --no-ignore --hidden PATTERN DIR
```
