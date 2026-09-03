---
name: el-check
description: Check elisp with ,el-check, which byte-compiles it in the running Emacs and reports errors and warnings. Use after editing any .el file — doom modules, package config, a helper — instead of eyeballing parens or hand-rolling an emacsclient byte-compile call.
---

# Checking elisp

`bin/,el-check` byte-compiles files in the running Emacs and prints what the
compiler says:

```
,el-check config/doom/modules/scoiatael/llm/autoload.el
,el-check config/doom/modules/scoiatael/**/*.el
,el-check --errors-only FILE...      # skip warnings
```

It exits non-zero if any file failed to compile, so it chains:

```
,el-check config/doom/**/*.el && echo clean
```

## Why byte-compile rather than read

Byte-compilation is the cheapest check that catches real mistakes. `read`
alone finds unbalanced parens; the compiler also finds:

- `Error: Invalid read syntax` — a stray paren, with line and column
- `Warning: reference to free variable` — a typo'd or hoisted-away variable
- `Warning: the function ... is not known to be defined` — missing require
- wrong argument counts, obsolete variables and functions

A deeply nested backquote that *looks* balanced is exactly where this pays
off: the reader error names the column, and a "free variable" warning right
before it usually means a form closed one paren early.

## Why through the running Emacs

The check sees the load-path, macros and package versions the config actually
runs with. `doomscript` runs a minimal Doom CLI environment where most
packages are absent from the load-path, so `use-package!` config and anything
touching a straight-installed package produce noise there instead of signal.

The `.elc` goes to a temp file, never next to the source, so checking leaves
the repo clean.

## Reading the output

Diagnostics are grouped under the `In some-function:` header they belong to,
with wrapped messages kept whole:

```
ok   /Users/lukas/dotfiles/config/doom/modules/scoiatael/llm/config.el
  In end of data:
    config.el:22:18: Warning: the function ‘gptel-context-quit’ is not known to
        be defined.
```

`In end of data:` means the reference is at top level — usually a keybinding
or hook naming a function from a package that is loaded lazily. Those are
expected in Doom config and not worth chasing; `--errors-only` hides them.

## Notes

Needs a running Emacs server — it says so and exits 1 when there is none.
There is no headless fallback, for the load-path reason above.

Warnings are not failures: the exit code tracks compilation, so a file full of
lazy-loading warnings still reports `ok` and exits 0.

If a file legitimately cannot compile outside its module context, check it
anyway and read the errors rather than skipping it — a read error is a read
error regardless of load-path.
