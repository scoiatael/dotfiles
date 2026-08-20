---
name: org-roam
description: Read and write the user's org-roam notes with the ,roam helper — search by title/alias/tag, print a note, follow backlinks, add or append notes. Use whenever a question touches their personal notes, or when something learned is worth recording there, instead of grepping the notes directory or reading org-roam.db.
---

# Working with org-roam notes

`bin/,roam` drives org-roam through the running Emacs, using org-roam's own
node accessors. 253 notes live in `$org-roam-directory`
(`/Users/lukas/My Drive/org/roam/`).

Prefer it over grepping the notes directory: it resolves aliases and tags,
knows the link graph, and reflects the live session rather than what has been
flushed to disk.

## Reading

```
,roam                      every node — title, #tags, id
,roam systemd              bare term searches title, alias and tag
,roam cat systemd-cred     the note's org source
,roam file systemd-cred    just the resolved path
,roam links zsh            backlinks in, and outbound links
,roam tags                 every tag with its node count
,roam open systemd-cred    visit it in the running Emacs
```

Terms resolve in order: exact id, exact title, then unique case-insensitive
substring. An ambiguous term lists the candidates and exits 1 — narrow it
rather than guessing which one was meant.

## Writing

```
,roam add "Some Title" --tags nix,gotcha --ref https://example.com --body "text"
echo "body" | ,roam add "Some Title"
,roam append systemd-cred --body "* Another heading"
,roam edit systemd-cred                     # $EDITOR, reindexed on exit
```

`add` prints the new id and path. Files are built to match the existing
convention — `:PROPERTIES:`/`:ID:`/optional `:ROAM_REFS:`/`:END:`, then
`#+title:`, then `#+filetags: :a:b:` — with the id from `org-id-new` and the
filename slug from `org-roam-node-slug`, so nothing here hardcodes the capture
template.

Every write reindexes via `org-roam-db-update-file`, so a new note is
searchable immediately and any clean buffer showing the file is reverted.

There is deliberately no delete verb. Removing notes stays a manual act in
Emacs.

## Gotchas

A running Emacs server is required; `,roam` says so and exits 1 when it is
missing. There is no headless fallback — `doomscript` runs a minimal Doom CLI
environment with org-roam absent from the load-path, so `(require 'org-roam)`
fails there.

**Never read `org-roam.db` directly.** It lags the live session (246 rows on
disk against 253 nodes in Emacs, when last checked), and every value is stored
as an emacsql S-expression, so titles carry literal quotes:
`"systemd-credentials"`. Ask the running Emacs instead. A second database at
`~/org/roam/org-roam.db` is a 2021 relic on an incompatible schema — ignore it.

Writes refuse when Emacs has unsaved changes to the target file, rather than
writing underneath a dirty buffer. `add` refuses a title that already exists
and names the existing id; `--force` overrides.

Note paths contain spaces — `~/org` symlinks to Google Drive and the notes
resolve under `/Users/lukas/My Drive/org/roam/`. Quote every path taken from
`,roam file`.
