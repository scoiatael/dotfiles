---
name: pr-comments
description: Read review feedback on a Github PR with `tt gh comments` — inline threads, review verdicts and discussion, in one chronological transcript with bot chatter and resolved threads filtered out. Use when asked to address review comments, see what a reviewer said, or check the feedback on a PR, instead of stitching together gh api / GraphQL calls.
---

# Reading PR review feedback

`tt gh comments` prints one PR's whole discussion — issue comments, review
bodies, and inline review threads — merged into a single chronological
transcript (source: dotfiles `packages/tt`, babashka; see [[tt]] for the rest
of the CLI).

Prefer it over `gh pr view --comments` (which omits inline review threads) and
over hand-written GraphQL: it already joins the three separate comment
collections, drops the noise, and keeps each thread's replies together.

```
tt gh comments                              the PR for the current branch
tt gh comments 326                          by number, in the current repo
tt gh comments WootingKb/wooting-mono#326   another repo
tt gh comments https://github.com/o/r/pull/1
tt gh comments --all                        keep resolved threads and bots
```

## Reading the output

Each entry is a header line — author, kind, `path:line` for inline comments,
date — followed by the indented body:

```
rjoken  inline  app/models/invoice.rb:9  2026-08-02
  is the token unique per attempt?
lukas  reply  app/models/invoice.rb:9  2026-08-02
  per invoice, yes
```

`kind` says where the comment lives: `comment` (top-level discussion),
`inline` (first comment of an unresolved review thread), `reply` (subsequent
comments in that thread), `resolved` (a resolved thread, `--all` only), or the
review verdict — `approved`, `changes_requested`, `commented`.

Entries sort by creation time, except that a thread's replies always follow
its opening comment, so a conversation reads top to bottom.

## Formats

`--format` / `-f`: `table` (default, the transcript above), `md` and `org`
(one linked list item per comment, the link pointing at the comment on
Github), `json` when consuming it yourself:

```
tt gh comments 326 -f json | jq '.[] | select(.kind == "inline")'
tt gh comments 326 -f md          # paste into a summary of what to address
```

JSON keys: `author`, `kind`, `where`, `created`, `body`, `url`.

## What gets filtered

By default the transcript hides what a reviewer did not write: comments from
`Bot` accounts, integration chatter that posts under a human account (Graphite
stack tables, Linear linkbacks, `<!-- …-pr-comment -->` markers), resolved
threads, and the empty review envelope that Github creates around a batch of
inline comments.

`--all` brings back the bots and the resolved threads — reach for it when a
review seems to be missing context, or when checking what was already settled.
Empty review envelopes stay hidden either way.

## Gotchas

- Read-only, deliberately. Replying to a review stays a manual act.
- With no argument the PR comes from the current branch of the current working
  directory — so a bare number is only unambiguous inside the repo it belongs
  to. Prefer `owner/repo#n` when working across repos. An unresolvable ref
  exits 1 naming what was tried, plus `gh`'s own diagnosis.
- `gh`'s login provides auth.
- One page is fetched (100 comments, 100 reviews, 100 threads, 50 replies per
  thread) with no pagination; when a PR exceeds that, a warning goes to stderr
  rather than the output quietly stopping short.
- If `tt` is not on PATH yet (home-manager not switched since it was added),
  run it as `nix run ~/dotfiles#tt -- gh comments …`.
