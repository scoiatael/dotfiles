---
name: tt
description: Query the user's work queues with the tt CLI — Linear issues/projects, Github PRs (review queue and own open PRs), and Notion tasks. Use when asked what's on their plate, for standup material, PR/review status, or anything about their Linear/Notion tasks, instead of hand-rolling gh/GraphQL calls.
---

# tt — work tracker CLI

`tt` wraps the Linear, Github, and Notion queries that back `todo.org`
(source: dotfiles `packages/tt`, babashka). Prefer it over raw `gh search`
or hand-written GraphQL — it already knows the right views, filters, and
sort order.

```
tt linear issues            open issues from the "My open issues" view
tt linear projects          projects from the "My projects" view
tt linear api '<query>'     raw Linear GraphQL, prints JSON
tt gh prs                   open PRs: review-requested + authored (drafts first)
tt gh todo                  authored non-draft PRs awaiting action
tt gh comments [<pr>]       one PR's review discussion — see [[pr-comments]]
tt notion tasks             unfinished Notion tasks assigned to the user
```

## Formats

`--format` (or `-f`): `table` (default, human), `org`, `json`, and for
`gh todo` also `md`. When consuming results yourself, use `json`:

```
tt gh prs --format json | jq '.[] | select(.type == "review")'
tt linear issues -f json
```

Rows sort by workflow state (Triage/Idea → Backlog/Paused → In Progress →
In Review); unknown states sort first.

## Ad-hoc Linear queries

`tt linear api` handles auth and pretty-prints; exits 1 when the response
contains GraphQL errors. Discover custom view IDs with:

```
tt linear api 'query { customViews { nodes { id name } } }'
```

## Gotchas

- Auth: Linear/Notion tokens come from `pass` (may trigger a gpg pinentry
  prompt if the agent cache is cold); `gh` uses its own login.
- If `tt` is not on PATH yet (home-manager not switched since it was
  added), run it as `nix run ~/dotfiles#tt -- <args>`.
- The org-babel blocks in `~/My Drive/org/todo.org` call `tt … --format org`;
  changing tt's columns changes those tables.
