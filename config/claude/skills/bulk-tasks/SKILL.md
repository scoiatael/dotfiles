---
name: bulk-tasks
description: Turn a list of notes, links or a markdown file into tracker tasks in bulk — unfurl each item, match the tracker's existing conventions, draft every issue into one editable markdown file, then create them all and link back. Use when asked to "create Linear tasks from these", turn post-mortem follow-ups, meeting notes or a Notion list into issues, or file many tasks at once.
---

# Bulk tasks from notes

The flow, in order. Each phase ends with the user's go-ahead — never create
anything before the draft file is approved.

## 1. Read the list

Usually a text/markdown file of links or one-line items. Paths with spaces
need quoting with `~` outside the quotes: `~/"My Drive/org/x.txt"`. Links
copied from HTML may carry `&amp;` — harmless for Notion/Linear (the ID is
all that matters), but don't paste them into descriptions.

## 2. Learn how the tracker is organised

Before drafting, read how existing issues of the same kind look — a saved
view the user points at, or the team's recent issues:

- Linear: `list_issues` with `customView` (URL accepted) and
  `fields: [title, status, labels, project, assignee, priority, parentId, cycleId, createdAt]`;
  `list_issue_labels` for the team (some labels are single-select groups:
  one label per group).
- Note team vs label (a "Dev/Ops team" may be a label inside another team),
  default status, whether issues get assignees / projects / priorities, title
  style.
- Look for a **precedent batch**: many issues created within minutes of each
  other is the last time someone did this — copy its shape.

Report the conventions in a few bullets; let the user correct them.

## 3. Unfurl every item

Fetch all items in parallel (`notion-fetch` takes the bare 32-hex ID from the
URL). Expect most pages to be a title only. Then:

- **Resolve mentions** in titles/bodies — Notion shows linked pages as
  `@Not found`; the `<mention url=…>` holds the real target, fetch it.
- **Ask for the source of each item.** Follow-ups usually come from a
  post-mortem or doc the page doesn't link. The user will hand them over one
  by one ("2 is <url>"); fetch each and note which action item it maps to.
  Several tasks often share one source.
- Present per item: what the source says + a draft one-paragraph description.
  Keep a running "item → source" table so progress stays visible.

## 4. Check existing issues — same scope only

Search the tracker (all pages of the view, plus `get_issue` for close
matches). An overlap is an existing issue covering **the same concrete
work**. Same topic, same symptom family, an aggregate version of a specific
fix, or a stopgap in a different system is *not* an overlap — at most a
`relatedTo`. Default to creating the new issue; never propose skipping,
merging or commenting instead unless it's a true duplicate.

Don't pad the list either: action items found in the sources that aren't on
the user's list may be mentioned once, then dropped if declined. The user's
list is what was decided.

## 5. Draft everything into one file

Write all drafts to `<scratchpad>/linear-drafts.md` and give the user the
full absolute path. The user edits it directly — after they say "reviewed",
**re-read the file**; it is now the source of truth (they may add, remove,
retitle or relabel items). Flag only real problems: duplicate titles, a
backlink pointing at the wrong page, label casing that doesn't match the
tracker.

```markdown
# <Batch name>

Team: <team> · Status: <status> · <defaults> · Unassigned unless noted

## 1. <Title>
Labels: DevOps, Server · Related: W-1234 · Assignee: <name>

<What to do and why, 2–4 sentences. Concrete file/function names.>

Source: [<source doc title>](<url>) (<date>) — <one-line impact>.
Notion task: <url of the originating item>
```

## 6. Create

1. Create the **first** issue alone and look at the result (markdown
   rendering, labels, status).
2. Create the rest in one parallel batch.
3. Verify anything the create response doesn't echo — e.g. `get_issue` with
   `fields: [relations]` for `relatedTo`.
4. Report a table: draft # → issue ID → title, and how to find them together
   (view + sort by created).

Linear `save_issue`: `team`, `state`, `title`, `labels` (names), `assignee`
(name), `relatedTo`, `description` as real markdown with literal newlines.

## 7. Link back

Append one line to each originating item, e.g. `notion-update-page` with
`command: insert_content`, `position: {type: end}`:
`Linear: [W-3592](https://linear.app/<org>/issue/W-3592)`. An item split into
several issues gets all of them on one line. Fetch one page afterwards to
confirm the line landed.

## Gotchas

- Second-hand facts from sources (dates, expiries, IPs) go in descriptions
  with their source and age — don't escalate them to the user as urgent
  without checking; offer the one command that verifies.
- Assignees: ask once for the whole batch; "unassigned" is a valid answer.
- Linear MCP and Notion tools are deferred — load `list_issues`,
  `list_issue_labels`, `get_issue`, `save_issue`, `notion-fetch`,
  `notion-update-page` via ToolSearch up front.
