---
name: gram-review
description: Hand finished work to the user for review in the Gram editor with ,gram-review, which opens every changed file as a base-vs-branch diff next to your review notes and reports back the user's notes and edits once they close the window. Use when work in a worktree is ready for the user to look at, or when they ask to review, check or look over the changes in Gram, instead of pasting the diff into chat.
---

# Handing a review to the user in Gram

`bin/,gram-review` opens one Gram window per review:

- the repo, so the user can browse beyond the diff
- your notes file
- a diff per changed file against the base: committed, uncommitted and untracked changes alike

The right side of each diff is the real file, so the user's edits land in the
worktree. The base side is a read-only copy.

## Steps

1. Write the notes to the session scratchpad as `REVIEW.md`, in this shape:

   ```markdown
   # <branch>: <one-line what it does>

   ## Look here first
   - `src/auth/login.ts:42` — why this line deserves the user's eyes

   ## Questions
   - a decision you made that the user might want otherwise

   ## Your notes
   <!-- write replies here; close the window when done -->
   ```

   Keep "Look here first" to 5 items or fewer, ranked. Use `path:line` so the user can jump.
2. Run it with `run_in_background`, because it blocks until the window closes:

   ```
   ,gram-review --notes <scratchpad>/REVIEW.md          # base: merge-base with origin/HEAD
   ,gram-review --notes <scratchpad>/REVIEW.md main     # another base
   ```

3. Tell the user in one line that the review is open in Gram and that closing
   the window hands it back.
4. When the task finishes, its output holds the notes file and the files the
   user edited. Read `git diff` for those files and treat their edits as
   decisions, not as something to undo. Then act on the notes.

## Notes

`--dry-run` prints the gram command without opening anything. Use it to
check which files a base picks up.

The helper exits 1 with a message when nothing differs from the base, or when
it cannot find a base. In the second case, pass one explicitly.
