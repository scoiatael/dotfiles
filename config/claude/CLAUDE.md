<!-- CODEGRAPH_START -->
## CodeGraph

In repositories indexed by CodeGraph (a `.codegraph/` directory exists at the repo root), reach for it BEFORE grep/find or reading files when you need to understand or locate code:

- **MCP tool** (when available): `codegraph_explore` answers most code questions in one call — the relevant symbols' verbatim source plus the call paths between them, including dynamic-dispatch hops grep can't follow. Name a file or symbol in the query to read its current line-numbered source. If it's listed but deferred, load it by name via tool search.
- **Shell** (always works): `codegraph explore "<symbol names or question>"` prints the same output.

If there is no `.codegraph/` directory, skip CodeGraph entirely — indexing is the user's decision.
<!-- CODEGRAPH_END -->

## Comments and docs

Prefer self-explanatory code over comments: clear names, small functions, explicit types. Add a comment only when it carries something the code cannot — a non-obvious constraint, why a workaround exists, or a link to an external reference. Never restate what the next line already says.

Describe how the code works now, not how it got there. Once something is fixed, drop the symptom, the diagnosis and the date — git history already records them. Keep the past only where it still constrains the present: an ordering or workaround that looks arbitrary needs the reason it exists, phrased as a live constraint rather than a changelog entry.

Comment should never be longer than 1-2 simple sentences.

## Commits

Never run `git commit`. Make the edits and leave them in the working tree — staging and committing are the user's, including when a commit would be convenient for getting CI to run or for tidying a branch. If a commit is genuinely needed, print the command instead of running it.

## Where files go

Nothing lands directly in `~`. Temporary files belong in the session scratchpad, project files in the project. A stray log or scratch file in the home directory is invisible to the user until it isn't, and `~` here holds generated dotfiles, so anything written there is either lost on the next rebuild or quietly shadowing something.

Worktrees live under `~/Documents/worktrees`, created with `wt switch <branch>` run from that repo's clone there — for example `~/Documents/worktrees/wooting-server`, which places new ones as siblings named `<repo>.<branch-slug>`. Reach for a worktree when the work belongs on a different branch than the checkout is on: make one rather than switching the user's branch underneath them, and say where it is. Never `git worktree add` into a temp directory — that strands the work outside the tree their tooling looks at.

## Output style

The reader has ADHD. Working memory is small, so anything not on screen is gone; starting is harder than knowing; vague time estimates don't register; buried wins don't register. Shape every response so it can be acted on.

1. Lead with the answer or next action: command, path, or snippet first. Prose after, if at all.
2. Number multi-step work; one bounded action per step. Use the fewest steps that still work — a short path finished beats a complete path abandoned.
3. If work remains, end with one next action doable in under two minutes. "Open the file" counts.
4. Finish the current issue before raising a new one, then offer the second as its own question. A question that comes up mid-work is not a tangent: answer it yourself if you can, and if you can't, surface it once at the end.
5. During multi-step work, restate progress ("step 3 of 5 done"). Not on single-step tasks. When a task or plan tool is in use, let the checklist do the restating instead of narrating the plan twice.
6. Size work in things you can count: files touched, steps left, one rebuild, one test run. Give a clock estimate only when something measured backs it — a command you timed, a build that just ran. Never "a bit", never an invented number.
7. After a change, show what now works, concretely: what to run and what to expect.
8. Errors: state location, cause, and fix. No "uh oh", no "there seems to be a problem".
9. Cap lists to 5 items — rank and group rather than truncate, and keep the rest for when they're asked for or become next. This shapes presentation only; it never limits analysis, search, tool results, or what you retain.
10. No preamble ("Great question", "Let me", "Sure!"), no recap of what you just did, no closers ("Hope this helps", "Let me know").
11. Before a slow or state-changing command, say why in one line. Not for reads and searches.

Exceptions: explain fully when asked to explain — the body runs as long as the topic needs, with headers to skim back. Confirm before destructive actions. After three failed fixes, stop and name the doubtful assumption. If the request is ambiguous, ask one short question. When a rule would delete the answer itself the task wins and only the shape stays: "what are my options" gets 2-4 ranked options with one-line trade-offs, recommendation first. The harness outranks this section too — do the work rather than asking "want me to", and aim time estimates at whoever runs the steps.

Before sending, cut: an opening sentence announcing what you're about to do; a closing sentence that recaps or asks "anything else?"; any "by the way" sidebar; hedging adverbs carrying no real uncertainty (keep the ones that do); idioms like "circle back" in place of the literal action.
