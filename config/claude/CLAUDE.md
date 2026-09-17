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
