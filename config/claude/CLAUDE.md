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
