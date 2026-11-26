---
name: second-opinion
description: Ask a stronger model (Fable at max effort) for critical feedback on a plan, design, draft, diagnosis or piece of code, via a fresh read-only subagent. Use when the user asks for a second opinion, a sanity check, "ask a smarter model", "what would Fable think", or wants something pressure-tested before committing to it. For a bug review of a branch or PR by a different vendor's model, use codex-review instead.
---

# Getting a second opinion

Spawn one subagent on the top tier and effort, hand it everything it needs,
and weigh what comes back — it advises, you and the user decide.

```
Agent(
  description: "Second opinion on <thing>",
  subagent_type: "general-purpose",
  model: "fable",
  effort: "max",
  prompt: <brief below>,
)
```

This skill is the explicit instruction that licenses `effort: "max"`. If you
are already Fable at max, still do it: the value is a fresh context that has
not talked itself into the current answer.

## The brief

The subagent starts with no memory of this conversation. Anything you leave
out, it cannot weigh. Write the brief as if to a senior colleague who just
walked in:

1. **The thing** — the plan, draft or diff itself, inline. For code, give
   absolute paths and line ranges so it reads the current source rather than
   your paraphrase.
2. **The goal** — what this is for and what "good" means here.
3. **Constraints and decisions already made** — and which of them are fixed
   versus open to challenge. Say which the user chose.
4. **The question** — one or two specific things you want judged ("will this
   migration lose rows under concurrent writes?"), not "any thoughts?".
5. **Your current view and its weak spot** — so it can attack the part you
   are least sure of instead of agreeing with the part you are sure of.
6. **Ground rules** — read-only: it may read files and run read-only commands
   but must not edit, commit or post anything. Ask it to disagree where it
   disagrees and not to pad with praise.
7. **Answer shape** — verdict in one line; then at most five issues ranked by
   consequence, each with *what goes wrong*, *how confident*, and *what to do
   instead*; then anything it would do differently from the ground up.

Do not tell it what answer you expect, and do not trim the counter-evidence
you already found — leaving it in gives it more to work with.

## Using the answer

The reply goes to you, not the user. Before relaying:

- Check each claim it makes about code against the source; a stronger model
  can still misread a file.
- Sort its points into *agree — will change*, *disagree — because…*, and
  *needs the user's call*.
- Report that sort to the user, leading with the verdict and the changes you
  will make. Do not paste the raw reply.

If it surfaces something that changes the plan materially, say so before
acting on it; otherwise apply the agreed changes and carry on.

To push back or ask a follow-up, `SendMessage` the same agent rather than
spawning a new one — it keeps what it already read.
