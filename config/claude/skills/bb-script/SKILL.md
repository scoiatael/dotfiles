---
name: bb-script
description: Write and run throwaway scripts in babashka (bb) — one-liners with bb -e, longer ones as a .clj file in the scratchpad. Use whenever a task needs a script rather than a shell command: parsing JSON or CSV, calling an HTTP API, walking files, reshaping data. Replaces reaching for python -c, ruby -e, or a scratch .sh; the PreToolUse hook denies those.
---

# Scripts go through bb

```
bb -e '(println (+ 1 2))'                       # one-liner
bb SCRIPT.clj arg1 arg2                         # a file, args in *command-line-args*
echo '{"a":1}' | bb -e '(prn (json/parse-string (slurp *in*) true))'
```

Longer than a couple of forms: write a `.clj` file in the session scratchpad and
run it. `python -c`, `ruby -e`, `bash -lc` and scratch `.py`/`.sh` files are
denied by the `claude-bash-guard` hook, at both write and run time.

Reach for a script when a pipeline would need `awk`, nested quoting, or more
than one pass over the data. A plain `rg`/`jq` pipeline is still fine.

## Everything below is built in — no bb.edn, no deps

| Need | Namespace |
|---|---|
| JSON | `cheshire.core` |
| HTTP | `babashka.http-client`, `babashka.curl` |
| Shell out | `babashka.process` (`shell`, `process`, `tokenize`) |
| Files, globs | `babashka.fs` |
| CSV, XML | `clojure.data.csv`, `clojure.data.xml` |
| Arg parsing | `babashka.cli`, `clojure.tools.cli` |
| Templating, HTML | `selmer.parser`, `hiccup2.core` |

Also present: `clojure.edn`, `clojure.set`, `clojure.walk`, `clojure.zip`,
`clojure.java.io`, `clojure.core.async`. Startup is a few milliseconds, so a
script is not a heavier choice than a shell loop.

## Shape of a script

```clojure
#!/usr/bin/env bb
(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def out (-> (p/shell {:out :string} "gh" "run" "list" "--json" "status,name")
             :out
             (json/parse-string true)))

(doseq [{:keys [name status]} (filter #(= "failure" (:status %)) out)]
  (println name status))
```

Notes that save a retry:

- `p/shell` throws on non-zero exit; pass `{:continue true}` to inspect `:exit`.
- `{:out :string}` to capture, otherwise output streams to the terminal.
- `fs/glob` returns paths, not strings — `(map str ...)` before handing them on.
- `*command-line-args*` is a seq of strings; `parse-long` for numbers.
- Guard the entry point so the file can also be loaded in a test:
  `(when (= *file* (System/getProperty "babashka.file")) (main))`

## Checking it

There is no byte-compile step. Run the script — bb reports the form and line on
failure. For anything worth keeping, put the pure functions above the entry
guard and exercise them from a sibling `test.clj` with `clojure.test`;
`packages/claude-bash-guard/` in this repo is the worked example, tests and all.
