# Auto-review gate

A `pre-commit` hook that (1) reviews your **staged** changes with a headless
`claude` and auto-fixes any problems it finds, then (2) runs the engine's
test suite. Problematic code never enters history. Ported from the Closure
game's gate (itself from cl-amiga), with the review dimensions adapted to
this repo.

## What the review enforces

- **HyperSpec conformance** — no implementation-specific extensions, no
  undefined behaviour; this code is also a conformance probe for clamiga.
- **Lisp quality** — readable, maintainable, idiomatic Common Lisp.
- **The engine ships no story** — mechanics live here, content lives in a
  game repo; a game-shaped name, table or constant in engine code is a
  finding. The suite plays `tests/world/`, a minimal fixture, and must stay
  playable with no game present.
- **Versioning discipline** — behaviour a game would notice bumps
  `src/version.lisp`; `+SAVE-VERSION+` moves only on a real save-format
  change.
- **Amiga constraints** — heap/stack frugality on 68k-class hardware,
  especially in per-frame and per-turn paths.
- **Tests are the specification** — features and fixes need tight checks in
  `tests/run-tests.lisp`.

## Flow

```
git commit
   └─ githooks/pre-commit  →  scripts/review/pre-commit.sh
        1. staged diff written to .reviews/staged.diff; claude READS it
           (Read+Bash, read-only)              →  .reviews/log-<timestamp>.md
             STATUS: CLEAN   →  (go to step 2)
             STATUS: ISSUES  →  fix agent edits the staged files,
                                re-stages them  →  (go to step 2)
        2. make test  (the engine's suite)
             pass  →  commit proceeds
             fail  →  commit aborted; output saved to .reviews/last-test.log
```

When the work also reaches a game repo (e.g. `../closure-tale`), that repo's
own gate reviews that part — one repo, one commit, one review.

## Install (per clone)

Git never auto-runs repo-supplied hooks, so each clone activates it once:

```sh
make install-hooks
```

This sets a **relative** `core.hooksPath=githooks` (local to the clone,
survives repo moves). The tracked `githooks/pre-commit` launcher then
delegates to `scripts/review/pre-commit.sh`.

## Escape hatches and tuning

Identical to the game's gate — see the table in the Closure repo's
`scripts/review/README.md`: `git commit --no-verify` skips one commit,
`CLAUDE_AUTO_REVIEW=0` disables, `CLAUDE_AUTO_FIX=0` blocks without fixing,
`CLAUDE_RUN_TESTS=0` skips the test stage; `CLAUDE_REVIEW_MODEL` /
`CLAUDE_FIX_MODEL` / `CLAUDE_REVIEW_TIMEOUT` / `CLAUDE_TEST_TARGET` /
`CLAUDE_TEST_TIMEOUT` tune it. Review logs live in `.reviews/`
(git-ignored), pruned after 30 days.
