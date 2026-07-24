# Lambda's Tale (engine)

A Bard's Tale-style dungeon-crawler **engine** in pure Common Lisp,
running on the clamiga built in the parent repo.  This is a **separate
subproject**: the instructions here override the repo-root `CLAUDE.md`
for anything under `examples/games/lambda-tale-engine/`.

## The repo-root gates do NOT apply here

This subproject contains no C.  For a change confined to this
directory, do **not** run — and do not treat as a gate:

- `make test` / `make test-plus` / `make test-extra` in the repo root
  (C unit tests, shell tests, trunk integration scripts)
- `make test-gc-stress`
- `make -f Makefile.cross test-amiga` (the parent repo's Amiga suite —
  it does not run this subproject's tests)
- the FASL versioning rule (`CL_FASL_VERSION` in `src/core/fasl.h`) and
  the GC-safety / C-coding rules — all of them are about runtime C code

The gate for a change here is this subproject's own suite:

```
make test     # engine suite, plays the tests/world/ fixture
make assets   # regenerate the default tile packs (data/gfx*)
```

Run the sibling game's suite too when a change could reach it:
`cd ../closure && make test`.

**The exception**: if the work leads you into the clamiga runtime
(`src/` in the repo root — a compiler bug, a missing CL function, an
FFI gap), that part is a repo-root change and the root `CLAUDE.md`
applies to it in full, gates included.  The engine is a good clamiga
stress test, and finding runtime bugs through it is expected; just
keep the two changes — and their gates — apart.

## Amiga testing

Not covered by the parent's `test-amiga`.  Boot FS-UAE and run the
suite by hand (it adds GUI smoke tests for both display profiles and
unattended autoplay sessions):

```
cd CLAmiga:examples/games/lambda-tale-engine
stack 128000
CLAmiga:build/amiga/clamiga --heap 8M --non-interactive --load tests/run-tests.lisp
```

`stack 128000` is required — 64K is not enough for the GUI load path.

## The engine ships no story

The hard boundary of this subproject: the engine holds **mechanics**,
a game holds **content**.  Never put story, campaign data, item or
spell tables, map content or anything Closure-specific in here — those
belong to a game directory (`../closure`).  When a game needs
something the engine cannot express, add the *mechanism* here (a new
`define-*` form, a cell-special op, an effect key) and let the game
supply the data.

The engine's own suite plays `tests/world/`, a minimal fixture world,
for exactly this reason: it must stay playable with no game present.

## Versioning

`src/version.lisp` holds the engine version (`MAJOR.MINOR.PATCH` +
`DD.MM.YYYY`).  Bump it when engine behaviour changes in a way a game
would notice.  It is **independent of the clamiga runtime version** in
`src/core/types.h` and of any game's version — the engine only
declares the `*GAME-*` slots that a game's own `version.lisp` fills in.

`+SAVE-VERSION+` in `src/save.lisp` is separate again: bump that one
only when the save-file format actually changes.

## Tests and docs

- **Tests are the specification** — the same rule as the parent repo.
  Every feature and every bug fix gets a check in `tests/run-tests.lisp`,
  using the file's own tiny harness (`check`, `check-true`,
  `check-error`, `with-rng`, `watch-messages`).
- Tests must be **tight**: exact expected values, edge cases and error
  paths, not just the happy path.
- All Lisp must conform to the **HyperSpec** — this code is also a
  conformance probe for clamiga itself.
- Keep `README.md` current with the code, user-facing and high-level;
  no changelog notes or internal detail (parent rule, unchanged).
- Test artifacts belong under `tests/` and in `.gitignore` — the suite
  must leave a clean `git status`.

## Branch

The engine lives on **master**.  The Closure game does not — it is
local-only on `closure-tale`.  When one piece of work touches both,
the engine part is a master commit and the game part a `closure-tale`
commit; do not carry engine changes on the game branch.

Playing the game is how engine gaps get found, so engine work often
starts with `closure-tale` checked out.  That is fine — commit it
there rather than switching branches mid-task — but the commit must be
**separate and marked**, so it cherry-picks to master later without
dragging game content behind it:

- **Engine paths only.** Nothing under `examples/games/closure/` in
  the same commit, and nothing from the repo root either.  A mixed
  commit cannot be cherry-picked; it has to be re-done by hand.
- **Prefix the subject `Engine: `.** That is what makes the commits to
  replay findable months later:
  `git log --oneline --grep="^Engine: " closure-tale` is the list of
  what master is still missing.
- **Self-contained.** Its own tests, README update and
  `src/version.lisp` bump ride along in the same commit, green on
  their own — the cherry-pick should need no follow-up.

If the game needs a matching change, that is a second commit on
`closure-tale` with no `Engine: ` prefix.
