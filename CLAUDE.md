# Lambda's Tale (engine)

A Bard's Tale-style dungeon-crawler **engine** in pure Common Lisp,
running on clamiga from the [cl-amiga
repo](../amigasources/cl-amiga) — this repo holds no C and builds no
binary.  Games live in sibling repos (e.g. `../closure-tale`) and load
the engine via `src/load.lisp`.

## The cl-amiga gates do NOT apply here

This repo contains no C.  For a change confined to this repo, do
**not** run — and do not treat as a gate — anything from cl-amiga:

- `make test` / `make test-plus` / `make test-extra` over there
  (C unit tests, shell tests, trunk integration scripts)
- `make test-gc-stress`
- `make -f Makefile.cross test-amiga` (cl-amiga's Amiga suite —
  it does not run this repo's tests)
- the FASL versioning rule (`CL_FASL_VERSION` in `src/core/fasl.h`) and
  the GC-safety / C-coding rules — all of them are about runtime C code

The gate for a change here is this repo's own suite:

```
make test     # engine suite, plays the tests/world/ fixture
make assets   # regenerate the default tile packs (data/gfx*)
```

Run the sibling game's suite too when a change could reach it:
`cd ../closure-tale && make test`.

**The exception**: if the work leads you into the clamiga runtime
(`../amigasources/cl-amiga` — a compiler bug, a missing CL function,
an FFI gap), that part is a cl-amiga change and that repo's
`CLAUDE.md` applies to it in full, gates included.  The engine is a
good clamiga stress test, and finding runtime bugs through it is
expected; just keep the two changes — and their gates — apart.

## Amiga testing

Not covered by cl-amiga's `test-amiga`.  The FS-UAE setup lives in
cl-amiga (`verify/realamiga/`); this repo is not inside the mounted
`CLAmiga:` volume, so mount it as its own volume on the command line
and run the suite by hand (it adds GUI smoke tests for both display
profiles and unattended autoplay sessions):

```
cd ../amigasources/cl-amiga
verify/realamiga/FS-UAE.app/Contents/MacOS/fs-uae verify/realamiga/verify.fs-uae \
  --hard_drive_2="$HOME/Development/MySources/lambda-tale" \
  --hard_drive_2_label=LambdaTale
```

(Without a `build/amiga/boot-override` file the boot runs cl-amiga's
own suite first — see `verify/realamiga/call-on-ustartup`; drop an
override or wait for it, then in the Amiga shell:)

```
cd LambdaTale:
stack 128000
CLAmiga:build/amiga/clamiga --heap 8M --non-interactive --load tests/run-tests.lisp
```

`stack 128000` is required — 64K is not enough for the GUI load path.

## The engine ships no story

The hard boundary of this repo: the engine holds **mechanics**,
a game holds **content**.  Never put story, campaign data, item or
spell tables, map content or anything Closure-specific in here — those
belong to a game repo (`../closure-tale`).  When a game needs
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

- **Tests are the specification** — the same rule as cl-amiga.
  Every feature and every bug fix gets a check in `tests/run-tests.lisp`,
  using the file's own tiny harness (`check`, `check-true`,
  `check-error`, `with-rng`, `watch-messages`).
- Tests must be **tight**: exact expected values, edge cases and error
  paths, not just the happy path.
- All Lisp must conform to the **HyperSpec** — this code is also a
  conformance probe for clamiga itself.
- Keep `README.md` current with the code, user-facing and high-level;
  no changelog notes or internal detail (cl-amiga's rule, unchanged).
- Test artifacts belong under `tests/` and in `.gitignore` — the suite
  must leave a clean `git status`.

## Repos, not branches

The engine is **its own repo** since 2026-07-24; before that it lived
in cl-amiga under `examples/games/lambda-tale-engine/`, carried on the
local `closure-tale` branch with an `Engine: ` commit-subject prefix
for cherry-picking to master.  That scheme is over: engine commits
land here, game commits land in `../closure-tale`, clamiga commits
land in `../amigasources/cl-amiga` — one repo per commit, no prefixes,
no cherry-picking.  When one piece of work touches more than one of
the three, split it by repo; each part rides with its own tests (and
here, its `src/version.lisp` bump) so it is green on its own.

A game repo pulls its dependencies in as **submodules**: Closure
vendors this engine at `lambda-tale/` and the runtime at `cl-amiga/`
inside its own tree.  After an engine change lands here, the game
advances its submodule pin in a game commit.
