# 1. Getting started

Lambda's Tale is a dungeon-crawler engine in pure Common Lisp.  It
runs on clamiga, the cl-amiga runtime: on the host for development,
on AmigaOS for play.  The engine ships no story — a game is a
directory of maps plus a campaign file — and this chapter takes you
from a fresh checkout to a world of your own.

## What you need

A clamiga binary.  Build it in the cl-amiga checkout with `make host`.
The engine's Makefile looks for it at `../cl-amiga/build/host/clamiga`
first — the sibling submodule inside a game repo — and then at
`../amigasources/cl-amiga/build/host/clamiga`, the development clone;
`CLAMIGA=/path/to/clamiga` points it anywhere else.  `HEAP` (default
`16M`) is the heap the targets hand clamiga.

## The make targets

| target | what it does |
|---|---|
| `make test` | runs the engine suite, which plays the fixture world in `tests/world/`.  Any `ERROR` line in the output fails the run even when the failure count is zero, because a top-level form that signals is skipped by the loader and its checks never run. |
| `make assets` | regenerates the default tile packs under `data/gfx/` and `data/gfx-hires/` from `tools/gen-walls.lisp` |
| `make pack ART=art/house.iff OUT=data/gfx-town/` | derives a whole tile pack from one front-on picture of a wall — see [Art and sound](5-art-and-sound.md) |
| `make preview PACK=data/gfx-town/ OUT=street.iff` | composites a pack into one picture, the way the Amiga draws it |
| `make install-hooks` | activates the repo's pre-commit review hook |

## Walk the fixture world

The fixture world is a keep with a shop and a tavern, and a dark
crypt under it.  On the host:

```
../cl-amiga/build/host/clamiga --heap 16M --load src/load.lisp \
    --eval '(tale:play "tests/world/keep.map")'
```

The host front-end is an ASCII walkabout: it reads raw keys when the
terminal allows and falls back to line input otherwise.  It draws the
wireframe view, the automap and every page; it plays no sound and
ignores animation frames.  [Playing](2-playing.md) lists the keys.

## On the Amiga

The engine repo is not inside the mounted `CLAmiga:` volume, so mount
it as a volume of its own when launching FS-UAE from the cl-amiga
checkout:

```
cd ../amigasources/cl-amiga
verify/realamiga/FS-UAE.app/Contents/MacOS/fs-uae verify/realamiga/verify.fs-uae \
  --hard_drive_2=/path/to/lambda-tale \
  --hard_drive_2_label=LambdaTale
```

Without a `build/amiga/boot-override` file the boot runs cl-amiga's
own suite first (see `verify/realamiga/call-on-ustartup` there); drop
an override or wait for it.  Then, in the Amiga shell:

```
cd LambdaTale:
stack 128000
CLAmiga:build/amiga/clamiga --heap 8M --non-interactive --load tests/run-tests.lisp
```

`stack 128000` is required — 64K is not enough for the GUI load path.
On AmigaOS the suite additionally runs GUI smoke tests for both
display profiles and three unattended autoplay sessions through the
fixture world.

To play rather than test, load the engine from a clamiga session and
call the Amiga front-end:

```lisp
(load "src/load.lisp")
(tale:play-amiga "tests/world/keep.map")                    ; own screen
(tale:play-amiga "tests/world/keep.map" :display :window)   ; a Workbench window
```

The custom screen is the real thing — pack palettes need it.  The
window view keeps the Workbench palette and is for development.
[Tuning and tooling](6-tuning-and-tooling.md) covers the display
profiles and the draw-distance knobs `play-amiga` takes.

## Loading the engine from a game

A game loads the engine from wherever it lives and names its starting
map.  The engine is **self-locating**: it finds its own sources and
default tile packs through `*load-truename*`, never through the
working directory.  The working directory belongs to the game — maps,
campaigns, zone packs and saves all resolve there, or relative to
their map file.

```lisp
(load "lambda-tale/src/load.lisp")     ; the engine, vendored as the game
                                       ; repo's lambda-tale/ submodule
(tale:play "mygame/village.map")       ; host front-end
(tale:play-amiga "mygame/village.map") ; AmigaOS front-end
```

Both front-ends load the `campaign.lisp` beside the map they start on,
so a world brings its own classes, monsters and items.  Closure's
`src/load.lisp` is the worked example of a game that self-locates the
engine the same way.

## A world of your own

A world is a **directory**: map files plus a `campaign.lisp` beside
them.

```
mygame/campaign.lisp     (define-item 'rusty-dagger :kind :weapon
                           :price 5 :damage "1d4") ...
mygame/village.map       the art, then:
                         (zone :kind :city :title "Frogmorton"
                               :gfx "gfx/")   ; the world's own pack
                         (special (3 7)
                           (location "Bree's Bargains" :shop
                                     :stock (rusty-dagger torch)))
                         (special (9 2) (travel "warrens.map"))
mygame/warrens.map       (zone :kind :dungeon :title "the warrens") ...
mygame/gfx/*.iff         optional zone tile pack (see the manifest)
```

```lisp
(tale:play "mygame/village.map")
```

Everything is data read with `*read-eval*` bound to `NIL`, except
`campaign.lisp`, which is a Lisp file of `define-*` calls.  The
fixture campaign in `tests/world/campaign.lisp` is the smallest
possible one; Closure's `worlds/closure/campaign.lisp` is the
full-size template.  [Worlds](3-worlds.md) is the map file,
[Campaigns](4-campaigns.md) the campaign file.

## The game's version

The engine has a version of its own and declares three slots for the
game's:

```lisp
(tale:engine-version-string)   ; => "0.50.0" — whatever src/version.lisp says
(tale:engine-version)          ; => 0, 50, 0   (three values)
tale:*engine-name*             ; => "Lambda's Tale"
tale:*engine-version-date*     ; => "02.09.2026"
```

A game's version moves independently of the engine's.
`tale:*game-name*`, `tale:*game-version*` and
`tale:*game-version-date*` are `NIL` until the game's own
`version.lisp`, loaded after the engine, fills them in:

```lisp
(in-package :tale)
(setf *game-name* "Closure" *game-version* "0.1"
      *game-version-date* "24.07.2026")
```

`../closure-tale/src/version.lisp` is the worked example.
