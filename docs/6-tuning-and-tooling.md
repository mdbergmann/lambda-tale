# 6. Tuning and tooling

The knobs a game or a machine turns, and the seams a tool hangs off.
Everything here is a `play-amiga` argument or a special variable in
the `TALE` package; a campaign sets its taste in `campaign.lisp`, a
launch script binds around the call, and the REPL can turn most of
them live.

## Display profiles

The Amiga custom screen's geometry comes from a **display profile**,
`play-amiga`'s `:profile` argument:

| profile | screen | colours | viewport | default draw depth |
|---|---|---|---|---|
| `:lores` (default) | 320x200 layout, ECS | 32 | 120x100 | 4 |
| `:hires` | 640x256 PAL hires | 16 | 240x130 | 3 |

`:lores` is a 200-line layout so that **one layout serves PAL and NTSC
alike** — an NTSC machine has no 256-line mode to fall back on.  The
screen itself opens as tall as the display it lands on, capped by the
profile's `screen-max-height` (256): PAL opens 320x256 and NTSC
320x200, with the game laid out identically on both and only the
background below it differing.  On a machine carrying both a chipset
display and an RTG board the chipset's native mode stays in front;
[design-notes.md](../specs/design-notes.md) has the reasoning.

Both profiles give the view about **2/5** of the screen and the log
**3/5**.  The split is a profile knob: the view column is exactly the
profile's `fp-width` and the log takes the remainder, so a custom
target with a different balance is a new profile plus a matching tile
pack.  Both are picked RTG-aware through `BestModeIDA`, with the tile
pack's palette, a borderless backdrop window and an Intuition menu
strip on the right mouse button.

`:display :window` opens a window on the Workbench screen instead —
no custom palette, for development:

```lisp
(tale:play-amiga "mygame/village.map" :display :window)
```

## Draw distance (slower machines)

The first-person view draws up to four distance levels ahead.
`play-amiga`'s **`:draw-depth`** (1–4) trades some of that distance
for frames:

```lisp
(tale:play-amiga "mygame/village.map" :draw-depth 2)
```

Each level dropped is up to three fewer wall blits per frame and ten
fewer piece images (plus their style variants) decoded at load time;
the corridor ends that much nearer, fading into the backdrop.  The far
levels are the *small* ones, so the win is mostly per-blit overhead —
measure on the target rather than assuming.

It is a **rendering** cap only: a tile pack must still ship all four
depths, the automap still records everything the party could see, and
darkness still shortens the view further when it is the tighter of
the two.  Each profile carries a default (`:lores` 4, `:hires` 3);
`:draw-depth` overrides it either way.  It is the only way in —
binding `tale:*draw-depth*` around `play-amiga` has no effect, because
the profile rebinds it on the way in.

## Draw width (faster machines)

**`:draw-flanks`** (0–8, default 1) sets how many cells to each *open
side* the view draws of a facing row of houses.  The default shows the
corridor's immediate neighbours — three houses where a whole street
front stands.  Raising it repeats the already-loaded flank pieces one
cell further out per step, each with its own building's style, until
the row, the viewport edge or a nearer wall ends it:

```lisp
(tale:play-amiga "mygame/village.map" :draw-flanks 8)  ; fill the horizon
(tale:play-amiga "mygame/village.map" :draw-flanks 0)  ; the lone BT house
```

Each step costs one more blit per visible row cell per frame and
nothing else — no extra piece files, no load time.  Like draw depth it
is a rendering knob only.

## Keeping packs loaded

Loading a pack decodes a directory of ILBMs into offscreen bitmaps —
real seconds on a 14MHz 68020 — so a zone boundary between two packs
pays for the swap each way.  `tale:*gfx-cache-packs*` trades memory
for that time by keeping the pack the party just left:

| value | behaviour |
|---|---|
| `0` | never cache; reload on every swap |
| `N` | keep up to N inactive packs, least recently used evicted first |
| `:auto` (default) | keep one, but drop the cache when free memory falls below `tale:*gfx-cache-min-free*` (1 MB) |

A `:lores` pack is roughly 40K of bitmaps plus 8K of chip-RAM masks.
The cache lives inside a `play-amiga` session and is freed with it:

```lisp
(let ((tale:*gfx-cache-packs* 0))          ; a tight machine: never cache
  (tale:play-amiga "worlds/closure/town.map"))
```

`tale:*wall-load-planar*` (on by default) loads pieces through the
planar path, poking ILBM rows straight into a scratch BitMap; bind it
to `NIL` for the portable chunky path.

## The dials

| variable | default | what it sets |
|---|---|---|
| `*encounter-rate*` | 1 | scales every zone's wandering chance: `2` doubles, `1/2` halves, `nil` or `0` disables wandering monsters entirely |
| `*idle-encounter-minutes*` | 30 | idle game minutes per wandering roll while the party stands; `nil` turns idle encounters off |
| `*idle-clock-rate*` | 4 | game minutes per real second while the party stands idle (Amiga): `1` is ambient, `20` a demo, `nil` the classic clock that only moves on an action |
| `*moonlight-depth*` | 3 | cells of sight outdoors at night with no light; `1` for near-black nights |
| `*light-fade-minutes*` | 4 | minutes per cell of sight a guttering light loses |
| `*sp-regen-minutes*` | 4 | daylight minutes outdoors per spell point regained |
| `*minutes-per-action*` | 1 | clock cost of a step, a turn or a combat round |
| `*new-game-minutes*` | 480 | the clock a fresh game starts at, day 1 08:00 |
| `*poison-bite*` | 1 | hit points poison costs per step and per round |
| `*combat-group-spacing*` | 20 | feet between one group's opening distance and the next when the encounter names none |
| `*combat-close-step*` | 10 | feet a walking group covers per round; `nil` for fixed skirmish lines |
| `*combat-speed*` | 1 | transcript pace, 1 (a second per line) to `+combat-speed-max+` (instant); the `+`/`-` keys move it |
| `*victory-image*` | `nil` | the spoils picture, map-relative |
| `*victory-linger*` | 3 | seconds the victory page stays before play resumes |
| `*notice-linger*` | 3 | seconds a location's one-line notice stays before its menu returns |
| `*message-ttl*` | 60 | seconds a log line lingers on the message board |
| `*save-dir*` | `"saves/"` | where named saves live, relative to the game |
| `*default-sky*`, `*default-ground*` | | the noon colours of a zone that declares none |
| `*art-sampler*` | `:box` | how the pack generator's piece-level entry points sample a painting |
| `*gfx-cache-packs*`, `*gfx-cache-min-free*` | `:auto`, 1 MB | the pack cache, above |
| `*draw-depth*`, `*draw-flanks*` | from the profile | the draw knobs, above — set them through `play-amiga`'s arguments |

The idle clock:

```lisp
(setf tale:*idle-clock-rate* 4)    ; brisk (default): a day in ~6 real min
(setf tale:*idle-clock-rate* 1)    ; ambient: a day in ~24 real min
(setf tale:*idle-clock-rate* 20)   ; demo: a day in ~72 real sec
(setf tale:*idle-clock-rate* nil)  ; off: time only moves on an action
```

The idle clock runs the same `advance-time` a step does, so every
consequence — spell-point regen, effect expiry, `:sunrise`,
`:sunset`, `:time-band` — fires exactly as a step's would.  Its
pace is driven off real time, so tick jitter loses no sub-minute time.

## The debug log

`(tale:debug-log-enable)` opens a timestamped trace file
(`tale-debug.log` by default, or pass a path) and
`(tale:debug-log-disable)` closes it again; setting the environment
variable `TALE_DEBUG_LOG` (a path, or `1` for the default) enables it
as the engine loads.  While enabled, the engine logs every image, map
and campaign load with its duration, every emitted event with its
handler count, and every key press — each line wall-clock timestamped
with a millisecond fraction and flushed immediately, so a session that
crashes still leaves the trace up to the moment it died.  Off by
default and free when off.  Game code writes its own lines with
`(tale:dlog "..." args...)` and times a block with `tale:dlog-timed`.

The log doubles as a launch profiler: the loader logs each source
file's load time and a `launch -> loaded` summary, and `play-amiga`
marks `new-game`, `display open` and `first frame up`, each with
milliseconds since clamiga started.  Together with clamiga's
`--boot-log` one trace shows where every second between launch and
the first rendered frame goes.

## Seams for tooling

Three specials let a game hang its own tools off a running session —
a debug console, a cheat menu, a recorder — without forking a
front-end.  The engine never reads any of them, and a release build
simply never sets them.

- **`tale:*game*`** is the game the running front-end is playing, or
  `nil` outside a session.  Both `play` and `play-amiga` assign it as
  they wire a game up, for a fresh game and a restored one alike, and
  leave it standing when the session ends, so a tool can still inspect
  what the party walked away from.  It is *assigned*, never bound:
  dynamic bindings are per thread in this runtime, so a `let` would
  be invisible to any other thread.
- **`tale:*key-hook*`** is a function of `(game char)` that both
  front-ends offer every key before their own dispatch — ahead even of
  the quit confirmation, so a console stays reachable when a page has
  wedged.  A true return means the hook consumed the key: the
  front-end redraws and no page sees it.
- **`tale:*tick-hook*`** is a function of `(game)` that the Amiga
  front-end calls once per heartbeat, about ten times a second.  It is
  where work arriving from outside the event loop gets run *on the
  loop's own task*: the front-end draws from that task and the game
  state carries no locks, so a channel fed by another thread posts its
  forms here rather than evaluating them wherever they were typed.  A
  true return means the hook changed something the frame does not
  show yet, and the front-end redraws; an idle hook must return
  `nil`, because a redraw at heartbeat rate is more than a 68020 has
  to give.  The host front-end blocks on the keyboard and has no
  heartbeat, so it never calls this.

The Closure game's `src/debug.lisp` is the worked example: an in-game
Lisp console, loaded only when `:debug` is on `*features*`.

## The art tools

`make pack` and `make preview` — a whole pack from one painting, and
a pack composited into a picture — are in [Art and
sound](5-art-and-sound.md); `make assets` regenerates the engine's own
default packs.  All randomness in the engine goes through
`tale:*rng*`, so a test or a tool can script an entire fight
deterministically.
