# Design notes

Reasoning that used to live in the README.  It is kept here because
it explains *why* a thing is the way it is, not what a user does with
it; each note names the manual chapter that states the resulting
behaviour.

## The lores layout budget

(Behaviour: [Tuning and tooling](../docs/6-tuning-and-tooling.md),
"Display profiles".)

`:lores` is a **320x200 layout** in **32 colours**, the ECS target:
half the chip-RAM/DMA cost of hires and near-square pixels for the
art.  200 lines rather than PAL's 256 so **one layout serves PAL and
NTSC alike** — an NTSC machine has no 256-line mode to fall back on.
That budget sizes the 120x100 viewport: the chrome pads, view, plaque
and the seven solid-set roster rows fill it exactly — roster row 7
ends on the last usable line, with the chrome ring's clearance below
it.  The effect strip at the message column's foot is 20 pixels
bought from dead seams in the layout — page slack, flush strip bottom
— so the message page above it still clears eleven rows of text:
enough that the shop's and the character sheet's last option stays on
the page.

## Screen height and display mode

The **screen** is a separate question from the layout, and it grows
to fit the display it landed on: `play-amiga` asks the display
database how tall the chosen mode really is
(`amiga.intuition:display-mode-height`) and opens the screen at that
height, capped by the profile's `screen-max-height` (256 here).  So
PAL opens 320x256 and NTSC 320x200, with the backdrop window clamped
to the 200-line layout either way — the game lays out identically on
both, and only the background below it differs.  There is no PAL/NTSC
test in the engine; the database answers for whatever the machine
actually is, RTG included.  A screen shorter than its display is one
an emulator's auto-zoom crops and rescales and a real monitor
letterboxes, which is the point of asking.

Which display it lands on is a third question, and the profile's
`screen-max-height` answers that one too: `BestModeIDA` is asked for
**256** rows here, not the layout's 200.  It matches on the size it
is handed, and on a machine carrying both a chipset display and an
RTG board 320x200 is an *exact* hit on an RTG mode — so asking for
the layout would put the game on the graphics card, where its pixels
are resampled to the monitor instead of shown.  Asking for the
tallest display the profile would accept keeps the native mode in
front there and changes nothing elsewhere: a machine with no 320x256
returns its own best fit, which the clamp above brings back to the
layout.

## How a pack loads

(Behaviour: [Art and sound](../docs/5-art-and-sound.md); the
`*wall-load-planar*` knob is in [Tuning and
tooling](../docs/6-tuning-and-tooling.md).)

On the Amiga the pieces load **without ever becoming chunky**: an
ILBM plane row and an Amiga BitMap plane row have the same layout, so
the rows are poked straight into a scratch planar BitMap and the
blitter moves them into the piece's own display-format bitmap
(`tale:*wall-load-planar*`, on by default; bind it to `NIL` for the
portable chunky path through `WriteChunkyPixels`).  The cookie-cut
mask comes from the same rows — for the usual pen-0 transparent key
it is just the OR of the planes, folded once and reused both to
decide whether a piece needs a mask at all and as the chip-RAM mask
plane.  Location pictures, portraits and effect icons load through
the same planar recipe.  All the per-byte and per-row work runs at C
speed on clamiga: the file arrives through the bulk `read-sequence`
path, the whole `BODY` is decoded in a single `ext:unpack-byterun1`
call, each plane's rows are gathered out of that interleaved buffer
with one `ext:copy-rows` call per plane (the pure-CL loops are kept
as the portable fallbacks), and the mask fold is a `map-into
#'logior` over the packed plane bytes.

`read-ilbm` (chunky pens) remains the general reader and is what the
host renderer, the pointer sprites and the asset generator use;
`read-ilbm-planar` is the Amiga load path.  The two are cross-checked
pen for pen in `tests/run-tests.lisp`, and the Amiga suite blits a
loaded piece back off the screen to confirm it carries the pens its
ILBM declares.

The rendering path is **RTG-safe** — no chipset or planar
assumptions: pieces are uploaded once into `AllocBitMap` bitmaps in
the display's native format, and the walls are composited over the
backdrop with `BltMaskBitMapRastPort`, transparent where the piece
uses pen 0.

## The menu model

(Behaviour: [Playing](../docs/2-playing.md), "Keys".)

Menu option rows carry their pick key (`menu-option` /
`menu-numbered` in `src/events.lisp`), so front-ends map clicks to
keys without parsing the text.  A page emits **one option per row**,
the Bard's Tale look; a page with fewer rows than that needs squeezes
in order — spacer rows go first, then the options themselves **pack
onto shared rows** (`fit-menu-lines`), each keeping its own click
target on the packed row (`menu-line-spans`).  So a short page loses
its blank lines and its vertical listing before it loses an option,
and the last row — the sheet carousel's `NEXT`, the shop's `Pool
gold` — stays on the page and stays clickable.

A menu list deeper than a page — a big shop stock, a full pack on the
sell page or the character sheet, a fat spell book — **scrolls**:
`u`/`d` (or the scrollbar at the page's right edge — a click above
the thumb is a window up, below it down) move the window and digits
pick within it (`menu-window` in `src/events.lisp`).  Because those
digits are the keys, they count from `1` again in every window — so a
scrolled list says which entries it is showing, `Spells: 9-16 of 24`,
on the head standing over it where the column can hold both and on a
row of its own where it cannot (`menu-scroll-head`).  Without it the
second window of a long book reads as the same eight entries over
again.

The menu strip is data — `*menu-strip*` in `src/keys.lisp`, where
the host suite checks both its layout and the pick decode; the Amiga
front-end turns it into the GadTools `NewMenu` array.  No item shows
a shortcut: Intuition can only ever offer right-Amiga+key there, and
the game's own keys are `Shift-S`, `Shift-L`, `Q`, `M`, `H`, `C`, `P`
and `U`, so the help page stays the one place that says what they
are.

## Notices and banners

(Behaviour: [Worlds](../docs/3-worlds.md), "Locations".)

A location page's title banner drops its closing `***` when the full
`*** TITLE ***` would overflow the narrowest takeover column — the
ornament may not cost the page a row.  For the same reason a location
model may answer a key with `(:notice TEXT)` instead of a note row: a
sentence wider than that column would wrap, and a wrapped row on a
full page costs the menu its spacer and packs its options onto shared
rows, so the page visibly reflows around the answer.  The instruction
puts TEXT alone under the banner (`notice-lines`), holds it
`*notice-linger*` seconds — the Amiga front-end; the host speaks it
through the log — and then the menu returns exactly as it was.

## The figure core is fixed

(Behaviour: [Art and sound](../docs/5-art-and-sound.md), "Figures".)

The figure core is eight hand-picked constants, deliberately **not**
derived from the art that uses it: a core computed over the bestiary
would mean monster #17 changes the core and every pack in every world
goes stale.  Fixed means a new monster is a new file and nothing else
moves.  The pen audit in `generate-figure` runs on the host, where a
per-pixel scan is free; a 68020 doing it at load time would not be.

## The type

The overlay pages, the takeover and the whole map page set the same
face as the log — the engine's 5x7-on-6px condensed bold cut, the
metrics of the actual Bard's Tale II text — so the Amiga UI carries
one type size throughout.  The roster's rows are set solid — one
glyph box each, no leading — the way a printed roster reads.

## Milestones

The engine grew in milestones: M0 the map model, movement, automap
knowledge and the host walkabout; M1 the wireframe first-person view;
M2 the events and cell-specials story layer, the party, round-based
combat and save games; M2.5 the Bard's Tale screen layout, large maps
and the RTG-aware custom screen; M3 ILBM loading and blitted wall
graphics; M3.5 swappable tile packs, per-pack palettes, the backdrop
and the display profiles; M4 the game proper — the separate Closure
project — with zones, travel, shops, day and night, spells, songs,
named saves and sound landing as it needed them.  The milestone tags
survive in a few source comments and in `specs/ui-and-engine.md`.
