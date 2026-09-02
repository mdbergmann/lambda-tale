# 5. Art and sound

On the Amiga the first-person view is drawn with **blitted wall
graphics**: every wall the view can show falls into a fixed Bard's
Tale-style screen slot — front walls, receding side walls, walls seen
through open sides, each with a door variant, at four depths — and
each slot is filled by a pre-rendered bitmap piece.  The pieces are
**IFF ILBM** files, one pack per display profile: `data/gfx/` for
`:lores` and `data/gfx-hires/` for `:hires`, each profile's viewport
dictating the piece sizes.  The default packs are drawn by the
procedural generator in `tools/gen-walls.lisp` (`make assets`), and
the test suite compares the checked-in files pixel for pixel against
the generator, so art and code cannot drift apart.

Each frame draws the ceiling/floor backdrop first, then composites the
walls back to front over it; the receding side pieces are cookie-cut
through a mask, transparent where the piece uses pen 0, so the
backdrop shows through the corners they do not cover.  The path is
RTG-safe — no chipset or planar assumptions, so it works unchanged
under Picasso96, CyberGraphX and MorphOS.  When the assets are missing
the view falls back to the wireframe renderer.

The host front-end draws the wireframe only; every picture, pack and
sound in this chapter is the Amiga's.

## Tile packs

A **tile pack** is a directory of IFF ILBM files.  A zone declares its
own pack in its map file — `(zone :kind :city :gfx "gfx/")` — and
travel swaps packs as the party crosses zones.  The directory resolves
relative to the map file when the pack lives in the world directory,
else relative to the game directory.  `play-amiga`'s `:gfx-dir`
argument overrides everything for the session; without either, the
active profile's own pack applies.  A pack is drawn for one profile's
viewport; a mis-sized pack is rejected at load time with a message
naming the file and both sizes, and the view falls back to the
wireframe.

`(tale:print-tile-manifest)` prints the full contract — every
filename with its exact pixel size for the **active profile** (wrap it
in `tale:with-display-profile` for another one) — so custom art can be
drawn to spec.  A pack must always ship the full set for all four
depths, whatever draw distance the machine that built it used.

A pack holds the 40 wall pieces plus optional extras:

- **Style variants** — `front-0-v1.iff`, `side-2-l-v2.iff`, … —
  per-building looks for any wall piece, probed in order (`-v1`,
  `-v2`, …) until one is missing.  The view deals them out
  deterministically **per building**: one walled-in mass of cells is
  one house and wears one look all the way along its front, so a
  street reads as a row of *different* houses.  A `(location ...
  :style N)` op anywhere in the mass pins that building's look.
  Variants are per piece, so trimming the far depths' files is a
  valid budget cut.
- **The backdrop** — `floor.iff` / `ceiling.iff` and `ground.iff` /
  `sky.iff`: one image fills the view above the horizon, one below,
  and the walls blit on top.  A zone takes **one** pair — `ceiling`
  and `floor` when it is `:dark`, `sky` and `ground` when it has a
  sky.  A missing file leaves a flat fill — pens 5 and 6 in the open,
  black underground.  **Put the ceiling or sky on pen 5 and the floor
  or ground on pen 6**: only those two pens follow the day bands and
  the zone's `:sky`/`:ground` colours, so whatever should darken with
  the hour or recolour per zone must *be* one of those two pens.  A
  dungeon pack laid out this way can be recoloured per zone for free,
  one pack dressing many dungeons.  The engine's demo packs predate
  that contract and do not keep it; Closure's
  `worlds/closure/make-dungeon-packs.lisp` is the worked example that
  does.
- `palette.iff` — any ILBM whose CMAP provides the pack's colours.
  Only the pens a pack owns are read from it (see the pen contract);
  entries for the engine's pens are ignored, so a stale palette cannot
  recolour the UI or the monsters.
- `pointer.iff`, `pointer-click.iff`, `pointer-forward.iff`,
  `pointer-back.iff`, `pointer-turn-left.iff`,
  `pointer-turn-right.iff` — optional mouse-pointer art: the neutral
  pointer, the one shown over a click target, and the four directional
  cursors shown over the view's walk zones.  At most 16 pixels wide, a
  hardware sprite, pens 0–3 only: pen 0 transparent, pens 1–3 shown as
  screen colours 17–19 taken from `pointer.iff`'s CMAP entries 1–3
  (all pointers share one palette).  The hot spot is the
  topmost-leftmost inked pixel.  Missing files get the engine's
  built-in art.

The generator ships two wall styles: the dungeon brick the default
packs are drawn with (`draw-wall-piece`) and a **city house style**
(`draw-city-wall-piece`, `:style :timber`, `:stone` or `:townhouse`)
that renders every piece as a timber-framed house.  A city pack using
it must carry `*house-colors*` in pens 7–9 of its palette; Closure's
`worlds/closure/gfx/make-pack.lisp` is the worked example.

Pack art is plain IFF ILBM — planar, ByteRun1-compressed, exactly what
Deluxe Paint and friends read and write — so pieces can be edited in
any Amiga paint program.

## The pen contract

The game runs on one screen with **one palette**, and walking into
another zone swaps the tile pack under it.  A bitmap is nothing but
pen indices, so an image loaded in one zone and still cached in the
next re-colours the moment the new pack's CMAP lands.  The screen's
pens are therefore split in two (`src/palette.lisp` is where the line
is drawn):

| pens | owner | |
|---|---|---|
| 0 | engine | transparent key in wall pieces |
| 1–3 | engine | UI white, grey, amber |
| 4 | engine | opaque black |
| 5–6 | **pack** | sky / ceiling, ground / floor |
| 7–16 | **pack** | art |
| 17–19 | engine | mouse-pointer sprite registers |
| 20–23 | **pack** | art |
| 24–31 | engine | the shared **figure core** |

Engine pens hold the same colour in every zone.  Pack pens are the
pack's own and change under the player's feet on zone travel, which
is the point: the night street and the cellar are the same pens in
different colours.  Only pack pens are loaded from a pack's
`palette.iff`.

**A fixed pen is shared, not lost.**  A wall may paint in the figure
core freely — the quantizer is offered it — it just cannot
*re-colour* it.  So a pack has 16 pens of its own plus 12 more to draw
with.

`:hires` is 16 colours, has no pen 24, and so has no figure core and
no pointer pens: 0–4 engine, 5–15 pack.  It is a **wall-pack target
only**; `:lores` is where the contract lives and where new art should
be drawn.

Pack colours need `:display :screen` — a window on the Workbench
screen keeps the Workbench palette.

**Transparency:** in a *wall* piece **pen 0 is transparent** — the
backdrop shows through it.  Paint solid black inside a wall (mortar,
joints, door frames) with **pen 4**, not pen 0.  The backdrops are
opaque, so pen 0 there is plain black.

The Closure game ships worked examples, all declared map-relative and
all generated: `worlds/closure/gfx/`, the town's painted houses under
a day-banded sky, and three dungeon packs — `gfx-rubble/`,
`gfx-ashlar/`, `gfx-rock/` — one painted stone each over the same
flagged floor and ceiling, from one `make-dungeon-packs.lisp`.

## Packs from one painting

Drawing 40 pieces to spec by hand is a lot of work, and the procedural
generators only make the looks they were coded for.  The third route
is `tools/gen-pack-from-art.lisp`: give it **one flat, front-on
picture of a wall** and it derives the whole pack.

```
make pack ART=art/house.iff OUT=data/gfx-town/
make preview PACK=data/gfx-town/ OUT=street.iff
```

The source can be an IFF ILBM of any size and depth — indexed art is
expanded through its CMAP, 24/32-bit "deep" ILBMs are read directly —
or a **binary PPM (P6)**, the bridge for art that was never an Amiga
file:

```
ffmpeg -i house.png -pix_fmt rgb24 house.ppm
```

The format is sniffed, not guessed from the name.  `write-deep-ilbm`
turns any of them back into a 24-bit IFF, so art that arrived as a PNG
becomes a source you can keep editing in DPaint.

**Sampling.**  Front and flank slots are rectangles cut at the front
slot's scale; side slots are the trapezoid between two perspective
planes.  How a piece samples the painting is the `:sampler` argument
(`tale:*art-sampler*` for the piece-level entry points):

- **`:box`** (the default) averages the source pixels each screen
  pixel covers.  Soft, and it keeps thin lines from dropping out —
  right for a painting richer than the slots.  Its natural source size
  is the viewport: 120×100 at `:lores`, 240×130 at `:hires`.
- **`:point`** takes the one source pixel under each screen pixel's
  centre.  Hard-edged, the way a hand-cut pack looks.  Paint at the
  nearest **front** slot for it — `(wall-piece-rect planes '(:front
  0))`, 100×84 at `:lores` and 202×110 at `:hires` — and the wall the
  party stands before is the painting pixel for pixel; only the far
  pieces are sampled.

The nearest wall fills 84% of the view — Bard's Tale's eye: the own
cell's side walls a sliver at each edge, a thin band of roof above
and floor below — the next 45%, then 25% and 13%
(`tale:*plane-fractions*`).

**More than one house.**  `:variants` takes further wall pictures,
each a whole extra look, written as the `-v1`, `-v2`, … files the
view deals out per building.  `:style N` on a location pins a
building's look, counting the base source as 0.  `:pictures` adds
location pictures to the same quantization:

```lisp
(generate-pack-from-art "art/house-1.iff"
  :out "worlds/closure/gfx/"
  :variants '("art/house-2.ppm" "art/house-3.ppm" "art/house-4.ppm")
  :pictures '(("art/house-1.iff" . "house-1.iff")))
```

Because a pack is **one shared CMAP** and pictures blit with the live
screen palette rather than their own, every source — base, variants
and pictures — is quantized *together*, which is what makes a shop's
takeover art belong to the same street it stands in.  The pens are a
budget: four looks sharing the pack's colours get noticeably less
each than one look with all of them.  The pen layout follows the
contract above, with the pointer's pens and the figure core held
back: **14 art colours at `:lores`**, 9 at `:hires`.  Quantization
works on the 12-bit grid the screen can actually show, and no two art
pens may land on the same screen colour or duplicate a fixed one.

Every pack gets a **`palette.gpl`** beside its `palette.iff`: the
same colours as a GIMP palette, which GIMP, Aseprite, Krita and
Inkscape all import, each entry named with its pen number and role
and marked `[FIXED]` where the engine owns it.  Draw the next house
*against* that palette, then pass the pack's `palette.iff` back as
`:palette-source` and quantization becomes a lossless lookup:

```lisp
(generate-pack-from-art "art/house-5.ppm"
  :out "worlds/closure/gfx/"
  :palette-source "worlds/closure/gfx/palette.iff")
```

Every piece kind has a `-door-` twin.  By default both come from the
same source, which suits a dense street whose facade already has a
door in it; pass `:door-source` to give the door pieces their own art.

`tools/preview-view.lisp` composites a pack the way the Amiga front
end does — same blit list, same order, same cookie-cut rule — and
hands back an ILBM, so art can be judged on the host without booting
an emulator.  `make preview` renders a fixture street that shows every
piece kind at more than one depth.  It is a preview, not a second
renderer: where it and the Amiga disagree, the Amiga is right.

## Figures: art that travels between zones

A monster portrait, a hero portrait or an effect icon is **not part
of any pack**.  It is cached by the path it was loaded from and keeps
rendering after the player walks into a zone with a different
palette, so it must be drawn in pens no pack can move: **pen 0** for
transparency, plus **1–4 and 24–31** — twelve solid colours and a
cookie-cut key.

`generate-figure` is the build step, and it *enforces* that rather
than trusting it:

```lisp
(tale::generate-figure "art/skeleton.png" "worlds/closure/gfx/skeleton.iff"
                       :transparent '(255 0 255))
```

`:transparent` names the source colour meaning "nothing here"; those
pixels become pen 0.  Every other pixel is quantized into the figure
palette, and the written image is audited pen by pen: a pixel on a
pack pen is an **error** naming the file, the pen, its role and the
coordinate.

The figure core is eight hand-picked constants, deliberately **not**
derived from the art that uses it: fixed means a new monster is a new
file and nothing else moves.  Given white, grey, amber and black from
the UI pens, the core covers what hangs off a figure — a three-step
flesh ramp (also wood and leather), two steels (armour and cold
shadow), a red and a green (cloth, blood, scales, slime), and a bone
highlight.

## Pictures a world names

| what | where it is named | shown |
|---|---|---|
| a location's picture | `(location ... :image FILE)` | in the view column while its page is up |
| a location's facade | `(location ... :facade FILE)` | when the party faces its door from the street; the `:image` shows there without one |
| a hero class's portrait | `(define-hero-class ... :image FILE :image-woman FILE)` | beside the character sheet; stamped onto the hero at creation |
| a race's portrait | `(define-race ... :image FILE)` | optional |
| a monster's portrait | `(define-monster ... :image FILE)` | in the view column for as long as the fight lasts |
| an effect's icon | `:image FILE` on a timed spell, a song or a usable item | in the effect strip while the effect burns |
| the spoils | `tale:*victory-image*` | in the view column when the last foe falls |

All of them resolve relative to the map file.  `tools/gen-walls.lisp`
draws placeholder scenes and portraits (`draw-location-scene`,
`draw-portrait`, `draw-monster-portrait`); Closure's
`worlds/closure/gfx/make-pack.lisp` shows how a world generates and
ships them.

## Animated images

Any image the view column or the effects band shows may ship
**animation frames** beside it: `mon-kobold.iff` is frame 0,
`mon-kobold-f1.iff`, `mon-kobold-f2.iff`, … the frames after it,
probed in order until one is missing.  Frames must match the base
image's size and follow the same pen contract; the placeholder
generators take a frame argument and draw a two-frame pulse.

On the Amiga the frames cycle in place at about three steps a second,
never through a full redraw.  At load time the frames are diffed
against the base and only the rectangle where they actually differ is
re-blitted per step, so a portrait that only moves its eyes costs an
eyes-sized blit.  Identical frames are dropped at load; a mis-sized
frame is a loud error like any mis-sized pack piece.  The host
renderer ignores frames entirely.

## Sound packs

Sound works like tiles: the engine names the cues, a **sound pack**
ships them.  The vocabulary covers the moments the game already
announces —

`hit`, `slay`, `miss`, `hurt`, `death`, `combat`, `victory`,
`defeat`, `door`, `blocked`, `cast`, `song`, `level`, `coin`, `drink`

— and a pack is a directory of IFF **8SVX** samples named after them
(`hit.8svx`, `door.8svx`, …), declared per zone in the map right
beside the tile pack:

```lisp
(zone :kind :city :gfx "gfx/" :sfx "sfx/")
```

A pack may ship any subset; missing cues stay silent.  `hit.8svx` is
the pack's probe file, and the directory resolves beside the map file
first, then relative to the game directory.  On the Amiga the samples
are uploaded to chip RAM once and one `audio.device` channel plays
them, a new cue cutting the one still sounding; travel swaps sound
packs exactly like tile packs.  `read-8svx` and `write-8svx`
round-trip the format, uncompressed and Fibonacci-delta, so asset
generators need no second toolchain.

A location may also name a **tune** — `(location ... :music
"sfx/guild-theme.8svx")`, resolved relative to the map file like
`:image`.  It loops on a second, quieter channel for as long as the
party stands inside (a save loaded inside the location picks it
straight back up) and falls silent at the door; the cue channel keeps
speaking over it.

The host front-end stays silent.  `*sound-backend*` is the single
hook, so a host player or a test can install its own.
