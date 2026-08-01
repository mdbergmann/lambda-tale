# Lambda's Tale — UI, map-scale and platform spec

Design constraints for the game from M2.5 onward.  The test suite
(`tests/run-tests.lisp`) is the executable form of this spec; when the
two disagree, fix one of them.

## Engine first

Lambda's Tale is an **engine**; the shipped campaign is one instance of
it.  Cities, dungeons, shops, items, monsters and story are all
**first-class, data-driven concepts** — the engine never hard-codes a
story fact, a map, a shop inventory or an item.  Anything M4 and later
adds (towns, shops, sound, spells) must land as engine mechanics plus
campaign data, never as code that knows about "the" town.

## The world: zones and travel (M4)

- A game world is a set of **zones** — ordinary map files linked by
  travel.  A map file self-describes through a `(zone ...)` data form
  after the art: `:kind` (`:dungeon` default, `:city`, ... — an open
  set), `:title` (display name), `:wrap`, `:start-facing`.
- **Cities and dungeons are the same thing to the engine**: maps with
  walls, doors, specials.  `:kind` is data for campaigns and front-ends
  (per-zone tile packs via the ZONE form's :gfx), not an engine branch.
- The special op `(travel FILE [X Y] [FACING])` moves the party to
  another zone: stairs, city gates, portals are all map data.  `FILE`
  resolves relative to the current map's directory; the party arrives
  at `(X,Y)`/`FACING` or the target's start.  Ops after a `travel` in
  the same special are skipped (they belong to the cell just left).
- The game keeps **every visited zone's map and automap knowledge
  alive** (`game-zones`); traveling back restores both.  `:enter-zone`
  is emitted on arrival; travel is depth-capped like teleport chains
  and refused during combat.
- Save games (v2) record the current zone plus **all zones' automap
  knowledge**; other zones reload lazily from their map files when the
  party travels back.
- **A world is a directory**: map files plus a `campaign.lisp` beside
  them.  `load-campaign` (called by both front-ends) loads the
  campaign next to the starting map, so a designer's world brings its
  own classes, monsters and items — never the demo's.

## Locations, items and shops (M4)

- A **location** is an enterable place on a cell — a building in a
  city, a hut in a dungeon: the special op
  `(location TITLE KIND ARG...)`.  Stepping onto the cell enters it;
  the game gains a modal location state (like combat: no walking until
  `leave-location`), `:enter-location`/`:leave-location` frame it.
  Leaving is the Bard's Tale exit: a location entered through a door
  steps the party back out onto the cell it came from, facing away
  from the building (a location entered without a step — travel, a
  script — leaves the party where it stands).  Kinds without engine
  mechanics (`:house` and friends) show the interior notice with a
  lone clickable **EXIT**.
  KIND is an open set; the engine ships `:shop`, `:tavern`, `:temple`
  and `:energy` mechanics, campaigns script other kinds via events.
- A **temple** (`:temple`, 2026-07-26) heals for gold: `:price` gold
  per missing hit point (default 2) plus a flat `:raise` fee (default
  50) to bring a fallen hero back.  Its menu asks twice (2026-07-29):
  *Who wishes healing?* — a digit per hurt hero — then *Who will
  pay?* — any purse in the party, the patient's own included.  A purse
  short of the whole job buys what it can, wound by wound (a raising
  asks the fee plus the first wound); one that buys nothing leaves
  **Not enough Gold** as the menu's last line
  (`temple-lines`/`temple-act` over a `temple-view`, the shop's
  stateful shape — front-ends get the right view per kind from
  `make-location-view`).  An **energy fount** (`:energy`) refills a
  caster's spell points at `:price` gold apiece (default 3), living
  casters only (`energy-lines`/`energy-act`).  Both emit `:coin` on
  payment, plus `:temple-heal`/`:energy-restored`.
- **Items are campaign data** (`define-item`): kind (`:weapon` /
  `:armor` / `:shield` / `:misc`), price, damage dice, AC bonus
  (descending AC — the bonus subtracts), optional class restrictions,
  optional player-facing `:description` — the pack page's `i` opens
  the **item card** (facts plus that text; `item-card-lines`).
- Heroes carry up to **8 items** (`+inventory-limit+`) and equip **one
  weapon, one armor, one shield**; combat uses the equipped weapon's
  dice and the equipment-adjusted AC (`hero-attack-dice`,
  `hero-effective-ac`).  Packs and equipment live in save games.
- A **shop** is a location with `:stock (ITEM-NAME...)` — unlimited
  stock, Bard's Tale style; it buys anything back at half price.
  Freshly bought equipment auto-equips when the slot is free and the
  class allows it.
- Any location may keep **hours**: `:closed :night` (a day-band, or a
  list of bands) keeps the door shut through them — the party is told
  and an entering step bounces back onto the street, facing the door.
- The shop interaction is modeled **platform-free** in the engine
  (`shop-view` / `shop-lines` / `shop-act`): both front-ends feed keys
  into the same model and draw the same text lines, so the whole flow
  is testable on the host.  Keys: `1`-`7` pick the shopping hero,
  `1`-`9` buy/sell, `s`/`b` flip the page, `i` inspect stock (the
  item card, before any gold is spent), `Esc` back/leave.
- Gold moves both ways (2026-07-26): `p` (sheet and shop pages) pools
  the party's gold onto one hero (`pool-gold`), and the sheet's `t`
  trades it back out — pick who receives, type the sum (digits,
  Backspace, Return — the save menu's text-entry manners), Esc backs
  out a page at a time (`trade-view` / `trade-lines` / `trade-act`,
  `trade-gold` underneath).  The pack page's `t` throws an item away
  for good behind a clickable yes/no (`discard-item`) — the one pack
  action that destroys, hence the only one with an are-you-sure.
- Menu lines are **structured** (2026-07-19): a pickable option row is
  `(TEXT . KEY)` (`menu-option`/`menu-numbered`, accessors
  `menu-line-text`/`menu-line-key` in events.lisp), plain lines stay
  strings.  Since 2026-07-25 a page names **only its own keys** as
  plain words whose first letter is the key (`Sell`, `Pool gold`),
  each such row a `menu-option` so it clicks, **one option per row**;
  the common navigation (digit picks, `Esc`, `u`/`d` scrolling,
  `+`/`-` speed) lives on the help screen instead of on every page.
  The older bracket-hint convention (`[S]ell  [Esc] back`) is still
  understood: `fit-menu-lines` packs such rows back together (whole
  options only, never mid-hint) when a page runs out of rows, and
  `menu-key-spans` locates the tokens.  The fitter squeezes in page
  order — hint rows pack, spacers drop, command rows pack — and as
  the **last resort** (2026-08-01) drops plain informational rows
  from the head (`%drop-info-rows`: the title, the header — never a
  pick, a command or a hint), so a page a wrapped line pushed over
  keeps its navigation instead of losing the foot to truncation.  A
  plain line that overflows the page and carries a two-space gap (the
  shop header's `NAME buys.  Gold: N gp`) wraps **at its gaps**
  (`wrap-menu-line`), each segment a whole row, instead of
  mid-sentence.  A pointing front-end maps
  clicks on either straight to the model's keys — the Amiga UI's
  hotspot list (`*hotspots*` in amiga-ui.lisp) is rebuilt on every
  redraw from exactly what was drawn, so the whole game plays by
  mouse: walk zones on the view, roster rows, menu and option rows,
  and click-anywhere-to-close on the map/help/sheet pages.
- Menu lists **scroll** (2026-07-19): a list deeper than a page
  (`+menu-page-size+`, 7 — party-sized lists never scroll) windows to
  a full page of rows; `u`/`d` move the window (help-screen
  knowledge) and digits pick **within the visible window** (row 1 =
  the window's first row), which keeps every item of an arbitrarily
  long list reachable with single-digit keys.  Since 2026-07-28 the
  window spends no rows on marker hints: `menu-scrolled-lines`
  reports the scroll geometry through `*menu-scroll*` and the Amiga
  UI draws a clickable scrollbar at the page's right edge instead —
  a click above the thumb scrolls a window up, below it down.
  The window math lives in one place
  (`menu-window` / `menu-window-pick` / `menu-scroll` /
  `menu-scrolled-lines` in events.lisp) and both the `*-lines`
  renderers and the `*-act` key handlers go through it, so display
  and picks cannot disagree.  It applies to the shop's buy/sell
  pages, the use/cast/sing item lists and the save-slot picker; the
  character sheet scrolls its stat block the same way (page 8, the
  pack expanded to one row per item — `hero-sheet-lines`'s TOP
  argument plus `hero-sheet-scroll`, the scroll offset itself staying
  front-end state).  Scroll offsets reset when a menu changes page or
  backs out.

## Map size

- The engine must support dungeon levels of **at least 30x30 cells**
  (Bard's Tale I level size — itself a C64/A500 memory constraint we do
  not inherit).
- To stay flexible the engine and all UI code must work unchanged up to
  **64x64 and 128x128** — no fixed-size buffers, no whole-map screen
  layouts, nothing quadratic per step.
- Per-cell engine cost stays small: walls are a `(h w 4)` keyword
  array, automap knowledge is one fixnum per cell, save games store
  knowledge as row lists.  A 128x128 level is ~16K cells and must load,
  play and save-round-trip on an 8MB Amiga.

## Screen layout (both front-ends)

The in-game screen is split Bard's Tale style.  (Revised 2026-07-16
after the first playtest: the 6x6 minimap viewport was dropped — it
ate the text column's space without earning it; the automap lives
solely in the full map mode under `m`.)

```
+----------------------+---------------------------+
|                      |  message log              |
|  first-person view   |  (microfont, newest line  |
|                      |   at the bottom, older    |
|                      |   lines scroll up)        |
+----------------------+---------------------------+
| location plaque      | effect icons [+ rose]     |
+----------------------+---------------------------+
| party roster (header + up to 7 rows)             |
+--------------------------------------------------+
```

(Revised 2026-07-18 for the lo-res display profile: the narrow
active-spells strip between the view and the text column did not fit
320 pixels, so both profiles share this two-column split — BT1/BT3 on
the Amiga use the same arrangement.)

(Revised 2026-07-19: the columns settle at roughly **2/5 view, 3/5
text** of the content span — the text matters more than the picture.
The split is not engine code but a profile knob: the view column is
exactly the profile's `fp-width` and the log always takes whatever is
left, so a game or target that wants a different balance ships a
profile, not a patch.)

- **Message log** (right column): everything the game says — combat
  transcript, door/wall feedback, story messages — appended at the
  bottom, older lines scrolling up, exactly like Bard's Tale's text
  column.  Backed by the engine's `attach-message-log` ring
  (`:message` events); front-ends render as many trailing lines as
  fit.  On the Amiga the log renders in the engine's own **condensed
  bold microfont** (src/microfont.lisp — the 5x7 small face on 6x8
  cells, rendered once per distinct line into a cached offscreen
  bitmap and blitted; since 2026-07-26, matching the ~6px advance of
  the actual Bard's Tale II text), well under topaz 8's 8px so the
  narrow column holds more text.
- **Effect strip** (below the log page, separated by a small gap, on
  the grey chrome; the profiles keep it **20px** — just clearing the
  16px icons, so the log page above gets the room): the party's
  active effects — shield, light and friends, Bard's Tale style — as
  **icons only, laid out left to right in effect order**; no text
  labels (the log announces casting and expiry).  The engine carries
  them as `game-effects`: **records** with a display name, an
  optional expiry on the game clock (`add-effect`'s `:duration`;
  `advance-time` announces and drops the expired), a payload plist of
  engine facts — `(:ac N)` feeds the party's effective AC, `(:light
  t)` defeats darkness, `(:compass t)` orients the party — and an
  optional icon image: a file name resolved against the current map's
  directory (`effect-image-path`, the zone tile-pack rule) — 16x16
  ILBMs with pen 0 as the transparent key (`draw-effect-icon` in
  tools/gen-walls.lisp draws placeholder art; a file that will not
  load logs once and the effect shows nothing in the strip).  The
  host UI stays text.  Re-adding a name refreshes it in place.
  Effects live in save games.  An effect that carries `(:compass t)`
  shows the **live compass rose** in its own slot instead of its
  icon — the diamond with the amber needle pointing at the party's
  facing and the facing letter beside it — so the rose sits wherever
  the granting spell/item/song sits in the strip, not at a fixed
  corner (`compass-points` remains the full-rose geometry helper).
  While one burns (`compass-active-p`) the map footer shows the
  facing; without one the party has no facing readout (Bard's Tale's
  Magic Compass rule).  Turning is silent (no "You turn left." log
  noise).
- **Party roster** (bottom, full width, right under the location
  plaque — there is **no status line**): a header row and one roster
  row per party member, the Bard's Tale table — `# CHARACTER AC HIT
  PTS SPL PTS CL` (max/current hit points, max/current spell points,
  class code via `hero-class-abbrev` — always **two characters**, so
  the name column keeps the room; a downed hero's name and hit
  points turn amber) — the layout must reserve room for **7 rows**.
  Those rows are **set solid**: one glyph box each, none of the
  leading the message log's lines carry, and the header sits a
  couple of pixels under the plaque rather than a full line under it.
  A roster is read column-down, not line by line, so the rows want to
  group — and on the 200-line screen there is no room for leading
  anyway.
  The number columns are **right-aligned** in a fixed field
  (`roster-cell`, `+roster-num-cells+`), heading included, so digits
  of different width line up down the table; a profile's
  `roster-cols` must therefore leave every numeric column that much
  room before the next one.
  What the status line used to carry moved: the key reference to the
  **help page** (`h`/`?`, `help-lines`), position/facing/clock to the
  **map footer**, contextual prompts (win/lose) into the message log.
  Combat keys live on the **round-orders page** (`combat-orders-lines`
  / `combat-orders-act` in combat.lisp): every round opens on the
  party-level engage page (`a` attacks, `f` flees — the whole party or
  nobody, and only at the top of a round; a failed flee costs a
  free monster round and asks again), then every living hero picks an
  action in turn (attack/defend/cast/play/use, `Esc` undoes,
  `+`/`-` set the transcript speed), then the round runs with each
  message lingering `combat-message-delay` seconds.  A won fight
  lingers `*victory-linger*` seconds on the campaign's
  `*victory-image*` treasure picture in the view column.

## Message-area takeovers (locations, character sheet)

(Added 2026-07-19: interactions used to draw as a page over the view
column; the user-directed rework moved them into the message area,
with pictures in the view column.)

- Entering a **location** (shop, tavern, any kind) or opening a
  **character sheet** (`1`-`7`) does not cover the view column with a
  menu page: the interaction **takes over the message area** — its
  menu lines render at the top of the log page (microfont on the
  Amiga, `%amiga-draw-takeover`).  The menu owns the whole page — the
  log-tail split under a separator rule it first shipped with read
  poorly and is gone — so game feedback waits for the page to close;
  what must be seen NOW gets a page of its own (a refused cast on the
  spell card, a level-up's notes, below).  The page interior
  repaints wholesale on every redraw (a `cls`) — switching pages
  never leaves stale text.
- **A level-up's notes page** (2026-08-01): the sheet's `l` takes the
  banked level, whose messages (the new level, the one stat gain, the
  spells learned) would land in the hidden log.  Both front-ends mark
  the log first (`log-length`), then show what the rise said
  (`log-since`) as a takeover page of its own — `level-notes-lines`:
  the notes, a spacer, the carousel's centered `NEXT` row — and any
  key (or the NEXT row's click) turns back to the sheet.  One level
  per press, so each rise gets its own page.
- The **view column** meanwhile shows a picture when the campaign
  ships one: the location op's `:image`, or the sheet hero's class
  portrait (`define-hero-class :image`) — both resolved relative to
  the current map file (the effect-icon rule: `location-image-path`,
  `hero-image-path`).  Outside a location the same rule applies to the
  street: when the party faces a door whose far cell holds a location
  with a picture, the facade shows in the view column
  (`facing-location-image-path`; `:facade` names the street face and
  wins over `:image`, the picture shown inside) — houses have faces
  before the party steps in.  Without a picture the live first-person view
  stays.  Pictures are opaque ILBMs drawn centered on black,
  center-cropped when they overhang the viewport
  (`%amiga-draw-picture`); a file that will not load logs once and
  the view falls back.  Placeholder art comes from
  `draw-location-scene` / `draw-portrait` (tools/gen-walls.lisp);
  Closure ships viewport-sized scenes and 64x64 portraits
  (worlds/closure/gfx/make-pack.lisp).
- The **cast/use/sing menus and the save picker draw as an overlay
  page** (`%amiga-draw-page`), never as a takeover of the whole
  window.  The page draws in the same **condensed small face** as the
  log and the takeover, so the whole UI carries one type size and a
  long slot or spell list fits the lo-res page.  All four take the
  same box (`%menu-page-box`): a **centered dialog**, four fifths of
  the content width — every picker lists names (slots, spells, songs,
  pack items) and a name truncates in the lores view column, so one
  shape serves them all.  The dialog covers the message area, so the
  caller skips drawing the log under it and repaints (`fresh-play`)
  when the picker closes.  The
  full-page sheet overlay (`%amiga-draw-sheet`) stays available as a
  drawing primitive but the play flow uses the takeover.
- The sheet content is the platform-free `hero-sheet-lines` (the stat
  block — every line within 20 cells at worst-case values, inside the
  lores takeover's 27 — then a blank line and the key hints; no
  header, the roster pane already shows who is who); the host UI shows it as
  its `:sheet` mode under the same keys (`1`-`7` switch, `u`/`d`
  scroll a long stat block, `Esc` back).
- The sheet is a **carousel** (2026-07-29): a `NEXT` row — the `n`
  key, the word centered on the lores takeover column
  (`menu-next-option`), the whole row a click target — closes each of
  its pages and turns to the next: the stat block (name, then `Race
  Class` spelled out under it, for a raced hero), the pack page
  (`equip-lines`, whose `equip-act` answers `n` with `:next`), a
  caster's or singer's **spells/songs page**, and from the last page
  back around to the stat block.  The stat block's old `Inventory`
  row is gone — `NEXT` is the way to the pack now (`i` stays as a
  keyboard shortcut) — and the spellbook left the stat block for the
  new page.
- The **spells/songs page** is its own model (`magic-view` /
  `magic-lines` / `magic-act`, gated by `hero-magic-p`), the
  `shop-view` pattern again.  `magic-entries` flattens the hero's
  spellbook and songbook into ONE numbered list — a single run of
  pick digits addresses the whole book even for a hero who casts and
  sings — windowed at `+menu-page-size+`, with `Spells:`/`Songs:`
  heads emitted over the first entry of each kind **within the
  window**, so a scrolled page still says what it is looking at.
  There is no separate inspect mode the way the pack page has one:
  the rows carry pick digits, so typing one *is* the inspection and a
  digit opens that entry's **card**.
- The **cards** (`spell-card-lines` / `song-card-lines`, the
  `item-card-lines` idea for magic) carry the tier and the cost
  against the caster's own points (or the song's level against tunes
  in hand), the four-letter incantation, the spellbook's range and
  duration words, and what the thing does — the campaign's
  `:description` when it wrote one, else the effect read back out of
  the spec by `effect-summary-lines`.  A card also **acts**: `c`
  casts the spell, `p` plays the song.  A song resolves at once; a
  spell goes through `begin-cast`, which resolves it on the spot when
  it needs no aiming and otherwise hands back a `cast-view` (caster
  and spell already picked) for the front-end to go on with — so
  aiming reuses the `c`-menu's model rather than growing a second
  one.  `magic-act` reports these as `:done` (the front-end closes
  the sheet, so the log can be read — the takeover owns the whole
  page) and `(:cast VIEW)`.
- A cast the caster **cannot manage** right now keeps the card up and
  puts the reason *on the page* (`spell-refusal` → `magic-view`'s
  `refusal` slot: "Not enough spell points.", "Only in a fight."),
  cleared by the next key press.  The cast menu logs its refusal
  instead, and can: it has the log beneath it.  This page does not —
  the takeover owns the whole area — so a logged line would go unread,
  while closing the sheet to show one would lose the player's place.
  The rule generalises: **a takeover cannot speak through the log.**
- **Effect prose** (`effect-summary-lines` in game.lisp, beside the
  vocabulary it reads): an effect spec in player's words, one phrase
  per key from `*effect-phrases*` plus the timed run, dice quoted as
  their span via `dice-range-text` (`(:heal "4d4")` → `Heals 4-16`).
  Deriving it keeps the words honest — the same plist feeds the
  sentence and the cast.  A campaign's `:description` (new on
  `define-spell`/`define-song`) overrides it where the derived line is
  too plain; `:notes` stays **designer-facing** and never reaches a
  card, which is why the two are separate fields.

## Full map view (`m`)

- The play view carries no map at all; the **automap lives in a
  full-screen map mode** toggled with the `m` key.
- Map mode covers the entire window/screen, draws the explored automap
  centered on the party (clamped to what fits at a readable cell size),
  and returns to the play view on `m` or `Esc`.  It draws black ink on
  the grey page (doors and the party amber); walls come from
  `map-edge-runs` — merged straight segments, one OS line call per
  stretch — so a city-sized map opens fast on a 68020.
- The space the map leaves to its right carries the **legend of found
  locations** (`map-legend-entries`): one `MARKER NAME` microfont line
  per location whose cell the party has explored, each marker also
  drawn amber on its map cell.  Only special places are legended —
  a location of kind `:house` is scenery and carries no marker, or a
  city's front doors would bury the shops and taverns.
- The **two-line footer** carries what the play page has no room for:
  the zone title, the party position `(x,y)` — plus the facing while a
  compass effect burns — and the game clock on the first line, the map
  size (and the `FULL` marker) on the second.  No key hints — those
  live on the help page.
- The whole map page is drawn in the same **5x7 small face** as every
  other page — legend, footer and cell glyphs alike
  (`microfont-small-glyph`); its 6px advance still enters the 7px
  cells a 30x30 city draws at lores.
- `f` inside map mode toggles the omniscient debug view (full map
  regardless of knowledge); it exists for development, not gameplay.
- `h`/`?` opens the **help page** (the key reference, `help-lines`)
  from both the play view and map mode; `h` or `Esc` returns to where
  it was opened.
- `q` still quits from map mode.  Map mode is unavailable during
  combat.

## Quitting (`q`, `Esc`, the menu strip)

- Leaving the game **always asks first**: `q`, `Esc` and the menu
  strip's Quit raise the confirmation (`quit-confirm-lines` /
  `quit-confirm-act` in help.lisp), and only `y` ends the session.
  `n`, `Esc` — or, on the Amiga, a click anywhere beside the box —
  back out to where the player was.
- The reason is `Esc`: it is the key a player reaches for to back out
  of a page, and on the game's own screen quitting tears the display
  down, so a stray press must not cost an unsaved run.
- The confirmation is **modal**: it eats every other key (a second `q`
  included), the box owns the frame's click targets, and while it is
  up the idle clock, the log's expiry sweep and the animation
  heartbeat all stand still — nothing may draw over it.
- The **endgame page is exempt**: after a win or a defeat the log
  itself asks for `q`, and that `q` quits straight away.
- The Amiga box is drawn by `%amiga-draw-confirm`, sized to its own
  text and centered on the inner window, on top of whatever page it
  interrupts; the host front-end prints the same lines under the page.

## Party

- The roster holds **up to 7 members**: **6 regular heroes plus one
  guest slot** — in Bard's Tale tradition the 7th slot is for a
  summoned/charmed monster or story NPC.
- Engine: `+party-limit+` = 7; `join-party` appends a hero and refuses
  (message + `NIL`, no error) when the roster is full; a successful
  join emits `:party-joined`.
- Combat, saves and both roster panes must handle all 7 rows.

## Amiga display: window and custom screen

The Amiga front-end supports two displays, selected by
`play-amiga`'s `:display` argument:

- `:window` (default) — an Intuition window on the Workbench/public
  screen, as before.  This stays the development default because it
  coexists with the shell running the test suite.
- `:screen` — the game opens its **own Intuition screen** and covers it
  with a borderless backdrop window (input + menus as usual).
- Screen geometry, viewport, tile pack and layout tuning come from a
  **display profile** (`src/profiles.lisp`, `play-amiga`'s `:profile`
  argument): **`:lores`** — a 320x200 layout, 5 bitplanes (32
  colors), the ECS target and the default (half the chip-RAM/DMA cost
  of hires, near-square pixels for the art, the Bard's Tale
  presentation; 200 lines so the one layout serves PAL and NTSC
  alike, since NTSC has no 256-line mode to fall back on) — and
  **`:hires`** — 640x256 PAL hires, 4 bitplanes
  (16 colors), the classic look with the larger 240x130 viewport.  A
  future target (say a big RTG screen) is a new profile plus an asset
  pack, not new code.
- Mode selection must be **RTG-aware, no chipset assumptions** (this is
  the M3 roadmap rule): ask the display database via
  `graphics.library/BestModeIDA` for the profile's nominal geometry
  and open the screen with whatever ID it returns; only fall back to a
  plain PAL request when the database has no answer.  Bitmaps and
  rendering stay behind OS calls only.
- The **screen height is asked for too, not assumed**: the same
  database says how tall the chosen mode's display really is
  (`QueryOverscan`, via `amiga.intuition:display-mode-height`), and the
  screen opens at that height clamped to the profile's
  `screen-max-height` (`screen-height-for`).  The backdrop window stays
  the profile's layout height, so the game lays out identically
  wherever it lands and the surplus is background.  This is the same
  rule as mode selection — no PAL/NTSC branch anywhere, because the
  database answers for RTG as readily as for the chipset.  A screen
  shorter than its display gets cropped and rescaled by an emulator's
  auto-zoom and letterboxed by a monitor, which is what asking avoids.
- The window and the screen share the profile's geometry — the window
  version must fit a PAL Workbench, and both displays lay out
  identically.  The layout is computed from the actual inner
  width/height and the rastport font's metrics (line height and
  character cell — no hardcoded 8px glyph math), so the window's
  title bar, the borderless screen, and whatever an RTG driver
  promotes the mode to all come out right.  On a PAL Workbench the
  window opens at 0,0 — there is no room for an offset.
- `lib/amiga/intuition.lisp` provides `open-screen` / `close-screen` /
  `with-screen` (OpenScreenTagList/CloseScreen) and
  `lib/amiga/graphics.lisp` provides `best-mode-id` and `set-rgb4`.

## Wall graphics (M3)

- The Amiga first-person view is composited from **pre-rendered wall
  pieces** in fixed Bard's Tale screen slots; the slot geometry
  (`wall-piece-rect` / `view-blit-list` in view.lisp; blit records
  carry a per-building STYLE that picks among a pack's optional
  `-vN` piece variants — one style per walled-in mass of cells, from
  a location op's `:style` or a hash of the mass, so one house wears
  one look and a street still shows different houses) derives from the
  same perspective planes as the wireframe display list, so both
  renderers agree about where walls are.
- Piece set per depth (4 depths): front wall, receding left/right side
  walls (trapezoids with the ceiling/floor corners baked in — pieces
  are rectangular blits, correctness comes from back-to-front order),
  left/right flank walls (the neighbor's front wall seen through an
  open side), each with a door variant — 40 pieces.
- A flank stands at the same distance as the front wall of its depth,
  so its slot is that wall's **full perspective width** (one cell at
  plane depth+1), clipped to the viewport — not the narrow strip
  between the near and far planes, which drew a house across an open
  side about half as wide as it should be.  How much of it the party
  can see depends on the walls in front of it (`flank-visible-x`):
  the blit record carries the visible rect plus the source x offset
  into the piece, so a partly hidden flank is cropped, never
  squashed.
- Assets are **IFF ILBM** files, one per piece, named by
  `wall-piece-file` — one pack per display profile (`data/gfx/` for
  `:lores`, `data/gfx-hires/` for `:hires`), since the piece sizes
  derive from the profile's viewport.  `src/ilbm.lisp` is a pure-CL ILBM reader/writer
  (ByteRun1 + uncompressed, interleaved masks skipped, unknown chunks
  skipped) — it must keep working on the host, where the tests and the
  art generator run.
- Art is **generated, not hand-drawn**: `tools/gen-walls.lisp` draws
  every piece procedurally (4-color dungeon palette: black, white,
  grey brick, amber doors) and `make assets` writes the files.  The
  test suite regenerates every piece in memory and compares it
  **pixel-for-pixel** against the checked-in file — assets can never
  drift from the generator.
- Rendering is **RTG-safe, OS calls only** (the M3 roadmap rule): the
  pieces are uploaded once per session into `AllocBitMap` bitmaps
  (friend = the window's bitmap, depth = the display's, so blits copy
  all planes), chunky pens via `WriteChunkyPixels` (V40+, per-pixel
  fallback on V39), composited with `BltBitMapRastPort`.  No planar
  poking, no chip-ram assumptions, no bytes-per-row math.
- When the active pack is missing, unreadable or sized for another
  profile the view **falls back to the wireframe renderer** (and says
  so in the message log); the blitted path also requires the layout's
  full profile-viewport size.
- `lib/amiga/graphics.lisp` carries the bindings: `alloc-bitmap` /
  `free-bitmap` / `with-bitmap`, `get-bitmap-attr`, `init-rastport` /
  `with-bitmap-rastport`, `write-chunky`, `read-pixel` / `write-pixel`,
  `blt-bitmap-rastport`, `gfx-version`.

## Test requirements

- Map: parse/movement/knowledge/save round-trip on generated 64x64 and
  128x128 maps; `map-viewport` clamping at all four edges and on maps
  smaller than the requested window (map mode uses it to clamp).
- World: zone-form parsing (kind/title/wrap/start-facing, bad kinds
  rejected), relative path resolution (incl. Amiga volumes), travel
  with per-zone knowledge persistence and zone reuse, ops-after-travel
  skipping, travel-loop depth cap, save round-trip across zones with
  lazy knowledge restore; older saves load with defaults standing in
  for the keys their day lacked, newer-than-build saves and saves that
  fail to restore signal clear errors (the front-ends log them instead
  of crashing).
- Items: registry (titles, bad kinds), pack limit, equip rules (one
  per kind, class restrictions, misc rejected), attack dice and
  effective AC — including scripted combat rounds where the weapon
  carries the kill and the armor turns the blow.
- Shops: enter/leave events and modality, buy (gold, pack space,
  auto-equip), sell (half price, unequip), and the full `shop-act` /
  `shop-lines` interaction from hero pick to leaving; the shipped
  town/cellar world walks end-to-end (gate, shoppe, tavern, ladder).
- Menu scrolling: the `menu-window` clamps (short lists whole, deep
  lists windowed, offsets clamped at both ends), window-relative
  digit picks, the `*menu-scroll*` scrollbar geometry, and scrolled
  walks through the shop (buy and sell), use, cast, save-picker and
  character-sheet models — including that offsets reset on page
  flips/backing out and that a digit past the window picks nothing.
- Message log: ring limit, trailing-lines query, oldest-first order.
- Party: `join-party` up to 7, refusal at 8, `:party-joined` event.
- Effects: `add-effect` refresh-in-place, `remove-effect`, fresh game
  has none; durations set expiries on the clock, payloads round-trip
  through saves, `:ac` payloads sum, `:light` defeats darkness.
- Time: fresh game at day 1 08:00; steps, turns and combat rounds cost
  a minute, blocked bumps are free; daylight boundary values;
  `:sunrise`/`:sunset` events with their log lines; `clock-line`
  formatting; timed-effect expiry (message + `:effect-expired`).
- Darkness: night or `(zone :dark t)` shrink `game-view-depth` to 1 —
  the view and the automap alike; `(zone :dark N)` keeps the zone dark
  with N cells of sight (capped at `+view-depth+`); a `(:light t)`
  effect restores the full depth; `at-night`/`at-day` specials switch
  on the pure clock.
- Spells: `define-spell` validation (a plist over the shared effect
  vocabulary — instant and timed keys combine freely; timed ones need
  durations), class/level knowledge gates, refusals that say why
  and cost nothing, scripted damage/heal/buff/light casts, sp payment
  and daylight-outdoors regen (night, :dark zones and full sp regen
  nothing), `(:cast SPELL [TARGET])` combat-round actions beside the
  bare keywords, and the full `cast-lines`/`cast-act` key walk — out
  of combat, in combat (one caster casts, the rest attack) and the
  Esc unwind.
- Takeovers: `hero-sheet-lines` (summary block, key hints, the
  carousel's `NEXT` row) and the spells/songs page (`magic-lines` /
  `magic-act`: one numbering over both books, heads that follow the
  window, a digit opening the card, `c`/`p` acting on it, the `NEXT`
  close), the cards' own fields, `effect-summary-lines` over the
  vocabulary and `begin-cast`'s three outcomes;
  `location-image`/`location-image-path` and the class portraits
  resolve map-relative, absent ones NIL; generated scenes/portraits
  size to order and keep to the fixed UI pens; Amiga smoke: the
  takeover page draws cached and uncached, a real picture draws in
  the view column, a missing file defers to the caller after logging
  once.
- Named saves: `slot-path`/`save-slots` (empty without the dir), the
  full `save-menu-lines`/`save-menu-act` key walk — name entry with
  junk-char rejection, Backspace, the live echo, the empty-name and
  length guards, Return and code-13 commits, slot digits for
  overwrite and load, the combat refusal page (digits dead, Esc
  lives) — and a real save/load round trip through the picked paths.
- Amiga (FS-UAE suite): smoke tests for the layout draw calls (incl.
  the effects strip and map page), an unattended `*autoplay*` session
  that enters and leaves map mode, and a `:display :screen` session
  exercising the custom-screen path.
- ILBM: reader/writer round trips (both compressions, pad-boundary
  widths, depths 1-8), ByteRun1 edge cases, palette, unknown-chunk
  skipping, corrupt-file errors.
- Wall pieces: slot geometry (containment, mirroring, blit-list order
  vs. the display list), asset/generator pixel equality for all 40
  pieces; Amiga smoke test loads the assets into bitmaps, blits a view
  and reads a known pixel back.
