# 3. Worlds

A world is a **directory**: map files plus a `campaign.lisp` beside
them.  Every map file is a **zone** — a city, a dungeon level, a
cellar — and cities and dungeons are the same thing to the engine:
maps with walls, doors and specials.  The `travel` op links zones,
each zone the party has visited keeps its own map and automap
knowledge alive for the whole session, and save games carry the whole
world.  This chapter is the map file; the campaign file is
[Campaigns](4-campaigns.md).

## The map format

Maps are ASCII art on a `(2W+1) x (2H+1)` character grid.  The header
of `src/map.lisp` states the exact rules; `tests/world/crypt.map` is
a small example.

```
+-+-+-+
|@  | |
+ +D+ +
| |  <|
+-+-+-+
```

| character | meaning |
|---|---|
| `-`, `\|` | walls |
| `D` | a door |
| `@` | the party's start |
| `>`, `<` | stairs down and up, by convention; any other cell character is a feature |

Walls are stored per cell, so a one-way phantom wall is expressible,
and a map may wrap Bard's Tale-style (`:wrap t`).  Maps can be as
large as 128x128; Bard's Tale I's 30x30 is the classic size.

The character-by-character parse costs real seconds on a 14MHz
machine for a large map, so a successful parse writes a binary
sidecar beside it — `town.map` → `town.mapc` — that loads in one bulk
read while it is newer than the map.  Editing the map reparses and
rewrites it.  Delete `.mapc` files freely: they are pure cache, and a
sidecar written on the host works on the Amiga.

## The story layer

After the art a map file carries Lisp data forms, read with
`*read-eval*` bound to `NIL` and never evaluated.  The forms start at
the first line beginning with `(` or `;`.

```lisp
(zone :kind :dungeon :title "the cellar" :dark t)

(special (1 2)
  (once (message "Something stirs in the darkness...")
        (encounter ("giant rat" "1d3+1"))))
```

### The zone form

| key | value |
|---|---|
| `:kind` | `:dungeon` (the default), `:city`, or any keyword — data for campaigns and front-ends, not an engine branch |
| `:title` | the zone's name, shown on the plaque and the map footer |
| `:wrap` | `t` for a map that wraps at its edges |
| `:start-facing` | `:north`, `:east`, `:south` or `:west` at the start cell |
| `:gfx` | the tile-pack directory, resolved beside the map file first, then relative to the game directory |
| `:sfx` | the sound-pack directory, resolved the same way |
| `:dark` | `t` for pitch black, a positive integer for that many cells of sight; omitted, the zone has a sky |
| `:sky`, `:ground` | `(R G B)` noon colours the day bands tint from; in a dark zone the ceiling and floor colours, used as declared |
| `:encounters` | the wandering-monster table, `((MONSTER COUNT-DICE [WEIGHT] [DISTANCE]) ...)` |
| `:encounter-chance` | percent chance per step |
| `:night-encounters`, `:night-encounter-chance` | the pair an outdoor zone switches to after dark, each falling back to the base key it goes without |
| `:idle-encounter-minutes` | this zone's own idle vigil in game minutes; an explicit `nil` makes loitering here safe |

### Darkness and light

A zone declared `:dark t` is **pitch black**, the Bard's Tale
dungeon: until a light effect burns the party sees nothing at all —
the view is black, the automap learns only the cell the party stands
on, and the door it faces, and any location behind it, goes unseen.
`:dark N` keeps it dark but grants N cells of sight, a dimly glowing
place.  A zone without `:dark` has a sky and, at night, a moon: sight
falls to `*moonlight-depth*` cells, three by default.

A light effect — a torch, a lamp, a light spell or song — restores the
full depth, then **gutters**: over its last minutes the circle of
sight draws in a cell at a time, one cell per `*light-fade-minutes*`
remaining (four by default, so the last twelve minutes go three cells,
two, one, out), and in a pitch-black zone the walls, ceiling and floor
dim with it — a palette-only effect on the pack's own pens, so
portraits, monsters and text keep their colour.  The view always shows
whichever reaches further, the light or the zone's own glow.  The
schedule is in absolute minutes, not a share of the light's life: a
four-hour lamp gutters over the same last minutes as a half-hour
spell.

### Sky and ground colours

Outdoors the first-person sky and ground take a different colour in
each band of the day: a bright blue that lifts toward dawn, softens
through the afternoon, warms at dusk and sinks to near-black at night.
It is a palette-only effect — two colour registers reloaded when the
band turns, no new art and no redraw — so it is free even on a 14 MHz
machine.

```lisp
(zone :kind :city :title "Closure" :gfx "gfx/"
      :sky (102 170 204) :ground (110 96 74))
```

`:sky` and `:ground` are `(R G B)` triples (a `#(...)` vector works
too) giving the zone's **noon** colour; the engine derives the other
bands by tinting that base, so a zone that paints a red alien sky
still goes dark at nightfall.  A zone that declares neither uses the
engine defaults, `*default-sky*` and `*default-ground*`.

**Indoor zones take their colours too, but not the clock.**  In a
`:dark` zone the same two pens are the *ceiling* and the *floor*, and
a ceiling does not brighten at dawn, so a dark zone's `:sky` and
`:ground` are used exactly as declared.  That is how several dungeons
share one tile pack and still look like different places: the pack
paints the stone, the zone line colours it.  A dark zone that declares
neither keeps whatever its pack loaded — there is no engine default
underground.  The two are independent, so a zone may colour its floor
and leave its roof to the pack.  The pack's side of this contract —
the ceiling on pen 5, the floor on pen 6 — is in [Art and
sound](5-art-and-sound.md).

### Wandering monsters

```lisp
(zone :kind :city :title "Closure" :gfx "gfx/"
      :encounters (("giant rat" "1d3" 2) ("footpad" 1))
      :encounter-chance 2
      :night-encounters (("footpad" "1d2" 2) ("skeleton" "1d2"))
      :night-encounter-chance 8)
```

Every successful step rolls `:encounter-chance` percent; when it comes
up, one table entry — `(MONSTER COUNT-DICE [WEIGHT] [DISTANCE])`,
drawn by weight (default 1), the distance in feet the group opens at —
spawns as a group and combat starts exactly as from an `encounter`
op.  After dark an outdoor zone switches to its `:night-*` pair, each
falling back to the base key it goes without, so "same monsters, more
often" and "meaner monsters, same rate" are both one extra key.  A
`:dark` zone keeps its base pair at all hours: there is no night
underground.  The roll happens after the target cell's special has run
— a scripted fight, an entered location or a `travel` all preempt it
— and the step's own minute decides the table, so the sunset-crossing
step already rolls against the night pair.

While the party stands idle under the Amiga front-end's living clock,
the zone draws one roll of the same check every
`*idle-encounter-minutes*` of game time, thirty by default;
`:idle-encounter-minutes M` gives a zone its own period, and an
explicit `nil` makes loitering there safe, so a world can let its
streets ambush walkers yet leave campers alone.  `*encounter-rate*`
scales every zone's chance globally, or disables wandering monsters
outright; see [Tuning and tooling](6-tuning-and-tooling.md).

## Cell specials

A special is a list of ops attached to a cell, `(special (X Y) OP...)`.
They run in order the moment the party's foot lands.

| op | effect |
|---|---|
| `(message TEXT...)` | show each TEXT in the log |
| `(set-flag KEY)`, `(clear-flag KEY)` | set or clear a story flag |
| `(when-flag KEY OP...)`, `(unless-flag KEY OP...)` | run OP... if the flag is set, or if it is not |
| `(at-night OP...)`, `(at-day OP...)` | run OP... by the clock — night is 22:00 to 06:00 — whatever the zone's darkness |
| `(once OP...)` | run OP... only the first time ever; one `once` per cell, keyed by map and cell |
| `(teleport X Y [FACING])` | relocate the party within the zone; the target cell's special fires too |
| `(travel FILE [X Y] [FACING])` | move the party to another zone; FILE is relative to this map's directory, and X Y FACING omitted mean the target's own start.  Ops after it are skipped. |
| `(location TITLE KIND ARG...)` | a location on this cell — see below |
| `(spin)` | face a random direction, silently: the classic spinner square |
| `(damage DICE [TEXT])` | hurt every living hero by DICE each |
| `(trap DICE [TEXT] [DIFFICULTY])` | a floor trap — see below |
| `(heal DICE)` | heal every living hero |
| `(gold DICE)` | treasure, paid to the leading hero |
| `(give-item NAME)` | the first living hero with pack room takes it; with every pack full it is left where it lay |
| `(take-item NAME)` | one copy leaves the party; carrying none is silent |
| `(when-item NAME OP...)`, `(unless-item NAME OP...)` | run OP... if anyone carries NAME, or if nobody does; a fallen hero's pack counts |
| `(encounter (MONSTER COUNT [DISTANCE])...)` | start combat; ops after it are skipped |
| `(event TOPIC ARG...)` | emit a story event for the campaign's subscribers |
| `(ask TEXT... OP...)` | put a yes/no question; OP... runs only on a yes |

The four item ops signal an error on an unregistered item name,
because that is a typo in map data.  `when-item` asks what the party
is carrying *now*, where a flag only remembers that it once could.

**Asking first.**  Every op acts the moment the party's foot lands,
and for stairs that is too soon: a stair cell is a cell to walk
across, and a descent is a decision.  `ask` is Bard's Tale's *"Stairs
down.  Take them?"* as map data: the leading strings are the question,
free prose the page wraps, and the ops after them run only on a yes.

```lisp
(special (3 2)
  (ask "Stone stairs spiral down into darkness.  Take them?"
       (travel "undervault.map" 1 1 :south)))
```

The question is a box over the play page: `y` takes the offer, `n` or
`Esc` declines and the party simply stands on the cell, `q` still
asks to quit.  It eats every other key until answered, pauses the
Amiga's idle clock, and a step that ends on a question draws no
wandering roll.  Ops after the `ask` on the same cell run at once; a
`once` belongs *inside* the `ask`, not around it, or a no would spend
it.

**Naming a hero in map text.**  The ops carry strings, never forms,
so a text names the party in braces: `{leader}` is the hero walking
in front — the first living one in marching order, the rank `gold`
pays — and `(message "{leader} stays behind.")` reads *Percival stays
behind*.  `damage` and `trap` read their TEXT the same way.  The
vocabulary is closed: an unknown `{token}` is an error, and a brace
with no closing twin is just a brace.

**Traps.**  A trap has three layers of defence: a levitating party
floats over it; else a trap-skilled hero may spot and disarm it for
this crossing; else it springs — TEXT defaults to "A trap springs!" —
and every living hero rolls a saving throw, d20 plus level plus the
LCK bonus plus any save-bonus effects, against DIFFICULTY (default 14)
to halve the damage.  TEXT and DIFFICULTY may come in either order.  A
disarm-traps spell effect destroys traps ahead for good; wrap a trap
in `once` for a one-shot.

## Locations

A **location** — a shop, or any enterable building — is the
`(location TITLE KIND ARG...)` op on a cell.  Entering the cell opens
its page, which takes over the message area; leaving steps the party
back out onto the street.  Facing the door from the street shows the
location's facade in the view column.

| kind | its own keys | what it is |
|---|---|---|
| `:shop` | `:stock (ITEM...)` | sells the stock, buys anything back at half price |
| `:tavern` | `:price N`, `:down FILE` | a drink at N gold refills a singer's tunes; `:down` is a trapdoor, a `travel` to FILE |
| `:temple` | `:price N` (default 2), `:raise M` (default 50), `:raise-per-hp R` (default 0), `:cures ((AILMENT PRICE) ...)` | heals at N per missing hit point, raises the fallen for M plus R per point of full health, lifts the listed conditions at the listed prices |
| `:energy` | `:price N` (default 3) | refills a living caster's spell points at N gold apiece; singers refill at the tavern |
| `:guild` | `:gold DICE` | characters and parties; a fresh hero signs on with DICE gold (default 0) |
| `:house`, any other | none | a plain interior: the title over *There is nothing to do here* and an EXIT row.  `:house` is scenery and carries no marker on the automap; a place that should show on the map gets a kind of its own. |

Keys every kind takes:

| key | value |
|---|---|
| `:image FILE` | the picture shown in the view column while the page is up |
| `:facade FILE` | the street face shown when the party faces the door from outside; without one the `:image` shows from the street too |
| `:plaque TEXT` | a short name for the plaque under the view while the party stands inside; without one the plaque keeps the zone's name |
| `:music FILE` | an 8SVX tune that loops on the Amiga for as long as the party stands inside |
| `:closed BAND` or `(BAND...)` | the day bands the door will not open in: `:morning`, `:noon`, `:afternoon`, `:evening`, `:night` |
| `:style N` | pins the building's wall-piece variant; the whole walled-in mass this cell belongs to wears that look |

Picture files resolve relative to the map file, so a world directory
carries its own art.

**The shop.**  The stock is item names from the campaign; a bad name
is caught loudly at the door.  The shop buys anything back at half
price, sells unfit items with a warning, and its buy page lets the
party inspect an item before any gold changes hands.

**The temple.**  Its menu asks twice: *Who wishes healing?* — a digit
per hero they have work for, wounded or merely ailing — then *Who
will pay?* — any purse in the party.  The purse buys in one order: the
raising first (a purse short of the fee buys nothing at all, since
there is no cleansing a corpse), then each cure it can cover whole,
then wounds one at a time.  Only the conditions `:cures` names are
treated here; a condition this house does not treat walks out
however much gold is offered.

**The guild** is the Adventurers' Guild of Bard's Tale tradition.
Heroes not marching wait in the game's **roster**, saved with the game.
*Create a character* walks race, class, portrait and name, then shows
the 3d6 roll for keeping or rolling again; *Add a member* moves a
waiting hero into the party, *Remove a member* sends one back, and
*Delete a character* strikes a name for good behind a yes/no.  *Save
game* and *Load game* open the picker right there.  A campaign that
starts at its guild puts the location on the map's start cell and
defines a roster instead of a party (see [Campaigns](4-campaigns.md));
the boot walks straight into the guild menu, and the guild will not
send an empty party out.  Leaving a location entered without a step —
the boot's start cell, a `travel` arrival — still steps out the front
door when the cell has exactly one passable side.

**Hours.**  A closed location tells the party so, bounces an entering
step back onto the street facing the shut door, and fires
`:location-closed` for campaign scripts.  The op stays map data, so
the closed shop keeps its street facade.

A page's title banner drops its closing `***` when the full `*** TITLE
***` would overflow the narrowest takeover column, so a long title
costs the page no row.

## What the automap shows

A location counts as found once its cell is explored or the party has
stood before it facing the door.  Found locations of every kind but
`:house` get a marker on the map page and a line in its legend.  The
automap records every wall the party could see, whatever the machine
actually drew; the draw-distance knobs in [Tuning and
tooling](6-tuning-and-tooling.md) never shorten it.
