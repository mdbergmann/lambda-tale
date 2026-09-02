# 2. Playing

Both front-ends draw the Bard's Tale split screen: the first-person
view with the location plaque under it on the left, the scrolling
message log on the right with a slim strip of active-effect icons
below it, and the numbered party roster at the bottom.  The text
carries the game: the view column takes about two fifths of the
screen and the log the other three.

## The screen

- **The view column** shows the corridor ahead.  The view never looks
  around corners: it walks the cells straight ahead and stops at
  walls, doors and the draw distance.  Facing a location's door from
  the street puts its facade in the column, so a city's houses have
  faces.  Inside a location the column shows the location's picture,
  on a character sheet the hero's portrait, and in a fight the enemy's
  portrait — it belongs to the leading group and passes to the next
  one as groups fall.  Without a picture the live view stays.
- **The message log** scrolls, newest at the bottom.  Locations, the
  character sheet and the combat pages **take over** the log area:
  their menu renders at the top of the page with the trailing log
  lines still scrolling underneath.
- **The effect strip** under the log shows one icon per active
  working, in effect order; an effect granting a compass shows the
  live rose in its slot.  On the Amiga the strip clicks to the
  magic-at-work page.
- **The roster** lists `# CHARACTER AC HIT PTS SPL PTS CL` for up to
  seven members, two-letter class codes, a white up-arrow beside a
  hero with a level banked, and a four-letter code for the worst of a
  hero's conditions: `DEAD`, `STON`, `PARA`, `INSA`, `POIS`.
- **The pickers** — save and load, cast a spell, play a song, use an
  item — open as a centered dialog over the page, all four the same
  size.
- **Questions** — a stair that asks before it is taken, the quit
  confirmation — are a box over the play page: `y` accepts, `n` or
  `Esc` declines, and the box eats every other key until it is
  answered.

## Keys

The help page under `h` or `?` carries this reference inside the
game.

### Walking and pages

| key | action |
|---|---|
| `w` / `s` | step forward / step back, facing kept |
| `a` / `d` | turn left / right |
| `m` | the full automap page |
| `h`, `?` | the help page |
| `c` | cast a spell: pick the caster, the spell, and a target when the spell needs one |
| `u` | use an item: the user, the item, and a target for a heal |
| `p` | play a bard song: the singer, then the song |
| `e` | the magic-at-work page — every active effect, what it does and how long it has |
| `1`–`7` | that party member's character sheet |
| `Shift-S` / `Shift-L` | the save / load slot picker |
| `q`, `Esc` | quit, after a yes/no |

In every picker and menu `1`–`9` pick a row, `Esc` backs out, and
`u`/`d` (or the scrollbar at the page's right edge on the Amiga)
scroll a list longer than the page.  Because the digits count from
`1` again in every window, a scrolled list says which entries it
shows: `Spells: 9-16 of 24`.  A short page keeps its options and
loses its blank lines first, so the last row always stays on the page.

### The character sheet

| key | action |
|---|---|
| `1`–`7` | switch to another hero |
| `n` | next page: the stat block, the pack page, a caster's or singer's spells/songs page, back around |
| `p` | pool the party's gold onto this hero |
| `t` | trade gold to another hero: a digit picks who receives, then the sum is typed — digits, Backspace, Return |
| `o` | marching order: a digit names this hero's new slot and the others close ranks |
| `l` | take a banked level, one per press; each rise reports the new level, the stat gain and the spells learned on a page of its own |
| `c` | change class, offered only where the ladder allows it |
| `Esc` | back |

### The pack page

| key | action |
|---|---|
| `1`–`9` | put a pack item on or take it off again; a worn item is starred |
| `p` | pass an item to another party member: a digit picks what, a digit picks who, each row showing the room left in their pack |
| `i` | inspect an item: its card — kind, damage, AC bonus, price, class restriction and the campaign's description |
| `t` | throw an item away for good, behind a yes/no |
| `r` | read the quest pieces, offered only to a hero carrying one |
| `n` | next page |

Items a hero's class cannot use are marked `(u)`; the card spells
`(unfit)` out.  A two-handed weapon is marked `(2H)`.

### The spells/songs page

A digit opens that spell's or song's **card**: the tier, what the cast
costs against the hero's own points, the four-letter incantation, the
range and duration, and what it does.  `c` on a spell card casts it,
`p` on a song card plays it, asking for a target only when needed.

### The map page

`f` toggles the omniscient debug view; `u`/`d` or the scrollbar
scroll a map too tall to show whole (Amiga only — the host shows what
fits); `m`, `Esc` or a click elsewhere close it.  The map is black ink
on the grey page, doors and the party amber.  A legend beside it lists
the special places the party has found — shops, taverns and the
like, each marker also drawn on its cell; plain houses are scenery
and carry none — and a footer shows the zone, position and game
clock.  The page shows the map whole while it can do so legibly and
scrolls it when it cannot, opening centered on the party.

### Combat

| key | action |
|---|---|
| `f` / `r` | fight or run — the whole party's one choice, at the top of each round |
| `a` | attack; the row is dropped for a hero with nothing in reach |
| `d` | defend |
| `c`, `p`, `u` | cast, play a song, use an item as this hero's order |
| `Esc` | undo the previous hero's pick |
| `y` / `n` | on the review page: fight the round / throw the orders away and pick again from the first hero |
| `+` / `-` | transcript speed, five steps from a second per line (the start) to instant |
| `e` | the magic-at-work page, over the orders |

### Inside locations

| page | keys |
|---|---|
| shop | `1`–`7` pick the shopping hero, `1`–`9` buy or sell, `s`/`b` flip between the buy and sell pages, `i` inspect an item before any gold changes hands, `p` pool the party's gold onto the shopper, `Esc` leave |
| tavern | `1`–`7` buy that hero a drink, `d` down the trapdoor where the tavern has one, `Esc` leave |
| temple | a digit picks who is healed, then a digit picks whose purse pays |
| energy fount | the same two questions, for spell points |
| guild | `c` create a character (`k` keeps the roll, `r` rolls again), `a` add a member, `r` remove a member, `d` delete a character behind a yes/no, `s` save, `l` load, `Esc` leave |
| any other | `Esc` leaves |
| save / load picker | `1`–`9` pick a slot, `n` type a new name, `Esc` cancel |

## The mouse (Amiga)

The whole game plays by mouse.  Clicking the first-person view walks:
the left and right quarters turn, the middle steps forward, its bottom
band steps back.  Clicking a roster row opens that character sheet.
Clicking a menu's numbered rows or its lettered option rows acts as
those keys, and the map, help and sheet pages close on a click
elsewhere.  The pointer is an open hand that becomes a pointing
finger over anything clickable and the arrow of the move a click
would make over the view; an hourglass shows during the loads that
take real seconds — tile packs, save games, first-sight pictures.

The pages that open out of nothing have no row to click, so the right
mouse button carries a **menu strip**: `Game` holds `Save`, `Load`
and `Quit`; `Screens` holds `Map`, `Help`, `Cast`, `Play` and `Use`.
No item shows a shortcut — the help page is the one place that says
what the keys are.  The `Screens` items only ever *open* a page, and
they decline while a picker, a shop or a combat round owns the keys.

## Time, day and night

Every step, turn and combat round costs a minute.  A fresh game starts
at day 1, 08:00; daylight runs 06:00 to 22:00.  The clock names five
bands of the day — morning, noon, afternoon, evening, night — and
each turn of the band is announced in the log: "The sun rises.", "The
sun climbs high.", "The afternoon wears on.", "Dusk gathers.", "Night
falls."  Outdoors the sky and ground take a different colour in each
band.  Timed effects carry durations on the clock and wear off with a
message.

On the Amiga time also passes **while the party stands still** in
free exploration: the sky cycles, casters slowly regain magic
outdoors, timed effects burn down, and every half hour of loitering
draws one wandering-monster roll — a loitering party draws less
attention than a marching one.  Standing inside a location, a menu, a
fight or the map and help pages stops the clock.

**Darkness.**  A pitch-black zone shows nothing at all without a
light: the view is black, the automap learns only the cell the party
stands on, and the door it faces goes unseen.  A dim zone grants a few
cells of sight.  Outdoors at night there is a moon, and sight falls to
a few cells.  A light — a torch, a lamp, a light spell or song —
restores the full depth, then **gutters** over its last minutes, a
cell at a time (by default the last twelve minutes go three cells,
two, one, out), and in a pitch-black zone the walls, ceiling and floor
dim with it, so a failing torch is read off the view itself.  The view
always shows whichever reaches further, the light or the zone's own
glow.

## Combat

Every round opens on the **engage page** — fight or run, the whole
party's choice; a failed run costs a free monster round and asks
again.  Then the **round-orders page** asks one hero at a time, and
when the last has picked it turns into the **review** under *Is this
OK?*.  Then the round runs: heroes strike first, then every surviving
monster swings at a random front-rank hero or shoots from where it
stands.  Each round opens with a `-- Round N --` line, and its
transcript plays out one message at a time on a fresh page.

**Ranks and reach.**  The first three living members in marching
order are the front ranks: the heroes monsters can hit in melee, and
the only ones who can trade melee blows back.  A hero behind them
attacks only with a missile — an equipped bow and arrows, or a thrown
weapon — and the orders page drops the attack row for a hero with
nothing in range.

**Distance.**  The enemy stands at a distance in feet, ten feet to a
dungeon square.  A group at ten feet is toe to toe with the front
rank, the only distance at which a melee blow lands either way.  At
the end of every round the groups still walking cover their ground
and say so — a runner crosses in one round what a shambler takes
four over.  Blows and bolts land on the nearest group; a group spell
breaks the group it lands among.  A monster with a missile shoots
from where it stands, and a shot may pick any living hero, back rank
included — the only thing in the game that reaches the back ranks.

**Spoils.**  A won fight pays each fallen monster's XP and gold to
**every** hero still standing, not divided among them; a hero who
went down takes neither.  A fallen monster may turn up an item it
carried, at most one find per fight.  On the Amiga the treasure
picture takes the view column the moment the last foe falls, the
spoils are told under it, and the page lingers a moment before play
resumes.

## Ailments

Four conditions a hero carries until something lifts them; none wears
off with time:

| ailment | in play |
|---|---|
| poison | bites for a hit point on every party step and every combat round; it can kill |
| insanity | the hero takes no orders and strikes a companion at random |
| paralysis | the hero cannot act at all |
| stone | a statue: cannot act, and stands in the rank taking blows |

They stack — a poisoned hero can be paralysed too — and each is
cured on its own.  The paralysed and the stone are never asked for
orders, and a party where nobody can act loses the fight.  Temples
lift the conditions their priests treat, at a price per ailment, and
some spells cure the ones they name.  Saves carry them.

## Heroes

Heroes have STR, DEX, IQ, CON and LCK, a descending armor class, hit
points from the class's hit dice, and spell points or tunes when the
class casts or sings.  Experience banks toward thresholds; a crossed
one flags the hero in the roster, and the rise is taken by hand on the
character sheet.  A level-up rolls the class hit dice again and gives
every ability a chance to rise by one, at most one ability per level;
gains thin out toward the cap of 18.  High CON pays into hit points
and high DEX into the effective armor class, and low scores cost
nothing.

**Equipment.**  A hero carries up to eight items and wears one of
each equipment kind — weapon, armor, shield, helmet, gloves, bow,
arrow, instrument, ring, wand, figurine — and every worn piece's AC
counts.  A two-handed weapon will not go on beside a shield.
Carrying is not using: an unfit item passes freely between heroes, so
one hero can haul another's gear, and the fallen both give and
receive.

**Spells.**  A caster knows every spell of their class at or below
their level; a fresh level's spells arrive with the rise.  Spell
points trickle back while walking outdoors in daylight, and an energy
fount refills them for gold.

**Songs.**  A singer pays tunes — one per song, one per level when
rested — and only one song plays at a time: a new one displaces the
old.  A class may need an instrument in hand to play at all.  Tunes
come back with a drink at a tavern; a tireless instrument spends none.

**Changing class.**  Where the campaign allows it, a hero who has
carried a class far enough may set it down for another in the same
family: level 1 and no experience in the new one, but hit points,
spell points, gold, abilities, armor and pack carry across untouched,
and the class left behind keeps granting every spell it had opened.
An art already worn cannot be taken up a second time.

**Quest pieces.**  A pack holds eight things.  A plot piece — a key,
a token, a proof — rides outside that limit, has no price, cannot be
sold or thrown away, and reads on the pack page's own quest page.

## Locations

- **Shops** sell their stock and buy anything back at half price.
  Items a hero's class cannot use are marked; the shop still sells
  them, since another hero may carry them.
- **Taverns** sell drinks that refill a singer's tunes, and may hold a
  trapdoor to the way below.
- **Temples** heal wounds at so much gold per hit point, raise the
  fallen for a fee, and lift the conditions they treat.  The fee buys
  the life, not the health: a raised hero comes back at one hit point
  and every point above it is a wound like any other.  The purse buys
  in one order — the raising first, then each cure it can cover
  whole, then wounds one at a time with what is left — and a purse
  that buys nothing is told *Not enough Gold*.
- **Energy founts** refill a living caster's spell points at so many
  gold apiece.
- **The guild** is where characters are made and parties formed.
  Heroes not marching wait in the roster, which is saved with the
  game.  Creating a character walks race, class, portrait where the
  class carries two, and a typed name — letters, digits, spaces, `-`
  and `_`, up to twelve — then shows the 3d6 roll for keeping or
  rolling again; the fresh hero signs on with the purse the campaign
  gives.  Saving and loading are offered there too, and a save made
  inside any location restores inside it.
- **Hours.**  A location may be closed in some bands of the day; the
  party is told, and an entering step is bounced back onto the street
  facing the shut door.
- Leaving a location steps the party back out onto the street, facing
  away from the door.

## Saving

Up to nine **named saves** live side by side as `saves/NAME.sav`.
The picker takes a slot by number or a new name, refused once nine
slots exist so every slot stays reachable by its digit.  Saving is
refused during combat.  A save is a single readable Lisp form carrying
the whole world: the current zone and position, the game clock,
active effects, every visited zone's automap knowledge, story flags,
the party with packs, equipment and ailments, and the roster waiting
at the guild.
