# 4. Campaigns

`campaign.lisp` sits beside the maps and is the one file in a world
that is **evaluated**.  Both front-ends load it next to whatever map
they start on.  It is Lisp in the `TALE` package: `define-*` calls
that register the world's races, classes, items, spells, songs and
monsters, plus the two hooks that put a party into the game.

The fixture campaign the engine suite plays is the smallest possible
one:

```lisp
(in-package :tale)

(define-hero-class :w-fighter :hp-dice "1d10+4" :damage "1d8" :ac 8
                              :singer t)  ; the fixture's singing fighter
(define-hero-class :w-wizard  :hp-dice "1d6+2"  :damage "1d4" :ac 10
                              :caster t)

(define-spell 'w-flame :cost 1 :level 1 :classes '(:w-wizard)
  :light t :duration 60)
(define-spell 'w-bolt  :cost 2 :level 1 :classes '(:w-wizard)
  :damage "1d4+1")
(define-spell 'w-mend  :cost 2 :level 1 :classes '(:w-wizard)
  :heal "1d8")
(define-spell 'w-compass :cost 1 :level 1 :classes '(:w-wizard)
  :compass t :duration 120 :image "fx-needle.iff")

(define-song 'w-march :buff-ac 1 :duration 30)

(define-item 'w-torch :price 2 :use '(:light t :duration 30) :consumed t)
(define-item 'w-sword :kind :weapon :price 20 :damage "1d6+1")

(define-monster "crypt rat"
  :level 1 :hp-dice "1d6" :ac 10 :damage "1d3" :xp 12 :gold "1d6")

(defun default-party ()
  (list (make-hero "Wilhelm" :w-fighter :gold "3d20+60")
        (make-hero "Wanda"   :w-wizard  :gold "3d20+60")))
```

Closure's `worlds/closure/campaign.lisp` is the full-size template.
Dice are strings in the usual notation, `"2d6+1"`, wherever a number
may vary; a plain integer is accepted in the same places.

## The party hooks

The front-ends look for two functions when a fresh game starts, and
call whichever the campaign defines:

- **`default-party`** returns the heroes marching at boot.
- **`default-roster`** returns the heroes waiting at the guild.  A
  campaign that starts at its guild defines this one, puts a `:guild`
  location on the map's start cell, and the boot walks straight into
  the guild menu; the guild will not send an empty party out.

Both are optional.  With neither the game is a bare walkabout.

## Races

`(define-race NAME &key str dex iq con lck classes description image)`
registers a race, NAME a keyword.

| key | value |
|---|---|
| `:str`, `:dex`, `:iq`, `:con`, `:lck` | ability modifiers, default 0, added in place to the 3d6 rolls — no extra dice |
| `:classes` | the class keywords the race may take; `nil` means any registered class |
| `:description` | one lore line |
| `:image` | an optional portrait, map-relative |

```lisp
(define-race :dwarf :str 2 :con 2 :iq -2
  :classes '(:warrior :paladin :rogue :bard :hunter :monk)
  :description "Amazingly strong and healthy, but not always bright.")
```

Registration order is the guild's menu order; re-registering a name
keeps its spot.

## Hero classes

`(define-hero-class NAME &key ...)` registers a class, NAME a keyword.

| key | default | meaning |
|---|---|---|
| `:hp-dice` | `"1d8"` | hit dice, rolled at creation and again at every level, plus the CON bonus |
| `:damage` | `"1d4"` | bare-handed attack dice |
| `:ac` | 10 | starting armor class, descending |
| `:caster` | `nil` | `t` for a spell-casting class: spell points are 2 per level plus the IQ bonus |
| `:singer` | `nil` | `t` for a song-playing class: one tune per level when rested |
| `:sings-with` | `nil` | an item kind the singer must have **equipped** to play at all; `nil` sings bare-handed |
| `:image`, `:image-woman` | | portrait files, map-relative; a class open to both may carry two |
| `:description` | | the class's lore line |
| `:extra-attack-levels` | | one extra strike per N levels beyond the first — the warrior's art |
| `:crit-chance` | | percent chance a landed blow fells the foe outright, growing one point per level — the hunter's |
| `:ac-per-level` | | one point of natural armor per N levels beyond the first, floored at -10 — the monk's |
| `:trap-skill` | | percent chance to spot and disarm a springing floor trap, growing one point per level — the rogue's |
| `:startable` | `t` | `nil` keeps the class off the creation menu; the only way in is a class change |
| `:change-at` | | the level a hero must reach before they may *leave* this class |
| `:change-group` | | the family the class changes within; a hero only moves between classes naming the same group |
| `:requires` | | an alist `((CLASS . LEVEL) ...)` a hero must have *attained*, now or in a class since left, before entering this one |

A class missing `:change-at` or `:change-group` is for life.

**Making heroes.**  `(make-hero NAME CLASS &key race woman gold)`
rolls the abilities 3d6 each in the order STR, DEX, IQ, CON, LCK,
adds the race's modifiers, rolls the hit dice, and stamps the chosen
portrait on for life.  A race that does not allow the class is an
error, and so is a class registered `:startable nil`.  `:gold` is the
starting purse; dice are welcome.

```lisp
(make-hero "Grod" :warrior :race :dwarf)   ; a stout dwarf warrior
(make-hero "Grod" :conjurer :race :dwarf)  ; error: dwarves cast no spells
(make-hero "Nym"  :rogue)                  ; raceless is still fine
(make-hero "Mab"  :rogue :woman t)         ; wears the woman's portrait
```

The party holds up to seven members: six regular heroes plus one
guest slot for a summoned monster or a story NPC.

**The experience ladder** is the campaign's.
`(define-xp-table '(TOTAL ...) :growth N)` registers the running
totals to reach levels 2, 3, 4, … in order, with `:growth` (default
3/2) compounding the last of them once per level beyond the list so
the ladder never stops.  The form checks that the totals are
positive and strictly increasing and that the growth exceeds 1.  A
game that registers none plays the engine's own gentle curve,
50 × L × (L−1).

**Level-up.**  A crossed threshold flags the hero in the roster; the
rise is taken on the character sheet, one level per press.  Each rise
rolls the class hit dice again (plus the CON bonus) and gives every
ability a Bard's Tale chance to rise by one — a d18 per stat, the
score rises when the draw lands at or above it, at most one ability
per level — and names each spell the new level opens.  High CON pays
into hit points and high DEX into the effective armor class, never
the reverse.

**Changing class** freezes the art being left at its current level
and starts the new one over: level 1, no experience, but hit points,
spell points, gold, abilities, armor and pack carry across, and the
maximum spell points never fall.  A class left behind keeps granting
every spell it had opened and never opens another; a hero's book is
the sum of every art they have worn.  An art already worn cannot be
taken up a second time.

## Items

`(define-item NAME &key ...)` registers an item type, NAME a symbol.

| key | default | meaning |
|---|---|---|
| `:title` | the capitalized name | `SHORT-SWORD` → "Short Sword" |
| `:kind` | `:misc` | one of `:weapon :armor :shield :helmet :gloves :bow :arrow :instrument :ring :wand :figurine :misc` |
| `:price` | 0 | shop price; the shop buys back at half.  Refused on a quest piece. |
| `:damage` | | attack dice — a weapon's, or the arrows' when shot from a bow |
| `:reach` | | feet the item carries as a missile; a weapon given one is a **thrown** weapon that shoots from any rank with no bow.  Omitted, the missile goes unmeasured and always reaches. |
| `:ac` | 0 | armor-class bonus while equipped |
| `:classes` | | the classes that may use it; `nil` means any |
| `:two-handed` | | weapons only: will not go on beside a shield, nor a shield beside it; the pages mark it `(2H)` |
| `:use` | | makes the item usable — see below |
| `:consumed` | | spent on use |
| `:quest` | | a plot piece — see below |
| `:tireless` | | instruments only: songs played on it spend no tune |
| `:image` | | the effect-strip icon of the effect a use installs |
| `:description` | | the player-facing line the item card shows |
| `:notes` | | designer notes, carried as data for generated catalogues and never shown |

**Equipment.**  A hero carries up to eight items and equips one of
each kind but `:misc`; every worn piece's AC counts, and combat uses
the equipped gear.  A hero behind the front ranks attacks only with a
missile: an equipped bow *and* arrows (the arrows carry the dice, the
shot aims by DEX instead of STR), or a thrown weapon.  Items a hero's
class cannot use still pass freely between heroes and still sell in
shops, marked unfit.

**Usable items** — a torch, a potion, a wand — take one of three
shapes in `:use`:

- instant keys of the effect vocabulary that need no battle, such as
  `(:heal DICE)` or `(:summon NAME)`; the damage family is refused;
- a timed spec, such as `(:light t :duration 30)`, that installs its
  effect;
- `(:cast SPELL)` — using the item casts the already-registered spell
  for free, with no spell points and no spellbook; a battle spell
  waits for a fight.  Register the spell first.

Using happens from the use menu in the open or as a combat-round
order.

**Quest pieces.**  `:quest t` marks a plot piece.  It rides outside
the eight-slot limit, has no price, no shop buys it and no hand
throws it away; it reads on the pack page's own quest page, a title
and the item's `:description` under it.  The gate ops (`when-item`,
`take-item`) see it like any other item, so a piece is still
something the party is *carrying*, spendable at the door it opens.

## Spells

`(define-spell NAME &rest plist)` registers a spell, NAME a symbol.
The plist mixes display metadata with the effect spec itself.

| key | default | meaning |
|---|---|---|
| `:title` | the downcased name | `MAGE-FLAME` → "mage flame" |
| `:cost` | 1 | spell points per cast |
| `:level` | 1 | the caster level that opens it |
| `:classes` | `nil` | the caster classes that know it; `nil` means any caster |
| `:reach` | | feet the spell carries in a fight; none means it reaches whatever it is aimed at |
| `:code`, `:range`, `:duration-text` | | lore the card shows and the engine never interprets |
| `:image` | | the effect-strip icon, for the timed kinds |
| `:description` | | the player-facing line, shown where the derived words are too plain |
| `:notes` | | designer notes, never shown on a card |
| effect keys | | one or more keys from the tables below |
| `:duration` | | game minutes, or `:indefinite`; required with any timed key |

A caster **knows** every registered spell of their class at or below
the level they have reached in that class — no separate learning
step.  A spell's card derives what it does from the effect spec
(`(:heal "4d4")` reads "Heals 4-16"), so the words cannot drift from
the mechanics.  Spell points trickle back while walking outdoors in
daylight.

### The effect vocabulary

Spells, songs and usable items all speak it, and the keys **combine
freely**: a restoration heals *and* cures, a batchspell installs five
enchantments in one casting.

**Instant keys** resolve at cast time:

| key | value | effect |
|---|---|---|
| `:damage` | dice | strikes the nearest living monster |
| `:damage-per-level` | dice | the roll times the caster's level |
| `:damage-group` | dice | every monster of the nearest group |
| `:damage-all` | dice | every living monster within reach |
| `:slay` | percent | chance to fell the nearest monster outright |
| `:heal` | dice or `:full` | heals one chosen hero |
| `:heal-party` | dice or `:full` | heals every living hero |
| `:resurrect` | `t` | the fallen rise at 1 hp |
| `:cure` | `(AILMENT...)` | lifts exactly the ailments named |
| `:scry` | `t` | speaks the party's position |
| `:disarm-traps` | integer | destroys the traps up to N squares ahead, for good |
| `:teleport` | integer | a fold in space: the cast menu asks a heading and a count up to N; a wrapping zone folds around the seam, a plain map's edge refuses and the spell is spent |
| `:teleport` | `t` | flight to a **named destination** the campaign registered; a digit picks one |
| `:push-foes`, `:halt-foes`, `:calm` | `t` | flavour: they cast, pay and speak, awaiting their subsystem |
| `:summon` | string | flavour: a summoned ally, to come |

The damage family and `:slay` are combat-only; neither teleport casts
in combat.

**Timed keys** merge into one effect record with a `:duration`:

| key | value | effect |
|---|---|---|
| `:buff-ac` | integer | party armor-class bonus |
| `:light` | `t` | the party carries light |
| `:night-vision` | `t` | sight in darkness |
| `:reveal` | `t` | magical sight: light and more |
| `:compass` | `t` | the party sees its facing while it burns |
| `:levitate` | `t` | the party floats over floor traps |
| `:buff-damage` | integer | party melee damage bonus |
| `:save-bonus` | integer | weighs into every saving throw |
| `:regen-sp` | integer | multiplies the daylight spell-point trickle |
| `:extra-attacks` | integer | extra strikes per round |
| `:combat-heal` | dice | mends the party every round |
| `:foes-ac` | integer | the enemy is easier to hit |
| `:foes-attack` | integer | the enemy hits less often |

A light, night-vision or reveal effect defeats darkness.

### Named destinations

A homing spell needs somewhere to home to, and where that is, is the
campaign's business:

```lisp
(define-destination 'testville-guild :title "The Guild at Testville"
                    :map "town.map" :x 14 :y 28 :facing :north)
```

`:x`, `:y` and `:facing` are optional and omitted mean the map's own
start, exactly as for the `travel` op — which is also how `:map` is
resolved, relative to the zone the party stands in when the flight
begins.  Registration order is menu order, and registering a name
twice replaces the destination without moving it.  The flight is a
`travel`: the zone loads or is remembered, automap and all, and the
arrival cell's special fires.  With nothing registered a `:teleport
t` spell says so on its card.

## Songs

`(define-song NAME &rest plist)` registers a song, NAME a symbol.  A
song is always a timed effect over the vocabulary above.

| key | default | meaning |
|---|---|---|
| `:title` | the downcased name | |
| `:level` | 1 | the singer level that opens it |
| `:image` | | the effect-strip icon |
| `:description`, `:notes` | | as for spells |
| timed keys | | one or more, combining: `:regen-sp 2 :extra-attacks 1 :duration 60` |
| `:duration` | | game minutes or `:indefinite`; **required** |

Singers pay **tunes** — one per song, one per level when rested — and
only one song plays at a time: striking up a new one displaces the
old.  A class with `:sings-with` must have that kind of item equipped
to play at all.  Tunes come back with a drink at a tavern; a
`:tireless` instrument spends none.

## Monsters

`(define-monster NAME &key ...)` registers a monster type, NAME a
string.

| key | default | meaning |
|---|---|---|
| `:level` | 1 | |
| `:hp-dice` | `"1d8"` | |
| `:ac` | 10 | descending |
| `:damage` | `"1d4"` | its blow |
| `:missile` | | dice its shot throws while its group is still walking in |
| `:missile-reach` | | feet the shot carries; omitted, it always reaches.  Wants a `:missile`. |
| `:missile-verb` | `"SHOOTS"` | the transcript's word for a landed shot: `"SPITS AT"`, `"WAILS AT"` |
| `:speed` | | feet its group closes per round instead of the global step; `0` stands where the fight found it |
| `:xp` | 10 | paid to every standing hero when it falls |
| `:gold` | 0 | likewise; dice welcome |
| `:item` | | an item it carries into the fight |
| `:item-chance` | 100 | percent each fallen carrier rolls; one find per fight at most |
| `:inflicts` | | `((AILMENT PERCENT) ...)` — a landed blow or shot rolls each entry on the hero it struck |
| `:image` | | its portrait, map-relative |

**How a fight opens.**  An `encounter` op or a wandering table names
groups, `(MONSTER COUNT [DISTANCE])`, COUNT dice welcome.  Ten feet is
one dungeon square; a group at ten feet is in melee.  Groups given no
distance line up from melee backwards, one `*combat-group-spacing*`
apart, so a fight of one group opens toe to toe.  Each round the
groups still walking close by `*combat-close-step*` or their own
`:speed`.  Melee blows land only at melee, in either direction;
missiles and spells reach as far as their `:reach`, and an unmeasured
one carries however far the fight asks, which keeps a campaign
written before distance existed playing unchanged.  A shot picks any
living hero, whatever rank; a blow picks a random front-rank hero.
Monsters that have closed swing rather than shoot.

**Spoils.**  The sum of the fallen's XP and gold goes to each hero
still standing.  `*victory-image*` names the treasure picture the
Amiga shows when the last foe falls.

## Ailments

Four conditions, in the order they display, cure and roll:
`:poison`, `:insanity`, `:paralysis`, `:stone`.  What each does in
play is in [Playing](2-playing.md).  Who hands them out and what
lifting one costs is campaign data:

- `define-monster ... :inflicts ((:poison 25) (:stone 5))`;
- a spell's `:cure (:poison :insanity)` lifts exactly what it names
  and refuses anything that is not an ailment; with `:heal-party` or
  `:resurrect` it reaches the whole roster;
- a temple's `:cures ((:poison 60) ...)` prices each condition its
  priests treat.

From Lisp, `afflict-hero` lays one on (once — an ailment does not
deepen), `cure-ailment` lifts one quietly, and `cure-hero` lifts a
list and says what it lifted.

## Events and flags

The engine never hard-codes a story fact.  It emits events the
campaign subscribes to, and story state lives in flags:

```lisp
(on-event game :enter-location
  (lambda (game location)
    (when (string= (location-title location) "The Guild")
      (unless (flag game :welcomed)
        (say game "The guildmaster nods.")
        (set-flag game :welcomed)))))
```

`(on-event GAME TOPIC HANDLER)` subscribes a function of the game
and the event's arguments; handlers on one topic run in subscription
order.  `(say GAME CONTROL ARGS...)` puts a formatted line in the
log.  `(flag GAME KEY)`, `(set-flag GAME KEY [VALUE])` and
`(clear-flag GAME KEY)` are the story flags; the `when-flag` op reads
the same table from map data.  The running game is `tale:*game*`.

| topic | arguments | when |
|---|---|---|
| `:message` | TEXT | a line for the log |
| `:enter-cell` | X Y | the party arrives on a cell — a step, a teleport, a travel, a step out of a location |
| `:enter-zone` | MAP | travel entered a zone |
| `:blocked` | DIRECTION | a step into a wall |
| `:door` | DIRECTION | a step through a door |
| `:enter-location`, `:leave-location` | LOCATION | |
| `:location-closed` | LOCATION | the door would not open at this hour |
| `:question` | QUESTION | an `ask` op put its question |
| `:combat-start` | MONSTERS | |
| `:combat-end` | `:victory`, `:defeat` or `:fled` | fires the moment the last foe falls, *before* the spoils are told |
| `:hit` | MONSTER DAMAGE | a hero's blow lands |
| `:slay` | MONSTER | a blow fells its target |
| `:miss` | HERO MONSTER | |
| `:loot` | ITEM HERO | a fallen monster's item is taken |
| `:hero-hurt` | HERO AMOUNT | |
| `:hero-died` | HERO | |
| `:party-defeated` | | the last hero fell |
| `:hero-revived` | HERO | |
| `:afflict`, `:cure` | HERO AILMENT | |
| `:level-up`, `:class-change` | HERO | |
| `:party-joined`, `:party-left` | HERO | the guild moved a hero |
| `:coin` | AMOUNT | gold changed hands: a purchase, a sale, a temple, a fount, a pooled purse |
| `:drink` | HERO | at the tavern |
| `:temple-heal`, `:energy-restored` | HERO | |
| `:item-passed` | FROM TO NAME | |
| `:item-discarded`, `:item-used` | HERO NAME | |
| `:spell-cast`, `:song-sung` | HERO NAME | |
| `:effect-expired` | NAME | a timed effect wore off |
| `:sunrise`, `:sunset` | | 06:00 and 22:00 |
| `:time-band` | BAND | the day turned into a new band |
| any TOPIC a map's `(event TOPIC ARG...)` op names | ARG... | |

Sound cues hang off the same events; see [Art and
sound](5-art-and-sound.md).

## Testing a campaign

All randomness goes through `tale:*rng*`, so a campaign's own tests
can script a fight or a roll deterministically the way the engine
suite does.  The engine suite's `tests/run-tests.lisp` is the
executable specification of every mechanism this chapter describes;
the sections are named after the features.
