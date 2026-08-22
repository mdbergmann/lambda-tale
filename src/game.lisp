;;; Lambda's Tale — game state and movement.

(in-package :tale)

(defstruct (game (:constructor %make-game))
  map                 ; dungeon-map — the zone the party is in
  knowledge           ; map-knowledge for the current zone
  (x 0)
  (y 0)
  (facing +north+)    ; direction index 0..3
  (time *new-game-minutes*) ; game minutes since campaign start (time.lisp)
  party               ; list of HERO (NIL for a bare walkabout)
  roster              ; list of HERO waiting at the guild — created or
                      ; removed members not marching with the party
                      ; (see the :GUILD location kind, locations.lisp);
                      ; a hero is in the party or the roster, never both
  (flags (make-hash-table :test 'equal)) ; story flags (see events.lisp)
  handlers            ; event subscriptions: alist topic -> handler list
  combat              ; active COMBAT or NIL
  effects             ; active EFFECT records (shield, light, ...), see below
  location            ; active LOCATION (shop, ...) or NIL, see locations.lisp
  ;; Idle game-minutes accrued toward the next wandering-monster roll
  ;; under the living-world clock (see MAYBE-IDLE-ENCOUNTER); not saved
  ;; — a loaded game starts a fresh vigil.
  (idle-encounter-clock 0)
  ;; The world: every zone the party has visited this session, keyed by
  ;; map file path -> (MAP . KNOWLEDGE).  TRAVEL-PARTY switches zones,
  ;; keeping each zone's map and automap knowledge alive.
  (zones (make-hash-table :test 'equal))
  ;; Automap knowledge of zones restored from a save but not yet
  ;; revisited: alist path -> knowledge row lists (see save.lisp).
  zone-knowledge)

;;; ---------------------------------------------------------------------
;;; Seams for tooling
;;;
;;; Three hooks the engine itself never uses.  They exist so a game can
;;; hang a debug build off a running session — a console, a cheat menu,
;;; a recorder — without the engine learning anything about it, and
;;; without a front-end fork.  All three are inert until something sets
;;; them, and a release build simply never does.

(defvar *game* nil
  "The game the running front-end is playing, or NIL outside a session.
ASSIGNED — never bound — by PLAY and PLAY-AMIGA as they wire a game up,
whether it is a fresh one or one just restored from a save, and left
standing when the session ends so tooling can still inspect what the
party walked away from.

The assignment is deliberate: bindings are per-thread in this runtime,
so a LET here would be invisible to any other thread and would vanish
the moment the front-end returned.  The engine never reads this.")

(defvar *key-hook* nil
  "NIL, or a function of (GAME CHAR) that both front-ends offer every
key before their own dispatch — ahead even of the quit confirmation, so
a console stays reachable when a page has wedged.  A true return means
the hook consumed the key: the front-end redraws and no page sees it.")

(defvar *tick-hook* nil
  "NIL, or a function of (GAME) the Amiga front-end calls once per
heartbeat, beside the *AUTOPLAY* step.  It is where work that arrives
from outside the event loop gets run ON the loop's own task — the
front-end draws from that task and the game state has no locks, so a
debug channel posts its forms here rather than evaluating them wherever
they were typed.

A true return means the hook changed something the frame does not show
yet, and the front-end redraws.  An idle hook MUST return NIL: the
heartbeat runs about ten times a second, and a full redraw at that rate
is more than a 68020 has to give.

The host front-end blocks on the keyboard and has no heartbeat, so it
never calls this.")

(defun observe (game)
  "Record what the party can see from its position into the automap:
the standing cell fully, and for each cell in the view cone its front and
side walls plus the front walls seen through open sides.

The cone is GAME-VIEW-DEPTH — what the light allows — NOT the drawn
RENDER-VIEW-DEPTH: the draw-distance knob is a speed setting, and a
player who lowers it must still map what the party could see.

A location straight ahead is found without entering: standing right
before a shop, facing its front, is knowledge — the cell beyond the
door (or across the open cell edge) is marked found and the automap
legend lists the place (see MAP-LEGEND-ENTRIES).  One cell only, and
even in the dark — the wall dead ahead is always visible
(GAME-VIEW-DEPTH never drops below one cell)."
  (let ((map (game-map game))
        (k (game-knowledge game))
        (x (game-x game))
        (y (game-y game))
        (f (game-facing game)))
    (know-cell k x y)
    (dolist (s (compute-view map x y f (game-view-depth game)))
      (know-wall k (view-slice-cx s) (view-slice-cy s) f)
      (know-wall k (view-slice-cx s) (view-slice-cy s) (turn-dir f -1))
      (know-wall k (view-slice-cx s) (view-slice-cy s) (turn-dir f 1))
      (when (view-slice-lx s)
        (know-wall k (view-slice-lx s) (view-slice-ly s) f))
      (when (view-slice-rx s)
        (know-wall k (view-slice-rx s) (view-slice-ry s) f)))
    (when (wall-passable-p (cell-wall map x y f))
      (multiple-value-bind (nx ny) (neighbor map x y f)
        (when (and nx (cell-location-op map nx ny))
          (know-found k nx ny))))))

(defun new-game (map &key party roster)
  "Start a fresh game on MAP at its start position, with PARTY (a list
of heroes; NIL for a bare walkabout) and ROSTER (heroes waiting at the
guild — see the :GUILD location kind).  The start cell's special is NOT
triggered here — subscribe your event handlers first, then call
TRIGGER-SPECIAL once."
  (let ((g (%make-game :map map
                       :knowledge (make-map-knowledge map)
                       :x (dungeon-map-start-x map)
                       :y (dungeon-map-start-y map)
                       :facing (dir-index (dungeon-map-start-facing map))
                       :party party
                       :roster roster)))
    (setf (gethash (dungeon-map-name map) (game-zones g))
          (cons map (game-knowledge g)))
    (observe g)
    g))

;;; ---------------------------------------------------------------------
;;; The world: travel between zones (cities, dungeons — all just maps).

(defun %resolve-map-path (base file)
  "Resolve FILE relative to the directory of BASE (the current map's
file path).  FILE stays as-is when it is absolute (leading '/' or an
Amiga volume ':')."
  (if (or (and (> (length file) 0) (char= (char file 0) #\/))
          (find #\: file))
      file
      (let* ((slash (position #\/ base :from-end t))
             (colon (position #\: base :from-end t))
             (cut (cond ((and slash colon) (max slash colon))
                        (slash slash)
                        (colon colon))))
        (if cut
            (concatenate 'string (subseq base 0 (1+ cut)) file)
            file))))

(defun load-campaign (map-file)
  "Load the campaign that belongs to MAP-FILE: the campaign.lisp in
the same directory as the map (hero classes, monsters, items, the
starting party — see the Closure game's worlds/closure/campaign.lisp
for a worked example).  A world is a directory of map files plus its
campaign.lisp; the front-ends call this so a designer's own world
brings its own definitions.  Returns the loaded path, or NIL when
there is none."
  (let ((path (%resolve-map-path map-file "campaign.lisp")))
    (when (probe-file path)
      (dlog-timed ("campaign ~A" path)
        (load path))
      path)))

(defun zone-gfx-dir (game)
  "The current zone's declared tile pack, or NIL: the map's
(ZONE :GFX DIR), resolved in two steps so worlds stay portable —
relative to the map file's directory when the pack lives there (a
self-contained world directory), else relative to the game directory
(a pack the game ships beside its worlds).  The probe is the front-0.iff
every pack must hold; a wrong directory still surfaces through the
wall loader's loud wireframe fallback."
  (let* ((map (game-map game))
         (gfx (dungeon-map-gfx map)))
    (when gfx
      (let ((local (%resolve-map-path (dungeon-map-name map) gfx)))
        (if (probe-file (concatenate 'string local "front-0.iff"))
            local
            gfx)))))

(defvar *step-dir* nil
  "Direction index of the step being taken while MOVE-PARTY triggers
the target cell's special, NIL outside a step.  ENTER-LOCATION records
it as the location's entry direction, so LEAVE-LOCATION can step the
party back out through the door it came in (the Bard's Tale exit).
Declared here, ahead of TRAVEL-PARTY, so the compiler already knows
the symbol is special when TRAVEL-PARTY rebinds it to NIL below.")

(defun travel-party (game file &optional x y facing)
  "Move the party to another zone: the map at FILE, resolved relative
to the current map's directory.  The party arrives at cell (X,Y) facing
FACING when given, else at the target map's start.  A zone already
visited keeps its map and automap knowledge; a new one is loaded from
disk.  Emits :ENTER-ZONE and :ENTER-CELL and triggers the arrival
cell's special."
  (when (game-combat game)
    (error "travel-party: the party is in combat"))
  (let* ((path (%resolve-map-path (dungeon-map-name (game-map game)) file))
         (zone (gethash path (game-zones game)))
         (map (car zone))
         (knowledge (cdr zone)))
    (unless zone
      (setf map (load-map-file path)
            knowledge (make-map-knowledge map))
      ;; A save may carry this zone's automap knowledge from an earlier
      ;; visit — restore it on first (re)entry.
      (let ((pending (assoc path (game-zone-knowledge game) :test #'equal)))
        (when pending
          (%rows->knowledge knowledge (cdr pending))
          (setf (game-zone-knowledge game)
                (remove pending (game-zone-knowledge game)))))
      (setf (gethash path (game-zones game)) (cons map knowledge)))
    (let ((tx (or x (dungeon-map-start-x map)))
          (ty (or y (dungeon-map-start-y map))))
      (unless (and (integerp tx) (< -1 tx (dungeon-map-width map))
                   (integerp ty) (< -1 ty (dungeon-map-height map)))
        (error "Travel target (~S,~S) is outside the ~Dx~D map ~A"
               tx ty (dungeon-map-width map) (dungeon-map-height map) path))
      (setf (game-map game) map
            (game-knowledge game) knowledge
            (game-x game) tx
            (game-y game) ty
            (game-facing game)
            (dir-index (or facing (dungeon-map-start-facing map))))
      (observe game)
      (emit game :enter-zone map)
      (say game "You enter ~A." (map-title map))
      (emit game :enter-cell tx ty)
      ;; Arriving here is never a step (see *STEP-DIR*'s docstring): a
      ;; location special on the target cell must record no entry
      ;; direction, even when TRAVEL-PARTY itself was called from a
      ;; MOVE-PARTY step (a cell's special triggering another zone's
      ;; special) — else the stale outer *STEP-DIR* leaks in.
      (let ((*step-dir* nil))
        (trigger-special game)))))

;;; ---------------------------------------------------------------------
;;; Named destinations — the places a spell can carry the party to.
;;;
;;; A homing spell needs somewhere to home to, and where that is, is
;;; the campaign's business: the engine holds the list and the flight,
;;; the game names the guilds worth flying to.  A destination is a
;;; travel target under a player-facing title, and the cast menu shows
;;; the registered ones in registration order — the game writes the
;;; menu by the order it registers them.

(defstruct (destination (:constructor %make-destination))
  name          ; symbol, e.g. TESTVILLE-GUILD
  title         ; display string, e.g. "The Guild at Testville"
  map           ; map file, resolved like the TRAVEL op's (see below)
  x y           ; arrival cell, or NIL for the map's own start
  facing)       ; arrival heading, or NIL for the map's start facing

(defvar *destinations* '()
  "Registered destinations, in registration order — the cast menu's
order.  Campaign data fills this through DEFINE-DESTINATION.")

(defun define-destination (name &key title map x y facing)
  "Register destination NAME (a symbol) as the map file MAP, arriving
at cell (X,Y) FACING a direction — each of the three optional, and
omitted means the map's own start, exactly as for the TRAVEL op.
TITLE defaults to the capitalized name.  MAP is resolved the way
TRAVEL resolves its file: relative to the map the party is standing
in when the flight begins, so a world keeps its maps in one directory
or names them relative to it.  Registering a name twice replaces the
destination and keeps its place in the menu.  Returns the destination."
  (unless (and name (symbolp name))
    (error "define-destination: NAME must be a symbol, not ~S" name))
  (unless (stringp map)
    (error "define-destination ~S: :MAP must be a map file name, not ~S"
           name map))
  (when (or (and x (not y)) (and y (not x)))
    (error "define-destination ~S: give both :X and :Y or neither" name))
  (let ((new (%make-destination
              :name name
              :title (or title (string-capitalize (substitute #\Space #\-
                                                              (string name))))
              :map map :x x :y y :facing facing))
        (old (find name *destinations* :key #'destination-name)))
    (if old
        (setf *destinations* (substitute new old *destinations*))
        (setf *destinations* (append *destinations* (list new))))
    new))

(defun find-destination (name &optional (errorp t))
  "The registered destination NAME, or NIL when ERRORP is false and
no such destination is registered."
  (or (find name *destinations* :key #'destination-name)
      (when errorp
        (error "Unknown destination ~S (register it with ~
                DEFINE-DESTINATION)" name))))

(defun destinations ()
  "The registered destinations, in menu order."
  (copy-list *destinations*))

(defun travel-to-destination (game name)
  "Carry the party to destination NAME: TRAVEL-PARTY to its map and
cell, so the arrival cell's special triggers and the zone's automap
knowledge is kept, exactly as walking in would.  Signals an error on
an unregistered NAME — a typo in campaign data should be loud."
  (let ((where (find-destination name)))
    (travel-party game (destination-map where)
                  (destination-x where) (destination-y where)
                  (destination-facing where))))

;;; Active effects — the UI's spell strip (shield, light, ...).
;;; An effect is a record: a display name, an optional expiry on the
;;; game clock (ADVANCE-TIME announces and drops it, see time.lisp),
;;; a payload plist of engine facts the mechanics read:
;;;   (:ac N)      party armor class bonus (see HERO-EFFECTIVE-AC)
;;;   (:light t)   the party carries light (see GAME-DARK-P)
;;;   (:compass t) the party knows its facing (see COMPASS-ACTIVE-P)
;;; and an optional icon image — a file name the front-end resolves
;;; and draws in the effects band (NIL = the text label alone).
;;; Effects live in save games (see save.lisp).

(defstruct (effect (:constructor %make-effect))
  name          ; display key (string or symbol); EQUAL identity
  expires-at    ; game minute the effect ends, or NIL (until removed)
  payload       ; readable plist of engine facts: (:ac N), (:light t)
  image)        ; icon file name for the effects band, or NIL

(defun add-effect (game name &key duration payload image)
  "Add active effect NAME (a string or symbol).  DURATION minutes from
now sets the expiry (NIL = until removed); PAYLOAD is a readable plist
of engine facts; IMAGE names the effect's icon file (NIL = text only).
Re-adding NAME refreshes its expiry, payload and image in place — a
recast spell burns anew, keeping its spot in the strip.
Returns the effect list."
  (let ((expires (when duration (+ (game-time game) duration)))
        (existing (find-effect game name)))
    (if existing
        (setf (effect-expires-at existing) expires
              (effect-payload existing) payload
              (effect-image existing) image)
        (setf (game-effects game)
              (append (game-effects game)
                      (list (%make-effect :name name
                                          :expires-at expires
                                          :payload payload
                                          :image image))))))
  (game-effects game))

(defun remove-effect (game name)
  "Remove active effect NAME.  Returns the remaining effect list."
  (setf (game-effects game)
        (remove name (game-effects game)
                :key #'effect-name :test #'equal)))

(defun find-effect (game name)
  "The active EFFECT named NAME, or NIL."
  (find name (game-effects game) :key #'effect-name :test #'equal))

(defun effect-label (effect)
  "EFFECT's display string for the UI's effects strip."
  (string-downcase (princ-to-string (effect-name effect))))

(defun effect-image-path (game effect)
  "EFFECT's icon file resolved like a zone tile pack — relative to the
current map file's directory, so a self-contained world directory
carries its own icons — or NIL when the effect has none."
  (let ((image (effect-image effect)))
    (when image
      (%resolve-map-path (dungeon-map-name (game-map game)) image))))

;;; The timed-effect vocabulary — the keys a spell, song or usable
;;; item may speak to install a timed effect (see APPLY-EFFECT-SPEC).
;;; Each entry is (SPEC-KEY PAYLOAD-KEY VALUE-KIND): the spec key the
;;; campaign writes, the payload key the mechanics read, and the value
;;; shape the validators enforce.  A spec may combine several keys —
;;; they merge into ONE effect record (Bard's Tale's Batchspell is five
;;; enchantments in one casting).

(defparameter *timed-effect-keys*
  '((:buff-ac       :ac            integer) ; party AC bonus (descending AC)
    (:light         :light         flag)    ; the party carries light
    (:night-vision  :night-vision  flag)    ; sight in darkness (cat eyes)
    (:reveal        :reveal        flag)    ; magical sight: light and more
    (:compass       :compass       flag)    ; the party knows its facing
    (:levitate      :levitate      flag)    ; floating over floor traps
    (:buff-damage   :damage-bonus  integer) ; party melee damage bonus
    (:save-bonus    :save-bonus    integer) ; saving-throw bonus
    (:regen-sp      :regen-sp      integer) ; sp-regen multiplier
    (:extra-attacks :extra-attacks integer) ; extra strikes per round
    (:combat-heal   :combat-heal   dice)    ; heals the party each round
    (:foes-ac       :foes-ac       integer) ; foes easier to hit
    (:foes-attack   :foes-attack   integer)) ; foes hit less often
  "The timed-effect vocabulary: (SPEC-KEY PAYLOAD-KEY VALUE-KIND).")

;;; The instant-effect vocabulary — resolved at cast/use time, no
;;; effect record.  (SPEC-KEY VALUE-KIND COMBAT-ONLY-P).  The keys
;;; marked flavor-only await their subsystem (summoned allies, foe
;;; morale); they cast, pay and speak, so campaign data can already
;;; carry the canonical spell.

(defparameter *instant-effect-keys*
  '((:damage           dice    t)   ; strikes the nearest living monster
    (:damage-per-level dice    t)   ; the roll multiplied by caster level
    (:damage-group     dice    t)   ; every monster of the nearest group
    (:damage-all       dice    t)   ; every living monster
    (:slay             percent t)   ; chance to fell the nearest monster
    (:push-foes        flag    t)   ; flavor: hurls the foes back
    (:halt-foes        flag    t)   ; flavor: freezes the foes
    (:calm             flag    t)   ; flavor: soothes the foes
    (:heal             heal    nil) ; heals one chosen hero (:full = all)
    (:heal-party       heal    nil) ; heals every living hero
    (:resurrect        flag    nil) ; raises the fallen to 1 hp
    (:cure             ailments nil) ; lifts the named ailments (party.lisp)
    (:scry             flag    nil) ; speaks the party's position
    (:disarm-traps     integer nil) ; traps ahead made safe (reach in squares)
    (:teleport         teleport nil) ; N: offset teleport, max N squares;
                                    ;   T: carries the party to a named
                                    ;   destination (DEFINE-DESTINATION)
    (:summon           string  nil)) ; flavor: a summoned ally (to come)
  "The instant-effect vocabulary: (SPEC-KEY VALUE-KIND COMBAT-ONLY-P).")

(defun %effect-value-ok-p (kind value)
  "Does VALUE fit KIND (the vocabulary tables' value shapes)?"
  (ecase kind
    (flag    (eq value t))
    (integer (and (integerp value) (plusp value)))
    (percent (and (integerp value) (< 0 value 101)))
    (dice    (and (or (integerp value) (stringp value))
                  (ignore-errors (parse-dice value) t)))
    (heal    (or (eq value :full)
                 (and (or (integerp value) (stringp value))
                      (ignore-errors (parse-dice value) t))))
    ;; a cure names ailments and nothing else, so a typo in a campaign's
    ;; :CURE list is refused where it is written (AILMENT-P, party.lisp)
    (ailments (and (consp value) (every #'ailment-p value)))
    (string  (stringp value))
    (teleport (or (eq value t)
                  (and (integerp value) (plusp value))))))

(defun check-effect-spec (context name spec &key timed-only)
  "Validate SPEC, a plist over the effect vocabulary (*TIMED-EFFECT-
KEYS* and — unless TIMED-ONLY — *INSTANT-EFFECT-KEYS*, plus :DURATION).
Signals a clear error naming CONTEXT (\"define-spell\", ...) and NAME
on the first problem; returns the timed keys and the instant keys
present, as two values."
  (let ((timed '()) (instant '()))
    (loop for tail on spec by #'cddr
          for key = (first tail)
          do (let ((value (second tail))
                   (tentry (assoc key *timed-effect-keys*))
                   (ientry (assoc key *instant-effect-keys*)))
               (cond
                 ((eq key :duration))   ; checked against the keys below
                 (tentry
                  (unless (%effect-value-ok-p (third tentry) value)
                    (error "~A ~S: ~S ~S is no ~(~A~) value"
                           context name key value (third tentry)))
                  (push key timed))
                 ((and ientry timed-only)
                  (error "~A ~S: ~S is an instant effect -- only the ~
                          timed vocabulary (~{~S~^ ~}) fits here"
                         context name key
                         (mapcar #'first *timed-effect-keys*)))
                 (ientry
                  (unless (%effect-value-ok-p (second ientry) value)
                    (error "~A ~S: ~S ~S is no ~(~A~) value"
                           context name key value (second ientry)))
                  (push key instant))
                 (t
                  (error "~A ~S: unknown effect key ~S -- the vocabulary: ~
                          timed ~{~S~^ ~}~:[; instant ~{~S~^ ~}~;~]"
                         context name key
                         (mapcar #'first *timed-effect-keys*)
                         timed-only
                         (mapcar #'first *instant-effect-keys*))))))
    (unless (or timed instant)
      (error "~A ~S: the spec ~S names no effect" context name spec))
    (let ((duration (getf spec :duration)))
      (cond (timed
             (unless (or (eq duration :indefinite)
                         (and (integerp duration) (plusp duration)))
               (error "~A ~S: a timed effect needs a :duration -- a ~
                       positive integer of game minutes, or :indefinite"
                      context name)))
            (duration
             (error "~A ~S: :duration ~S without a timed effect"
                    context name duration))))
    (values (nreverse timed) (nreverse instant))))

(defun effect-spec-combat-only-p (spec)
  "True when SPEC carries an instant effect that needs a fight (the
damage family, :SLAY and the foe-handling keys)."
  (loop for tail on spec by #'cddr
        thereis (let ((entry (assoc (first tail) *instant-effect-keys*)))
                  (and entry (third entry) t))))

(defparameter *foe-facing-timed-keys* '(:foes-ac :foes-attack)
  "The timed keys aimed at the enemy rather than at the party — the
words that blunt a line's aim or its guard.  They install a party-wide
effect record like any other timed key, but what they work on is the
foes, which is why they are measured against a distance.")

(defun effect-spec-reaches-foes-p (spec)
  "True when SPEC is aimed at the enemy at all: an instant that needs a
fight (EFFECT-SPEC-COMBAT-ONLY-P) or one of the foe-facing timed keys.
This is the set a :REACH is measured for — a word that slows a line
must still carry to that line — while a mending or scrying word aims
at no distance and measures none."
  (or (effect-spec-combat-only-p spec)
      (loop for tail on spec by #'cddr
            thereis (and (member (first tail) *foe-facing-timed-keys*) t))))

(defun effect-spec-target-kind (spec)
  "What SPEC needs aimed at: :HERO when it heals, cures or raises one
chosen hero; :OFFSET when it teleports a real distance (an integer
:TELEPORT — the menu asks for a heading and a count); :DESTINATION
when it carries the party somewhere named (:TELEPORT T — the menu
lists the registered destinations); else :NONE (damage strikes the
melee target, buffs and light cover the party, :heal-party needs no
choosing).  Spells and usable items share this rule."
  (cond ((and (not (getf spec :heal-party))
              (or (getf spec :heal) (getf spec :resurrect)
                  (getf spec :cure)))
         :hero)
        ((integerp (getf spec :teleport)) :offset)
        ((eq (getf spec :teleport) t) :destination)
        (t :none)))

;;; ---------------------------------------------------------------------
;;; Reading an effect spec back out in player's words.
;;;
;;; The spell and song cards (and any campaign tool that wants to say
;;; what a thing does) need prose, and deriving it from the spec keeps
;;; the words honest: a spell that says "Heals 4-16" heals 4-16
;;; because the same plist feeds both the sentence and the cast.  A
;;; campaign may still write its own :DESCRIPTION when the derived
;;; line is too plain — that overrides nothing, it reads alongside.
;;;
;;; Every line is kept inside +TAKEOVER-COLUMNS+ so the narrow lores
;;; card never wraps mid-figure.

(defparameter *effect-phrases*
  '(;; instant
    (:damage           "Damage ~A")
    (:damage-per-level "Damage ~A a level")
    (:damage-group     "Group damage ~A")
    (:damage-all       "Damage ~A to all")
    (:slay             "Fells a foe (~D%)")
    (:push-foes        "Hurls the foes back")
    (:halt-foes        "Freezes the foes")
    (:calm             "Soothes the foes")
    (:heal             "Heals ~A")
    (:heal-party       "Heals all ~A")
    (:resurrect        "Raises the fallen")
    (:cure             "Cures ~{~A~^, ~}")
    (:scry             "Tells where you are")
    (:disarm-traps     "Disarms traps (~D sq)")
    (:teleport         "Folds space (~D sq)")
    (:summon           "Summons ~A")
    ;; timed
    (:buff-ac          "AC ~D better")
    (:light            "Light")
    (:night-vision     "Sight in the dark")
    (:reveal           "Magical sight")
    (:compass          "Shows your facing")
    (:levitate         "Floats over traps")
    (:buff-damage      "Damage +~D")
    (:save-bonus       "Saving rolls +~D")
    (:regen-sp         "SP return x~D")
    (:extra-attacks    "+~D strike a round")
    (:combat-heal      "Heals ~A a round")
    (:foes-ac          "Foes easier to hit")
    (:foes-attack      "Foes hit less often"))
  "One player-facing phrase per effect key, in the order a card lists
them — a FORMAT string taking the key's value where the value is worth
saying, a plain sentence where it is not (a flag, or a number the
player never sees).")

(defun %effect-phrase (key value)
  "KEY's phrase with VALUE read into it, or NIL when KEY names no
phrase.  Dice values render as their span (\"4-16\") and a :FULL heal
as the word."
  (let ((phrase (second (assoc key *effect-phrases*))))
    (when phrase
      (cond
        ;; :TELEPORT alone of the valued keys may also be a bare flag
        ;; (a flight to a named destination) — then it has no square
        ;; count to quote
        ((eq key :teleport)
         (if (eq value t)
             "Takes you to a known place"
             (format nil phrase value)))
        ;; a flag's phrase is already a whole sentence
        ((eq value t) phrase)
        ((member key '(:heal :heal-party))
         (if (eq value :full)
             (if (eq key :heal) "Heals fully" "Heals all fully")
             (format nil phrase (dice-range-text value))))
        ((member key '(:damage :damage-per-level :damage-group
                       :damage-all :combat-heal))
         (format nil phrase (dice-range-text value)))
        ((eq key :cure)
         (format nil phrase (mapcar #'ailment-noun value)))
        (t (format nil phrase value))))))

(defun effect-duration-text (spec)
  "SPEC's timed run in player's words — \"for 60 minutes\", \"until
dispelled\" — or NIL when nothing in it lasts."
  (let ((duration (getf spec :duration)))
    (cond ((null duration) nil)
          ((eq duration :indefinite) "until dispelled")
          ((= duration 1) "for a minute")
          (t (format nil "for ~D minutes" duration)))))

(defun effect-summary-lines (spec)
  "SPEC — a plist over the effect vocabulary — as player-facing lines,
one phrase per line in the vocabulary's own order (instant keys
first), closing with the timed run when the spec carries one.  The
spell and song cards draw these; a spec naming nothing gives NIL."
  (let ((lines '()))
    (dolist (table (list *instant-effect-keys* *timed-effect-keys*))
      (dolist (entry table)
        (let* ((key (first entry))
               (value (getf spec key)))
          (when value
            (let ((phrase (%effect-phrase key value)))
              (when phrase (push phrase lines)))))))
    (let ((duration (effect-duration-text spec)))
      (when (and duration lines)
        (push duration lines)))
    (nreverse lines)))

(defun %effects-sum (game key)
  (let ((n 0))
    (dolist (e (game-effects game) n)
      (incf n (or (getf (effect-payload e) key) 0)))))

(defun effects-ac-bonus (game)
  "The summed :AC bonuses of the active effects (a party-wide shield)."
  (%effects-sum game :ac))

(defun effects-damage-bonus (game)
  "The summed melee damage bonuses of the active effects."
  (%effects-sum game :damage-bonus))

(defun effects-save-bonus (game)
  "The summed saving-throw bonuses of the active effects — every
SAVING-THROW roll (see party.lisp) adds it."
  (%effects-sum game :save-bonus))

(defun effects-extra-attacks (game)
  "Extra strikes every attacking hero gets per combat round from the
active effects."
  (%effects-sum game :extra-attacks))

(defun effects-foes-ac (game)
  "How much easier the foes are to hit (added to their descending AC)."
  (%effects-sum game :foes-ac))

(defun effects-foes-attack (game)
  "How much worse the foes swing (subtracted from their to-hit bonus)."
  (%effects-sum game :foes-attack))

(defun effects-regen-sp (game)
  "The spell-point regeneration multiplier: the largest :REGEN-SP among
the active effects, at least 1."
  (let ((m 1))
    (dolist (e (game-effects game) m)
      (setf m (max m (or (getf (effect-payload e) :regen-sp) 1))))))

(defun effects-combat-heal (game)
  "The :COMBAT-HEAL dice of the active effects, as a list — each heals
every living hero at the end of a combat round."
  (let ((dice '()))
    (dolist (e (game-effects game) (nreverse dice))
      (let ((d (getf (effect-payload e) :combat-heal)))
        (when d (push d dice))))))

(defun light-active-p (game)
  "True when any active effect lets the party see: a :LIGHT payload, or
its magical kin :REVEAL (revelation light) and :NIGHT-VISION (cat
eyes) — all three defeat darkness."
  (and (some (lambda (e)
               (let ((p (effect-payload e)))
                 (or (getf p :light) (getf p :reveal)
                     (getf p :night-vision))))
             (game-effects game))
       t))

(defun compass-active-p (game)
  "True when any active effect orients the party (a :COMPASS payload) —
only then do the front-ends show the compass rose and the facing."
  (and (some (lambda (e) (getf (effect-payload e) :compass))
             (game-effects game))
       t))

(defun levitate-active-p (game)
  "True when any active effect floats the party (a :LEVITATE payload) —
a floating party passes over floor traps (see the TRAP op)."
  (and (some (lambda (e) (getf (effect-payload e) :levitate))
             (game-effects game))
       t))

(defun apply-effect-spec (game name spec &key image extra-payload)
  "Install SPEC's timed keys — the shared vocabulary spells, usable
items and songs speak (*TIMED-EFFECT-KEYS*, e.g. :buff-ac 2 :light t
:duration 30) — as ONE active effect NAME with icon IMAGE; several
timed keys merge into a single record.  A :duration of :INDEFINITE
burns until removed.  EXTRA-PAYLOAD is appended to the payload (a
song's :SONG marker).  Returns the effect list; rejects a spec naming
no timed effect."
  (let ((payload '()))
    (dolist (entry *timed-effect-keys*)
      (let ((value (getf spec (first entry))))
        (when value
          (setf payload (append payload (list (second entry) value))))))
    (unless payload
      (error "apply-effect-spec ~S: ~S names no timed effect (one of ~
              ~{~S~^ ~})"
             name spec (mapcar #'first *timed-effect-keys*)))
    (let ((duration (getf spec :duration)))
      (add-effect game name
                  :duration (if (eq duration :indefinite) nil duration)
                  :payload (append payload extra-payload)
                  :image image))))

(defun turn-left (game)
  (setf (game-facing game) (turn-dir (game-facing game) -1))
  (advance-time game)
  (observe game)
  (dir-keyword (game-facing game)))

(defun turn-right (game)
  (setf (game-facing game) (turn-dir (game-facing game) 1))
  (advance-time game)
  (observe game)
  (dir-keyword (game-facing game)))

(defun turn-around (game)
  (setf (game-facing game) (dir-opposite (game-facing game)))
  (advance-time game)
  (observe game)
  (dir-keyword (game-facing game)))

(defun move-party (game &optional (relative :forward))
  "Attempt to step the party one cell.  RELATIVE is :forward or :back
\(a Bard's Tale back-step keeps the current facing).  Returns
:moved, :door (stepped through a door) or :blocked.  Entering a cell
emits :ENTER-CELL and triggers the cell's special; a door step emits
:DOOR first; bumping a wall emits :BLOCKED.  Signals an error during
combat — there is no walking away from a fight (see ATTEMPT-FLEE)."
  (when (game-combat game)
    (error "move-party: the party is in combat (attack or flee first)"))
  (when (game-location game)
    (error "move-party: the party is inside ~A (LEAVE-LOCATION first)"
           (location-title (game-location game))))
  (let* ((dir (ecase relative
                (:forward (game-facing game))
                (:back (dir-opposite (game-facing game)))))
         (wall (cell-wall (game-map game) (game-x game) (game-y game) dir)))
    (if (not (wall-passable-p wall))
        (progn
          (emit game :blocked (dir-keyword dir))
          :blocked)
        (multiple-value-bind (nx ny)
            (neighbor (game-map game) (game-x game) (game-y game) dir)
          (if (null nx)
              (progn
                (emit game :blocked (dir-keyword dir))
                :blocked)
              (progn
                (setf (game-x game) nx
                      (game-y game) ny)
                ;; The door creaks before the room answers: the cue
                ;; must precede whatever the target cell's special
                ;; emits (an encounter sting, a location).
                (when (eq wall :door)
                  (emit game :door (dir-keyword dir)))
                ;; The step costs time before the party looks around:
                ;; a light that gutters out right now shrinks what this
                ;; very step maps, and an AT-NIGHT special on the target
                ;; cell must see the post-step clock.
                (advance-time game)
                ;; time passed, so the venom worked: poison takes its
                ;; due on the step itself, before the cell has its say
                (poison-bite game)
                (observe game)
                (emit game :enter-cell nx ny)
                (let ((map (game-map game)))
                  (let ((*step-dir* dir))
                    (trigger-special game))
                  ;; The cell's own story has had its say; now the
                  ;; zone's wandering monsters may find the party —
                  ;; unless a TRAVEL op just switched zones (the roll
                  ;; belongs to the map the step was taken on).
                  (when (eq map (game-map game))
                    (maybe-wandering-encounter game)))
                (if (eq wall :door) :door :moved)))))))
