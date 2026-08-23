;;; Lambda's Tale — combat.
;;;
;;; Monster types are campaign data (DEFINE-MONSTER); the engine only
;;; knows the mechanics.  Combat is Bard's Tale round-based: the party
;;; declares actions, heroes strike first, then every surviving monster
;;; swings at a random front-rank hero — or shoots, where it stands off
;;; and its type can (see below).  The whole transcript travels as
;;; :MESSAGE events; :COMBAT-START and :COMBAT-END frame the fight.
;;;
;;; Reach cuts both ways (HERO-IN-REACH-P): the front ranks are the
;;; heroes the monsters can hit AND the only heroes who can trade
;;; melee blows back.  A back-rank hero attacks only over the enemy's
;;; heads — with an equipped bow-and-arrow pair (HERO-MISSILE-DICE,
;;; the arrows carrying the dice); bare of one, the attack action is
;;; simply out of reach for it, and the orders page says so instead
;;; of recording it.
;;;
;;; The enemy answers in kind.  A monster type the campaign gave a
;;; :MISSILE shoots from where it stands while its group is still
;;; walking in (MONSTER-CAN-SHOOT-P), and an arrow — or a spat venom,
;;; or a word out of the dark — does not care which rank a hero stands
;;; in: its field is the whole party, the mirror of the back-rank hero
;;; shooting over the enemy's heads.  Bare of a missile, a group short
;;; of melee has nothing to spend its round on but the ground it covers.
;;;
;;; To-hit: d20 + bonus hits when it reaches 20 - AC (descending AC,
;;; unarmored 10 = hit on 10+) — melee rides on STR, a shot on DEX.
;;; Defending is +4 AC for the round.

(in-package :tale)

(defstruct (monster-type (:constructor %make-monster-type))
  name                ; string, e.g. "giant rat"
  (level 1)           ; to-hit bonus
  (hp-dice "1d8")
  (ac 10)
  (damage "1d4")
  missile             ; dice its answer from a distance throws, or NIL:
                      ; a type that must close before it can do anything
  missile-reach       ; how far that answer carries, in feet — NIL is
                      ; the unmeasured missile that always reaches
  (missile-verb "SHOOTS")  ; the transcript's word for a landed shot
  speed               ; feet it covers in a round — its own gait — or
                      ; NIL to walk at *COMBAT-CLOSE-STEP* like the rest
  (xp 10)
  (gold-dice 0)
  item                ; item this monster may carry, or NIL
  (item-chance 100)   ; percent chance the carried item drops
  (inflicts '())      ; ((AILMENT PERCENT) ...) rolled on a landed blow
  image)              ; portrait file name, map-relative, or NIL

(defvar *monster-types* (make-hash-table :test 'equalp))

(defun define-monster (name &key (level 1) (hp-dice "1d8") (ac 10)
                                 (damage "1d4") missile missile-reach
                                 (missile-verb "SHOOTS") speed
                                 (xp 10) (gold 0)
                                 item (item-chance 100) inflicts image)
  "Register monster type NAME (a string).  Campaign data calls this.
:MISSILE gives the type an answer from a distance — the dice its
arrow, its venom or its breath throws while its group is still walking
in — and :MISSILE-REACH how far in feet that answer carries, measured
against where the group stands; leave the reach out and the missile
goes unmeasured and always reaches, the rule the hero's own missiles
follow.  A shot picks any living hero, whatever rank they stand in,
and lands with the same to-hit roll and the same :INFLICTS as a blow;
:MISSILE-VERB is the transcript's word for one (\"SHOOTS\", \"SPITS
AT\").  A type without a :MISSILE is the plain fighter it always was:
until its group reaches melee its round is the ground it covers.
:SPEED is that gait — the feet it covers in a round, its own instead
of *COMBAT-CLOSE-STEP*; 0 nails it where the fight found it, which
with a missile is how a campaign fields an enemy that stands off and
shoots.
:ITEM names an item the monster carries into the fight — after a
victory each fallen carrier rolls :ITEM-CHANCE percent, and the first
success hands the party that item, one find per fight at most (see
%AWARD-LOOT).  :INFLICTS ((AILMENT PERCENT) ...) is what its blows
carry besides damage — a landed hit rolls each entry's percent chance
to lay that ailment on the hero it struck (%MONSTER-INFLICT), which is
how a campaign expresses the Bard's Tale monster powers (poison,
insanity, paralysis, stone).  :IMAGE names the type's portrait file,
resolved beside the map like a location picture (COMBAT-IMAGE-PATH) —
the Amiga front-end shows it in the view column for as long as the
fight lasts."
  (dolist (entry inflicts)
    (unless (and (consp entry) (null (cddr entry))
                 (ailment-p (first entry))
                 (integerp (second entry))
                 (< 0 (second entry) 101))
      (error "define-monster ~S: :inflicts wants (AILMENT PERCENT) ~
              entries over ~S, got ~S"
             name *ailments* entry)))
  (when missile
    (parse-dice missile))               ; bad dice fail here, not mid-fight
  (when (and missile-reach
             (not (and (integerp missile-reach) (plusp missile-reach))))
    (error "define-monster ~S: :missile-reach must be a positive ~
            integer in feet (got ~S)" name missile-reach))
  (when (and missile-reach (not missile))
    (error "define-monster ~S: :missile-reach wants a :missile to ~
            carry it" name))
  (unless (and (stringp missile-verb) (plusp (length missile-verb)))
    (error "define-monster ~S: :missile-verb must be a word for the ~
            transcript (got ~S)" name missile-verb))
  (when (and speed (not (and (integerp speed) (not (minusp speed)))))
    (error "define-monster ~S: :speed must be feet per round, zero or ~
            more (got ~S)" name speed))
  (setf (gethash name *monster-types*)
        (%make-monster-type :name name :level level :hp-dice hp-dice
                            :ac ac :damage damage :missile missile
                            :missile-reach missile-reach
                            :missile-verb missile-verb :speed speed
                            :xp xp :gold-dice gold
                            :item item :item-chance item-chance
                            :inflicts inflicts :image image))
  name)

(defun find-monster-type (name)
  (or (gethash name *monster-types*)
      (error "Unknown monster ~S (register it with DEFINE-MONSTER)" name)))

;;; ---------------------------------------------------------------------
;;; Distance — where the groups stand, and how they close.
;;;
;;; Ten feet is one dungeon square, the scale the spellbook already
;;; speaks in (a 30' scrying reaches three cells), and the enemy stands
;;; in whole squares: a group is 10, 30, 50 feet off, never 25.  A group
;;; at +MELEE-DISTANCE+ is toe to toe with the front rank, and that is
;;; the only distance at which melee lands — in either direction.
;;; Everything further out is missile and spell work, each measured
;;; against its own :REACH, and at the end of every round the far groups
;;; walk one *COMBAT-CLOSE-STEP* nearer.
;;;
;;; A fight of one group opens at melee and plays exactly as it did
;;; before distance existed; the rules below only begin to speak when an
;;; encounter fields several groups, or names a distance of its own.

(defconstant +melee-distance+ 10
  "Feet at which a group stands toe to toe with the front rank — the
only distance where a melee blow lands, and the distance every group
is walking towards.")

(defparameter *combat-group-spacing* 20
  "Feet between one encounter group's opening distance and the next: the
first group stands at +MELEE-DISTANCE+ and each later one starts this
much further back (10, 30, 50 feet ...), so a fight of several groups
gives the archers and casters their rounds while the rest walk in.  An
encounter may name its own distances instead — see START-COMBAT.")

(defparameter *combat-close-step* 10
  "Feet a group beyond melee walks in at the end of each round — the
gait of every monster type that names no :SPEED of its own.  NIL (or a
non-positive value) nails those groups where the fight found it; a
type with its own speed keeps walking at it either way.")

(defstruct (monster (:constructor %make-monster))
  kind                ; MONSTER-TYPE
  hp
  (group 0)           ; which entry of START-COMBAT's spec spawned it:
                      ; a group stands together, closes together, and is
                      ; what a group-wide spell covers
  (distance +melee-distance+))  ; feet between it and the front rank

(defun monster-alive-p (monster)
  (> (monster-hp monster) 0))

(defun monster-in-melee-p (monster)
  "Has the monster's group closed all the way in?  Only then does it
swing, and only then can a front-rank hero swing back."
  (<= (monster-distance monster) +melee-distance+))

(defun monster-can-shoot-p (monster)
  "Can MONSTER answer from where it stands, short of melee?  True when
its type carries a :MISSILE whose :MISSILE-REACH covers the ground
still between them — an unmeasured missile carrying however far the
fight asks, exactly as the hero's own does (HERO-STRIKE-FUNCTION).
Melee comes first for the enemy too: a monster that has closed swings
rather than shoots, so this is only asked of the ones still walking."
  (let* ((type (monster-kind monster))
         (reach (monster-type-missile-reach type)))
    (and (monster-type-missile type)
         (or (null reach) (>= reach (monster-distance monster))))))

(defun monster-step (monster)
  "Feet MONSTER covers at the end of a round: its type's own :SPEED
where the campaign gave it a gait, else *COMBAT-CLOSE-STEP*, the pace
everything else walks at.  Zero — or a dial turned off — leaves the
line where it stands."
  (let ((speed (monster-type-speed (monster-kind monster))))
    (if speed speed *combat-close-step*)))

(defstruct (combat (:constructor %make-combat))
  monsters            ; list of MONSTER (the fallen stay, filtered below)
  defenders           ; heroes defending during the current round
  (round-no 0))       ; completed rounds; COMBAT-ROUND counts them up

(defun alive-monsters (combat)
  (remove-if-not #'monster-alive-p (combat-monsters combat)))

(defun combat-distance (combat)
  "Feet to the nearest living group — the number every reach is
measured against — or NIL when nothing is left standing."
  (let ((nearest nil))
    (dolist (m (alive-monsters combat) nearest)
      (when (or (null nearest) (< (monster-distance m) nearest))
        (setf nearest (monster-distance m))))))

(defun nearest-monster (combat)
  "The living monster a blow or a bolt lands on: the first survivor of
the nearest group, encounter order breaking a tie — or NIL when the
fight is over."
  (let ((distance (combat-distance combat)))
    (when distance
      (find distance (alive-monsters combat) :key #'monster-distance))))

(defun monsters-in-reach (combat reach)
  "The living monsters standing within REACH feet — all of them when
REACH is NIL, the unmeasured case a campaign gets by saying nothing."
  (if (null reach)
      (alive-monsters combat)
      (remove-if (lambda (m) (> (monster-distance m) reach))
                 (alive-monsters combat))))

(defun group-monsters (combat group)
  "The living monsters of encounter group GROUP."
  (remove-if-not (lambda (m) (eql (monster-group m) group))
                 (alive-monsters combat)))

(defun combat-group-indices (combat)
  "The encounter groups still standing, in encounter order."
  (let ((seen '()))
    (dolist (m (alive-monsters combat) (nreverse seen))
      (pushnew (monster-group m) seen))))

(defun combat-groups (combat)
  "The living enemy as one row per encounter group — a list of
\(MONSTER-TYPE COUNT DISTANCE) — nearest first, encounter order
breaking a tie.  Two groups of one kind stay two rows: they were met
apart and they stand apart."
  (let ((rows '()))
    (dolist (index (combat-group-indices combat))
      (let ((members (group-monsters combat index)))
        (push (list (monster-kind (first members))
                    (length members)
                    (monster-distance (first members)))
              rows)))
    (stable-sort (nreverse rows) #'< :key #'third)))

(defun group-label (type count)
  "A group named the way every page names it: \"3 grey gnawers\", and a
lone one without the s."
  (format nil "~D ~A~A" count (monster-type-name type)
          (if (> count 1) "s" "")))

(defun combat-enemy-image (combat)
  "The picture file name of the enemy the party faces: the :IMAGE of
the first living monster type that names one, in encounter order — the
leading group's portrait, and the next group's once it falls — or NIL
when no monster in the fight has a picture."
  (dolist (m (alive-monsters combat))
    (let ((image (monster-type-image (monster-kind m))))
      (when image
        (return image)))))

(defun combat-image-path (game)
  "The current fight's enemy portrait resolved like an effect icon —
relative to the current map file's directory, so a self-contained
world directory carries its own art — or NIL: no combat, or no
monster in it names an :IMAGE.  The Amiga front-end shows it in the
view column while the round-orders page takes over the message area."
  (let ((combat (game-combat game)))
    (when combat
      (let ((image (combat-enemy-image combat)))
        (when image
          (%resolve-map-path (dungeon-map-name (game-map game)) image))))))

(defparameter *victory-image* nil
  "Picture file name for the spoils of a won fight — a treasure
chest — resolved beside the current map like a monster portrait
(VICTORY-IMAGE-PATH), or NIL for none.  A campaign sets it; the Amiga
front-end shows it in the view column over the victory transcript for
*VICTORY-LINGER* seconds before play resumes.")

(defparameter *victory-linger* 3
  "Seconds the front-ends keep the victory screen — the treasure
picture over the won fight's transcript — before giving the view
column back to the walls.")

(defun victory-image-path (game)
  "*VICTORY-IMAGE* resolved relative to the current map file's
directory (the COMBAT-IMAGE-PATH rule), or NIL when no campaign named
one."
  (when *victory-image*
    (%resolve-map-path (dungeon-map-name (game-map game))
                       *victory-image*)))

(defun combat-banner (combat)
  "The line that opens a fight.  A group already at melee is simply
named; one standing off says how far off, which is the whole news of
the opening round."
  (with-output-to-string (s)
    (write-string "You face " s)
    (let ((first t))
      (dolist (group (combat-groups combat))
        (unless first (write-string " and " s))
        (setf first nil)
        (write-string (group-label (first group) (second group)) s)
        (when (> (third group) +melee-distance+)
          (format s " at ~D feet" (third group)))))
    (write-char #\! s)))

(defun %spawn-monsters (name count group distance)
  (let ((type (find-monster-type name))
        (monsters '()))
    (dotimes (i count (nreverse monsters))
      (push (%make-monster
             :kind type
             :hp (max 1 (roll-dice (monster-type-hp-dice type)))
             :group group
             :distance distance)
            monsters))))

(defun %group-start-distance (index)
  "Where encounter group INDEX stands when the fight opens, feet: the
first at melee, each later one *COMBAT-GROUP-SPACING* further back."
  (+ +melee-distance+ (* index (max 0 *combat-group-spacing*))))

(defun start-combat (game spec)
  "Begin combat.  SPEC is a list of (MONSTER-NAME COUNT [DISTANCE])
groups; COUNT may be dice (see PARSE-DICE).  DISTANCE is where that
group stands when the fight opens, in feet — leave it out and the
groups line up from +MELEE-DISTANCE+ backwards, one
*COMBAT-GROUP-SPACING* apart, so the first is always already at melee.
Returns the new COMBAT."
  (when (game-combat game)
    (error "start-combat: combat is already in progress"))
  (let ((monsters '())
        (index -1))
    (dolist (group spec)
      (incf index)
      (setf monsters
            (append monsters
                    (%spawn-monsters (first group)
                                     (max 1 (roll-dice (second group)))
                                     index
                                     (or (third group)
                                         (%group-start-distance index))))))
    (let ((combat (%make-combat :monsters monsters)))
      (setf (game-combat game) combat)
      (emit game :combat-start monsters)
      (say game "~A" (combat-banner combat))
      combat)))

;;; ---------------------------------------------------------------------
;;; Wandering monsters — the Bard's Tale random encounter.
;;;
;;; A zone opts in by declaring a table and a chance (ZONE :ENCOUNTERS
;;; ((MONSTER COUNT-DICE [WEIGHT])...) :ENCOUNTER-CHANCE P); every
;;; successful step rolls against it — MOVE-PARTY calls
;;; MAYBE-WANDERING-ENCOUNTER after the cell's own special has had its
;;; say, so a scripted (ENCOUNTER ...) fight always wins.  After dark
;;; an outdoor zone switches to its :NIGHT-ENCOUNTERS table and
;;; :NIGHT-ENCOUNTER-CHANCE when it declares them — the classic
;;; "the streets are meaner at night" — each falling back to the base
;;; key it goes without, while a :DARK zone keeps its base pair at all
;;; hours: there is no night underground.  *ENCOUNTER-RATE* scales the
;;; whole thing.

(defparameter *encounter-rate* 1
  "Scales every zone's wandering-encounter chance: 1 as authored, 2
doubles it, 1/2 halves it.  NIL (or any non-positive value) disables
wandering monsters entirely — combat then comes only from
\(ENCOUNTER ...) cell specials.  Read on every step, so a rebinding
takes effect at once: a campaign sets its taste, a difficulty setting
may rebind it later, the REPL can turn it live.")

(defun %zone-encounter-check (map time)
  "The wandering-monster chance and table MAP presents at clock TIME,
as (values CHANCE TABLE) — the :NIGHT-* keys after dark in an outdoor
zone, each falling back to its base key when not declared.  A :DARK
zone keeps the base pair at all hours.  CHANCE is NIL when the zone
declares a table but no chance (then nothing ever shows up), and both
are NIL when there is no table at all."
  (let* ((night (and (not (dungeon-map-dark map))
                     (not (daylight-p time))))
         (table (or (and night (dungeon-map-night-encounters map))
                    (dungeon-map-encounters map))))
    (when table
      (values (or (and night (dungeon-map-night-encounter-chance map))
                  (dungeon-map-encounter-chance map))
              table))))

(defun %pick-encounter (table)
  "One (MONSTER COUNT-DICE [WEIGHT [DISTANCE]]) entry drawn from TABLE,
weighted by each entry's WEIGHT (default 1).  DISTANCE, when the table
names one, is how far off the group is met, in feet — the wanderers a
zone lets you see coming.  A single-entry table draws no random number,
so scripted tests spend rolls only where a choice exists."
  (if (null (rest table))
      (first table)
      (let ((r (roll (let ((total 0))
                       (dolist (e table total)
                         (incf total (or (third e) 1)))))))
        (dolist (e table)
          (let ((w (or (third e) 1)))
            (if (< r w)
                (return e)
                (decf r w)))))))

(defun maybe-wandering-encounter (game)
  "Roll the current zone's wandering-monster check and start the fight
when it comes up: with CHANCE * *ENCOUNTER-RATE* percent probability
\(see %ZONE-ENCOUNTER-CHECK for which chance and table apply at the
current clock) one weighted table entry spawns as a COUNT-DICE strong
group.  A disabled *ENCOUNTER-RATE*, a zone without a table, a fight
already running or the party standing inside a location all skip the
check without drawing a random number.  Returns the new COMBAT, or
NIL when no monsters showed up."
  (let ((rate *encounter-rate*))
    (when (and rate (plusp rate)
               (not (game-combat game))
               (not (game-location game)))
      (multiple-value-bind (chance table)
          (%zone-encounter-check (game-map game) (game-time game))
        (when (and chance (< (roll 100) (* chance rate)))
          (let ((entry (%pick-encounter table)))
            (start-combat game (list (list (first entry)
                                           (second entry)
                                           (fourth entry))))))))))

(defparameter *idle-encounter-minutes* 30
  "Idle game-minutes per wandering-monster roll: while the party
stands still under the living-world clock, every full half-hour of
game time accrued draws one roll of the zone's wandering check — the
same chance a step pays per minute, but far more thinly spread, since
a loitering party draws less attention than a marching one.  NIL (or
a non-positive value) turns idle encounters off; steps roll as ever.
A zone may override this dial with (ZONE :IDLE-ENCOUNTER-MINUTES M) —
its own period, or an explicit NIL for a friendly zone where
loitering is safe (see MAYBE-IDLE-ENCOUNTER).")

(defun maybe-idle-encounter (game minutes)
  "Credit MINUTES of idle game time toward the wandering vigil and
roll the zone's check once per full period accrued — the zone's own
\(ZONE :IDLE-ENCOUNTER-MINUTES M), or *IDLE-ENCOUNTER-MINUTES* where
the zone names none; a zone's explicit NIL keeps its loiterers safe —
the remainder carrying forward on GAME.  Chance, table and guards are
MAYBE-WANDERING-ENCOUNTER's; the first fight wins and later periods
in the same credit drain without a draw.  The front-end's idle
heartbeat calls this right after its ADVANCE-TIME — a step's own
roll stays MOVE-PARTY's.  Returns the new COMBAT, or NIL while the
streets stay quiet."
  (let* ((zone (dungeon-map-idle-encounter-minutes (game-map game)))
         (period (if (eq zone :default) *idle-encounter-minutes* zone)))
    (when (and period (plusp period))
      (incf (game-idle-encounter-clock game) minutes)
      (let ((combat nil))
        (loop while (>= (game-idle-encounter-clock game) period)
              do (decf (game-idle-encounter-clock game) period)
                 (setf combat (or combat
                                  (maybe-wandering-encounter game))))
        combat))))

;;; ---------------------------------------------------------------------
;;; Attack resolution

(defun %attack-hits-p (bonus ac)
  "Roll d20 + BONUS against descending AC: hit on 20 - AC or better."
  (>= (+ 1 (roll 20) bonus) (- 20 ac)))

(defun %strike-monster (game attacker-name monster dmg)
  "Apply DMG to MONSTER with the hit/slay transcript — shared by melee
and damage spells so the log reads the same either way.  The verbs are
CAPITALS on purpose: landed damage must stand out when the transcript
scrolls by, while misses stay lowercase and quiet.  A hit that leaves
the monster standing names its remaining hit points — the party can
read how close the kill is; the killing blow names its damage too, so
a spell's or an arrow's worth shows even when it ends the fight."
  (let ((type (monster-kind monster)))
    (decf (monster-hp monster) dmg)
    (if (monster-alive-p monster)
        (progn
          (emit game :hit monster dmg)
          (say game "~A HITS the ~A for ~D damage (~D hp left)."
               attacker-name (monster-type-name type) dmg
               (monster-hp monster)))
        (progn
          (emit game :slay monster)
          (say game "~A SLAYS the ~A for ~D points!"
               attacker-name (monster-type-name type) dmg)))))

(defun %hero-strike (game hero monster to-hit-bonus dice damage-bonus)
  "One strike — melee blow and arrow shot share the resolution.
Active effects weigh in: :FOES-AC makes the monster easier to hit,
:DAMAGE-BONUS strengthens the hit.  A class with :CRIT-CHANCE (the
hunter's art) may fell the monster outright on a hit — the chance
grows one point per hero level, and the hunter's eye guides blow and
arrow alike."
  (let ((type (monster-kind monster)))
    (if (%attack-hits-p (+ (hero-level hero) to-hit-bonus)
                        (+ (monster-type-ac type) (effects-foes-ac game)))
        (let ((crit (hero-class-property (hero-class hero) :crit-chance)))
          (if (and crit (< (roll 100) (+ crit (hero-level hero))))
              (progn
                (say game "~A strikes a vital spot!" (hero-name hero))
                (%strike-monster game (hero-name hero) monster
                                 (monster-hp monster)))
              (%strike-monster game (hero-name hero) monster
                               (max 1 (+ (roll-dice dice)
                                         damage-bonus
                                         (effects-damage-bonus game))))))
        (progn
          (emit game :miss hero monster)
          (say game "~A misses the ~A."
               (hero-name hero) (monster-type-name type))))))

(defun %hero-attack (game hero monster)
  "One melee strike: STR guides the arm and weighs into the damage."
  (%hero-strike game hero monster (stat-bonus (hero-str hero))
                (hero-attack-dice hero) (stat-bonus (hero-str hero))))

(defun %hero-shoot (game hero monster)
  "One arrow over the front ranks' heads: DEX aims the shot (the
archer's stat), the equipped arrows alone carry the damage — no STR
behind a bowstring."
  (%hero-strike game hero monster (stat-bonus (hero-dex hero))
                (hero-missile-dice hero) 0))

(defun %insane-strike (game hero)
  "An insane hero's round: the madness spends it on a companion instead
of on the enemy (Bard's Tale's insanity, which turns a sword inward).
The blow lands without a to-hit roll — there is no parrying a friend —
for the hero's own melee dice and STR.  With no companion left standing
the madness rages at the air and costs the party nothing."
  (let ((victims (remove hero (alive-heroes game))))
    (if (null victims)
        (say game "~A rages at the air." (hero-name hero))
        (let* ((victim (nth (roll (length victims)) victims))
               (dmg (max 1 (+ (roll-dice (hero-attack-dice hero))
                              (stat-bonus (hero-str hero))))))
          (say game "~A, raving, STRIKES ~A for ~D damage!"
               (hero-name hero) (hero-name victim) dmg)
          (damage-hero game victim dmg)))))

(defun hero-strike-function (game hero)
  "How HERO reaches the enemy this round, as the function that resolves
one strike — or NIL when nothing is in reach.  Two ways in: a
front-rank hero (HERO-IN-REACH-P) trades melee with a group that has
closed to +MELEE-DISTANCE+, and a hero of any rank shoots or throws
over their heads when the missile carries as far as the nearest group
stands (HERO-MISSILE-DICE / HERO-MISSILE-REACH; a missile the campaign
never measured carries however far it must).  Melee comes first: a
front-rank archer with the enemy on top of them swings rather than
shoots."
  (let* ((combat (game-combat game))
         (distance (and combat (combat-distance combat))))
    (cond ((null distance) nil)
          ((and (hero-in-reach-p game hero)
                (<= distance +melee-distance+))
           #'%hero-attack)
          ((and (hero-missile-dice hero)
                (let ((reach (hero-missile-reach hero)))
                  (or (null reach) (>= reach distance))))
           #'%hero-shoot))))

(defun hero-can-attack-p (game hero)
  "Can HERO take the attack action this round?  True when
HERO-STRIKE-FUNCTION finds a way to the enemy.  The orders page asks
before offering A; COMBAT-ROUND asks again and lets an out-of-reach
attack pass as a wasted action."
  (and (hero-strike-function game hero) t))

(defun %monster-inflict (game monster hero)
  "What a landed blow carries besides its damage: each of the monster
type's :INFLICTS entries rolls its percent chance on HERO, in
*AILMENTS* order so a scripted fight always draws these dice in the
same order.  A hero the blow felled catches nothing — the ailments are
the living's to carry."
  (let ((inflicts (monster-type-inflicts (monster-kind monster))))
    (when (and inflicts (hero-alive-p hero))
      (dolist (a *ailments*)
        (let ((entry (assoc a inflicts)))
          (when (and entry (< (roll 100) (second entry)))
            (afflict-hero game hero a)))))))

(defun %monster-strike (game combat monster targets dice verb)
  "One monster strike — the swing and the shot share the resolution, as
the hero's blow and arrow do.  TARGETS is the heroes it may find, DICE
what it throws and VERB the transcript's word for a landed hit; a miss
stays the quiet lowercase line either way.  Level is the to-hit bonus,
a defence is worth 4 AC, and what a landed hit carries besides its
damage (%MONSTER-INFLICT) does not care how far it travelled: a venom
spat across thirty feet is the same venom."
  (let* ((hero (nth (roll (length targets)) targets))
         (type (monster-kind monster))
         (ac (- (hero-effective-ac hero game)
                (if (member hero (combat-defenders combat)) 4 0))))
    ;; a :FOES-ATTACK effect (a word of fear) blunts the monster's swing
    (if (%attack-hits-p (- (monster-type-level type)
                           (effects-foes-attack game))
                       ac)
        (let ((dmg (max 1 (roll-dice dice))))
          (say game "The ~A ~A ~A for ~D damage."
               (monster-type-name type) verb (hero-name hero) dmg)
          (damage-hero game hero dmg)
          (%monster-inflict game monster hero))
        (say game "The ~A misses ~A."
             (monster-type-name type) (hero-name hero)))))

(defun %monster-attack (game combat monster)
  "The swing of a monster that has closed: the front ranks are the
heroes it can reach, and one of them at random takes it."
  (%monster-strike game combat monster (front-ranks game)
                   (monster-type-damage (monster-kind monster))
                   "HITS"))

(defun %monster-shoot (game combat monster)
  "The answer of a monster still walking in: an arrow, a venom, a word
out of the dark.  Rank is a melee line and a missile flies over it —
the mirror of the back-rank hero shooting over the enemy's heads — so
the whole standing party is its field, and the back ranks are for once
no safer than the front."
  (%monster-strike game combat monster (alive-heroes game)
                   (monster-type-missile (monster-kind monster))
                   (monster-type-missile-verb (monster-kind monster))))

(defun %monsters-act (game combat)
  "The enemy's half of the round: every monster that has closed to
melee swings, and every one still walking in shoots if its type
carries a missile that reaches (MONSTER-CAN-SHOOT-P).  A group with
neither has nothing to spend the round on but the ground it covers
(%CLOSE-IN)."
  (dolist (m (alive-monsters combat))
    (when (party-alive-p game)
      (cond ((monster-in-melee-p m) (%monster-attack game combat m))
            ((monster-can-shoot-p m) (%monster-shoot game combat m))))))

(defun %close-in (game combat)
  "End of the round: every group still short of melee covers its own
MONSTER-STEP and says so, the ones arriving loudest.  A step of NIL
(or less than one foot) leaves that line where it stands — the
shambler eats the archers' rounds while the runner is on top of the
party in one, which is what a type's :SPEED is for."
  (dolist (index (combat-group-indices combat))
    (let* ((members (group-monsters combat index))
           (step (monster-step (first members)))
           (from (monster-distance (first members))))
      (when (and step (plusp step) (> from +melee-distance+))
        (let* ((count (length members))
               (to (max +melee-distance+ (- from step)))
               (label (group-label (monster-kind (first members)) count))
               (many (> count 1)))
          (dolist (m members)
            (setf (monster-distance m) to))
          (if (= to +melee-distance+)
              (say game "The ~A close~:[s~;~] in!" label many)
              (say game "The ~A advance~:[s~;~] to ~D feet."
                   label many to)))))))

(defun %award-victory (game combat)
  "Pay out a won fight, the Bard's Tale way: the fallen monsters' XP
adds up and their :GOLD dice are rolled one head at a time, and the
sum is what **each** standing hero takes — the pot is not divided, so
a hero fights no poorer for having company.  A hero who went down
takes nothing (ALIVE-HEROES, HP above zero), and the party total
therefore grows with the number still on its feet."
  (let ((xp 0)
        (gold 0))
    (dolist (m (combat-monsters combat))
      (incf xp (monster-type-xp (monster-kind m)))
      (incf gold (roll-dice (monster-type-gold-dice (monster-kind m)))))
    (say game "Victory!  Each hero wins ~D experience and ~D gold." xp gold)
    (let ((survivors (alive-heroes game)))
      (when survivors
        (dolist (h survivors)
          (incf (hero-gold h) gold)
          (award-xp game h xp))
        (%award-loot game combat survivors)))))

(defun %award-loot (game combat survivors)
  "The Bard's Tale find: each fallen monster whose type names an :ITEM
rolls its :ITEM-CHANCE, and the FIRST success hands the item over —
one find per fight at most, so a pack of carriers is a better chance
at the same single prize, not a shower of prizes.  The item goes to
the first survivor with pack room (GIVE-ITEM says when a pack is
full, and the next hero steps up); with every pack full the find is
simply left behind."
  (dolist (m (combat-monsters combat))
    (let* ((type (monster-kind m))
           (item (monster-type-item type)))
      (when (and item (< (roll 100) (monster-type-item-chance type)))
        (say game "The ~A carried a ~A!"
             (monster-type-name type) (item-title item))
        (dolist (h survivors)
          (when (give-item game h item)
            (emit game :loot item h)
            (say game "~A takes it." (hero-name h))
            (return)))
        (return)))))

(defun %combat-outcome (game combat)
  (cond ((not (party-alive-p game))
         (setf (game-combat game) nil)
         (say game "The party has been defeated...")
         (emit game :combat-end :defeat)
         :defeat)
        ;; frozen stiff is beaten too: with nobody able to swing, the
        ;; fight would run rounds forever while the monsters helped
        ;; themselves.  Checked after the wipe, so a dead party still
        ;; reads as defeated rather than as helpless.
        ((not (party-can-act-p game))
         (setf (game-combat game) nil)
         (say game "The party stands helpless...")
         (emit game :combat-end :defeat)
         :defeat)
        ((null (alive-monsters combat))
         ;; the fight ends when the last foe falls, and the event says so
         ;; BEFORE the spoils are told: a front-end that shows the won
         ;; fight's treasure (the Amiga's chest picture) wants it up
         ;; first and the victory lines — the xp, the gold, who takes the
         ;; find — paced over it, not the picture arriving after the
         ;; text it illustrates
         (setf (game-combat game) nil)
         (emit game :combat-end :victory)
         (%award-victory game combat)
         :victory)
        (t :ongoing)))

;;; ---------------------------------------------------------------------
;;; Combat transcript speed.  The engine only keeps the setting; the
;;; front-ends linger COMBAT-MESSAGE-DELAY seconds on each :MESSAGE
;;; while a round plays out, so the transcript reads like a fight
;;; instead of arriving as one block.

(defconstant +combat-speed-max+ 5
  "The fastest combat transcript speed: no lingering at all.")

(defparameter *combat-speed* 1
  "Combat transcript speed, 1 (slow) .. +COMBAT-SPEED-MAX+ (instant).
Combat starts slow — the transcript should read like a fight the first
time out; the +/- keys during the round orders speed it up.")

(defun combat-message-delay ()
  "Seconds the front-ends linger on each combat message: 0.25s per
speed step below the maximum (1.0s at speed 1, 0.0 at speed 5)."
  (/ (- +combat-speed-max+
        (max 1 (min +combat-speed-max+ *combat-speed*)))
     4.0))

(defun adjust-combat-speed (game delta)
  "Bump *COMBAT-SPEED* by DELTA, clamped to 1..+COMBAT-SPEED-MAX+, and
say where it landed.  Returns the new speed."
  (setf *combat-speed*
        (max 1 (min +combat-speed-max+ (+ *combat-speed* delta))))
  (say game "Combat speed ~D of ~D~:[~; (instant)~]."
       *combat-speed* +combat-speed-max+
       (= *combat-speed* +combat-speed-max+))
  *combat-speed*)

;;; ---------------------------------------------------------------------
;;; Rounds

(defun combat-round (game &optional actions)
  "Fight one round.  ACTIONS lists an action per ACTING hero in party
order (ACTING-HEROES — the standing, and neither paralysed nor stone;
a helpless hero is not asked and spends nothing)
— :attack (the default), :defend, (:cast SPELL [TARGET]) to
cast a spell (see CAST-SPELL; a failed cast wastes the round),
\(:sing SONG) to strike up a song (see SING-SONG; likewise), or
\(:use ITEM [TARGET]) to use an item (see USE-ITEM — how a Wizhelm
fires its spell in battle; likewise).  Heroes strike the nearest living
monster — front-rank heroes in melee once a group has closed, anyone
whose missile carries that far by bow or thrown weapon
\(HERO-STRIKE-FUNCTION); an :attack with neither is out of reach and
wastes the action (the orders page refuses it up front, but a scripted
round may still ask).  Then the monsters that have closed strike back,
and the ones still walking come a step nearer (%CLOSE-IN).  The round
costs one clock tick.  Returns :victory, :defeat or :ongoing."
  (let ((combat (game-combat game)))
    (unless combat
      (error "combat-round: no combat is in progress"))
    (advance-time game)
    (say game "-- Round ~D --" (incf (combat-round-no combat)))
    ;; the helpless are named once a round, so a player watching a
    ;; statue's turn go by knows why nothing happened on it
    (dolist (h (alive-heroes game))
      (when (hero-helpless-p h)
        (say game "~A is ~A and cannot move." (hero-name h)
             (ailment-title (if (hero-ailment-p h :stone)
                                :stone
                                :paralysis)))))
    (let ((pairs (mapcar (lambda (h) (cons h (or (pop actions) :attack)))
                         (acting-heroes game))))
      ;; madness voids a defence as it voids every other order
      (setf (combat-defenders combat)
            (let ((d '()))
              (dolist (p pairs (nreverse d))
                (when (and (eq (cdr p) :defend)
                           (not (hero-ailment-p (car p) :insanity)))
                  (push (car p) d)))))
      (dolist (p pairs)
        (let ((a (if (hero-ailment-p (car p) :insanity) :insane (cdr p))))
          (cond ((eq a :insane)
                 (%insane-strike game (car p)))
                ((eq a :attack)
                 (let* ((hero (car p))
                        (strike (hero-strike-function game hero)))
                   (if (null strike)
                       (say game "~A cannot reach the enemy."
                            (hero-name hero))
                       ;; a warrior's training (:EXTRA-ATTACK-LEVELS)
                       ;; and a martial effect (:EXTRA-ATTACKS) grant
                       ;; more strikes; each re-aims at the nearest
                       ;; survivor
                       (dotimes (i (+ 1 (hero-extra-attacks hero)
                                      (effects-extra-attacks game)))
                         (let ((target (nearest-monster combat)))
                           (when target
                             (funcall strike game hero target)))))))
                ((and (consp a) (eq (first a) :cast))
                 (cast-spell game (car p) (second a) (third a)))
                ((and (consp a) (eq (first a) :sing))
                 (sing-song game (car p) (second a)))
                ((and (consp a) (eq (first a) :use))
                 (use-item game (car p) (second a) (third a)))))))
    (%monsters-act game combat)
    ;; and the lines that are still walking cover their ground
    (when (party-alive-p game)
      (%close-in game combat))
    ;; the venom takes its round's due before the healer's does, so a
    ;; mending round can outrun the poison it cannot lift
    (poison-bite game)
    ;; a :COMBAT-HEAL effect (Zanduvar Carack) mends the party each round
    (dolist (dice (effects-combat-heal game))
      (dolist (h (alive-heroes game))
        (heal-hero game h (max 0 (roll-dice dice)))))
    (%combat-outcome game combat)))

;;; ---------------------------------------------------------------------
;;; The round-orders interaction model (shared by both front-ends).
;;;
;;; Bard's Tale style: every living hero picks an action for the round
;;; — attack, defend, cast, play, use — and only then does the round
;;; run.  The page asks ONE hero at a time, as the original does: the
;;; hero at hand has the page to itself, and when the last of them has
;;; picked, the page turns into the review — every hero with the order
;;; it gave — and asks whether that is what the party wants.  Y fights
;;; the round, N throws the orders away and starts asking again from
;;; the first hero.
;;;
;;; Before any hero is asked, every round opens on the one party-level
;;; choice — Fight or Run.  Running is all-or-nothing (the whole party
;;; runs or nobody does) and only stands there, at the top of the
;;; round: F commits the party for THIS round's orders (the view's
;;; ENGAGED flag; each round's fresh view asks anew), and a failed
;;; flight costs the free round the monsters just took — the next
;;; round opens on the same choice.
;;;
;;; The model collects (HERO . ACTION) pairs in party order; C,
;;; P and U open the cast/sing/use pickers for the hero at hand in
;;; their :ORDERS mode, which hands the pick back as a round action
;;; instead of fighting a round itself (see %CAST-COMMIT /
;;; %SING-COMMIT / %USE-COMMIT).  Esc undoes the previous
;;; hero's pick — and on the review page it is N — and +/- set the
;;; transcript speed.  COMBAT-ORDERS-ACT returns (:FIGHT ACTIONS) only
;;; when the review is accepted; the front-end fights the round then.
;;;
;;; One hero per page is also what keeps the page inside the narrow
;;; message column: it costs a handful of rows whatever the party's
;;; size, where a page carrying every hero at once grew with the
;;; roster and lost its footer at the bottom of a lores column.

(defstruct (combat-orders (:constructor %make-combat-orders))
  engaged             ; T once the party chose to stand and fight this
                      ; round — until then the page offers the one
                      ; party-level choice, Fight or Run
  chosen              ; (HERO . ACTION) pairs picked so far, party order
  sub                 ; CAST-VIEW/SING-VIEW picking for the hero at hand
  review)             ; T once every hero has picked: the review page,
                      ; awaiting Y (fight) or N (start over)

(defun make-combat-orders ()
  (%make-combat-orders))

(defun combat-orders-hero (game view)
  "The hero the orders view is asking about, or NIL once every hero who
can act has an action.  The helpless — paralysed, stone — are never
asked: they are not in ACTING-HEROES, and COMBAT-ROUND resolves against
the same list, so the orders line up with the heroes who give them."
  (nth (length (combat-orders-chosen view)) (acting-heroes game)))

(defun %orders-action-label (action)
  "A short display label for a round action."
  (cond ((eq action :attack) "attack")
        ((eq action :defend) "defend")
        ((and (consp action) (eq (first action) :cast))
         (format nil "cast ~A~@[ on ~A~]"
                 (spell-title (second action))
                 (let ((target (third action)))
                   (and target (hero-name target)))))
        ((and (consp action) (eq (first action) :sing))
         (format nil "play ~A" (song-title (second action))))
        ((and (consp action) (eq (first action) :use))
         (format nil "use ~A~@[ on ~A~]"
                 (item-title (second action))
                 (let ((target (third action)))
                   (and target (hero-name target)))))
        (t (string-downcase (princ-to-string action)))))

(defun %orders-head-lines (game)
  "The block both orders pages open with: the coming round and the
enemy the party faces, one row per living group."
  (let ((combat (game-combat game)))
    (append
     (list (format nil "*** Combat -- Round ~D ***"
                   (1+ (combat-round-no combat)))
           "")
     (mapcar (lambda (group)
               ;; the distance rides in parentheses and only while the
               ;; group is still walking in — the row stays inside the
               ;; narrow message column either way
               (format nil "  ~A~@[ (~D')~]"
                       (group-label (first group) (second group))
                       (when (> (third group) +melee-distance+)
                         (third group))))
             (combat-groups combat)))))

(defun %orders-hero-lines (game view)
  "The page that asks ONE hero for its order: the head block, the hero
at hand, and the actions — one per row, first letter as the key (no
bracket noise), and only the actions this hero can actually take: an
:ATTACK out of reach, a Cast for the spell-less, a Play for the
song-less or a Use from an empty pack would only draw the refusal the
key handler keeps for scripted input.  Defend always stands; running
is not a hero's to give — it is the party's choice at the top of the
round, and this round's went when the party engaged.
The navigation keys (Esc undo, +/- speed) are common knowledge and
live on the help screen instead.  The page draws in the message
column (the Amiga takeover), 27 characters wide at lores; every row
here fits that column whole, and the full six-action list still fits
the lores page height (the suite checks both)."
  (let ((hero (combat-orders-hero game view)))
    (append
     (%orders-head-lines game)
     (list ""
           (format nil "What will ~A do?" (hero-name hero))
           "")
     (when (hero-can-attack-p game hero)
       (list "Attack"))
     (list "Defend")
     (when (and (hero-caster-p hero) (spells-for-hero hero))
       (list "Cast"))
     (when (and (hero-singer-p hero) (songs-for-hero hero))
       (list "Play"))
     (when (usable-items hero)
       (list "Use")))))

(defun %orders-engage-lines (game)
  "The page every round opens on: the enemy block and the one choice
that is the party's to make as a whole — stand and fight, or run.
Either the whole party flees or no one does; once the party engages,
the choice waits for the top of the next round (each round's fresh
orders view asks anew — see COMBAT-ORDERS-ENGAGED)."
  (append
   (%orders-head-lines game)
   (list ""
         "Fight"
         "Run")))

(defun %orders-review-lines (game view)
  "The review page: every hero with the order it gave, and the
question the round waits on.  It does not repeat the enemy block —
every hero's page just showed it, and the rows the list needs are the
rows a full party's orders take.  +/- still answers here (see
%ORDERS-REVIEW-ACT); the page names the two keys the question is
about."
  (append
   (list (format nil "*** Round ~D orders ***"
                 (1+ (combat-round-no (game-combat game))))
         "")
   (mapcar (lambda (pair)
             (format nil "  ~12A ~A" (hero-name (car pair))
                     (%orders-action-label (cdr pair))))
           (combat-orders-chosen view))
   (list ""
         "Is this OK?"
         "Yes fight  No redo")))

(defun combat-orders-lines (game view)
  "The round-orders page as menu lines (the SHOP-LINES pattern): the
party-level engage page while the fight waits on its opening choice
(Fight or Run), then the page asking the hero at hand for its
order, or — once every hero has one — the review page listing them
all under \"Is this OK?\".  While a cast/sing/use pick is open, its
page shows instead."
  (let ((sub (combat-orders-sub view)))
    (cond
      ((cast-view-p sub) (cast-lines game sub))
      ((sing-view-p sub) (sing-lines game sub))
      ((use-view-p sub) (use-lines game sub))
      ((not (combat-orders-engaged view))
       (%orders-engage-lines game))
      ((combat-orders-review view) (%orders-review-lines game view))
      ((combat-orders-hero game view) (%orders-hero-lines game view))
      ;; nobody left to ask and nothing to review (the party fell while
      ;; the orders were open): the head block alone
      (t (%orders-head-lines game)))))

(defun %orders-record (game view action)
  "Record ACTION for the hero at hand.  When that completes the list,
raise the review page (the round waits for Y).  Always returns NIL —
only the review hands the front-end its (:FIGHT ACTIONS)."
  (setf (combat-orders-chosen view)
        (append (combat-orders-chosen view)
                (list (cons (combat-orders-hero game view) action))))
  (unless (combat-orders-hero game view)
    (setf (combat-orders-review view) t))
  nil)

(defun %orders-sub-act (game view sub char)
  "Forward CHAR to the open cast/sing/use picker.  A completed pick
lands as the hero-at-hand's action; Esc backs out to the action keys."
  (let ((result (cond ((cast-view-p sub) (cast-act game sub char))
                      ((sing-view-p sub) (sing-act game sub char))
                      (t (use-act game sub char)))))
    (cond ((and (consp result) (eq (first result) :action))
           (setf (combat-orders-sub view) nil)
           (%orders-record game view (second result)))
          ((or (eq result :cancelled)
               ;; Esc on the picker's first page clears its preset
               ;; hero — that is the whole picker backing out
               (null (cond ((cast-view-p sub) (cast-view-hero sub))
                           ((sing-view-p sub) (sing-view-hero sub))
                           (t (use-view-hero sub)))))
           (setf (combat-orders-sub view) nil)
           nil)
          (t nil))))

(defun %orders-review-act (game view char)
  "Apply key CHAR to the review page: Y fights the round with the
orders as they stand, N (or Esc) throws them away and asks the party
again from the first hero, +/- still set the pace."
  (case (char-downcase char)
    (#\y (list :fight (mapcar #'cdr (combat-orders-chosen view))))
    ((#\n #\Escape)
     (setf (combat-orders-chosen view) '()
           (combat-orders-review view) nil)
     nil)
    (#\+ (adjust-combat-speed game 1) nil)
    (#\- (adjust-combat-speed game -1) nil)
    (t nil)))

(defun %orders-engage-act (game view char)
  "Apply key CHAR to the engage page: F commits the party to this
round's orders (the choice returns at the top of the next round), R
runs — the whole party or nobody — and +/- set the pace as
everywhere else."
  (case (char-downcase char)
    (#\f (setf (combat-orders-engaged view) t) nil)
    (#\r :flee)
    (#\+ (adjust-combat-speed game 1) nil)
    (#\- (adjust-combat-speed game -1) nil)
    (t nil)))

(defun combat-orders-act (game view char)
  "Apply key CHAR to the round-orders page.  Returns (:FIGHT ACTIONS)
when the review page is accepted — the front-end fights the round with
them (COMBAT-ROUND) — :FLEE when the party runs from the round's
engage page (ATTEMPT-FLEE; the choice only stands there), else NIL."
  (let ((sub (combat-orders-sub view))
        (hero (combat-orders-hero game view)))
    (cond
      (sub (%orders-sub-act game view sub char))
      ((not (combat-orders-engaged view))
       (%orders-engage-act game view char))
      ((combat-orders-review view) (%orders-review-act game view char))
      ((null hero) nil)                 ; nobody left to ask
      (t
       (case (char-downcase char)
         (#\a (if (hero-can-attack-p game hero)
                  (%orders-record game view :attack)
                  (say game "~A cannot reach the enemy."
                       (hero-name hero)))
              nil)
         (#\d (%orders-record game view :defend))
         (#\c (if (and (hero-caster-p hero) (spells-for-hero hero))
                  (setf (combat-orders-sub view)
                        (make-cast-view :in-combat :orders :hero hero))
                  (say game "~A cannot cast." (hero-name hero)))
              nil)
         (#\p (if (and (hero-singer-p hero) (songs-for-hero hero))
                  (setf (combat-orders-sub view)
                        (make-sing-view :in-combat :orders :hero hero))
                  (say game "~A cannot play." (hero-name hero)))
              nil)
         (#\u (if (usable-items hero)
                  (setf (combat-orders-sub view)
                        (make-use-view :in-combat :orders :hero hero))
                  (say game "~A has nothing to use." (hero-name hero)))
              nil)
         (#\+ (adjust-combat-speed game 1) nil)
         (#\- (adjust-combat-speed game -1) nil)
         (#\Escape
          (let ((chosen (combat-orders-chosen view)))
            (when chosen
              (setf (combat-orders-chosen view) (butlast chosen))))
          nil)
         (t nil))))))

(defun attempt-flee (game)
  "Try to run from combat — the whole party or nobody.  Success (even
odds) ends the fight; failure gives the monsters a free round, and
the next round's orders open on the same Fight-or-Run choice.
Returns :fled, :defeat or :ongoing.  The orders pages only offer the
run on the engage page at the top of a round, but a scripted caller
may try it any time."
  (let ((combat (game-combat game)))
    (unless combat
      (error "attempt-flee: no combat is in progress"))
    (setf (combat-defenders combat) '())
    (if (< (roll 100) 50)
        (progn
          (setf (game-combat game) nil)
          (say game "The party flees!")
          (emit game :combat-end :fled)
          :fled)
        (progn
          (say game "No escape!")
          (%monsters-act game combat)
          (%combat-outcome game combat)))))
