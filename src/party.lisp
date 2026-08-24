;;; Lambda's Tale — heroes and the party.
;;;
;;; Hero classes are campaign data, not engine facts: the campaign
;;; registers them with DEFINE-HERO-CLASS in its campaign.lisp and
;;; MAKE-HERO rolls a level-1 character of that class.  Armor class is
;;; Bard's Tale style descending: lower is better, unarmored is 10.

(in-package :tale)

(defstruct (hero (:constructor %make-hero))
  name
  class               ; keyword registered via DEFINE-HERO-CLASS
  race                ; keyword registered via DEFINE-RACE, or NIL
  portrait            ; portrait file stamped at creation (the class's
                      ; :image or :image-woman), or :NONE when that
                      ; class carries neither — the face is the
                      ; person's, not the job's, so CHANGE-CLASS never
                      ; touches it and HERO-IMAGE never substitutes the
                      ; new class's portrait for a stamped :NONE; a
                      ; bare NIL instead means a hero from before
                      ; portraits stuck (a save older than v8), where
                      ; HERO-IMAGE falls back to the class :image
  (level 1)
  (xp 0)
  (max-hp 1)
  (hp 1)
  (max-sp 0)          ; spell points; 0 = not a caster
  (sp 0)
  (str 10) (dex 10) (iq 10) (con 10) (lck 10)
  (ac 10)             ; descending: lower is better
  (damage "1d4")      ; the hero's bare attack dice (no weapon)
  (gold 0)
  (items '())         ; pack contents: item names.  The gear is capped at
                      ; +inventory-limit+; the :QUEST pieces ride outside
                      ; it, so the list itself may be longer (PACK-BURDEN)
  (equipped '())      ; equipped subset: one :weapon, :armor, :shield each
  (tunes 0)           ; song charges (singers; refilled at a tavern)
  (ailments '())      ; conditions carried until cured (see *AILMENTS*)
  (class-levels '()))  ; (class . level) for every class LEFT behind —
                      ; the rating CHANGE-CLASS froze on the way out, and
                      ; what a departed class's spells still answer to
                      ; (HERO-CLASS-LEVEL).  The current class is never
                      ; in here: its rating is HERO-LEVEL itself.

(defvar *hero-classes* (make-hash-table :test 'eq))

(defun define-hero-class (name &key (hp-dice "1d8") (damage "1d4") (ac 10)
                                    caster singer sings-with
                                    image image-woman description
                                    extra-attack-levels crit-chance
                                    ac-per-level trap-skill
                                    (startable t) change-at change-group
                                    requires)
  "Register hero class NAME (a keyword) with its hit dice, attack dice
and starting armor class; CASTER T marks a spell-casting class (spell
points from level and IQ, see %HERO-MAX-SP), SINGER T a song-playing
class (one tune charge per level, see songs.lisp).  SINGS-WITH names
an item kind (*ITEM-KINDS*) the singer must have EQUIPPED to play at
all — the Bard's Tale rule that the music needs an instrument in hand;
NIL (the default) lets the class sing bare-handed.  IMAGE names the
class's portrait file (map-relative, like effect icons) — the Amiga
front-end shows it beside the character sheet; NIL = no portrait.
IMAGE-WOMAN names a second portrait for a class open to both men and
women: MAKE-HERO stamps one of the two onto the hero at creation
(:WOMAN picks this one), and the face stays the hero's for life.
DESCRIPTION is the class's lore line (display data).
EXTRA-ATTACK-LEVELS N grants one extra strike per N levels beyond the
first (the warrior's art, see HERO-EXTRA-ATTACKS); CRIT-CHANCE N is
the percent chance a landed blow fells the foe outright, growing one
point per level (the hunter's art, see combat.lisp); AC-PER-LEVEL N
improves the class's natural armor by one every N levels beyond the
first (the monk's art, see LEVEL-UP — floored at -10, Bard's Tale's
best); TRAP-SKILL N is the percent chance to spot and disarm a
springing floor trap, growing one point per level (the rogue's art,
see the TRAP op in specials.lisp).

STARTABLE NIL bars the class from character creation — MAKE-HERO
refuses it, and the only way in is CHANGE-CLASS (Bard's Tale kept its
higher arts off the creation menu this way).

Three keys govern that second ladder, and a class needs the first two
to walk it at all.  CHANGE-AT N is the level a hero of this class must
reach before they may LEAVE it.  CHANGE-GROUP is the family it changes
within: a hero may only move between classes that name the SAME group,
which is how \"a mage moves up to another mage class\" is said — a
class with no group (or no CHANGE-AT) is for life.  REQUIRES is an
alist of (CLASS . LEVEL) a hero must have ATTAINED — now or in a class
they have since left — before they may ENTER this one; the top art
asks for several at once.  Campaign data calls this."
  (when (and extra-attack-levels
             (not (and (integerp extra-attack-levels)
                       (plusp extra-attack-levels))))
    (error "define-hero-class ~S: :extra-attack-levels ~S must be a ~
            positive integer" name extra-attack-levels))
  (when (and crit-chance
             (not (and (integerp crit-chance) (< 0 crit-chance 101))))
    (error "define-hero-class ~S: :crit-chance ~S must be a percent ~
            (1-100)" name crit-chance))
  (when (and ac-per-level
             (not (and (integerp ac-per-level) (plusp ac-per-level))))
    (error "define-hero-class ~S: :ac-per-level ~S must be a ~
            positive integer" name ac-per-level))
  (when (and trap-skill
             (not (and (integerp trap-skill) (< 0 trap-skill 101))))
    (error "define-hero-class ~S: :trap-skill ~S must be a percent ~
            (1-100)" name trap-skill))
  (when (and change-at
             (not (and (integerp change-at) (plusp change-at))))
    (error "define-hero-class ~S: :change-at ~S must be a positive ~
            integer" name change-at))
  (when (and change-group (not (symbolp change-group)))
    (error "define-hero-class ~S: :change-group ~S must be a symbol"
           name change-group))
  ;; the shape only — a required class need not be registered yet, so
  ;; campaign data may name the arts in any order
  (dolist (entry requires)
    (unless (and (consp entry) (keywordp (car entry))
                 (integerp (cdr entry)) (plusp (cdr entry)))
      (error "define-hero-class ~S: :requires entry ~S must be ~
              (CLASS . LEVEL) with a positive LEVEL" name entry)))
  (when sings-with
    (unless (member sings-with *item-kinds*)
      (error "define-hero-class ~S: :sings-with ~S names no item kind ~
              (see *ITEM-KINDS*)" name sings-with))
    (unless singer
      (error "define-hero-class ~S: :sings-with ~S on a class that is ~
              no singer" name sings-with)))
  (setf (gethash name *hero-classes*)
        (list :hp-dice hp-dice :damage damage :ac ac
              :caster caster :singer singer :sings-with sings-with
              :image image :image-woman image-woman
              :description description
              :extra-attack-levels extra-attack-levels
              :crit-chance crit-chance
              :ac-per-level ac-per-level
              :trap-skill trap-skill
              :startable startable
              :change-at change-at
              :change-group change-group
              :requires requires))
  name)

(defun startable-hero-classes ()
  "The classes a new character may be rolled as — HERO-CLASSES minus
the ones registered :STARTABLE NIL.  A creation menu picks from here."
  (remove-if-not (lambda (class) (hero-class-property class :startable))
                 (hero-classes)))

(defun hero-extra-attacks (hero)
  "Extra strikes HERO's class training grants per combat round: one
per :EXTRA-ATTACK-LEVELS levels beyond the first (a level-5 warrior
with 4 strikes twice), zero for everyone else."
  (let ((per (hero-class-property (hero-class hero) :extra-attack-levels)))
    (if per
        (floor (1- (hero-level hero)) per)
        0)))

(defun hero-trap-skill (hero)
  "HERO's chance (percent) to spot and disarm a springing floor trap:
the class's :TRAP-SKILL grown one point per level, capped at 99 —
even the surest hand slips.  0 for the untrained."
  (let ((base (hero-class-property (hero-class hero) :trap-skill)))
    (if base
        (min 99 (+ base (hero-level hero)))
        0)))

(defun hero-classes ()
  "The registered hero classes as a sorted list of keywords — whatever
the loaded campaign declared with DEFINE-HERO-CLASS (the engine ships
none of its own)."
  (let ((names '()))
    (maphash (lambda (name plist) (declare (ignore plist))
               (push name names))
             *hero-classes*)
    (sort names #'string< :key #'symbol-name)))

(defun hero-class-property (class key)
  (let ((plist (gethash class *hero-classes*)))
    (unless plist
      (error "Unknown hero class ~S (register it with DEFINE-HERO-CLASS)"
             class))
    (getf plist key)))

(defun hero-class-level (hero class)
  "The level HERO has ATTAINED in CLASS — HERO-LEVEL for the class they
wear now, the rating CHANGE-CLASS froze for one they have left, and 0
for one they never held.  This, not HERO-LEVEL, is what a spell's own
class answers to (SPELL-KNOWN-P): an art keeps handing out the spells
it opened long after its owner has moved on, and never opens another."
  (if (eq class (hero-class hero))
      (hero-level hero)
      (or (cdr (assoc class (hero-class-levels hero))) 0)))

(defun hero-held-class-p (hero class)
  "Has HERO ever worn CLASS — now, or before a CHANGE-CLASS?"
  (and (or (eq class (hero-class hero))
           (assoc class (hero-class-levels hero)))
       t))

(defun make-hero (name class &key race woman (gold 0))
  "Create a level-1 hero of CLASS: hp from the class hit dice plus the
CON bonus (minimum 1 — a sickly hero still lives), abilities rolled 3d6
in the order str, dex, iq, con, lck, then adjusted by RACE's ability
modifiers when a race is given (see DEFINE-RACE).  A RACE that does not
permit CLASS is an error, and so is a CLASS registered :STARTABLE NIL —
those are reached by CHANGE-CLASS alone.  WOMAN T picks the class's
woman's portrait (:IMAGE-WOMAN) over the default :IMAGE — an error on
a class that carries no second portrait; either way the chosen file,
or the lack of one, is stamped onto the hero for life (see the
PORTRAIT slot).  GOLD is the starting purse (campaign data decides;
dice strings welcome)."
  ;; A class that is closed to new characters is caught with the race
  ;; check below: both are design errors, both before any dice roll, so
  ;; a scripted roll order never half-runs.
  (unless (hero-class-property class :startable)
    (error "~A is not open to a new character - it is reached by ~
            changing class; the open ones are: ~{~A~^, ~}"
           (string-capitalize (substitute #\Space #\- (string class)))
           (mapcar (lambda (c)
                     (string-capitalize (substitute #\Space #\- (string c))))
                   (startable-hero-classes))))
  ;; A race that cannot take this class is a design error caught early,
  ;; before any dice roll, with a message that lists the legal classes.
  (when (and race (not (race-allows-class-p race class)))
    (error "A ~A cannot be a ~A - the race may be: ~{~A~^, ~}"
           (race-title race)
           (string-capitalize (substitute #\Space #\- (string class)))
           (mapcar #'race-title (race-classes (find-race race)))))
  ;; Asking for a woman's portrait where the class carries none is a
  ;; design error too, caught with the others before any dice roll.
  (when (and woman (not (hero-class-property class :image-woman)))
    (error "The ~A carries no woman's portrait - :image-woman in ~
            define-hero-class adds one"
           (string-capitalize (substitute #\Space #\- (string class)))))
  ;; Keep the roll order (hp, str, dex, iq, con, lck, gold) — the test
  ;; suite scripts heroes through *RNG* and depends on it.  Racial
  ;; modifiers adjust the rolled scores in place (they draw no dice), so
  ;; the roll order stays intact and spell points read the modified IQ.
  (let* ((hp (max 1 (roll-dice (hero-class-property class :hp-dice))))
         (str (roll-dice "3d6")) (dex (roll-dice "3d6"))
         (iq (roll-dice "3d6")) (con (roll-dice "3d6"))
         (lck (roll-dice "3d6"))
         (r (and race (find-race race))))
    (when r
      (setf str (clamp-stat (+ str (race-str r)))
            dex (clamp-stat (+ dex (race-dex r)))
            iq  (clamp-stat (+ iq  (race-iq r)))
            con (clamp-stat (+ con (race-con r)))
            lck (clamp-stat (+ lck (race-lck r)))))
    ;; CON pays into hp from level 1 on — applied after the racial
    ;; adjustment (a dwarf's hardiness counts) and after the rolls, so
    ;; the scripted roll order above stays intact.
    (incf hp (stat-gift con))
    (let ((sp (%hero-max-sp class 1 iq)))
      (%make-hero :name name :class class :race race
                  :portrait (or (hero-class-property
                                 class (if woman :image-woman :image))
                                :none)
                  :max-hp hp :hp hp
                  :max-sp sp :sp sp
                  :str str :dex dex :iq iq :con con :lck lck
                  :ac (hero-class-property class :ac)
                  :damage (hero-class-property class :damage)
                  :gold (roll-dice gold)
                  :tunes (if (hero-class-property class :singer) 1 0)))))

(defun stat-bonus (stat)
  "Bonus for an ability score: +1 per 2 points above 10, negative below."
  (floor (- stat 10) 2))

(defun stat-gift (stat)
  "Bard's Tale's kindness: STAT-BONUS when the score earns one, never
a penalty — 0 below 11.  Hit points (CON) and armor class (DEX) use
this, so a poor score costs nothing; the attack maths (STR to-hit and
damage, DEX missiles) keep the full signed STAT-BONUS."
  (max 0 (stat-bonus stat)))

(defun saving-throw (game hero difficulty &key (bonus 0))
  "HERO rolls to shrug off a harm: d20 + level + the LCK bonus + the
party's active :SAVE-BONUS effects + BONUS, against DIFFICULTY.  True
on DIFFICULTY or better.  Luck carries the save (the full signed
STAT-BONUS — the one score with no other job), veterans shrug what
fells the fresh, and Anti-Magic's :save-bonus finally weighs in.
BONUS is the hook for a later spell resistance or an item's gift."
  (>= (+ 1 (roll 20) (hero-level hero) (stat-bonus (hero-lck hero))
         (effects-save-bonus game) bonus)
      difficulty))

(defun %hero-max-sp (class level iq)
  "Spell points for a CLASS/LEVEL/IQ hero: 2 per level plus the IQ
bonus for casters (minimum 1); everyone else has none."
  (if (hero-class-property class :caster)
      (max 1 (+ (* 2 level) (stat-bonus iq)))
      0))

(defun hero-caster-p (hero)
  "True when HERO can cast spells (a caster class with spell points)."
  (> (hero-max-sp hero) 0))

(defun hero-singer-p (hero)
  "True when HERO plays songs (a :SINGER class, see songs.lisp)."
  (and (hero-class-property (hero-class hero) :singer) t))

(defun hero-max-tunes (hero)
  "Song charges a rested singer holds: one per level (Bard's Tale
songs-per-day), none for everyone else."
  (if (hero-singer-p hero) (hero-level hero) 0))

(defun hero-alive-p (hero)
  (> (hero-hp hero) 0))

;;; ---------------------------------------------------------------------
;;; Ailments — the Bard's Tale conditions a hero carries about until
;;; something lifts them.  The engine owns the four states and what each
;;; one does in play; a campaign says who hands them out (DEFINE-MONSTER
;;; :INFLICTS) and what lifting one costs (a :TEMPLE's :CURES table).
;;;
;;;   :poison     bites for *POISON-BITE* hp every step and every round
;;;   :insanity   the hero fights on its own, striking a companion
;;;   :paralysis  the hero cannot act at all
;;;   :stone      a statue: cannot act, and stands there taking blows
;;;
;;; None of them wears off with time — an ailment is cured or it is
;;; carried (a temple's priests, a spell's :CURE), which is what makes
;;; it worth pricing.  They stack: a poisoned hero can be paralysed too,
;;; and each is cured on its own.

(defparameter *ailments* '(:poison :insanity :paralysis :stone)
  "The ailment vocabulary, in the order they are displayed, cured and
rolled.  A hero's AILMENTS list is a subset of this, held in this
order; anything outside it is a campaign error (AFFLICT-HERO).")

(defparameter *poison-bite* 1
  "Hit points poison costs its carrier on every party step and every
combat round.  Poison can kill: the bite goes through DAMAGE-HERO like
any other harm.")

(defparameter *ailment-titles*
  '((:poison . "poisoned") (:insanity . "insane")
    (:paralysis . "paralysed") (:stone . "stone"))
  "Display adjective per ailment — how a hero's condition reads.")

(defparameter *ailment-codes*
  '((:poison . "POIS") (:insanity . "INSA")
    (:paralysis . "PARA") (:stone . "STON"))
  "The roster table's four-character condition code per ailment, cut to
the width of the DEAD it stands beside (HERO-CONDITION-CODE).")

(defun ailment-p (x)
  "Is X one of the engine's ailments?  The check campaign data is held
to — a spell's :CURE list, a monster's :INFLICTS table, a temple's
:CURES table all name ailments and are refused when they name anything
else."
  (and (member x *ailments*) t))

(defun ailment-title (ailment)
  "AILMENT as a display adjective — what a hero IS: :poison ->
\"poisoned\".  The roster's parenthetical and the affliction line."
  (or (cdr (assoc ailment *ailment-titles*))
      (string-downcase (symbol-name ailment))))

(defun ailment-noun (ailment)
  "AILMENT as a display noun — what is LIFTED: :poison -> \"poison\".
The keywords are the nouns; a cleansing is cleansed of poison, not of
poisoned."
  (string-downcase (symbol-name ailment)))

(defun hero-ailment-p (hero ailment)
  "Is HERO carrying AILMENT?"
  (and (member ailment (hero-ailments hero)) t))

(defun hero-condition-titles (hero)
  "HERO's condition as a display string — \"down\" for the fallen, then
every ailment in *AILMENTS* order (\"down, poisoned\") — or NIL when
there is nothing the matter."
  (let ((parts (append (unless (hero-alive-p hero) (list "down"))
                       (mapcar #'ailment-title
                               (remove-if-not (lambda (a)
                                                (hero-ailment-p hero a))
                                              *ailments*)))))
    (when parts
      (format nil "~{~A~^, ~}" parts))))

(defun hero-condition-code (hero)
  "The roster table's code for the worst thing about HERO — \"DEAD\",
then stone, paralysis, insanity, poison in that order — or NIL for a
hero with nothing the matter.  The engine picks the word so both
front-ends (and the suite) show the same one."
  (cond ((not (hero-alive-p hero)) "DEAD")
        (t (dolist (a '(:stone :paralysis :insanity :poison))
             (when (hero-ailment-p hero a)
               (return (cdr (assoc a *ailment-codes*))))))))

(defun hero-helpless-p (hero)
  "Is HERO unable to take any action at all — paralysed or turned to
stone?  A helpless hero is not asked for a round order and takes none,
but still stands in the rank and still takes blows."
  (or (hero-ailment-p hero :paralysis)
      (hero-ailment-p hero :stone)))

(defun hero-can-act-p (hero)
  "Can HERO take an action this round — standing, and neither paralysed
nor stone?  An insane hero can: madness acts, just not as ordered."
  (and (hero-alive-p hero) (not (hero-helpless-p hero))))

(defun afflict-hero (game hero ailment)
  "Lay AILMENT on HERO.  Says so and emits :AFFLICT the first time;
returns T then, NIL when HERO already carried it (an ailment does not
deepen).  AILMENT must be one of *AILMENTS*."
  (unless (member ailment *ailments*)
    (error "Unknown ailment ~S (one of ~S)" ailment *ailments*))
  (unless (hero-ailment-p hero ailment)
    ;; kept in *AILMENTS* order, so a hero's conditions always read and
    ;; cure in the same order however they were picked up
    (let ((carried (cons ailment (hero-ailments hero))))
      (setf (hero-ailments hero)
            (remove-if-not (lambda (a) (member a carried)) *ailments*)))
    (say game "~A is ~A!" (hero-name hero) (ailment-title ailment))
    (emit game :afflict hero ailment)
    t))

(defun cure-ailment (game hero ailment)
  "Lift AILMENT from HERO, quietly — the caller does the talking (a
temple's receipt, a spell's cleansing).  Emits :CURE and returns T when
HERO was carrying it, NIL when there was nothing to lift."
  (when (hero-ailment-p hero ailment)
    (setf (hero-ailments hero) (remove ailment (hero-ailments hero)))
    (emit game :cure hero ailment)
    t))

(defun cure-hero (game hero &optional (ailments *ailments*))
  "Lift every one of AILMENTS that HERO carries, naming them in one
line (\"Ava is cleansed of poison.\").  Returns the list actually
lifted, NIL when HERO carried none of them."
  (let ((lifted '()))
    (dolist (a *ailments*)
      (when (and (member a ailments) (cure-ailment game hero a))
        (push a lifted)))
    (setf lifted (nreverse lifted))
    (when lifted
      (say game "~A is cleansed of ~{~A~^ and ~}." (hero-name hero)
           (mapcar #'ailment-noun lifted)))
    lifted))

(defconstant +party-limit+ 7
  "Maximum roster size: six regular heroes plus one guest slot (a
summoned/charmed monster or story NPC, Bard's Tale tradition).")

(defun hero-class-title (hero)
  "The hero's class as a display string: :war-mage -> \"War Mage\"."
  (string-capitalize (substitute #\Space #\- (string (hero-class hero)))))

(defun hero-race-title (hero)
  "The hero's race as a display string (\"Dwarf\", \"Half-Elf\"), or NIL
when the hero has no race."
  (and (hero-race hero) (race-title (hero-race hero))))

(defun class-abbrev (class)
  "CLASS as the roster's CL column code, always two characters so the
name column keeps the room: the initials of the first two words of a
multi-word class, else the first two letters — :war-mage -> \"WM\",
:conjurer -> \"CO\"."
  (let* ((name (string class))
         (words (loop with start = 0
                      for dash = (position #\- name :start start)
                      collect (subseq name start dash)
                      while dash
                      do (setf start (1+ dash)))))
    (string-upcase
     (if (rest words)
         (map 'string (lambda (w) (char w 0)) (subseq words 0 2))
         (subseq name 0 (min 2 (length name)))))))

(defun hero-class-abbrev (hero)
  "HERO's current class as the roster's CL column code — CLASS-ABBREV
of HERO-CLASS."
  (class-abbrev (hero-class hero)))

(defun hero-art-ratings (hero)
  "The level HERO has attained in every class they have ever worn, as
(CLASS . LEVEL) pairs, oldest first and the class they wear now last.
One pair for a hero who never changed class — the sheet leaves the
line out for them, since the name and Level rows already say it."
  (append (reverse (hero-class-levels hero))
          (list (cons (hero-class hero) (hero-level hero)))))

(defun %art-rating-lines (hero)
  "The Arts rows of the stat block: every art HERO has worn with the
level it stands at, three to a row so the line keeps inside the
sheet's 20 cells even at two-digit levels (\"Arts CO13 MA13 SO13\" is
19).  NIL for a hero who has worn only one class."
  (let ((ratings (hero-art-ratings hero)))
    (when (rest ratings)
      (loop for tail = ratings then (nthcdr 3 tail)
            while tail
            collect (format nil "Arts~{ ~A~}"
                            (mapcar (lambda (rating)
                                      (format nil "~A~D"
                                              (class-abbrev (car rating))
                                              (cdr rating)))
                                    (subseq tail 0 (min 3 (length tail)))))))))

(defun hero-summary-lines (hero &optional game)
  "The character sheet's first page as a list of text lines — the stat
block a player sees when they open a roster slot.  The pack is not in
it (that lists on the carousel's pack page — EQUIP-LINES), and
neither is the spellbook (the spells/songs page — HERO-MAGIC-LINES).
The armor class is the one the roster prints — HERO-EFFECTIVE-AC,
equipment and the DEX gift included, and the party's :AC effects when
GAME is given — never the bare base slot: the sheet draws right
beside the roster, and a base 'AC 8' next to the roster's equipped
'AC 2' reads as a stale, unequipped hero (most jarringly right after
loading a save).  Every line stays within 20 character cells at
worst-case values (three-digit hit/spell points, a negative AC), well
inside the lores message-area takeover's +TAKEOVER-COLUMNS+
small-face cells, so the block never wraps mid-figure; a raced hero's
race-and-class line steps under the name for the same reason.  Pure
(no I/O), so both the Amiga sheet view and the tests render from the
same source."
  (append
   ;; the name, with "Race Class" spelled out under it — or "Name the
   ;; Class" on one line when the hero is raceless
   (if (hero-race-title hero)
       (list (hero-name hero)
             (format nil "~A ~A" (hero-race-title hero)
                     (hero-class-title hero)))
       (list (format nil "~A the ~A" (hero-name hero)
                     (hero-class-title hero))))
   (list (format nil "Level ~D  XP ~D" (hero-level hero) (hero-xp hero))
         (format nil "HP ~D/~D  AC ~D"
                 (hero-hp hero) (hero-max-hp hero)
                 (hero-effective-ac hero game)))
   ;; a hero who has changed class needs the ratings spelled out: the
   ;; Level row above speaks for the current art alone, and the arts
   ;; left behind are what half the spellbook still answers to
   (%art-rating-lines hero)
   (when (hero-caster-p hero)
     (list (format nil "SP ~D/~D" (hero-sp hero) (hero-max-sp hero))))
   ;; the count stays on the sheet under a tireless instrument — it
   ;; is what the tavern refills and what the singer falls back on
   ;; with the instrument off — with the reason it will not fall
   (when (hero-singer-p hero)
     (list (format nil "Tunes ~D/~D~:[~; (tireless)~]"
                   (hero-tunes hero) (hero-max-tunes hero)
                   (hero-tireless-p hero))))
   (list (format nil "STR ~D DEX ~D IQ ~D"
                 (hero-str hero) (hero-dex hero) (hero-iq hero))
         (format nil "CON ~D LCK ~D" (hero-con hero) (hero-lck hero))
         (format nil "Gold ~D gp~@[ ~A~]" (hero-gold hero)
                 (unless (hero-alive-p hero) "(down)")))
   ;; the conditions, one per line: four of them at once would run past
   ;; the sheet's 20 cells on one line, and each word is short alone
   (mapcar (lambda (a) (string-capitalize (ailment-title a)))
           (remove-if-not (lambda (a) (hero-ailment-p hero a)) *ailments*))))

(defun hero-image (hero)
  "HERO's portrait file name — the one stamped at creation (the
class's :IMAGE or :IMAGE-WOMAN, see MAKE-HERO).  A hero stamped :NONE
(made in a class with neither) always answers NIL, class change or
not; a hero stamped with nothing at all predates portraits (a save
older than v8) and falls back to the current class's :IMAGE instead."
  (let ((portrait (hero-portrait hero)))
    (case portrait
      ((:none) nil)
      ((nil) (hero-class-property (hero-class hero) :image))
      (t portrait))))

(defun hero-image-path (game hero)
  "HERO's portrait file resolved like an effect icon — relative to the
current map file's directory — or NIL when the class has none."
  (let ((image (hero-image hero)))
    (when image
      (%resolve-map-path (dungeon-map-name (game-map game)) image))))

(defconstant +sheet-page-size+ 8
  "Body rows a character-sheet carousel page shows at once; a longer
block scrolls with u/d — see MENU-WINDOW.  Both windowing pages count
against it.  The stat block reaches nine rows — and so scrolls — for a
raced hero who both casts and sings (the race line, the SP line and
the Tunes line at once), and for any raced hero who has changed class
(the Arts rows, one per three arts worn); the spells/songs page
overflows far more readily, as soon as a book and its head run past
eight rows.")

(defun hero-sheet-lines (game index &optional (top 0) ordering changing)
  "The character-sheet page for roster slot INDEX as text lines: the
hero's stat block (windowed at scroll offset TOP when it overflows
+SHEET-PAGE-SIZE+ rows — see *MENU-SCROLL*) and the sheet's
own key hints, a blank line between the two — no header; the roster
pane already shows who is who.  The front-ends draw these verbatim
(the SHOP-LINES pattern) and feed u/d through HERO-SHEET-SCROLL.
The page is the first stop of the sheet carousel: the closing NEXT
row (MENU-NEXT-OPTION, 'n' or a click) turns to the hero's pack page,
from there to a caster's or singer's spells/songs page, and from the
last page back here.  ORDERING true is the marching-order pick ('o'):
the hints give way to the where-to prompt, a digit there moves the
hero (MOVE-HERO) and Esc cancels.  CHANGING true is the class pick
('c'), the same shape: the hints give way to the arts open to the hero
(HERO-CLASS-CHANGE-TARGETS, numbered), a digit there takes that one
(CHANGE-CLASS) and Esc cancels."
  (let* ((hero (nth index (game-party game)))
         (body (when hero (hero-summary-lines hero game))))
    (append
     (when hero
       (menu-scrolled-lines body top
                            (lambda (i line)
                              (declare (ignore i))
                              line)
                            +sheet-page-size+))
     ;; the sheet's own letter keys stay on the page (first letter
     ;; picks), a blank line between them so the short page breathes;
     ;; the digit pick, u/d scrolling and Esc are common knowledge —
     ;; the help screen carries those.  The pack lost its row to the
     ;; carousel: NEXT (below) is the way there now.
     (cond
       ((and hero ordering)
        (list ""
              (format nil "Move ~A where?" (hero-name hero))))
       ;; the arts open to the hero, numbered — the pick list IS the
       ;; prompt, since a class change is not a thing to do by guesswork
       ((and hero changing)
        (append
         (list "" "Take up which art?")
         (loop for class in (hero-class-change-targets hero)
               for n from 1
               collect (format nil "~D) ~A" n
                               (string-capitalize
                                (substitute #\Space #\- (string class)))))))
       (t
        (append
         (list "")
         ;; a banked level heads the keys — the rise happens here,
         ;; and only while one is actually due
         (when (and hero (hero-level-up-pending-p hero))
           (list (menu-option #\l "Level up")
                 ""))
         ;; and the second ladder right under it, only while some art
         ;; will actually have the hero
         (when (and hero (hero-can-change-class-p hero))
           (list (menu-option #\c "Change class")
                 ""))
         (list (menu-option #\p "Pool gold")
               ""
               (menu-option #\t "Trade gold")
               ""
               (menu-option #\o "Order party")
               ""
               (menu-next-option))))))))

(defun hero-sheet-scroll (game index top char)
  "The sheet page's scroll offset after key CHAR (u/d — see
MENU-SCROLL), or NIL when CHAR does not scroll or slot INDEX is empty."
  (let ((hero (nth index (game-party game))))
    (when hero
      (menu-scroll top char (length (hero-summary-lines hero game))
                   +sheet-page-size+))))

(defun hero-magic-p (hero)
  "True when HERO has a spells/songs page on the sheet carousel — the
hero casts or sings."
  (or (hero-caster-p hero) (hero-singer-p hero)))

;;; ---------------------------------------------------------------------
;;; The spells/songs page (the sheet carousel's third stop — the
;;; SHOP-VIEW pattern again): the hero's book as a numbered list, a
;;; digit opens that entry's card, and the card casts or plays it.
;;; There is no separate inspect mode the way the pack page has one:
;;; the rows carry pick digits, so typing one IS the inspection.

(defstruct (magic-view (:constructor %make-magic-view))
  hero                ; the hero whose book this is
  pending             ; the entry whose card is showing, or NIL on the list
  refusal             ; why the showing card's spell will not go off
                      ; right now (SPELL-REFUSAL), or NIL — the card
                      ; says so on the page, since the takeover hides
                      ; the log that the cast menu would speak through
  (top 0))            ; scroll offset into the entry list

(defun make-magic-view (hero)
  (%make-magic-view :hero hero))

(defun magic-entries (hero)
  "HERO's book as pickable entries — (:SPELL . NAME) for each spell
known, then (:SONG . NAME) for each song, both in registration order.
One flat list, so a single run of pick digits addresses the whole book
even for a hero who casts and sings both; MAGIC-LINES puts the section
heads back in as it draws."
  (append (mapcar (lambda (name) (cons :spell name))
                  (when (hero-caster-p hero) (spells-for-hero hero)))
          (mapcar (lambda (name) (cons :song name))
                  (when (hero-singer-p hero) (songs-for-hero hero)))))

(defun %magic-entry-title (entry)
  (if (eq (car entry) :spell)
      (spell-title (cdr entry))
      (song-title (cdr entry))))

(defun magic-lines (game view)
  "The spells/songs page as menu lines — the front-ends draw these
verbatim (the SHOP-LINES pattern).  On the list: the hero's book
windowed to +BOOK-PAGE-SIZE+ entries (u/d scroll it, the geometry
riding *MENU-SCROLL* for the scrollbar), each row numbered so a digit
opens its card, with a Spells:/Songs: head standing over the first
entry of each kind IN THE WINDOW — the heads follow the window rather
than the book, so a scrolled page still says what it is looking at.
On a card: SPELL-CARD-LINES or SONG-CARD-LINES, closing with the
casting key.  Either way the carousel's NEXT row has the last word."
  (declare (ignore game))
  (let* ((hero (magic-view-hero view))
         (pending (magic-view-pending view)))
    (if pending
        (append
         (if (eq (car pending) :spell)
             (spell-card-lines hero (cdr pending))
             (song-card-lines hero (cdr pending)))
         ;; a refused cast puts its reason on the card — the page has
         ;; no log under it to speak through
         (when (magic-view-refusal view)
           (list "" (magic-view-refusal view)))
         (list ""
               (if (eq (car pending) :spell)
                   (menu-option #\c "Cast it")
                   (menu-option #\p "Play it"))
               ""
               (menu-next-option)))
        (let ((entries (magic-entries hero)))
          (append
           (if entries
               (multiple-value-bind (start end above below)
                   (menu-window (length entries) (magic-view-top view)
                                +book-page-size+)
                 (setf *menu-scroll* (when (or above below)
                                       (list start end (length entries))))
                 (let ((kind nil)
                       (i 0)
                       (rows '()))
                   (dolist (entry (subseq entries start end) (nreverse rows))
                     (incf i)
                     (unless (eq (car entry) kind)
                       (setf kind (car entry))
                       (when (> i 1) (push "" rows))
                       ;; the FIRST head in the window carries the
                       ;; range marker ("Spells:      9-16 of 24"):
                       ;; the row numbers are the keys and so start
                       ;; from 1 in every window, and without the
                       ;; range a scrolled book reads as the same
                       ;; eight entries over again.  A window that
                       ;; straddles both kinds says the book's range
                       ;; once, over the kind it opens on
                       (dolist (row (if (> i 1)
                                        (list (if (eq kind :spell)
                                                  "Spells:" "Songs:"))
                                        (menu-scroll-head
                                         (if (eq kind :spell)
                                             "Spells:" "Songs:"))))
                         (push row rows)))
                     (push (menu-numbered
                            i (format nil "~D) ~A" i
                                      (%magic-entry-title entry)))
                           rows))))
               (list "The book is empty."))
           (list ""
                 (menu-next-option)))))))

(defun magic-act (game view char)
  "Apply key CHAR to the spells/songs page.  On the list a digit opens
that entry's card, u/d scroll a long book, n turns the carousel and
Esc leaves the page.  On a card c casts the spell (or p plays the
song), Esc goes back to the list and n turns the carousel.  A cast the
caster cannot manage right now keeps the card up with the reason on it
(SPELL-REFUSAL — the cast menu logs its refusal, but nothing under
this page shows the log) and answers NIL.  Returns
:NEXT when the carousel should turn, :CANCELLED when the page should
give way to the stat block, :DONE when a cast or a tune resolved — the
front-end closes the sheet then, so the log can be read — (:CAST VIEW)
when the spell still wants a target and the front-end should go on
with that CAST-VIEW, else NIL."
  (let ((hero (magic-view-hero view))
        (pending (magic-view-pending view))
        (digit (digit-char-p char)))
    ;; last press's refusal has been read by now — any new key clears
    ;; it, and the one that earns a fresh one sets it again below
    (setf (magic-view-refusal view) nil)
    (cond
      ;; a card is showing
      (pending
       (cond
         ((member char '(#\n #\N)) :next)
         ((eql char #\Escape)
          (setf (magic-view-pending view) nil)
          nil)
         ((and (eq (car pending) :spell) (member char '(#\c #\C)))
          ;; a refusal keeps the card up and says why ON it: closing
          ;; the sheet to log the reason would lose the player's place,
          ;; and staying silent would tell them nothing
          (let ((refusal (spell-refusal game hero (cdr pending))))
            (cond (refusal
                   (setf (magic-view-refusal view) refusal)
                   nil)
                  (t
                   (let ((cast (begin-cast game hero (cdr pending))))
                     (if cast (list :cast cast) :done))))))
         ((and (eq (car pending) :song) (member char '(#\p #\P)))
          ;; the same courtesy the spell card pays: a refusal keeps
          ;; the card up and says why ON it (SONG-REFUSAL), since the
          ;; sheet has no log beneath to speak through
          (let ((refusal (song-refusal hero (cdr pending))))
            (cond (refusal
                   (setf (magic-view-refusal view) refusal)
                   nil)
                  (t
                   (sing-song game hero (cdr pending))
                   :done))))
         (t nil)))
      ;; the list
      ((member char '(#\n #\N)) :next)
      ((eql char #\Escape) :cancelled)
      (digit
       (let ((entry (menu-window-pick (magic-entries hero)
                                      (magic-view-top view) digit
                                      +book-page-size+)))
         (when entry (setf (magic-view-pending view) entry)))
       nil)
      (t
       (let ((top (menu-scroll (magic-view-top view) char
                               (length (magic-entries hero))
                               +book-page-size+)))
         (when top (setf (magic-view-top view) top)))
       nil))))

(defun party-full-p (game)
  (>= (length (game-party game)) +party-limit+))

(defun join-party (game hero)
  "Append HERO to the party.  Returns T and emits :PARTY-JOINED on
success; when the roster already holds +PARTY-LIMIT+ members, says so
and returns NIL (no error — recruiting past a full party is a normal
game situation, not a bug)."
  (if (party-full-p game)
      (progn
        (say game "The party is full - ~A cannot join." (hero-name hero))
        nil)
      (progn
        (setf (game-party game)
              (append (game-party game) (list hero)))
        (say game "~A joins the party!" (hero-name hero))
        (emit game :party-joined hero)
        t)))

(defun remove-from-party (game hero)
  "Take HERO out of the party, the others closing ranks — JOIN-PARTY's
inverse (the guild's Remove a member, see locations.lisp).  Says
nothing — the caller knows where the hero is going (the guild hall,
a story exit) and says so itself.  Returns HERO and emits :PARTY-LEFT
on success, NIL when HERO is not in the party."
  (when (member hero (game-party game))
    (setf (game-party game) (remove hero (game-party game) :count 1))
    (emit game :party-left hero)
    hero))

(defun move-hero (game from to)
  "Move the hero in roster slot FROM to slot TO (both 0-based), the
others closing ranks — the Bard's Tale marching-order change.  Order
matters: the first three living members are the ones monsters can
reach (see FRONT-RANKS).  Says the move and returns T; an empty or
out-of-range slot, or a move to the hero's own place, quietly returns
NIL.  The character sheet offers it on 'o'."
  (let* ((party (game-party game))
         (n (length party)))
    (when (and (< -1 from n) (< -1 to n) (/= from to))
      (let* ((hero (nth from party))
             (others (remove hero party :count 1)))
        (setf (game-party game)
              (append (subseq others 0 to)
                      (list hero)
                      (subseq others to)))
        (say game "~A now marches in position ~D."
             (hero-name hero) (1+ to))
        t))))

(defun pool-gold (game hero)
  "Pool the party's gold onto HERO, Bard's Tale style: every other
member (standing or down) hands their purse over.  Says the result and
returns the amount HERO gained — 0 when nobody had anything to give.
The shop pages and the character sheet offer it on 'g'."
  (let ((gained 0))
    (dolist (h (game-party game))
      (unless (eq h hero)
        (incf gained (hero-gold h))
        (setf (hero-gold h) 0)))
    (if (plusp gained)
        (progn
          (incf (hero-gold hero) gained)
          (say game "The party pools its gold: ~A holds ~D gp."
               (hero-name hero) (hero-gold hero)))
        (say game "Nobody else has gold to pool."))
    gained))

(defun trade-gold (game from to amount)
  "Hand AMOUNT gold from FROM's purse to TO's — the character sheet's
't', POOL-GOLD's counterpart for splitting a purse instead of piling
it.  Returns T and emits :COIN; says why and returns NIL when TO is
FROM, AMOUNT is not positive, or FROM holds less than AMOUNT.  A
fallen hero both gives and receives, as with POOL-GOLD."
  (cond ((eq from to)
         (say game "~A already holds that gold." (hero-name from))
         nil)
        ((not (plusp amount)) nil)
        ((< (hero-gold from) amount)
         (say game "~A does not have ~D gold." (hero-name from) amount)
         nil)
        (t
         (decf (hero-gold from) amount)
         (incf (hero-gold to) amount)
         (say game "~A hands ~D gold to ~A." (hero-name from) amount
              (hero-name to))
         (emit game :coin amount)
         t)))

;;; The trade interaction (the SHOP-VIEW pattern): the sheet's 't'
;;; opens it, a digit picks who receives, then the sum is typed —
;;; digits append, Backspace deletes, Return trades, Esc steps back a
;;; page at a time (the save menu's text-entry manners, on numbers).

(defstruct (trade-view (:constructor %make-trade-view))
  hero                ; the giver — the sheet's hero
  to                  ; the chosen recipient, or NIL while picking
  (amount ""))        ; the sum being typed, a digit string

(defun make-trade-view (hero)
  (%make-trade-view :hero hero))

(defconstant +trade-amount-limit+ 6
  "Digits the trade page accepts — six covers any purse the game
mints, and the amount row stays a row.")

(defun trade-lines (game view)
  "The trade-gold page as menu lines — the front-ends draw these
verbatim (the SHOP-LINES pattern): first the party as numbered rows
(who receives, the giver marked), then the amount being typed.  The
entry page names its own keys — typing is not the common navigation
the help screen covers (the save menu's rule)."
  (let ((hero (trade-view-hero view))
        (to (trade-view-to view)))
    (append
     (list "*** Trade Gold ***" ""
           (format nil "~A holds ~D gp." (hero-name hero)
                   (hero-gold hero))
           "")
     (if to
         (list (format nil "How much for ~A?  ~A_"
                       (hero-name to) (trade-view-amount view))
               ""
               "Type the sum; Return trades")
         (append
          (list "Give gold to whom?" "")
          (let ((i 0))
            (mapcar (lambda (h)
                      (incf i)
                      (menu-numbered
                       i (format nil "~D) ~A  (~D gp)~A" i (hero-name h)
                                 (hero-gold h)
                                 (if (eq h hero) " (giver)" ""))))
                    (game-party game))))))))

(defun trade-act (game view char)
  "Apply key CHAR to the trade-gold page.  On the pick page a digit
chooses who receives (the giver refuses politely) and Esc closes; on
the amount page digits build the sum, Backspace deletes, Return trades
(TRADE-GOLD says why when it cannot, and the sum starts over) and Esc
steps back to the pick page.  Returns :DONE when the trade lands,
:CANCELLED when Esc leaves the pick page, else NIL."
  (let ((hero (trade-view-hero view))
        (to (trade-view-to view))
        (digit (and (characterp char) (digit-char-p char))))
    (cond
      ((null to)
       (cond ((and digit (<= 1 digit (length (game-party game))))
              (let ((h (nth (1- digit) (game-party game))))
                (if (eq h hero)
                    (say game "~A already holds that gold."
                         (hero-name hero))
                    (setf (trade-view-to view) h)))
              nil)
             ((eql char #\Escape) :cancelled)
             (t nil)))
      ((or (eql char #\Return) (eql char #\Newline)
           (eql char (code-char 13)))
       (let ((amount (if (plusp (length (trade-view-amount view)))
                         (parse-integer (trade-view-amount view))
                         0)))
         (cond ((zerop amount)
                ;; nothing (or only zeros) typed — nothing changes
                ;; hands, back to the pick page
                (setf (trade-view-to view) nil
                      (trade-view-amount view) "")
                nil)
               ((trade-gold game hero to amount) :done)
               (t
                ;; the purse fell short — the say line said so, the
                ;; sum starts over for another try
                (setf (trade-view-amount view) "")
                nil))))
      ((eql char #\Escape)
       (setf (trade-view-to view) nil
             (trade-view-amount view) "")
       nil)
      ((or (eql char #\Backspace) (eql char (code-char 8)))
       (when (plusp (length (trade-view-amount view)))
         (setf (trade-view-amount view)
               (subseq (trade-view-amount view) 0
                       (1- (length (trade-view-amount view))))))
       nil)
      ((and digit (< (length (trade-view-amount view))
                     +trade-amount-limit+))
       (setf (trade-view-amount view)
             (concatenate 'string (trade-view-amount view)
                          (string char)))
       nil)
      (t nil))))

(defun alive-heroes (game)
  "The living party members, in party order."
  (remove-if-not #'hero-alive-p (game-party game)))

(defun party-alive-p (game)
  (not (null (alive-heroes game))))

(defun party-leader (game)
  "The hero the party's own words mean by \"the leader\": the first
living hero in marching order — the rank that walks in front and takes
what a cell hands out (the GOLD op's taker) — falling back to the
first hero of all when every one of them is down, so a line that names
the leader always has a name to use.  NIL only for an empty party."
  (or (first (alive-heroes game))
      (first (game-party game))))

(defun acting-heroes (game)
  "The heroes who can take a round action, in party order — standing,
and neither paralysed nor stone (HERO-CAN-ACT-P).  A round's orders are
collected over this list and resolved against it, so a helpless hero is
never asked for an order and never spends one."
  (remove-if-not #'hero-can-act-p (game-party game)))

(defun party-can-act-p (game)
  "Can anyone in the party still act?  A party frozen stiff is a beaten
party even though every hero lives — the fight would otherwise run
rounds forever with no one able to swing (see %COMBAT-OUTCOME)."
  (not (null (acting-heroes game))))

(defun front-ranks (game &optional (n 3))
  "The first N living heroes — the ones monsters can reach."
  (let ((alive (alive-heroes game)))
    (if (> (length alive) n) (subseq alive 0 n) alive)))

(defun hero-in-reach-p (game hero)
  "True when HERO stands in the front ranks — close enough to trade
melee blows with the enemy.  Reach cuts both ways: monsters swing
only at these heroes, and only these heroes can swing back (the back
ranks need a missile — see HERO-MISSILE-DICE)."
  (and (member hero (front-ranks game)) t))

(defun damage-hero (game hero amount)
  "Deal AMOUNT damage to HERO.  Emits :HERO-HURT when they take it and
stand, :HERO-DIED when this kills them (never both — the death cry
covers the blow) and :PARTY-DEFEATED when nobody is left standing.
Returns remaining hp."
  (setf (hero-hp hero) (max 0 (- (hero-hp hero) amount)))
  (if (zerop (hero-hp hero))
      (progn
        (say game "~A FALLS!" (hero-name hero))
        (emit game :hero-died hero)
        (unless (party-alive-p game)
          (emit game :party-defeated)))
      (emit game :hero-hurt hero amount))
  (hero-hp hero))

(defun heal-hero (game hero amount)
  "Heal HERO by AMOUNT, capped at max hp.  Returns the hp gained."
  (let* ((new (min (hero-max-hp hero) (+ (hero-hp hero) amount)))
         (gained (- new (hero-hp hero))))
    (setf (hero-hp hero) new)
    (when (> gained 0)
      (say game "~A recovers ~D hit point~A." (hero-name hero) gained
           (if (> gained 1) "s" "")))
    gained))

(defun poison-bite (game)
  "Poison's toll: every poisoned hero still standing loses
*POISON-BITE* hit points.  Taken on every party step and every combat
round, and it can kill — the bite goes through DAMAGE-HERO like any
other harm, so a hero can be walked to death by a venom nobody paid to
have drawn.  Returns the number of heroes bitten."
  (let ((bitten 0))
    (dolist (h (game-party game) bitten)
      (when (and (hero-alive-p h) (hero-ailment-p h :poison))
        (incf bitten)
        (say game "The poison bites ~A." (hero-name h))
        (damage-hero game h *poison-bite*)))))

;;; ---------------------------------------------------------------------
;;; Experience and levels

;;; The experience ladder is the campaign's, not the engine's: how long
;;; a game is, and how much of it a monster tier is worth, is content.
;;; The engine keeps the mechanism and its own curve for a game that
;;; registers none — the fixture world plays on that one.

(defparameter *xp-table* nil
  "The campaign's experience ladder: the running totals to reach levels
2, 3, 4, ... in order, or NIL for the engine's own curve.  Register one
with DEFINE-XP-TABLE, which checks its shape.")

(defparameter *xp-growth* 3/2
  "What a level past the end of *XP-TABLE* costs: the last total times
this, once per level beyond it.  Kept rational so the arithmetic stays
exact until XP-FOR-LEVEL's closing ROUND.")

(defun define-xp-table (totals &key (growth 3/2))
  "Register the campaign's experience ladder.  TOTALS are the running
experience totals to reach levels 2, 3, 4, ... in order; GROWTH is what
each level past the end of the list multiplies the last total by, so
the ladder never simply stops.  Returns TOTALS.

Signals an error unless TOTALS is a non-empty list of positive integers
that strictly increases, and GROWTH is greater than 1: a ladder that
stalls or turns back downhill would leave a hero either for ever ready
to rise or never ready at all, and a campaign should hear about that
when it loads rather than when someone plays it."
  (unless (and (consp totals)
               (every (lambda (n) (and (integerp n) (plusp n))) totals))
    (error "DEFINE-XP-TABLE: TOTALS must be a non-empty list of ~
            positive integers, not ~S" totals))
  (loop for (a b) on totals
        while b
        unless (< a b)
          do (error "DEFINE-XP-TABLE: TOTALS must strictly increase, ~
                     but ~D is followed by ~D" a b))
  (unless (and (realp growth) (> growth 1))
    (error "DEFINE-XP-TABLE: GROWTH must be a real greater than 1, ~
            not ~S" growth))
  (setf *xp-table* (copy-list totals)
        *xp-growth* growth)
  totals)

(defun xp-for-level (level)
  "Total experience required to reach LEVEL.  Level 1 is free.  With a
campaign ladder registered (DEFINE-XP-TABLE) the table answers for as
far as it reaches and *XP-GROWTH* compounds beyond its end; with none,
the engine's own curve does — 50 x LEVEL x (LEVEL - 1), which is gentle
enough for a one-dungeon fixture and far too gentle for a long game."
  (cond ((<= level 1) 0)
        ((null *xp-table*) (* 50 level (1- level)))
        (t (let ((n (length *xp-table*)))
             (if (<= level (1+ n))
                 ;; the table's first entry is level 2's total
                 (nth (- level 2) *xp-table*)
                 (round (* (nth (1- n) *xp-table*)
                           (expt *xp-growth* (- level 1 n)))))))))

(defun %maybe-raise-stats (game hero)
  "Bard's Tale stat growth on a level-up: every ability, in str dex iq
con lck order, draws a d18, and a draw at or above the current score
is a gain — the higher the score, the rarer, and a score of 18 (the
cap) can never rise.  At most ONE ability rises per level: the first
successful draw pays out and the rest are spent unheeded, so a rise
stays a moment and not a shower.  Still always five draws, so
scripted tests spend a fixed number of rolls per level."
  (let ((risen nil))
    (macrolet ((try (place label)
                 `(when (and (>= (roll 18) ,place) (not risen))
                    (setf risen t)
                    (incf ,place)
                    (say game "~A's ~A rises to ~D!"
                         (hero-name hero) ,label ,place))))
      (try (hero-str hero) "STR")
      (try (hero-dex hero) "DEX")
      (try (hero-iq hero)  "IQ")
      (try (hero-con hero) "CON")
      (try (hero-lck hero) "LCK"))))

(defun level-up (game hero)
  ;; the spellbook before the rise — level-gated, so comparing it
  ;; afterwards names exactly the spells the new level brings
  (let ((known (spells-for-hero hero)))
    (incf (hero-level hero))
    (say game "~A rises to level ~D!" (hero-name hero) (hero-level hero))
    ;; hp rolls first — the new level's dice plus the CON bonus, before
    ;; the stat gains, so a rising CON pays out from the next level on
    ;; (and the scripted tests' first roll stays the hit die)
    (let ((gain (max 1 (+ (roll-dice (hero-class-property
                                      (hero-class hero) :hp-dice))
                          (stat-gift (hero-con hero))))))
      (incf (hero-max-hp hero) gain)
      (incf (hero-hp hero) gain))
    (%maybe-raise-stats game hero)
    ;; class AC training: one point of natural armor every :AC-PER-LEVEL
    ;; levels beyond the first (the monk), floored at Bard's Tale's -10
    (let ((per (hero-class-property (hero-class hero) :ac-per-level)))
      (when (and per (zerop (mod (1- (hero-level hero)) per)))
        (setf (hero-ac hero) (max -10 (1- (hero-ac hero))))))
    ;; casters grow spell points like hit points: the new maximum
    ;; arrives as fresh, ready-to-burn sp (and reads a freshly risen IQ).
    ;; The maximum never FALLS — a hero who changed class keeps the
    ;; purse the old art earned (CHANGE-CLASS banks it) while the new
    ;; art's level-1 arithmetic climbs back up under it.
    (let ((new-sp (max (hero-max-sp hero)
                       (%hero-max-sp (hero-class hero) (hero-level hero)
                                     (hero-iq hero)))))
      (incf (hero-sp hero) (max 0 (- new-sp (hero-max-sp hero))))
      (setf (hero-max-sp hero) new-sp))
    ;; the spells the new level brings, named in registration order
    (dolist (name (remove-if (lambda (n) (member n known))
                             (spells-for-hero hero)))
      (say game "~A learns ~A!" (hero-name hero) (spell-title name)))
    (emit game :level-up hero)))

(defun hero-level-up-pending-p (hero)
  "Has HERO banked the experience for the next level?  The rise
itself is manual — ADVANCE-LEVEL, the character sheet's 'l' — so the
roster flags the readiness (the up-arrow beside the name) until the
player takes it."
  (>= (hero-xp hero) (xp-for-level (1+ (hero-level hero)))))

(defun advance-level (game hero)
  "Raise HERO one level when the experience is banked — the character
sheet's 'l'.  One level per call, so a doubly-crossed threshold takes
two presses and every rise gets its own moment.  Returns the new
level, or NIL when none is due."
  (when (hero-level-up-pending-p hero)
    (level-up game hero)
    (hero-level hero)))

(defun level-notes-lines (notes)
  "A rise's own page: NOTES — the messages ADVANCE-LEVEL said, oldest
first — as takeover lines, closed by the carousel's NEXT row.  The
character sheet owns the message area while it is up, so the rise's
story (the new level, the stat gains, the spells learned) would land
in a log nobody sees; the front-ends show it on a page of its own
instead and turn back to the sheet on the next key (or the NEXT row's
click)."
  (append notes (list "" (menu-next-option))))

(defun award-xp (game hero amount)
  "Grant HERO experience.  The level is never taken automatically:
crossing a threshold announces the readiness once, and the rise waits
for ADVANCE-LEVEL on the character sheet (Bard's Tale sent the party
home to the Review Board; here the sheet is the board)."
  (let ((was-ready (hero-level-up-pending-p hero)))
    (incf (hero-xp hero) amount)
    (when (and (not was-ready) (hero-level-up-pending-p hero))
      (say game "~A is ready for the next level!" (hero-name hero))))
  (hero-xp hero))

;;; ---------------------------------------------------------------------
;;; Changing class — Bard's Tale's second ladder.  A hero who has taken
;;; an art far enough (its :CHANGE-AT) may set it down for another: the
;;; level and the experience start over at 1 and 0, but the body keeps
;;; what it earned — hit points, spell points, gold, abilities, armor,
;;; pack.  The art left behind FREEZES at the level it was left
;;; (HERO-CLASS-LEVELS), and goes on granting exactly the spells it had
;;; opened — never one more.  That freeze is the whole point: the
;;; spells a hero can call on are the sum of every art they have worn,
;;; each answering to its own rating, which is why SPELL-KNOWN-P reads
;;; HERO-CLASS-LEVEL and not HERO-LEVEL.

(defun class-change-refusal (hero class)
  "Why HERO may not become CLASS right now, as a short line for the
sheet — or NIL when they may.  The sheet is +TAKEOVER-COLUMNS+ wide,
so the reasons stay short.  HERO-CLASS-CHANGE-TARGETS answers the
yes/no of the same rule for every registered class at once."
  (let ((change-at (hero-class-property (hero-class hero) :change-at))
        (group (hero-class-property (hero-class hero) :change-group)))
    (cond ((eq class (hero-class hero)) "Already that.")
          ((not (hero-alive-p hero)) "Not while down.")
          ((hero-held-class-p hero class) "Been there once.")
          ;; both keys, or the art is a life's work
          ((not (and change-at group)) "This art is for life.")
          ((< (hero-level hero) change-at)
           (format nil "Needs level ~D here." change-at))
          ;; a mage moves up to another mage class, never to a warrior
          ((not (eq group (hero-class-property class :change-group)))
           "Not that kind of art.")
          ((and (hero-race hero)
                (not (race-allows-class-p (hero-race hero) class)))
           (format nil "No ~A does that." (hero-race-title hero)))
          (t
           ;; the target's own entry price: levels attained in named
           ;; arts, the current one included (HERO-CLASS-LEVEL knows it)
           (let ((short (find-if-not
                         (lambda (entry)
                           (>= (hero-class-level hero (car entry))
                               (cdr entry)))
                         (hero-class-property class :requires))))
             (when short
               (format nil "~A ~D first."
                       (string-capitalize
                        (substitute #\Space #\- (string (car short))))
                       (cdr short))))))))

(defun hero-class-change-targets (hero)
  "The classes HERO may change into right now, sorted as HERO-CLASSES
sorts — the sheet's pick list.  Empty for a hero whose art has no
:CHANGE-AT, or who has not yet reached it."
  (remove-if (lambda (class) (class-change-refusal hero class))
             (hero-classes)))

(defun hero-can-change-class-p (hero)
  "True when some class is open to HERO — what the sheet's 'c' hint
rides on."
  (and (hero-class-change-targets hero) t))

(defun change-class (game hero class)
  "Move HERO into CLASS — the sheet's 'c'.  The art being left freezes
at its current level in HERO-CLASS-LEVELS (it will grant its opened
spells forever and never open another), then level and experience
start over at 1 and 0 while hit points, spell points, gold, abilities,
armor and pack carry across untouched.  Signals an error when
CLASS-CHANGE-REFUSAL has something to say — the sheet only ever offers
what HERO-CLASS-CHANGE-TARGETS listed.  Returns CLASS."
  (let ((refusal (class-change-refusal hero class)))
    (when refusal
      (error "~A cannot become a ~A: ~A" (hero-name hero)
             (string-capitalize (substitute #\Space #\- (string class)))
             refusal)))
  (let ((left (hero-class hero)))
    (push (cons left (hero-level hero)) (hero-class-levels hero))
    (setf (hero-class hero) class
          (hero-level hero) 1
          (hero-xp hero) 0
          ;; the bare-handed attack is the new art's, not the old one's:
          ;; class training, never something the body earned
          (hero-damage hero) (hero-class-property class :damage))
    ;; Spell points are kept whole — "as they were", the manual's word —
    ;; so the maximum can only ever rise here (a fresh caster who never
    ;; had any gets a level-1 purse; LEVEL-UP holds the same floor).
    (setf (hero-max-sp hero)
          (max (hero-max-sp hero)
               (%hero-max-sp class 1 (hero-iq hero)))
          (hero-sp hero) (min (hero-sp hero) (hero-max-sp hero)))
    ;; song charges follow the new art: a singer starts the level-1 day
    ;; with a full throat, anyone else stops carrying charges at all
    (setf (hero-tunes hero) (hero-max-tunes hero))
    (say game "~A sets down the ~A's art and takes up the ~A's!"
         (hero-name hero)
         (string-downcase (substitute #\Space #\- (string left)))
         (string-downcase (substitute #\Space #\- (string class))))
    (emit game :class-change hero))
  class)
