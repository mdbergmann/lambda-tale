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
  (items '())         ; pack contents: item names, at most +inventory-limit+
  (equipped '())      ; equipped subset: one :weapon, :armor, :shield each
  (tunes 0))          ; song charges (singers; refilled at a tavern)

(defvar *hero-classes* (make-hash-table :test 'eq))

(defun define-hero-class (name &key (hp-dice "1d8") (damage "1d4") (ac 10)
                                    caster singer image description
                                    extra-attack-levels crit-chance
                                    ac-per-level trap-skill)
  "Register hero class NAME (a keyword) with its hit dice, attack dice
and starting armor class; CASTER T marks a spell-casting class (spell
points from level and IQ, see %HERO-MAX-SP), SINGER T a song-playing
class (one tune charge per level, see songs.lisp).  IMAGE names the
class's portrait file (map-relative, like effect icons) — the Amiga
front-end shows it beside the character sheet; NIL = no portrait.
DESCRIPTION is the class's lore line (display data).
EXTRA-ATTACK-LEVELS N grants one extra strike per N levels beyond the
first (the warrior's art, see HERO-EXTRA-ATTACKS); CRIT-CHANCE N is
the percent chance a landed blow fells the foe outright, growing one
point per level (the hunter's art, see combat.lisp); AC-PER-LEVEL N
improves the class's natural armor by one every N levels beyond the
first (the monk's art, see LEVEL-UP — floored at -10, Bard's Tale's
best); TRAP-SKILL N is the percent chance to spot and disarm a
springing floor trap, growing one point per level (the rogue's art,
see the TRAP op in specials.lisp).  Campaign data calls this."
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
  (setf (gethash name *hero-classes*)
        (list :hp-dice hp-dice :damage damage :ac ac
              :caster caster :singer singer :image image
              :description description
              :extra-attack-levels extra-attack-levels
              :crit-chance crit-chance
              :ac-per-level ac-per-level
              :trap-skill trap-skill))
  name)

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

(defun make-hero (name class &key race (gold 0))
  "Create a level-1 hero of CLASS: hp from the class hit dice plus the
CON bonus (minimum 1 — a sickly hero still lives), abilities rolled 3d6
in the order str, dex, iq, con, lck, then adjusted by RACE's ability
modifiers when a race is given (see DEFINE-RACE).  A RACE that does not
permit CLASS is an error.  GOLD is the starting purse (campaign data
decides; dice strings welcome)."
  ;; A race that cannot take this class is a design error caught early,
  ;; before any dice roll, with a message that lists the legal classes.
  (when (and race (not (race-allows-class-p race class)))
    (error "A ~A cannot be a ~A — the race may be: ~{~A~^, ~}"
           (race-title race)
           (string-capitalize (substitute #\Space #\- (string class)))
           (mapcar #'race-title (race-classes (find-race race)))))
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

(defun hero-class-abbrev (hero)
  "The hero's class as the roster's CL column code, always two
characters so the name column keeps the room: the initials of the
first two words of a multi-word class, else the first two letters —
:war-mage -> \"WM\", :conjurer -> \"CO\"."
  (let* ((name (string (hero-class hero)))
         (words (loop with start = 0
                      for dash = (position #\- name :start start)
                      collect (subseq name start dash)
                      while dash
                      do (setf start (1+ dash)))))
    (string-upcase
     (if (rest words)
         (map 'string (lambda (w) (char w 0)) (subseq words 0 2))
         (subseq name 0 (min 2 (length name)))))))

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
   (when (hero-caster-p hero)
     (list (format nil "SP ~D/~D" (hero-sp hero) (hero-max-sp hero))))
   (when (hero-singer-p hero)
     (list (format nil "Tunes ~D/~D"
                   (hero-tunes hero) (hero-max-tunes hero))))
   (list (format nil "STR ~D DEX ~D IQ ~D"
                 (hero-str hero) (hero-dex hero) (hero-iq hero))
         (format nil "CON ~D LCK ~D" (hero-con hero) (hero-lck hero))
         (format nil "Gold ~D gp~@[ ~A~]" (hero-gold hero)
                 (unless (hero-alive-p hero) "(down)")))))

(defun hero-image (hero)
  "HERO's portrait file name (the class's :IMAGE), or NIL."
  (hero-class-property (hero-class hero) :image))

(defun hero-image-path (game hero)
  "HERO's portrait file resolved like an effect icon — relative to the
current map file's directory — or NIL when the class has none."
  (let ((image (hero-image hero)))
    (when image
      (%resolve-map-path (dungeon-map-name (game-map game)) image))))

(defconstant +sheet-page-size+ 8
  "Body rows a character-sheet carousel page shows at once; a longer
block scrolls with u/d — see MENU-WINDOW.  The spells/songs page only
overflows for a hero who both casts and sings; HERO-MAGIC-LINES
windows it the same way the stat block windows with
HERO-SHEET-SCROLL.")

(defun hero-sheet-lines (game index &optional (top 0) ordering)
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
hero (MOVE-HERO) and Esc cancels."
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
     (if (and hero ordering)
         (list ""
               (format nil "Move ~A where?" (hero-name hero)))
         (append
          (list "")
          ;; a banked level heads the keys — the rise happens here,
          ;; and only while one is actually due
          (when (and hero (hero-level-up-pending-p hero))
            (list (menu-option #\l "Level up")
                  ""))
          (list (menu-option #\p "Pool gold")
                ""
                (menu-option #\t "Trade gold")
                ""
                (menu-option #\o "Order party")
                ""
                (menu-next-option)))))))

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

(defun %hero-magic-body (hero)
  "The spells/songs page's body rows: the spellbook (SPELLS-FOR-HERO:
class + level) and the songbook (SONGS-FOR-HERO), each under its own
head with the titles indented one cell — so a campaign's spell and
song titles must respect the stat block's width.  NIL for a hero with
neither."
  (append
   (when (hero-caster-p hero)
     (cons "Spells:"
           (mapcar (lambda (name)
                     (format nil " ~A" (spell-title name)))
                   (spells-for-hero hero))))
   (when (hero-singer-p hero)
     (append
      (when (hero-caster-p hero) (list ""))
      (cons "Songs:"
            (mapcar (lambda (name)
                      (format nil " ~A" (song-title name)))
                    (songs-for-hero hero)))))))

(defun hero-magic-lines (game index &optional (top 0))
  "The sheet carousel's spells/songs page for roster slot INDEX as
text lines: the hero's spellbook and songbook (%HERO-MAGIC-BODY,
windowed at scroll offset TOP when it overflows +SHEET-PAGE-SIZE+
rows — see *MENU-SCROLL*) and the closing NEXT row that turns the
carousel back to the stat block.  The front-ends draw these verbatim
and feed u/d through HERO-MAGIC-SCROLL; they only open the page for a
HERO-MAGIC-P hero."
  (let ((hero (nth index (game-party game))))
    (append
     (when hero
       (menu-scrolled-lines (%hero-magic-body hero) top
                            (lambda (i line)
                              (declare (ignore i))
                              line)
                            +sheet-page-size+))
     (list ""
           (menu-next-option)))))

(defun hero-magic-scroll (game index top char)
  "The spells/songs page's scroll offset after key CHAR (u/d — see
MENU-SCROLL), or NIL when CHAR does not scroll or slot INDEX is
empty."
  (let ((hero (nth index (game-party game))))
    (when hero
      (menu-scroll top char (length (%hero-magic-body hero))
                   +sheet-page-size+))))

(defun party-full-p (game)
  (>= (length (game-party game)) +party-limit+))

(defun join-party (game hero)
  "Append HERO to the party.  Returns T and emits :PARTY-JOINED on
success; when the roster already holds +PARTY-LIMIT+ members, says so
and returns NIL (no error — recruiting past a full party is a normal
game situation, not a bug)."
  (if (party-full-p game)
      (progn
        (say game "The party is full — ~A cannot join." (hero-name hero))
        nil)
      (progn
        (setf (game-party game)
              (append (game-party game) (list hero)))
        (say game "~A joins the party!" (hero-name hero))
        (emit game :party-joined hero)
        t)))

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

;;; ---------------------------------------------------------------------
;;; Experience and levels

(defun xp-for-level (level)
  "Total experience required to reach LEVEL."
  (* 50 level (1- level)))

(defun %maybe-raise-stats (game hero)
  "Bard's Tale stat growth on a level-up: every ability, in str dex iq
con lck order, draws a d18 and rises by 1 when the draw lands at or
above the current score — the higher the score, the rarer the gain,
and a score of 18 (the cap) can never rise.  Always five draws, so
scripted tests spend a fixed number of rolls per level."
  (macrolet ((try (place label)
               `(when (>= (roll 18) ,place)
                  (incf ,place)
                  (say game "~A's ~A rises to ~D!"
                       (hero-name hero) ,label ,place))))
    (try (hero-str hero) "STR")
    (try (hero-dex hero) "DEX")
    (try (hero-iq hero)  "IQ")
    (try (hero-con hero) "CON")
    (try (hero-lck hero) "LCK")))

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
    ;; arrives as fresh, ready-to-burn sp (and reads a freshly risen IQ)
    (let ((new-sp (%hero-max-sp (hero-class hero) (hero-level hero)
                                (hero-iq hero))))
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
