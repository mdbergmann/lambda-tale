;;; Lambda's Tale — combat.
;;;
;;; Monster types are campaign data (DEFINE-MONSTER); the engine only
;;; knows the mechanics.  Combat is Bard's Tale round-based: the party
;;; declares actions, heroes strike first, then every surviving monster
;;; swings at a random front-rank hero.  The whole transcript travels as
;;; :MESSAGE events; :COMBAT-START and :COMBAT-END frame the fight.
;;;
;;; To-hit: d20 + bonus hits when it reaches 20 - AC (descending AC,
;;; unarmored 10 = hit on 10+).  Defending is +4 AC for the round.

(in-package :tale)

(defstruct (monster-type (:constructor %make-monster-type))
  name                ; string, e.g. "giant rat"
  (level 1)           ; to-hit bonus
  (hp-dice "1d8")
  (ac 10)
  (damage "1d4")
  (xp 10)
  (gold-dice 0)
  image)              ; portrait file name, map-relative, or NIL

(defvar *monster-types* (make-hash-table :test 'equalp))

(defun define-monster (name &key (level 1) (hp-dice "1d8") (ac 10)
                                 (damage "1d4") (xp 10) (gold 0) image)
  "Register monster type NAME (a string).  Campaign data calls this.
:IMAGE names the type's portrait file, resolved beside the map like a
location picture (COMBAT-IMAGE-PATH) — the Amiga front-end shows it in
the view column for as long as the fight lasts."
  (setf (gethash name *monster-types*)
        (%make-monster-type :name name :level level :hp-dice hp-dice
                            :ac ac :damage damage :xp xp :gold-dice gold
                            :image image))
  name)

(defun find-monster-type (name)
  (or (gethash name *monster-types*)
      (error "Unknown monster ~S (register it with DEFINE-MONSTER)" name)))

(defstruct (monster (:constructor %make-monster))
  kind                ; MONSTER-TYPE
  hp)

(defun monster-alive-p (monster)
  (> (monster-hp monster) 0))

(defstruct (combat (:constructor %make-combat))
  monsters            ; list of MONSTER (the fallen stay, filtered below)
  defenders           ; heroes defending during the current round
  (round-no 0))       ; completed rounds; COMBAT-ROUND counts them up

(defun alive-monsters (combat)
  (remove-if-not #'monster-alive-p (combat-monsters combat)))

(defun combat-groups (combat)
  "Alist of (MONSTER-TYPE . live count) over the living monsters,
in encounter order."
  (let ((groups '()))
    (dolist (m (alive-monsters combat) (nreverse groups))
      (let ((entry (assoc (monster-kind m) groups)))
        (if entry
            (incf (cdr entry))
            (push (cons (monster-kind m) 1) groups))))))

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

(defun combat-banner (combat)
  (with-output-to-string (s)
    (write-string "You face " s)
    (let ((first t))
      (dolist (group (combat-groups combat))
        (unless first (write-string " and " s))
        (setf first nil)
        (format s "~D ~A~A" (cdr group) (monster-type-name (car group))
                (if (> (cdr group) 1) "s" ""))))
    (write-char #\! s)))

(defun %spawn-monsters (name count)
  (let ((type (find-monster-type name))
        (monsters '()))
    (dotimes (i count (nreverse monsters))
      (push (%make-monster
             :kind type
             :hp (max 1 (roll-dice (monster-type-hp-dice type))))
            monsters))))

(defun start-combat (game spec)
  "Begin combat.  SPEC is a list of (MONSTER-NAME COUNT) groups; COUNT
may be dice (see PARSE-DICE).  Returns the new COMBAT."
  (when (game-combat game)
    (error "start-combat: combat is already in progress"))
  (let ((monsters '()))
    (dolist (group spec)
      (setf monsters
            (append monsters
                    (%spawn-monsters (first group)
                                     (max 1 (roll-dice (second group)))))))
    (let ((combat (%make-combat :monsters monsters)))
      (setf (game-combat game) combat)
      (emit game :combat-start monsters)
      (say game "~A" (combat-banner combat))
      combat)))

;;; ---------------------------------------------------------------------
;;; Attack resolution

(defun %attack-hits-p (bonus ac)
  "Roll d20 + BONUS against descending AC: hit on 20 - AC or better."
  (>= (+ 1 (roll 20) bonus) (- 20 ac)))

(defun %strike-monster (game attacker-name monster dmg)
  "Apply DMG to MONSTER with the hit/slay transcript — shared by melee
and damage spells so the log reads the same either way.  The verbs are
CAPITALS on purpose: landed damage must stand out when the transcript
scrolls by, while misses stay lowercase and quiet."
  (let ((type (monster-kind monster)))
    (decf (monster-hp monster) dmg)
    (if (monster-alive-p monster)
        (say game "~A HITS the ~A for ~D damage."
             attacker-name (monster-type-name type) dmg)
        (say game "~A SLAYS the ~A!"
             attacker-name (monster-type-name type)))))

(defun %hero-attack (game hero monster)
  "One melee strike.  Active effects weigh in: :FOES-AC makes the
monster easier to hit, :DAMAGE-BONUS strengthens the blow.  A class
with :CRIT-CHANCE (the hunter's art) may fell the monster outright on
a hit — the chance grows one point per hero level."
  (let ((type (monster-kind monster)))
    (if (%attack-hits-p (+ (hero-level hero) (stat-bonus (hero-str hero)))
                        (+ (monster-type-ac type) (effects-foes-ac game)))
        (let ((crit (hero-class-property (hero-class hero) :crit-chance)))
          (if (and crit (< (roll 100) (+ crit (hero-level hero))))
              (progn
                (say game "~A strikes a vital spot!" (hero-name hero))
                (%strike-monster game (hero-name hero) monster
                                 (monster-hp monster)))
              (%strike-monster game (hero-name hero) monster
                               (max 1 (+ (roll-dice (hero-attack-dice hero))
                                         (stat-bonus (hero-str hero))
                                         (effects-damage-bonus game))))))
        (say game "~A misses the ~A."
             (hero-name hero) (monster-type-name type)))))

(defun %monster-attack (game combat monster)
  (let* ((targets (front-ranks game))
         (hero (nth (roll (length targets)) targets))
         (type (monster-kind monster))
         (ac (- (hero-effective-ac hero game)
                (if (member hero (combat-defenders combat)) 4 0))))
    ;; a :FOES-ATTACK effect (a word of fear) blunts the monster's swing
    (if (%attack-hits-p (- (monster-type-level type)
                           (effects-foes-attack game))
                       ac)
        (let ((dmg (max 1 (roll-dice (monster-type-damage type)))))
          (say game "The ~A HITS ~A for ~D damage."
               (monster-type-name type) (hero-name hero) dmg)
          (damage-hero game hero dmg))
        (say game "The ~A misses ~A."
             (monster-type-name type) (hero-name hero)))))

(defun %monsters-act (game combat)
  (dolist (m (alive-monsters combat))
    (when (party-alive-p game)
      (%monster-attack game combat m))))

(defun %award-victory (game combat)
  (let ((xp 0)
        (gold 0))
    (dolist (m (combat-monsters combat))
      (incf xp (monster-type-xp (monster-kind m)))
      (incf gold (roll-dice (monster-type-gold-dice (monster-kind m)))))
    (say game "Victory!  The party wins ~D experience and ~D gold." xp gold)
    (let ((survivors (alive-heroes game)))
      (when survivors
        (incf (hero-gold (first survivors)) gold)
        (let ((share (floor xp (length survivors))))
          (dolist (h survivors)
            (award-xp game h share)))))))

(defun %combat-outcome (game combat)
  (cond ((not (party-alive-p game))
         (setf (game-combat game) nil)
         (say game "The party has been defeated...")
         (emit game :combat-end :defeat)
         :defeat)
        ((null (alive-monsters combat))
         (%award-victory game combat)
         (setf (game-combat game) nil)
         (emit game :combat-end :victory)
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
  "Fight one round.  ACTIONS lists an action per living hero in party
order — :attack (the default), :defend, (:cast SPELL [TARGET]) to
cast a spell (see CAST-SPELL; a failed cast wastes the round),
\(:sing SONG) to strike up a song (see SING-SONG; likewise), or
\(:use ITEM [TARGET]) to use an item (see USE-ITEM — how a Wizhelm
fires its spell in battle; likewise).  Heroes strike the first living
monster; then the surviving monsters strike back.  The round costs
one clock tick.  Returns :victory, :defeat or :ongoing."
  (let ((combat (game-combat game)))
    (unless combat
      (error "combat-round: no combat is in progress"))
    (advance-time game)
    (say game "-- Round ~D --" (incf (combat-round-no combat)))
    (let ((pairs (mapcar (lambda (h) (cons h (or (pop actions) :attack)))
                         (alive-heroes game))))
      (setf (combat-defenders combat)
            (let ((d '()))
              (dolist (p pairs (nreverse d))
                (when (eq (cdr p) :defend)
                  (push (car p) d)))))
      (dolist (p pairs)
        (let ((a (cdr p)))
          (cond ((eq a :attack)
                 ;; a warrior's training (:EXTRA-ATTACK-LEVELS) and a
                 ;; martial effect (:EXTRA-ATTACKS) grant more strikes;
                 ;; each re-aims at the front survivor
                 (dotimes (i (+ 1 (hero-extra-attacks (car p))
                                (effects-extra-attacks game)))
                   (let ((target (first (alive-monsters combat))))
                     (when target
                       (%hero-attack game (car p) target)))))
                ((and (consp a) (eq (first a) :cast))
                 (cast-spell game (car p) (second a) (third a)))
                ((and (consp a) (eq (first a) :sing))
                 (sing-song game (car p) (second a)))
                ((and (consp a) (eq (first a) :use))
                 (use-item game (car p) (second a) (third a)))))))
    (%monsters-act game combat)
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
;;; The model collects (HERO . ACTION) pairs in party order; C,
;;; P and U open the cast/sing/use pickers for the hero at hand in
;;; their :ORDERS mode, which hands the pick back as a round action
;;; instead of fighting a round itself (see %CAST-COMMIT /
;;; %SING-COMMIT / %USE-COMMIT).  F is
;;; party-level flight (from either page), Esc undoes the previous
;;; hero's pick — and on the review page it is N — and +/- set the
;;; transcript speed.  COMBAT-ORDERS-ACT returns (:FIGHT ACTIONS) only
;;; when the review is accepted; the front-end fights the round then.
;;;
;;; One hero per page is also what keeps the page inside the narrow
;;; message column: it costs a handful of rows whatever the party's
;;; size, where a page carrying every hero at once grew with the
;;; roster and lost its footer at the bottom of a lores column.

(defstruct (combat-orders (:constructor %make-combat-orders))
  chosen              ; (HERO . ACTION) pairs picked so far, party order
  sub                 ; CAST-VIEW/SING-VIEW picking for the hero at hand
  review)             ; T once every hero has picked: the review page,
                      ; awaiting Y (fight) or N (start over)

(defun make-combat-orders ()
  (%make-combat-orders))

(defun combat-orders-hero (game view)
  "The hero the orders view is asking about, or NIL once every living
hero has an action."
  (nth (length (combat-orders-chosen view)) (alive-heroes game)))

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
               (format nil "  ~D ~A~A" (cdr group)
                       (monster-type-name (car group))
                       (if (> (cdr group) 1) "s" "")))
             (combat-groups combat)))))

(defun %orders-hero-lines (game view)
  "The page that asks ONE hero for its order: the head block, the hero
at hand, and the actions — the one choice the page cannot proceed
without, so they stay on it, first letter as the key (no bracket
noise).  The navigation keys (Esc undo, +/- speed) are common
knowledge and live on the help screen instead.  The footer is two
short rows on purpose: the page draws in the message column (the
Amiga takeover), 27 characters wide at lores, and a row that has to
wrap costs a line.  Every row here fits that column whole."
  (append
   (%orders-head-lines game)
   (list ""
         (format nil "What will ~A do?"
                 (hero-name (combat-orders-hero game view))))
   (list ""
         "Attack  Defend  Cast"
         "Play  Use  Flee")))

(defun %orders-review-lines (game view)
  "The review page: every hero with the order it gave, and the
question the round waits on.  It does not repeat the enemy block —
every hero's page just showed it, and the rows the list needs are the
rows a full party's orders take.  F and +/- still answer here (see
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
page asking the hero at hand for its order, or — once every hero has
one — the review page listing them all under \"Is this OK?\".  While a
cast/sing/use pick is open, its page shows instead."
  (let ((sub (combat-orders-sub view)))
    (cond
      ((cast-view-p sub) (cast-lines game sub))
      ((sing-view-p sub) (sing-lines game sub))
      ((use-view-p sub) (use-lines game sub))
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
again from the first hero, F still runs, +/- still set the pace."
  (case (char-downcase char)
    (#\y (list :fight (mapcar #'cdr (combat-orders-chosen view))))
    ((#\n #\Escape)
     (setf (combat-orders-chosen view) '()
           (combat-orders-review view) nil)
     nil)
    (#\f :flee)
    (#\+ (adjust-combat-speed game 1) nil)
    (#\- (adjust-combat-speed game -1) nil)
    (t nil)))

(defun combat-orders-act (game view char)
  "Apply key CHAR to the round-orders page.  Returns (:FIGHT ACTIONS)
when the review page is accepted — the front-end fights the round with
them (COMBAT-ROUND) — :FLEE when the party runs (ATTEMPT-FLEE), else
NIL."
  (let ((sub (combat-orders-sub view))
        (hero (combat-orders-hero game view)))
    (cond
      (sub (%orders-sub-act game view sub char))
      ((combat-orders-review view) (%orders-review-act game view char))
      ((null hero) nil)                 ; nobody left to ask
      (t
       (case (char-downcase char)
         (#\a (%orders-record game view :attack))
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
         (#\f :flee)
         (#\+ (adjust-combat-speed game 1) nil)
         (#\- (adjust-combat-speed game -1) nil)
         (#\Escape
          (let ((chosen (combat-orders-chosen view)))
            (when chosen
              (setf (combat-orders-chosen view) (butlast chosen))))
          nil)
         (t nil))))))

(defun attempt-flee (game)
  "Try to run from combat.  Success (even odds) ends the fight; failure
gives the monsters a free round.  Returns :fled, :defeat or :ongoing."
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
