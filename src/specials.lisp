;;; Lambda's Tale — cell specials.
;;;
;;; A special is a list of ops attached to a map cell — in the .map file
;;; after the art (see map.lisp) or via (SETF CELL-SPECIAL).  Ops are
;;; pure data interpreted here; map files are never evaluated.  The ops:
;;;
;;;   (message TEXT...)          show each TEXT (a :MESSAGE event)
;;;   (set-flag KEY) (clear-flag KEY)
;;;   (when-flag KEY OP...)      run OP... if story flag KEY is set
;;;   (unless-flag KEY OP...)    ... if it is not
;;;   (at-night OP...)           run OP... between 20:00 and 06:00
;;;   (at-day OP...)             ... between 06:00 and 20:00 (pure clock
;;;                              tests — zone darkness is irrelevant)
;;;   (once OP...)               run OP... only the first time ever
;;;                              (one ONCE per cell; keyed by map + cell)
;;;   (teleport X Y [FACING])    relocate the party; the target cell's
;;;                              special triggers too (depth-capped)
;;;   (travel FILE [X Y] [FACING])  move the party to another zone (map
;;;                              file, relative to this map's directory):
;;;                              stairs, city gates, portals — cities and
;;;                              dungeons are all just maps (see
;;;                              TRAVEL-PARTY in game.lisp)
;;;   (location TITLE KIND ARG...)  enter a location (shop, temple, ...)
;;;                              on this cell — see locations.lisp
;;;   (spin)                     face a random direction (silently —
;;;                              classic spinner squares)
;;;   (damage DICE [TEXT])       hurt every living hero (DICE each)
;;;   (trap DICE [TEXT] [DIFFICULTY])  a floor trap: a levitating party
;;;                              floats over it, a trap-skilled hero
;;;                              (see :TRAP-SKILL) may spot and disarm
;;;                              it for this triggering, else it
;;;                              springs — TEXT (default "A trap
;;;                              springs!"), then every living hero
;;;                              saves (SAVING-THROW vs DIFFICULTY,
;;;                              default 14) for half of DICE.  TEXT
;;;                              and DIFFICULTY may come in either
;;;                              order.  Trap Zap disarms it for good;
;;;                              wrap in ONCE for a one-shot trap.
;;;   (heal DICE)                heal every living hero
;;;   (gold DICE)                treasure (goes to the leading hero)
;;;   (give-item NAME)           treasure with a name: the first living
;;;                              hero with pack room takes it, and with
;;;                              every pack full it is left behind (the
;;;                              combat find's rule) — a chest, a niche,
;;;                              a reward, anything a monster does not
;;;                              carry in for the party to kill it for
;;;   (take-item NAME)           one NAME leaves the party: a key spent
;;;                              on the gate it opened.  Carrying none
;;;                              is not an error
;;;   (when-item NAME OP...)     run OP... if anyone carries NAME
;;;   (unless-item NAME OP...)   ... if nobody does.  The pair is
;;;                              WHEN-FLAG's twin for things the party
;;;                              can hold, drop and hand around, where a
;;;                              flag only remembers that they once could
;;;   (encounter (MONSTER COUNT)...)  start combat; ops after this one
;;;                              are skipped (combat interrupts)
;;;   (event TOPIC ARG...)       emit a story event for subscribers

(in-package :tale)

(defvar *special-depth* 0)

(defun trigger-special (game)
  "Run the special of the cell the party stands on, if any.  Movement
and teleports call this; call it once by hand after NEW-GAME to fire
the start cell's special (after subscribing your event handlers)."
  (let ((ops (cell-special (game-map game) (game-x game) (game-y game))))
    (when ops
      (let ((*special-depth* (1+ *special-depth*)))
        (when (> *special-depth* 8)
          (error "Special recursion deeper than 8 at (~D,~D) of ~A - ~
                  teleport loop in the map data?"
                 (game-x game) (game-y game)
                 (dungeon-map-name (game-map game))))
        (run-special game ops)))
    (values)))

(defun run-special (game ops)
  "Interpret the special OPS in order.  Once combat starts — or a
TRAVEL op switches zones — the remaining ops are skipped: they belong
to the cell the party just left."
  (let ((map (game-map game)))
    (dolist (op ops)
      (when (or (game-combat game)
                (not (eq map (game-map game))))
        (return))
      (run-special-op game op)))
  (values))

(defun run-special-op (game op)
  (unless (and (consp op) (symbolp (first op)))
    (error "Invalid special op ~S (must be a list starting with an op name)"
           op))
  (case (first op)
    (message
     (dolist (text (rest op))
       (say game "~A" text)))
    (set-flag (set-flag game (second op)))
    (clear-flag (clear-flag game (second op)))
    (when-flag
     (when (flag game (second op))
       (run-special game (cddr op))))
    (unless-flag
     (unless (flag game (second op))
       (run-special game (cddr op))))
    (at-night
     (unless (daylight-p (game-time game))
       (run-special game (rest op))))
    (at-day
     (when (daylight-p (game-time game))
       (run-special game (rest op))))
    (once
     (let ((key (list :once (dungeon-map-name (game-map game))
                      (game-x game) (game-y game))))
       (unless (flag game key)
         (set-flag game key)
         (run-special game (rest op)))))
    (teleport
     (teleport-party game (second op) (third op) (fourth op)))
    (travel
     ;; (travel FILE), (travel FILE FACING), (travel FILE X Y [FACING])
     (destructuring-bind (file &rest args) (rest op)
       (if (or (null args) (not (integerp (first args))))
           (travel-party game file nil nil (first args))
           (travel-party game file (first args) (second args)
                         (third args)))))
    (location
     (enter-location game (rest op)))
    (spin
     (setf (game-facing game) (roll 4))
     (observe game))
    (damage
     (let ((dice (second op))
           (text (third op)))
       (when text
         (say game "~A" text))
       (dolist (h (alive-heroes game))
         (let ((n (max 0 (roll-dice dice))))
           (say game "~A TAKES ~D damage." (hero-name h) n)
           (damage-hero game h n)))))
    (trap (run-trap-op game (rest op)))
    (heal
     (dolist (h (alive-heroes game))
       (heal-hero game h (max 0 (roll-dice (second op))))))
    (gold
     (let ((n (roll-dice (second op)))
           (h (first (alive-heroes game))))
       (when h
         (incf (hero-gold h) n)
         (say game "The party finds ~D gold!" n))))
    (give-item (run-give-item-op game (second op)))
    (take-item (run-take-item-op game (second op)))
    (when-item
     (when (party-carrying-p game (second op))
       (run-special game (cddr op))))
    (unless-item
     (unless (party-carrying-p game (second op))
       (run-special game (cddr op))))
    (encounter (start-combat game (rest op)))
    (event (apply #'emit game (rest op)))
    (t (error "Unknown special op ~S in cell (~D,~D) of ~A"
              (first op) (game-x game) (game-y game)
              (dungeon-map-name (game-map game)))))
  (values))

(defun trap-disarmed-flag (map x y)
  "The story-flag key marking the trap on cell (X,Y) of MAP disarmed
for good (Trap Zap's work) — the ONCE key pattern."
  (list :trap-disarmed (dungeon-map-name map) x y))

(defun run-give-item-op (game name)
  "The GIVE-ITEM op: the map hands the party an item, the way GOLD
hands it coin.  The first living hero with pack room takes it —
%AWARD-LOOT's rule, so a chest and a corpse fill a pack the same way —
and GIVE-ITEM says whose pack was full on the way past.  With every
pack full the item is left where it lay and nobody carries it, which
is a game situation and not an error.  Returns the hero who took it,
or NIL.  Signals an error on an unregistered NAME."
  (find-item-type name)
  (say game "The party finds ~A!" (item-title name))
  (dolist (h (alive-heroes game))
    (when (give-item game h name)
      (say game "~A takes it." (hero-name h))
      (return-from run-give-item-op h)))
  nil)

(defun run-take-item-op (game name)
  "The TAKE-ITEM op: one NAME leaves the party, a key spent on the gate
it opened.  The first carrier in party order gives it up, standing or
fallen (PARTY-CARRIER's rule, so a WHEN-ITEM gate and the TAKE-ITEM
behind it never disagree about who counts), and the last copy leaves
the hands it was worn in (DROP-ITEM).  Carrying none is silent and not
an error: the map may spend what the party does not have, and the
gates that must not are written with WHEN-ITEM.  Returns the hero it
came from, or NIL."
  (let ((h (party-carrier game name)))
    (when h
      (drop-item game h name)
      h)))

(defun run-trap-op (game args)
  "The TRAP op: ARGS is (DICE [TEXT] [DIFFICULTY]), the two optionals
in either order.  A levitating party floats over; else the best
trap-skilled living hero may spot and disarm it (this triggering only
— the trap stays armed for the next visit); else it springs: TEXT,
then every living hero rolls a SAVING-THROW first and the damage DICE
second — the fixed roll order the test suites script against — a
save halving the blow."
  (let ((dice (first args))
        (text "A trap springs!")
        (difficulty 14))
    (unless dice
      (error "The trap op needs damage dice: (trap DICE [TEXT] [DIFFICULTY])"))
    (dolist (extra (rest args))
      (etypecase extra
        (string  (setf text extra))
        (integer (setf difficulty extra))))
    (let ((finder (best-trap-hero game)))
      (cond
        ((flag game (trap-disarmed-flag (game-map game)
                                        (game-x game) (game-y game)))
         ;; Trap Zap already announced the kill — a cleared corridor
         ;; shouldn't chatter.
         nil)
        ((levitate-active-p game)
         (say game "The party floats over a trap."))
        ((and finder (< (roll 100) (hero-trap-skill finder)))
         (say game "~A spots a trap and disarms it!" (hero-name finder)))
        (t
         (say game "~A" text)
         (dolist (h (alive-heroes game))
           (let* ((saved (saving-throw game h difficulty))
                  (n (max 0 (roll-dice dice)))
                  (n (if saved (floor n 2) n)))
             (when saved
               (say game "~A twists aside!" (hero-name h)))
             (say game "~A TAKES ~D damage." (hero-name h) n)
             (when (plusp n)
               (damage-hero game h n)))))))))

(defun best-trap-hero (game)
  "The living hero with the sharpest trap sense, or NIL when no one
has any."
  (let ((best nil))
    (dolist (h (alive-heroes game) best)
      (when (and (plusp (hero-trap-skill h))
                 (or (null best)
                     (> (hero-trap-skill h) (hero-trap-skill best))))
        (setf best h)))))

(defun teleport-party (game x y &optional facing)
  "Relocate the party to cell (X,Y), optionally FACING a direction, and
trigger the destination cell's special."
  (let ((map (game-map game)))
    (unless (and (integerp x) (< -1 x (dungeon-map-width map))
                 (integerp y) (< -1 y (dungeon-map-height map)))
      (error "Teleport target (~S,~S) is outside the ~Dx~D map ~A"
             x y (dungeon-map-width map) (dungeon-map-height map)
             (dungeon-map-name map)))
    (setf (game-x game) x
          (game-y game) y)
    (when facing
      (setf (game-facing game) (dir-index facing)))
    (observe game)
    (emit game :enter-cell x y)
    (trigger-special game)))
