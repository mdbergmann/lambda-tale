;;; Lambda's Tale — items, inventory and equipment.
;;;
;;; Item types are campaign data, not engine facts: the campaign
;;; registers them with DEFINE-ITEM in its campaign.lisp and maps
;;; refer to them by name in shop stock lists.  The engine only knows
;;; the mechanics: a hero carries up to +INVENTORY-LIMIT+ items and
;;; can equip one item of each equipment kind (*ITEM-KINDS* less
;;; :MISC — weapon, armor, shield, helmet, gloves, bow, arrow,
;;; instrument, ring, wand, figurine) at a time.  A :TWO-HANDED
;;; weapon and a shield exclude each other — the weapon fills both
;;; hands (the D&D rule).  Armor class is descending, so an item's
;;; :AC bonus *lowers* the effective AC.
;;;
;;; An item may also be USABLE (:USE) — a torch, a potion, a wand:
;;; using it applies an effect from the same vocabulary spells speak —
;;; instant keys that need no battle ((:heal DICE), (:summon NAME)),
;;; or timed keys through APPLY-EFFECT-SPEC ((:light t :duration MIN),
;;; (:buff-ac N :duration MIN)), but not both at once.  The battle
;;; instants (the damage family) may not ride on an item directly:
;;; a spell-triggering item says :USE (:CAST SPELL) instead and casts
;;; that registered spell for free — no spell points, no spellbook,
;;; the item is the magic (Bard's Tale's Wizhelm).  A :CONSUMED item
;;; leaves the pack on use, and :IMAGE names the effects-band icon of
;;; the installed effect.  The use interaction (USE-VIEW / USE-LINES /
;;; USE-ACT, the SHOP-VIEW pattern) lives here too, driven by both
;;; front-ends, and so does the pack page (EQUIP-VIEW — 'i' on the
;;; character sheet): a digit toggles a pack item on/off, class-unfit
;;; items are marked, 'p' hands an item to another party member
;;; (PASS-ITEM), and 'i' inspects one — the item card, the item's
;;; registered facts plus its player-facing :DESCRIPTION.

(in-package :tale)

(defconstant +inventory-limit+ 8
  "Maximum items a hero can carry (Bard's Tale pack size) — of the
gear, that is: a :QUEST piece is counted by nobody (see PACK-BURDEN).")

(defparameter *item-kinds*
  '(:weapon :armor :shield :helmet :gloves :bow :arrow
    :instrument :ring :wand :figurine :misc)
  "The item kinds (Bard's Tale's equipment categories).  Every kind
but :MISC is equipment: a hero wears one item of each kind at a time,
and every equipped item's :AC bonus counts.")

(defstruct (item-type (:constructor %make-item-type))
  name                ; symbol, e.g. SHORT-SWORD
  title               ; display string, e.g. "Short Sword"
  (kind :misc)        ; one of *ITEM-KINDS*
  (price 0)           ; shop price in gold
  damage              ; attack dice (weapons), or NIL
  reach               ; how far it carries as a missile, in feet: the
                      ; flight of these arrows, the throw of this axe.
                      ; A weapon with a reach is a thrown weapon and
                      ; needs no bow.  NIL leaves it unmeasured — a
                      ; missile that always carries (see MONSTERS-IN-REACH)
  (ac 0)              ; armor bonus: subtracted from descending AC
  classes             ; hero classes allowed to use it; NIL = anyone
  two-handed          ; T: the weapon fills both hands — no shield beside it
  use                 ; effect on use: a non-battle instant spec
                      ; (:heal DICE), a timed spec (:light t :duration
                      ; MIN), or (:cast SPELL); NIL = not usable
  consumed            ; T: one use, the item leaves the pack
  quest               ; T: a plot piece — carried outside the eight-slot
                      ; limit, on the pack's own quest page, and neither
                      ; sold nor thrown away (see PACK-BURDEN)
  image               ; effects-band icon for the timed :use, or NIL
  description         ; player-facing text — the pack page's item card
                      ; shows it; NIL = the card shows the facts alone
  notes)              ; designer notes (canon powers awaiting their
                      ; subsystem, story roles) — carried as data so
                      ; generated catalogues show them; no mechanics

(defvar *item-types* (make-hash-table :test 'eq))

(defun define-item (name &key title (kind :misc) (price 0) damage reach (ac 0)
                              classes two-handed use consumed quest image
                              description notes)
  "Register item type NAME (a symbol).  Campaign data calls this.
TITLE defaults to the capitalized name (SHORT-SWORD -> \"Short Sword\").
:TWO-HANDED (weapons only) makes the weapon fill both hands: it will
not go on beside a shield, nor a shield beside it.
:REACH is how far the item carries as a missile, in feet — the flight
of a quiver of arrows, the throw of a hurled axe.  A :WEAPON given one
is a thrown weapon: it shoots from any rank with no bow beside it (see
HERO-MISSILE-DICE).  Omit it and the missile goes unmeasured, carrying
to whatever the fight puts in front of it.
:DESCRIPTION is a player-facing string — the pack page's item card
\('i') shows it under the item's facts.
:NOTES is a designer-facing string (canon powers awaiting their
subsystem, story roles) — data, not mechanics, so generated
catalogues can surface it.
:USE makes the item usable, one of three shapes: instant keys of the
shared vocabulary that need no battle (e.g. (:heal DICE),
\(:summon NAME) — the damage family is refused); a timed spec
\(*TIMED-EFFECT-KEYS* in game.lisp, e.g. (:light t :duration 30))
that installs its effect; or (:cast SPELL) — using the item casts the
already-registered spell for free, so register the spell first.
:CONSUMED spends the item on use and :IMAGE names the installed
effect's band icon.
:QUEST marks a plot piece — a key, a token, a proof.  It rides outside
the eight-slot pack limit (PACK-BURDEN), reads on the pack's own quest
page instead of among the gear, and no shop buys it and no hand throws
it away: a party should never have to choose between carrying the
story and carrying a sword, nor be able to sell the way forward."
  (unless (member kind *item-kinds*)
    (error "define-item ~S: kind ~S is not one of ~{~S~^ ~}"
           name kind *item-kinds*))
  (when (and two-handed (not (eq kind :weapon)))
    (error "define-item ~S: :two-handed is a weapon trait (kind is ~S)"
           name kind))
  (when (and description (not (stringp description)))
    (error "define-item ~S: :description must be a string (got ~S)"
           name description))
  (when (and notes (not (stringp notes)))
    (error "define-item ~S: :notes must be a string (got ~S)" name notes))
  (when use
    (unless (consp use)
      (error "define-item ~S: :use ~S must be an effect plist -- ~
              (:heal DICE), a timed spec like (:light t :duration 30), ~
              or (:cast SPELL)"
             name use))
    (if (eq (first use) :cast)
        (progn
          (unless (and (= (length use) 2)
                       (symbolp (second use)) (second use))
            (error "define-item ~S: a casting :use is (:cast SPELL) ~
                    alone (got ~S)" name use))
          ;; a clear error now beats a broken item later: the spell
          ;; must already be registered (DEFINE-SPELL comes first)
          (find-spell-type (second use)))
        (multiple-value-bind (timed instant)
            (check-effect-spec "define-item" name use)
          (when (and timed instant)
            (error "define-item ~S: :use ~S mixes timed and instant ~
                    effects -- an item speaks one or the other"
                   name use))
          (when (effect-spec-combat-only-p use)
            (error "define-item ~S: :use ~S is a battle effect -- ~
                    register a spell and give the item :use ~
                    (:cast SPELL) instead"
                   name use)))))
  (when (and consumed (not use))
    (error "define-item ~S: :consumed without a :use" name))
  (when (and quest (plusp price))
    (error "define-item ~S: a :quest piece has no price -- no shop ~
            buys it and none sells it (got ~S)" name price))
  (when (and reach (not (and (integerp reach) (plusp reach))))
    (error "define-item ~S: :reach must be a positive integer in feet ~
            (got ~S)" name reach))
  (when (and reach (not (member kind '(:weapon :bow :arrow))))
    (error "define-item ~S: :reach is a missile trait (kind is ~S)"
           name kind))
  (setf (gethash name *item-types*)
        (%make-item-type
         :name name
         :title (or title
                    (string-capitalize (substitute #\Space #\- (string name))))
         :kind kind :price price :damage damage :reach reach
         :ac ac :classes classes
         :two-handed two-handed :use use :consumed consumed :quest quest
         :image image
         :description description :notes notes))
  name)

(defun find-item-type (name)
  (or (gethash name *item-types*)
      (error "Unknown item ~S (register it with DEFINE-ITEM)" name)))

(defun item-title (name)
  (item-type-title (find-item-type name)))

(defun item-usable-p (hero name)
  "Can HERO's class use item NAME?  (NIL :classes means anyone.)"
  (let ((classes (item-type-classes (find-item-type name))))
    (or (null classes)
        (and (member (hero-class hero) classes) t))))

(defun item-fit-marker (hero name)
  "\" (u)\" when HERO's class cannot use item NAME, else \"\" — the
sheet, pack and shop pages append it to the item's row so a class
mismatch shows before the player tries (or buys).  Kept to three
characters so a marked row does not wrap the narrow menu pages; the
item card spells the full (unfit) out."
  (if (item-usable-p hero name) "" " (u)"))

(defun item-hand-marker (name)
  "\" (2H)\" when item NAME fills both hands, else \"\" — the pack and
shop pages append it so the shield trade-off shows before the player
equips (or buys)."
  (if (item-type-two-handed (find-item-type name)) " (2H)" ""))

;;; ---------------------------------------------------------------------
;;; Inventory

(defun hero-carrying-p (hero name)
  (member name (hero-items hero)))

(defun quest-item-p (name)
  "Is item NAME a plot piece (DEFINE-ITEM's :QUEST)?  Those ride
outside the pack limit and are neither sold nor thrown away."
  (and (item-type-quest (find-item-type name)) t))

(defun pack-burden (hero)
  "How much of HERO's pack the eight-slot limit counts: the gear, and
not the quest pieces.  A party carrying eleven shards of a broken
crown is still a party with eight hands free."
  (count-if-not #'quest-item-p (hero-items hero)))

(defun pack-gear (hero)
  "HERO's gear as (INDEX . NAME) pairs in pack order — everything
PACK-BURDEN counts, with INDEX its position in HERO-ITEMS.  The index
travels with the name because EQUIPPED-INSTANCE-P counts duplicate
copies by it, so the pack page can star the worn one while showing
the quest pieces elsewhere."
  (let ((i -1))
    (loop for name in (hero-items hero)
          do (incf i)
          unless (quest-item-p name)
            collect (cons i name))))

(defun hero-quest-items (hero)
  "The plot pieces in HERO's pack, in pack order."
  (remove-if-not #'quest-item-p (hero-items hero)))

(defun party-quest-items (game)
  "Every plot piece the party carries, in party then pack order —
what the story has handed them so far."
  (loop for h in (game-party game)
        append (hero-quest-items h)))

(defun party-carrying-p (game name)
  "Does anyone in the party carry item NAME?  Standing or fallen: a key
in a dead man's pack is still the party's, so a gate that asks for one
opens for a party carrying its bearer's body.  Signals an error on an
unregistered NAME — a typo in map data should be loud."
  (find-item-type name)
  (and (find-if (lambda (h) (hero-carrying-p h name)) (game-party game)) t))

(defun party-carrier (game name)
  "The first hero in the party carrying item NAME, or NIL.  Party order,
standing or fallen — PARTY-CARRYING-P's rule, so a gate and whatever
spends the key behind it always agree about who counts."
  (find-item-type name)
  (find-if (lambda (h) (hero-carrying-p h name)) (game-party game)))

(defun give-item (game hero name)
  "Put item NAME into HERO's pack.  Returns T, or says the pack is full
and returns NIL (like JOIN-PARTY, a full pack is a game situation, not
a bug).  A :QUEST piece always fits: it is carried outside the limit
and so can never be the thing a full pack turns away."
  (find-item-type name)
  (if (and (not (quest-item-p name))
           (>= (pack-burden hero) +inventory-limit+))
      (progn
        (say game "~A's pack is full." (hero-name hero))
        nil)
      (progn
        (setf (hero-items hero) (append (hero-items hero) (list name)))
        t)))

(defun drop-item (game hero name)
  "Remove one item NAME from HERO's pack, unequipping it only when the
copy leaving is the last one — a worn name with a spare copy still in
the pack gives the spare away, and the hands keep what they hold.
Returns T, or NIL when the hero does not carry it."
  (declare (ignore game))
  (when (hero-carrying-p hero name)
    (when (<= (count name (hero-items hero))
              (count name (hero-equipped hero)))
      (setf (hero-equipped hero) (remove name (hero-equipped hero) :count 1)))
    (setf (hero-items hero) (remove name (hero-items hero) :count 1))
    t))

(defun pass-item (game from to name)
  "Hand one item NAME from FROM's pack to TO's — the pack page's 'p'.
The last copy is unequipped on the way (it leaves FROM's hands with
FROM's pack; a spare copy goes instead while one is there — see
DROP-ITEM) and lands at the end of TO's.  Returns T and emits :ITEM-PASSED;
says why and returns NIL when FROM does not carry it, TO is FROM, or
TO's pack is full — the item stays whole with FROM then, never
destroyed.  Class fit is deliberately no barrier: anyone may CARRY
anything (EQUIP-ITEM is where the fit is checked, so a mule can haul
the mage's armor), and a fallen hero both gives and receives, as with
POOL-GOLD."
  (find-item-type name)                 ; an unknown item is a bug, not a refusal
  (cond
    ((not (hero-carrying-p from name))
     (say game "~A does not carry ~A." (hero-name from) (item-title name))
     nil)
    ((eq from to)
     (say game "~A already carries ~A." (hero-name from) (item-title name))
     nil)
    ;; The hand-over comes first and the item leaves FROM only once it
    ;; has landed: GIVE-ITEM says "pack is full" itself, and dropping
    ;; first would destroy the item when the receiving pack is full.
    ((not (give-item game to name)) nil)
    (t
     (drop-item game from name)
     (say game "~A hands ~A to ~A." (hero-name from) (item-title name)
          (hero-name to))
     (emit game :item-passed from to name)
     t)))

(defun discard-item (game hero name)
  "Throw one item NAME from HERO's pack away for good — the pack
page's 't', behind its are-you-sure row (the only pack action that
destroys; a shop at least pays half).  The last copy is unequipped on
the way out (a spare goes first — see DROP-ITEM).  Returns T and
emits :ITEM-DISCARDED; says why and returns NIL when the hero does
not carry it, or when it is a :QUEST piece — the story is not the
player's to destroy, and the pack page never offers one."
  (find-item-type name)                 ; an unknown item is a bug, not a refusal
  (cond
    ((not (hero-carrying-p hero name))
     (say game "~A does not carry ~A." (hero-name hero)
          (item-title name))
     nil)
    ((quest-item-p name)
     (say game "~A will not part with ~A." (hero-name hero)
          (item-title name))
     nil)
    (t
     (drop-item game hero name)
     (say game "~A throws ~A away." (hero-name hero) (item-title name))
     (emit game :item-discarded hero name)
     t)))

;;; ---------------------------------------------------------------------
;;; Equipment

(defun equipped-of-kind (hero kind)
  "The equipped item of KIND, or NIL."
  (find kind (hero-equipped hero)
        :key (lambda (name) (item-type-kind (find-item-type name)))))

(defun equip-item (game hero name)
  "Equip item NAME from HERO's pack, replacing any equipped item of the
same kind.  Returns T; says why and returns NIL when the hero does not
carry it, the item is not equipment, the class cannot use it, or a
two-handed weapon and a shield would share the same pair of hands."
  (cond ((not (hero-carrying-p hero name))
         (say game "~A does not carry ~A." (hero-name hero) (item-title name))
         nil)
        ((eq (item-type-kind (find-item-type name)) :misc)
         (say game "~A cannot be equipped." (item-title name))
         nil)
        ((not (item-usable-p hero name))
         (say game "~A cannot use ~A." (hero-name hero) (item-title name))
         nil)
        ((and (item-type-two-handed (find-item-type name))
              (equipped-of-kind hero :shield))
         (say game "~A needs both hands -- ~A must put ~A away first."
              (item-title name) (hero-name hero)
              (item-title (equipped-of-kind hero :shield)))
         nil)
        ((and (eq (item-type-kind (find-item-type name)) :shield)
              (let ((weapon (equipped-of-kind hero :weapon)))
                (and weapon
                     (item-type-two-handed (find-item-type weapon)))))
         (say game "~A's hands are full with ~A."
              (hero-name hero)
              (item-title (equipped-of-kind hero :weapon)))
         nil)
        (t
         (let ((old (equipped-of-kind
                     hero (item-type-kind (find-item-type name)))))
           (when old
             (setf (hero-equipped hero)
                   (remove old (hero-equipped hero) :count 1))))
         (setf (hero-equipped hero)
               (append (hero-equipped hero) (list name)))
         (say game "~A equips ~A." (hero-name hero) (item-title name))
         t)))

(defun unequip-item (game hero name)
  "Unequip item NAME (it stays in the pack).  Returns T, or NIL when it
was not equipped."
  (declare (ignore game))
  (when (member name (hero-equipped hero))
    (setf (hero-equipped hero) (remove name (hero-equipped hero) :count 1))
    t))

(defun equipped-instance-p (hero name index)
  "True when pack position INDEX (0-based) holds the copy of NAME the
hero actually wears: the name is equipped and INDEX is its first
occurrence in the pack.  Two identical swords are indistinguishable
as items, so the first copy stands for the worn one — the pack page
stars exactly that row, and its digit is the one that takes the item
off (EQUIP-ACT), so with duplicates in the pack the display and the
toggle cannot disagree."
  (and (member name (hero-equipped hero))
       (eql index (position name (hero-items hero)))
       t))

(defun toggle-equip (game hero name)
  "Equip pack item NAME, or take it off when it is worn — the pack
page's one-key toggle.  Returns T on a change; says why and returns
NIL when the item cannot go on (see EQUIP-ITEM)."
  (if (member name (hero-equipped hero))
      (progn
        (unequip-item game hero name)
        (say game "~A removes ~A." (hero-name hero) (item-title name))
        t)
      (equip-item game hero name)))

(defun hero-attack-dice (hero)
  "The dice HERO attacks with: the equipped weapon's damage, else the
hero's bare (class) damage."
  (let ((weapon (equipped-of-kind hero :weapon)))
    (if weapon
        (or (item-type-damage (find-item-type weapon)) (hero-damage hero))
        (hero-damage hero))))

(defun %missile-pair (hero)
  "The equipped items behind HERO's shot, as (SHOT . LAUNCHER), or NIL
when the hero carries no missile: the arrows with the bow that strings
them, or a thrown weapon standing alone with no launcher.  The shot
carries the dice and the reach; the launcher only stands in for what
the shot leaves unsaid.  A strung pair wins over a throwable weapon —
an archer with a bow in hand uses it."
  (let ((bow (equipped-of-kind hero :bow))
        (arrows (equipped-of-kind hero :arrow))
        (weapon (equipped-of-kind hero :weapon)))
    (cond ((and bow arrows) (cons arrows bow))
          ((and weapon (item-type-reach (find-item-type weapon)))
           (cons weapon nil)))))

(defun %missile-property (hero reader)
  "READER applied to HERO's shot, falling back to its launcher — the
shared rule behind HERO-MISSILE-DICE and HERO-MISSILE-REACH."
  (let ((pair (%missile-pair hero)))
    (when pair
      (or (funcall reader (find-item-type (car pair)))
          (and (cdr pair)
               (funcall reader (find-item-type (cdr pair))))))))

(defun hero-missile-dice (hero)
  "The dice HERO shoots or throws with: the equipped arrows' damage — an
equipped bow beside them strings the pair — or the damage of an
equipped weapon that names a :REACH of its own, the thrown kind, which
needs no bow.  NIL when the hero carries neither.  This is what lets a
back-rank hero attack at all (see HERO-STRIKE-FUNCTION); the shot
carries the dice, the bow may stand in when the arrows name none."
  (%missile-property hero #'item-type-damage))

(defun hero-missile-reach (hero)
  "How far HERO's missile carries, in feet — the arrows' flight, the
weapon's throw — or NIL when the hero has no missile, or the campaign
measured none (which carries however far the fight asks)."
  (%missile-property hero #'item-type-reach))

(defun hero-effective-ac (hero &optional game)
  "HERO's armor class in play: descending AC minus the DEX gift (a
nimble hero is harder to hit; a clumsy one pays nothing, Bard's Tale
style — see STAT-GIFT) and the AC bonus of every equipped item — and,
when GAME is given, minus the party-wide :AC effect bonuses (a spell
shield lowers it further)."
  (let ((ac (- (hero-ac hero) (stat-gift (hero-dex hero)))))
    (dolist (name (hero-equipped hero))
      (decf ac (item-type-ac (find-item-type name))))
    (when game
      (decf ac (effects-ac-bonus game)))
    ac))

;;; ---------------------------------------------------------------------
;;; The pack page (opened from the character sheet with 'i' — the
;;; SHOP-VIEW pattern): the hero's pack as a numbered list, a digit
;;; toggles that item on/off, unfit items carry the (u) marker.
;;; 'p' hands an item to another party member and 'i' inspects one
;;; (the item card), and like SHOP-VIEW's :BUY/:SELL both flows are
;;; modes of the same view rather than pages of their own — the
;;; front-ends already feed every key here, so the whole flow costs
;;; them nothing.

(defstruct (equip-view (:constructor %make-equip-view))
  hero                ; the hero whose pack page this is
  (mode :pack)        ; :pack — equip/remove; :give — pick the item to
                      ; hand over; :to — pick who receives PENDING;
                      ; :inspect — pick the item whose card to show;
                      ; :toss — pick the item to throw away
  pending             ; the item chosen on the :GIVE, :INSPECT or
                      ; :TOSS page, else NIL
  (top 0))            ; scroll offset into the page's own lines

(defun make-equip-view (hero)
  (%make-equip-view :hero hero))

(defun item-card-lines (hero name)
  "The item card for item NAME — its registered facts one per row (the
kind with the (2H) marker, damage dice, the reach of a missile, a
non-zero AC bonus, the price, a class restriction with HERO's (unfit)
marker when it bites, a :USE flagged Usable) and the campaign's
player-facing :DESCRIPTION beneath, when it carries one."
  (let ((type (find-item-type name)))
    (append
     (list (format nil "*** ~A ***" (item-type-title type)) "")
     (list (format nil "Kind: ~A~A"
                   (string-capitalize (string (item-type-kind type)))
                   (item-hand-marker name)))
     (when (item-type-damage type)
       (list (format nil "Damage: ~A" (item-type-damage type))))
     (when (item-type-reach type)
       (list (format nil "Reach: ~D feet" (item-type-reach type))))
     (unless (zerop (item-type-ac type))
       (list (format nil "AC bonus: ~D" (item-type-ac type))))
     ;; a quest piece has no price and no shop — the row would say
     ;; nothing but zero
     (unless (item-type-quest type)
       (list (format nil "Price: ~D gold" (item-type-price type))))
     (when (item-type-classes type)
       ;; the card has the room the list rows lack: the full word
       ;; here, the terse (u) of ITEM-FIT-MARKER on the rows
       (list (format nil "Classes: ~{~A~^, ~}~A"
                     (mapcar (lambda (c)
                               (string-capitalize
                                (substitute #\Space #\- (string c))))
                             (item-type-classes type))
                     (if (item-usable-p hero name) "" " (unfit)"))))
     (when (item-type-use type)
       (list (if (item-type-consumed type)
                 "Usable, consumed on use"
                 "Usable")))
     (when (item-type-quest type)
       (list "A quest piece"))
     (when (item-type-description type)
       (list "" (item-type-description type))))))

(defun %equip-item-rows (hero)
  "The gear as numbered rows, or the one empty-pack row.  The numbers
run over PACK-GEAR — the eight the limit counts, so every row keeps a
single-digit key no matter how the page is scrolled, and EQUIP-ACT
picks by the printed number with no window math.  The quest pieces
are deliberately not here: they answer to none of this page's verbs
and would push the gear past nine (see %EQUIP-QUEST-ROWS).  The star
marks the worn COPY, not the worn name — with a duplicate in the pack
only one row wears it (EQUIPPED-INSTANCE-P), which is why the pairs
carry their pack index."
  (let ((gear (pack-gear hero)))
    (if gear
        (let ((i 0))
          (mapcar (lambda (pair)
                    (incf i)
                    (menu-numbered
                     i (format nil "~D) ~A~:[~;*~]~A~A" i
                               (item-title (cdr pair))
                               (equipped-instance-p hero (cdr pair) (car pair))
                               (item-hand-marker (cdr pair))
                               (item-fit-marker hero (cdr pair)))))
                  gear))
        (list "The pack is empty."))))

(defun %equip-quest-rows (hero)
  "The quest pieces as a document: each one's title and, under it, the
description it was registered with.  No numbers — the page has nothing
to pick, since a plot piece is neither worn, handed over nor thrown
away; it is read."
  (let ((quest (hero-quest-items hero)))
    (if quest
        (loop for name in quest
              append (cons (item-title name)
                           (let ((text (item-type-description
                                        (find-item-type name))))
                             (if text (list text "") (list "")))))
        (list "Nothing of the story yet."))))

(defun %equip-page-lines (game view)
  "The pack page for VIEW as one whole document, scroll ignored — the
window over it is EQUIP-LINES' business.  Six pages share the model,
as in SHOP-LINES: the pack itself (the title, the AC/attack header
showing the effect of every toggle, the items, then the page's own
letter keys and the carousel's NEXT — the sheet turns on to the
spells/songs page or back to the stat block, see HERO-SHEET-LINES),
the give, inspect and throw-away pickers (the same list under the
prompt row that tells the three apart), the recipient page (the
party, each with the room left in their pack), the throw-away
confirmation (the picked item behind a clickable yes/no — the one
pack action that destroys — the pack below it for a second look) and
the quest page (`r`), which is read rather than picked from."
  (let* ((hero (equip-view-hero view))
         (title (format nil "*** ~A's Pack ***" (hero-name hero))))
    (case (equip-view-mode view)
      (:to
       (append
        (list title
              (format nil "Give ~A to whom?"
                      (item-title (equip-view-pending view)))
              "")
        (let ((i 0))
          (mapcar (lambda (h)
                    (incf i)
                    (menu-numbered
                     i (format nil "~D) ~A  (pack ~D/~D)~A" i (hero-name h)
                               (pack-burden h) +inventory-limit+
                               (if (eq h hero) " (giver)" ""))))
                  (game-party game)))))
      (:give
       (list* title "Give what?" "" (%equip-item-rows hero)))
      (:inspect
       (list* title "Inspect what?" "" (%equip-item-rows hero)))
      (:quest
       (list* title "*** Quest pieces ***" "" (%equip-quest-rows hero)))
      (:toss
       (if (equip-view-pending view)
           (append
            (list title
                  (format nil "Throw away ~A?"
                          (item-title (equip-view-pending view)))
                  (menu-option #\y "Yes, be rid of it")
                  (menu-option #\n "No, keep it")
                  "")
            (%equip-item-rows hero))
           (list* title "Throw away what?" "" (%equip-item-rows hero))))
      (t
       (append
        (list title
              (format nil "AC ~D   Attack ~A"
                      (hero-effective-ac hero game) (hero-attack-dice hero))
              "")
        (%equip-item-rows hero)
        (list ""
              (menu-option #\p "Pass an item")
              (menu-option #\i "Inspect an item")
              (menu-option #\t "Throw away an item"))
        ;; the quest key only where there is something to read: a party
        ;; the story has handed nothing yet is offered no page.  'r'
        ;; and not 'q': both front ends take Q for quit before the pack
        ;; page ever sees a key, and this page is read, not picked from
        (when (hero-quest-items hero)
          (list (menu-option
                 #\r (format nil "Read the quest pieces (~D)"
                             (length (hero-quest-items hero))))))
        (list ""
              (menu-next-option)))))))

(defun equip-lines (game view)
  "The pack page as a list of menu lines — the front-ends draw these
verbatim (the SHOP-LINES pattern).  The page (%EQUIP-PAGE-LINES) is
windowed WHOLE at the view's scroll offset over +TAKEOVER-ROWS+ rows:
scrolling turns the one document — a full pack pushes the letter keys
and NEXT onto the second window — rather than sliding the item list
under a fixed head and foot, so what scrolling shows is exactly what
a taller page would, and the item numbers never move.  The item card
(ITEM-CARD-LINES for the item picked on the inspect page) is the one
page of its own."
  (if (and (eq (equip-view-mode view) :inspect)
           (equip-view-pending view))
      (item-card-lines (equip-view-hero view) (equip-view-pending view))
      (menu-scrolled-lines (%equip-page-lines game view)
                           (equip-view-top view)
                           (lambda (i line) (declare (ignore i)) line)
                           +takeover-rows+)))

(defun equip-act (game view char)
  "Apply key CHAR to the pack page.  On the pack itself a digit toggles
that item — the starred worn copy comes off, any other row goes on
(EQUIP-ITEM says why when it cannot; see EQUIPPED-INSTANCE-P for how
duplicates keep the star honest) — p opens the give page, i the
inspect page, t the throw-away page, r the quest page (where the plot
pieces are read; the key is offered only when the hero carries one),
u/d turn a windowed page and Esc
closes it.  On the give page a digit chooses the item to hand over,
and the recipient page then chooses who receives it (PASS-ITEM says
why when it cannot); the page stays open for the next item, the way
the shop's sell page keeps selling.  On the inspect page a digit
shows that item's card, and the page stays open for the next card.
On the throw-away page a digit picks the item and y then destroys it
(DISCARD-ITEM) while n keeps it — the page stays open for the next
item.  A digit always picks the item's printed number — the gear rows
are numbered absolutely, so a scrolled page changes what is visible,
never what a digit means — and every page change opens at its top.
The quest page takes no digit at all: it is a document, u/d and Esc.
Esc steps back one page at a time (SHOP-ACT's pattern).  Returns
:CANCELLED on Esc at the pack page, :NEXT on n there — the sheet
carousel's page turn, the front-end closes the pack and opens the
next page — else NIL."
  (let ((hero (equip-view-hero view))
        (mode (equip-view-mode view))
        (digit (digit-char-p char)))
    (flet ((picked-item ()
             ;; the printed number counts gear rows, so it indexes
             ;; PACK-GEAR and not the pack itself — the quest pieces
             ;; the rows skip must not shift what a digit means
             (when (and digit (plusp digit))
               (cdr (nth (1- digit) (pack-gear hero)))))
           (turn-to (mode)
             (setf (equip-view-mode view) mode
                   (equip-view-top view) 0))
           (scroll ()
             (let ((top (menu-scroll (equip-view-top view) char
                                     (length (%equip-page-lines game view))
                                     +takeover-rows+)))
               (when top (setf (equip-view-top view) top)))))
      (cond
        ;; picking who receives the chosen item
        ((eq mode :to)
         (cond ((and digit (<= 1 digit (length (game-party game))))
                (pass-item game hero (nth (1- digit) (game-party game))
                           (equip-view-pending view))
                ;; back to the give page for the next item
                (setf (equip-view-pending view) nil)
                (turn-to :give)
                nil)
               ((eql char #\Escape)
                (setf (equip-view-pending view) nil)
                (turn-to :give)
                nil)
               (t nil)))
        ;; picking the item to hand over
        ((eq mode :give)
         (cond (digit
                (let ((name (picked-item)))
                  (when name
                    (setf (equip-view-pending view) name)
                    (turn-to :to)))
                nil)
               ((eql char #\Escape)
                (turn-to :pack)
                nil)
               (t (scroll) nil)))
        ;; the are-you-sure, else picking the item to throw away
        ((eq mode :toss)
         (cond ((equip-view-pending view)
                (cond ((member char '(#\y #\Y))
                       (discard-item game hero (equip-view-pending view))
                       ;; the page stays open for the next item
                       (setf (equip-view-pending view) nil)
                       (turn-to :toss))
                      ((or (member char '(#\n #\N)) (eql char #\Escape))
                       (setf (equip-view-pending view) nil)
                       (turn-to :toss))
                      (t (scroll)))
                nil)
               (digit
                (let ((name (picked-item)))
                  (when name
                    (setf (equip-view-pending view) name)
                    ;; the confirmation reads from the page top
                    (turn-to :toss)))
                nil)
               ((eql char #\Escape)
                (turn-to :pack)
                nil)
               (t (scroll) nil)))
        ;; the item card, else picking the item whose card to show
        ((eq mode :inspect)
         (cond ((equip-view-pending view)
                (when (eql char #\Escape)
                  (setf (equip-view-pending view) nil))
                nil)
               (digit
                (let ((name (picked-item)))
                  (when name
                    (setf (equip-view-pending view) name)))
                nil)
               ((eql char #\Escape)
                (turn-to :pack)
                nil)
               (t (scroll) nil)))
        ;; the quest pieces — a document, read and scrolled
        ((eq mode :quest)
         (cond ((eql char #\Escape)
                (turn-to :pack)
                nil)
               (t (scroll) nil)))
        ;; the pack itself
        (digit
         (let ((pair (and (plusp digit) (nth (1- digit) (pack-gear hero)))))
           (when pair
             ;; the starred row (the worn copy) takes the item off; any
             ;; other row — an unworn duplicate included — puts that one
             ;; on, so a digit never silently strips a different row.
             ;; The pack index comes from the pair, not the row number:
             ;; the two part company once a quest piece is carried
             (if (equipped-instance-p hero (cdr pair) (car pair))
                 (toggle-equip game hero (cdr pair))
                 (equip-item game hero (cdr pair)))))
         nil)
        ((member char '(#\p #\P))
         (if (pack-gear hero)
             (turn-to :give)
             (say game "~A has nothing to give." (hero-name hero)))
         nil)
        ((member char '(#\i #\I))
         (if (pack-gear hero)
             (turn-to :inspect)
             (say game "~A has nothing to inspect." (hero-name hero)))
         nil)
        ((member char '(#\r #\R))
         (if (hero-quest-items hero)
             (turn-to :quest)
             (say game "~A carries nothing of the story." (hero-name hero)))
         nil)
        ((member char '(#\t #\T))
         (if (pack-gear hero)
             (turn-to :toss)
             (say game "~A has nothing to throw away." (hero-name hero)))
         nil)
        ((member char '(#\n #\N)) :next)
        ((eql char #\Escape) :cancelled)
        (t (scroll) nil)))))

;;; ---------------------------------------------------------------------
;;; Using items

(defun usable-items (hero)
  "The :USE-carrying items in HERO's pack the hero's class may use —
duplicates kept: two torches are two uses."
  (remove-if-not (lambda (name)
                   (and (item-type-use (find-item-type name))
                        (item-usable-p hero name)))
                 (hero-items hero)))

(defun %use-instant-p (use)
  "True when the validated item :USE spec speaks instant keys (else it
is a timed spec; a spec never mixes the two — DEFINE-ITEM refuses)."
  (loop for tail on use by #'cddr
        thereis (and (assoc (first tail) *instant-effect-keys*) t)))

(defun item-target-kind (name)
  "What using item NAME needs aimed at: :HERO when its effect — or the
spell a (:cast SPELL) use triggers — mends one chosen hero; :DESTINATION
when a (:cast SPELL) use triggers a spell that carries the party
somewhere named; else :NONE."
  (let ((use (item-type-use (find-item-type name))))
    (cond ((null use) :none)
          ((eq (first use) :cast) (spell-target-kind (second use)))
          (t (effect-spec-target-kind use)))))

(defun use-item (game hero name &optional target)
  "HERO uses item NAME (on TARGET, a hero, when the use mends one
chosen hero — defaults to the user).  Says why and returns NIL when
the hero does not carry it, the class cannot use it, it has no use,
or a (:cast SPELL) trigger is a battle spell with nothing to strike —
the item is not spent.  Otherwise applies the :USE — non-battle
instant keys through the spell machinery, a timed spec through
APPLY-EFFECT-SPEC, a (:cast SPELL) trigger as a free cast (no spell
points, no spellbook: the item is the magic) — spends a :CONSUMED
item, emits :ITEM-USED and returns T."
  (let* ((type (find-item-type name))
         (use (item-type-use type)))
    (cond
      ((not (hero-carrying-p hero name))
       (say game "~A does not carry ~A." (hero-name hero)
            (item-type-title type))
       nil)
      ((not (item-usable-p hero name))
       (say game "~A cannot use ~A." (hero-name hero)
            (item-type-title type))
       nil)
      ((null use)
       (say game "Nothing happens.")
       nil)
      (t
       (let ((spell (and (eq (first use) :cast) (second use))))
         (cond
           ((and spell
                 (%spell-strike-blocked-p game (find-spell-type spell)))
            nil)                        ; it said why; the item keeps
           (t
            (say game "~A uses ~A." (hero-name hero)
                 (item-type-title type))
            (cond
              (spell
               (say game "The ~A casts ~A!"
                    (item-type-title type) (spell-title spell))
               (%resolve-spell-cast game hero (find-spell-type spell)
                                    target))
              ((%use-instant-p use)
               (%apply-instant-effects game hero use target))
              (t
               (apply-effect-spec game (item-type-title type) use
                                  :image (item-type-image type))))
            (when (item-type-consumed type)
              (drop-item game hero name))
            (emit game :item-used hero name)
            t)))))))

;;; ---------------------------------------------------------------------
;;; The use interaction model (shared by both front-ends — the
;;; CAST-VIEW pattern: pick the user, the item, and — for a healing
;;; item — the target).

(defstruct (use-view (:constructor %make-use-view))
  hero                ; the chosen user, or NIL while picking
  item                ; the chosen item name, or NIL while picking
  in-combat           ; T: committing fights one COMBAT-ROUND;
                      ; :ORDERS: committing returns the pick as a
                      ; round action (the combat-orders flow)
  (top 0))            ; scroll offset into the item list

(defun make-use-view (&key in-combat hero)
  "HERO presets the user (the combat-orders flow asks for one hero's
pick); NIL starts at the who-uses page."
  (%make-use-view :in-combat in-combat :hero hero))

(defun %use-commit (game view target)
  "Resolve the completed pick: use directly; in combat fight one round
where the user uses the item and everyone else attacks; in :ORDERS
mode hand the pick back as (:ACTION (:USE ITEM [TARGET]))."
  (let ((hero (use-view-hero view))
        (item (use-view-item view)))
    (cond
      ((eq (use-view-in-combat view) :orders)
       (list :action (if target
                         (list :use item target)
                         (list :use item))))
      ((use-view-in-combat view)
       (combat-round game
                     (mapcar (lambda (h)
                               (if (eq h hero)
                                   (list :use item target)
                                   :attack))
                             (alive-heroes game)))
       :done)
      (t (use-item game hero item target)
         :done))))

(defun use-lines (game view)
  "The current use menu as a list of menu lines — the front-ends draw
these verbatim (the SHOP-LINES pattern); option rows carry their pick
key (see MENU-NUMBERED)."
  (let ((hero (use-view-hero view))
        (item (use-view-item view)))
    (append
     (list "*** Use an Item ***" "")
     (cond
       ((null hero)
        (append
         (list "Who uses?" "")
         (let ((i 0))
           (mapcar (lambda (h)
                     (incf i)
                     (menu-numbered
                      i (format nil "~D) ~A  (~D usable)"
                                i (hero-name h)
                                (length (usable-items h)))))
                   (game-party game)))))
       ((null item)
        (append
         (list (format nil "~A uses." (hero-name hero)) "")
         (menu-scrolled-lines
          (usable-items hero) (use-view-top view)
          (lambda (i name)
            (menu-numbered
             i (format nil "~D) ~A" i (item-title name)))))))
       ((eq (item-target-kind item) :destination)
        (destination-rows (format nil "~A -- where to?" (item-title item))))
       (t                              ; a mending item picks its target
        (append
         (list (format nil "~A on whom?" (item-title item)) "")
         (let ((i 0))
           (mapcar (lambda (h)
                     (incf i)
                     (menu-numbered
                      i (format nil "~D) ~A  (HP ~D/~D)"
                                i (hero-name h)
                                (hero-hp h) (hero-max-hp h))))
                   (game-party game)))))))))

(defun use-act (game view char)
  "Apply key CHAR to the use menu.  Returns :DONE when a use resolved
(the front-end drops the view) — in :ORDERS mode
\(:ACTION (:USE ...)) instead — :CANCELLED on Esc at the top level,
else NIL."
  (let ((hero (use-view-hero view))
        (item (use-view-item view))
        (digit (digit-char-p char)))
    (cond
      ;; picking the user
      ((null hero)
       (cond ((and digit (<= 1 digit (length (game-party game))))
              (let ((h (nth (1- digit) (game-party game))))
                (when (and (hero-alive-p h) (usable-items h))
                  (setf (use-view-hero view) h)))
              nil)
             ((eql char #\Escape) :cancelled)
             (t nil)))
      ;; picking the item
      ((null item)
       (cond (digit
              (let ((name (menu-window-pick (usable-items hero)
                                            (use-view-top view) digit)))
                (when name
                  (setf (use-view-item view) name)
                  ;; a mender picks its target next, a homing item its
                  ;; destination; anything else goes off at once
                  (if (member (item-target-kind name) '(:hero :destination))
                      nil
                      (%use-commit game view nil)))))
             ((eql char #\Escape)
              (setf (use-view-hero view) nil
                    (use-view-top view) 0)
              nil)
             (t
              (let ((top (menu-scroll (use-view-top view) char
                                      (length (usable-items hero)))))
                (when top (setf (use-view-top view) top)))
              nil)))
      ;; picking where the item's flight lands
      ((eq (item-target-kind item) :destination)
       (cond ((destination-by-digit digit)
              (%use-commit game view (destination-by-digit digit)))
             ((eql char #\Escape)
              (setf (use-view-item view) nil)
              nil)
             (t nil)))
      ;; picking the heal target
      (t
       (cond ((and digit (<= 1 digit (length (game-party game))))
              (%use-commit game view (nth (1- digit) (game-party game))))
             ((eql char #\Escape)
              (setf (use-view-item view) nil)
              nil)
             (t nil))))))
