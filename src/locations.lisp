;;; Lambda's Tale — locations: shops and other enterable places.
;;;
;;; A location is map data: the special op
;;;     (location TITLE KIND ARG...)
;;; attached to a cell (a building's door square in a city, a hut in a
;;; dungeon).  Stepping onto the cell enters the location: the game
;;; gains a modal LOCATION state (like combat), the front-ends switch to
;;; its menu.  KIND is an open set; the engine ships mechanics for
;;; :SHOP (ARG... is :stock (ITEM-NAME...); stock is unlimited, Bard's
;;; Tale style), :TAVERN (drinks, and maybe a trapdoor), :TEMPLE
;;; (healing and raising the fallen, for gold) and :ENERGY (spell
;;; points at so many gold apiece).  Unknown kinds still enter/leave
;;; cleanly — campaigns script them via the :ENTER-LOCATION event.
;;;
;;; Any kind may keep hours: :CLOSED names the day-band — :NIGHT, or a
;;; list like (:EVENING :NIGHT), see TIME-OF-DAY — during which the
;;; door will not open.  An entering step is told and bounced back
;;; onto the street facing the shut door (see ENTER-LOCATION); the
;;; location op stays top-level map data, so the street facade still
;;; shows (a closed shop keeps its face, unlike one hidden behind an
;;; AT-DAY op, which CELL-LOCATION-OP cannot know).
;;;
;;; The interaction itself is modeled here too, platform-free: a
;;; SHOP-VIEW holds the UI state (which hero is shopping, buy or sell
;;; page), SHOP-LINES renders the menu as text lines and SHOP-ACT maps
;;; a key press to mechanics — both front-ends drive the same model, so
;;; the whole shop flow is testable on the host without a UI.

(in-package :tale)

(defstruct (location (:constructor %make-location))
  title               ; display string, e.g. "Garth's Equipment Shoppe"
  kind                ; keyword: :shop, ...
  args                ; remaining plist, e.g. (:stock (short-sword ...))
  entry-dir)          ; direction index the party stepped in by, or NIL
                      ; (entered without a step) — see LEAVE-LOCATION

(defun location-arg (location key)
  (getf (location-args location) key))

(defun %closed-bands (location)
  "LOCATION's :CLOSED day-bands as a list — the arg accepts one band
keyword or a list of them; NIL when the location keeps no hours."
  (let ((closed (location-arg location :closed)))
    (if (listp closed) closed (list closed))))

(defun location-closed-p (location game)
  "True when LOCATION keeps hours and GAME's clock falls in them: the
:CLOSED arg names the day-band or bands (see TIME-OF-DAY) during
which the door will not open, e.g. :NIGHT or (:EVENING :NIGHT)."
  (member (game-time-of-day game) (%closed-bands location)))

(defun location-image (location)
  "LOCATION's picture file name — the :IMAGE arg of the location op —
or NIL.  The Amiga front-end shows it in the view column while the
location's menu takes over the message area.  The street face is the
:FACADE arg, shown before the party steps in — see
FACING-LOCATION-IMAGE-PATH."
  (location-arg location :image))

(defun location-image-path (game)
  "The current location's picture file resolved like an effect icon —
relative to the current map file's directory, so a self-contained
world directory carries its own art — or NIL: no location, or it
names no :IMAGE."
  (let* ((loc (game-location game))
         (image (and loc (location-image loc))))
    (when image
      (%resolve-map-path (dungeon-map-name (game-map game)) image))))

(defun cell-location-op (map x y)
  "The (TITLE KIND ARG...) tail of the LOCATION op attached to cell
\(X,Y), or NIL.  Only a top-level op counts — a location hidden behind
ONCE or a flag test is not knowable map data."
  (dolist (op (cell-special map x y))
    (when (and (consp op) (symbolp (first op))
               (string-equal (symbol-name (first op)) "LOCATION"))
      (return (rest op)))))

(defun facing-location-image-path (game)
  "The facade the party stands before: when the wall straight ahead is
a door and the cell beyond holds a LOCATION with a picture, that
picture's path resolved like LOCATION-IMAGE-PATH — else NIL.  :FACADE
names the street face and wins over :IMAGE (the picture shown inside,
see LOCATION-IMAGE-PATH); a location with only an :IMAGE shows that
from the street too.  The Amiga front-end shows it in the view
column, so a city street reads as houses with faces, not one long
grey wall (the Bard's Tale building-front look).  The wall directly
ahead is visible even in the dark (GAME-VIEW-DEPTH is never less than
one cell)."
  (let* ((map (game-map game))
         (x (game-x game))
         (y (game-y game))
         (f (game-facing game)))
    (when (eq (cell-wall map x y f) :door)
      (multiple-value-bind (nx ny) (neighbor map x y f)
        (when nx
          (let* ((loc (cell-location-op map nx ny))
                 (args (cddr loc))
                 (image (and loc (or (getf args :facade)
                                     (getf args :image)))))
            (when image
              (%resolve-map-path (dungeon-map-name map) image))))))))

(defun %step-out (game out facing advance)
  "Step the party through its cell's OUT side onto the neighbouring
cell when that wall is passable, left facing FACING — the exit step
shared by LEAVE-LOCATION (facing away from the door, clock ticking)
and a closed door's bounce (facing the shut door, free: ADVANCE NIL
skips the clock).  Never re-triggers the destination cell's special —
the party just came from there."
  (when (wall-passable-p (cell-wall (game-map game)
                                    (game-x game) (game-y game) out))
    (multiple-value-bind (nx ny)
        (neighbor (game-map game) (game-x game) (game-y game) out)
      (when nx
        (setf (game-x game) nx
              (game-y game) ny
              (game-facing game) facing)
        (when advance (advance-time game))
        (observe game)
        (emit game :enter-cell nx ny)))))

(defun enter-location (game spec)
  "Enter the location described by SPEC = (TITLE KIND ARG...) — the
LOCATION special op calls this.  Sets the game's modal location state
and emits :ENTER-LOCATION.  A location closed at this hour (see
LOCATION-CLOSED-P) is not entered: the party is told, an entering
step is bounced back onto the street before the shut door — facing
it, at no clock cost beyond the step already taken — :LOCATION-CLOSED
fires and NIL comes back."
  (destructuring-bind (title kind &rest args) spec
    (unless (stringp title)
      (error "location: title ~S must be a string" title))
    (unless (keywordp kind)
      (error "location ~S: kind ~S must be a keyword (:shop, ...)"
             title kind))
    (when (game-location game)
      (error "enter-location: already inside ~A"
             (location-title (game-location game))))
    (let ((loc (%make-location :title title :kind kind :args args
                               :entry-dir *step-dir*)))
      (when (eq kind :shop)
        (dolist (name (location-arg loc :stock))
          (find-item-type name)))   ; catch bad stock at entry, loudly
      (dolist (band (%closed-bands loc))
        (unless (assoc band *time-band-starts*)
          (error "location ~S: unknown day-band ~S in :closed"
                 title band)))     ; bad hours caught just as loudly
      (when (location-closed-p loc game)
        (say game "~A is closed." title)
        (let ((entry (location-entry-dir loc)))
          (when entry
            (%step-out game (dir-opposite entry) entry nil)))
        (emit game :location-closed loc)
        (return-from enter-location nil))
      (setf (game-location game) loc)
      ;; no log line — the location's own page names it; routine
      ;; comings and goings would only silt up the log
      (emit game :enter-location loc)
      loc)))

(defun leave-location (game)
  "Leave the current location.  When the party stepped in through a
door (the location's ENTRY-DIR), it steps back out onto the cell it
came from, facing away from the door — the Bard's Tale exit: leaving
a house puts you on the street before its front, not standing in the
doorway looking at the hearth.  The exit step costs time and maps
like any step, but does NOT re-trigger the street cell's special —
the party just came from there.  A location entered without a step
(TRAVEL, a script) leaves the party where it stands.  Emits
:LEAVE-LOCATION."
  (let ((loc (game-location game)))
    (when loc
      (setf (game-location game) nil)
      ;; as quiet as entering — the view coming back says it all
      (let ((entry (location-entry-dir loc)))
        (when entry
          (let ((out (dir-opposite entry)))
            (%step-out game out out t))))
      (emit game :leave-location loc))
    loc))

;;; ---------------------------------------------------------------------
;;; Shop mechanics

(defun item-price (name)
  (item-type-price (find-item-type name)))

(defun item-sell-price (name)
  "Shops buy back at half price."
  (floor (item-price name) 2))

(defun shop-stock (location)
  (location-arg location :stock))

(defun buy-item (game hero name)
  "HERO buys item NAME: checks gold and pack space, then pays and takes
it.  Freshly bought equipment is auto-equipped when the hero has nothing
of that kind equipped and can use it.  Returns T on success."
  (let ((type (find-item-type name)))
    (cond ((< (hero-gold hero) (item-type-price type))
           (say game "~A cannot afford ~A." (hero-name hero)
                (item-type-title type))
           nil)
          ((>= (length (hero-items hero)) +inventory-limit+)
           (say game "~A's pack is full." (hero-name hero))
           nil)
          (t
           (decf (hero-gold hero) (item-type-price type))
           (give-item game hero name)
           (emit game :coin (item-type-price type))
           (say game "~A buys ~A for ~D gold." (hero-name hero)
                (item-type-title type) (item-type-price type))
           (when (and (member (item-type-kind type)
                              '(:weapon :armor :shield))
                      (not (equipped-of-kind hero (item-type-kind type)))
                      (item-usable-p hero name))
             (equip-item game hero name))
           t))))

(defun sell-item (game hero name)
  "HERO sells item NAME back to the shop at half price.  Returns T, or
says why and returns NIL when the hero does not carry it."
  (if (not (hero-carrying-p hero name))
      (progn
        (say game "~A does not carry ~A." (hero-name hero) (item-title name))
        nil)
      (let ((price (item-sell-price name)))
        (drop-item game hero name)
        (incf (hero-gold hero) price)
        (emit game :coin price)
        (say game "~A sells ~A for ~D gold." (hero-name hero)
             (item-title name) price)
        t)))

;;; ---------------------------------------------------------------------
;;; The shop interaction model (shared by both front-ends).

(defstruct (shop-view (:constructor %make-shop-view))
  hero                ; the shopping HERO, or NIL while picking one
  (mode :buy)         ; :buy or :sell page, or :inspect — pick the
                      ; stock item whose card to show before buying
  pending             ; the item chosen on the :INSPECT page, else NIL
  (top 0))            ; scroll offset into the stock/pack list

(defun make-shop-view ()
  (%make-shop-view))

(defun shop-lines (game view)
  "The current shop menu as a list of menu lines — the front-ends draw
these verbatim (the same pattern as HERO-SUMMARY-LINES); option rows
carry their pick key (see MENU-NUMBERED).  The buy page's 'i' opens
the inspect page — the same stock, a digit showing that item's card
(ITEM-CARD-LINES) before any gold is spent."
  (let* ((loc (game-location game))
         (hero (shop-view-hero view)))
    (if (and (eq (shop-view-mode view) :inspect)
             (shop-view-pending view))
        (item-card-lines hero (shop-view-pending view))
        (append
         (list (format nil "*** ~A ***" (location-title loc)) "")
         (cond
           ((null hero)
            ;; the roster pane already lists the party, so the pick
            ;; page is a bare prompt naming the digit range — the
            ;; temple-lines pattern (the Amiga front-end lets the
            ;; roster rows click as their digits here)
            (let ((n (length (game-party game))))
              (list (format nil "Who is shopping?  (1~@[-~D~])"
                            (when (> n 1) n)))))
           ((member (shop-view-mode view) '(:buy :inspect))
            (append
             (list (format nil "~A ~A.  Gold: ~D gp"
                           (hero-name hero)
                           (if (eq (shop-view-mode view) :buy)
                               "buys" "browses")
                           (hero-gold hero))
                   "")
             (menu-scrolled-lines
              (shop-stock loc) (shop-view-top view)
              (lambda (i name)
                (menu-numbered i (format nil "~D) ~A~A~A  ~D gp"
                                         i (item-title name)
                                         (item-hand-marker name)
                                         (item-fit-marker hero name)
                                         (item-price name)))))
             ;; the page-specific keys stay (first letter picks, and
             ;; the option row clicks as its key); the digit pick and
             ;; Esc are common knowledge — the help screen's business.
             ;; They ride the first window only: a scrolled window
             ;; gives its rows to the stock (the keys keep working,
             ;; and u brings their rows back)
             (when (and (eq (shop-view-mode view) :buy)
                        (zerop (shop-view-top view)))
               (list ""
                     (menu-option #\s "Sell")
                     (menu-option #\i "Inspect")
                     (menu-option #\p "Pool gold")))))
           (t
            (append
             (list (format nil "~A sells.  Gold: ~D gp"
                           (hero-name hero) (hero-gold hero))
                   "")
             (menu-scrolled-lines
              (hero-items hero) (shop-view-top view)
              (lambda (i name)
                (menu-numbered i (format nil "~D) ~A~:[~;*~]~A~A  ~D gp"
                                         i (item-title name)
                                         (member name (hero-equipped hero))
                                         (item-hand-marker name)
                                         (item-fit-marker hero name)
                                         (item-sell-price name)))))
             (when (zerop (shop-view-top view))
               (list ""
                     (menu-option #\b "Buy")
                     (menu-option #\p "Pool gold"))))))))))

(defun shop-act (game view char)
  "Apply key CHAR to the shop interaction.  Digits pick within the
visible stock/pack window, u/d scroll it (see MENU-WINDOW), p pools
the party's gold onto the shopper (see POOL-GOLD), i on the buy page
opens the inspect page — a digit shows that item's card, and Esc from
the card lands straight back on the buy page (inspecting is a
one-card detour: the next digit buys again); Esc on the pick page
returns to the buy page too.  Mutates VIEW and the game state; returns
:LEFT when the party leaves the location (the front-end drops its
view then), else NIL."
  (let ((loc (game-location game))
        (hero (shop-view-hero view))
        (digit (digit-char-p char)))
    (cond
      ((null hero)
       (cond ((and digit (<= 1 digit (length (game-party game))))
              (setf (shop-view-hero view)
                    (nth (1- digit) (game-party game)))
              nil)
             ((member char '(#\Escape #\l #\L #\q #\Q))
              (leave-location game)
              :left)
             (t nil)))
      ;; the item card, else picking the stock item whose card to show
      ;; — before the page-leaving Esc, so Esc steps back one page
      ((eq (shop-view-mode view) :inspect)
       (cond ((shop-view-pending view)
              ;; closing the card ends the whole detour: back to
              ;; buying, so the next digit spends gold, not curiosity
              (when (eql char #\Escape)
                (setf (shop-view-pending view) nil
                      (shop-view-mode view) :buy))
              nil)
             (digit
              (let ((name (menu-window-pick (shop-stock loc)
                                            (shop-view-top view) digit)))
                (when name
                  (setf (shop-view-pending view) name)))
              nil)
             ((eql char #\Escape)
              (setf (shop-view-mode view) :buy)
              nil)
             (t
              (let ((top (menu-scroll (shop-view-top view) char
                                      (length (shop-stock loc)))))
                (when top (setf (shop-view-top view) top)))
              nil)))
      ((member char '(#\Escape))
       (setf (shop-view-hero view) nil
             (shop-view-mode view) :buy
             (shop-view-top view) 0)
       nil)
      ((eq (shop-view-mode view) :buy)
       (cond (digit
              (let ((name (menu-window-pick (shop-stock loc)
                                            (shop-view-top view) digit)))
                (when name
                  (buy-item game hero name)))
              nil)
             ((member char '(#\s #\S))
              (setf (shop-view-mode view) :sell
                    (shop-view-top view) 0)
              nil)
             ((member char '(#\i #\I))
              (setf (shop-view-mode view) :inspect)
              nil)
             ((member char '(#\p #\P))
              (pool-gold game hero)
              nil)
             (t
              (let ((top (menu-scroll (shop-view-top view) char
                                      (length (shop-stock loc)))))
                (when top (setf (shop-view-top view) top)))
              nil)))
      (t
       (cond (digit
              (let ((name (menu-window-pick (hero-items hero)
                                            (shop-view-top view) digit)))
                (when name
                  (sell-item game hero name)))
              nil)
             ((member char '(#\b #\B))
              (setf (shop-view-mode view) :buy
                    (shop-view-top view) 0)
              nil)
             ((member char '(#\p #\P))
              (pool-gold game hero)
              nil)
             (t
              (let ((top (menu-scroll (shop-view-top view) char
                                      (length (hero-items hero)))))
                (when top (setf (shop-view-top view) top)))
              nil))))))

;;; ---------------------------------------------------------------------
;;; Tavern mechanics: the singers' watering hole.  A :TAVERN location
;;; sells drinks — a drink refills a singer's tunes (see songs.lisp) —
;;; and may hold the way down: (location TITLE :tavern :price N
;;; :down FILE) travels to FILE through the trapdoor, Bard's Tale
;;; cellar style.

(defun tavern-price (location)
  "The price of a drink at LOCATION (:PRICE, default 3 gold)."
  (or (location-arg location :price) 3))

(defun buy-drink (game hero)
  "HERO buys a round at the current tavern: pays the price, and — for
a singer — the tunes come flooding back (TUNES refilled to the
maximum).  Returns T, or says why not and returns NIL."
  (let ((price (tavern-price (game-location game))))
    (cond ((< (hero-gold hero) price)
           (say game "~A cannot afford a drink." (hero-name hero))
           nil)
          (t
           (decf (hero-gold hero) price)
           (if (hero-singer-p hero)
               (progn
                 (setf (hero-tunes hero) (hero-max-tunes hero))
                 (say game "~A drinks; the tunes come flooding back."
                      (hero-name hero)))
               (say game "~A enjoys a fine ale." (hero-name hero)))
           (emit game :drink hero)
           t))))

(defun tavern-lines (game)
  "The tavern menu as text lines: one numbered row per party member,
the drink price, and the trapdoor when the tavern has one."
  (let ((loc (game-location game)))
    (append
     (list (format nil "*** ~A ***" (location-title loc)) ""
           (format nil "A drink is ~D gold." (tavern-price loc)) "")
     (let ((i 0))
       (mapcar (lambda (h)
                 (incf i)
                 (menu-numbered
                  i (format nil "~D) ~A  (~D gp~@[, Tunes ~A~])"
                            i (hero-name h) (hero-gold h)
                            (when (hero-singer-p h)
                              (format nil "~D/~D" (hero-tunes h)
                                      (hero-max-tunes h))))))
               (game-party game)))
     ;; the trapdoor row stays — it is world knowledge, not a key
     ;; reference (those live on the help screen)
     (when (location-arg loc :down)
       (list "" (menu-option #\d "Down the trapdoor"))))))

(defun tavern-act (game char)
  "Apply key CHAR to the tavern menu: a digit buys that hero a drink,
'd' takes the trapdoor down when the tavern has one, Esc leaves.
Returns :LEFT when the party leaves the location, else NIL."
  (let* ((loc (game-location game))
         (down (location-arg loc :down))
         (digit (digit-char-p char)))
    (cond ((and digit (<= 1 digit (length (game-party game))))
           (let ((h (nth (1- digit) (game-party game))))
             (when (hero-alive-p h)
               (buy-drink game h)))
           nil)
          ((and down (member char '(#\d #\D)))
           (leave-location game)
           (say game "Down through the trapdoor.")
           (travel-party game down)
           :left)
          ((member char '(#\Escape #\l #\L #\q #\Q))
           (leave-location game)
           :left)
          (t nil))))

;;; ---------------------------------------------------------------------
;;; Temple mechanics: healers for hire.  A :TEMPLE location heals a
;;; hero for gold — so many gold per missing hit point (:PRICE) plus a
;;; :RAISE fee to bring a fallen hero back to their feet, the Bard's
;;; Tale temple.  The raising fee may scale with the patient: a flat
;;; :RAISE plus :RAISE-PER-HP for every one of their full hit points,
;;; so bringing back a seasoned hero costs what their life is worth.
;;;
;;; A raising buys life, not health: the fee returns the hero at a
;;; SINGLE hit point, and every hit point above that is a wound like
;;; any other, bought at :PRICE.  Making the fallen whole therefore
;;; costs the fee plus the rest of their health, and a purse that
;;; covers only the fee still gets them standing.
;;;
;;; The menu asks twice: who wishes healing, then
;;; who will pay — any purse in the party may cover any patient (a
;;; fallen hero's own included; the purse survives its owner).  A
;;; purse short of the whole job still buys what it can, wound by
;;; wound; one too short for any healing at all leaves the temple's
;;; Not enough Gold notice instead.

(defun temple-price (location)
  "LOCATION's healing rate, gold per missing hit point (:PRICE,
default 2)."
  (or (location-arg location :price) 2))

(defun temple-raise-base (location)
  "The flat part of LOCATION's fee for raising a fallen hero (:RAISE,
default 50)."
  (or (location-arg location :raise) 50))

(defun temple-raise-rate (location)
  "The scaling part of LOCATION's raising fee, gold per full hit point
of the hero raised (:RAISE-PER-HP, default 0 — a flat fee, no
scaling)."
  (or (location-arg location :raise-per-hp) 0))

(defun temple-raise-fee (location hero)
  "What LOCATION's priests ask to raise HERO from the dead: the flat
TEMPLE-RAISE-BASE plus TEMPLE-RAISE-RATE for each of HERO's full hit
points.  It buys the life alone — HERO comes back at one hit point —
so the health above that is bought as wounds at the healing rate."
  (+ (temple-raise-base location)
     (* (temple-raise-rate location) (hero-max-hp hero))))

(defun temple-cures (location)
  "LOCATION's cure price table — ((AILMENT PRICE) ...) from :CURES, or
NIL when this house treats no ailments at all (the engine's default: a
temple mends flesh and raises the dead, and a campaign says which
conditions its priests know how to lift).  Signals an error on a
malformed table, so a campaign learns of a typo the first time the
party walks in."
  (let ((cures (location-arg location :cures)))
    (dolist (entry cures)
      (unless (and (consp entry) (null (cddr entry))
                   (ailment-p (first entry))
                   (integerp (second entry))
                   (not (minusp (second entry))))
        (error "~A: :cures wants (AILMENT PRICE) entries over ~S, got ~S"
               (location-title location) *ailments* entry)))
    cures))

(defun temple-cure-price (location ailment)
  "What LOCATION's priests ask to lift AILMENT, or NIL when they do not
treat it — an untreated condition is carried out of the temple however
much gold the party offers, and some other house's priests (or a spell)
must lift it."
  (second (assoc ailment (temple-cures location))))

(defun temple-curable (location hero)
  "The work LOCATION's priests can do on HERO's conditions, as
((AILMENT PRICE) ...) in *AILMENTS* order: every ailment HERO carries
that this house treats.  NIL for a hero carrying nothing, or nothing
these priests know."
  (let ((work '()))
    (dolist (a *ailments* (nreverse work))
      (let ((price (and (hero-ailment-p hero a)
                        (temple-cure-price location a))))
        (when price
          (push (list a price) work))))))

(defun temple-work-p (location hero)
  "Have LOCATION's priests anything to sell for HERO — wounds to mend
(the fallen always have some), or a condition they treat?  The menu's
rows and its patient pick ask this one question, so a hero who earns a
row can be picked and a hero who cannot be picked earns none."
  (and (or (< (hero-hp hero) (hero-max-hp hero))
           (temple-curable location hero))
       t))

(defun %temple-turn-away (game hero)
  "Say why the priests will not take HERO: nothing the matter at
all, or nothing they know how to lift."
  (if (hero-ailments hero)
      (say game "These priests cannot lift what ~A carries."
           (hero-name hero))
      (say game "~A needs no healing." (hero-name hero))))

(defun temple-wounds (hero)
  "The hit points HERO's priests can sell: the missing ones, counted
from where the patient stands once the priests are done raising them —
a hero comes back from the dead at one hit point, so a fallen hero's
first hit point rides with the fee and is not a wound."
  (- (hero-max-hp hero) (max 1 (hero-hp hero))))

(defun temple-cost (location hero)
  "What LOCATION's priests ask to make HERO whole: the raise fee when
HERO is down, every condition these priests treat (TEMPLE-CURABLE), and
the healing rate over the wounds (TEMPLE-WOUNDS).  Zero for a hale
hero, and a condition this house does not treat is not in it."
  (+ (if (hero-alive-p hero) 0 (temple-raise-fee location hero))
     (let ((sum 0))
       (dolist (work (temple-curable location hero) sum)
         (incf sum (second work))))
     (* (temple-price location) (temple-wounds hero))))

(defstruct (temple-view (:constructor %make-temple-view))
  patient             ; the hero the priests will heal, or NIL while
                      ; the menu asks who wishes healing
  note)               ; a notice for the menu's last line ("Not enough
                      ; Gold"), or NIL; cleared by the next key

(defun make-temple-view ()
  (%make-temple-view))

(defun temple-heal (game hero payer)
  "The priests work on HERO out of PAYER's purse, and in this order: the
raising first, then every condition they treat, then the wounds with
whatever is left.  A raising comes with one hit point and no more; each
cure is bought whole or not at all — a purse short of the poison's price
skips the poison and spends on what it can reach — and the remaining
gold buys wounds one at a time.  Returns T and emits :COIN and
:TEMPLE-HEAL when anything was bought; otherwise returns (VALUES NIL
REASON) — :HALE when there is nothing the matter with HERO that these
priests could mend (said aloud), :POOR when PAYER's purse does not
reach the cheapest thing they could do (quiet: the temple menu shows
the notice, see TEMPLE-LINES)."
  (let* ((loc (game-location game))
         (price (temple-price loc))
         (down (not (hero-alive-p hero)))
         (fee (if down (temple-raise-fee loc hero) 0))
         (wounds (temple-wounds hero))
         (curable (temple-curable loc hero))
         ;; the cheapest thing on offer for this patient: no purse under
         ;; it buys anything at all.  For the fallen that is the fee and
         ;; only the fee — there is no cleansing or mending a corpse, so
         ;; the raising gates the whole visit.
         (cheapest (if down
                       fee
                       (let ((prices (append (mapcar #'second curable)
                                             (when (plusp wounds)
                                               (list price)))))
                         (and prices (reduce #'min prices))))))
    (cond ((null cheapest)
           ;; nothing to sell — either a hale hero, or one whose only
           ;; trouble is a condition this house does not treat
           (%temple-turn-away game hero)
           (values nil :hale))
          ((< (hero-gold payer) cheapest)
           (values nil :poor))
          (t
           (let ((purse (hero-gold payer))
                 (cost 0)
                 (lifted '()))
             (when down                  ; raised at a single hit point
               (decf purse fee)
               (incf cost fee)
               (setf (hero-hp hero) 1))
             (dolist (work curable)      ; each cure whole or not at all
               (when (<= (second work) purse)
                 (decf purse (second work))
                 (incf cost (second work))
                 (cure-ailment game hero (first work))
                 (push (first work) lifted)))
             (let* ((bought (min wounds
                                 (if (zerop price)
                                     wounds
                                     (floor purse price))))
                    (whole (and (= bought wounds)
                                (= (length lifted) (length curable)))))
               (incf cost (* bought price))
               (incf (hero-hp hero) bought)
               (decf (hero-gold payer) cost)
               (emit game :coin cost)
               (when lifted
                 (say game "~A is cleansed of ~{~A~^ and ~}."
                      (hero-name hero)
                      (mapcar #'ailment-noun (nreverse lifted))))
               (cond ((and down whole)
                      (say game "The priests chant, and ~A rises, whole ~
                                 again!  (~D gold)"
                           (hero-name hero) cost))
                     (down
                      (say game "The priests chant, and ~A rises — ~
                                 wounds remain.  (~D gold)"
                           (hero-name hero) cost))
                     ((zerop bought)     ; a cleansing and nothing else
                      (say game "The priests ask ~D gold for it." cost))
                     (whole
                      (say game "The priests mend ~A's wounds.  (~D gold)"
                           (hero-name hero) cost))
                     (t
                      (say game "The priests mend some of ~A's wounds.  ~
                                 (~D gold)"
                           (hero-name hero) cost)))
               (emit game :temple-heal hero)
               t))))))

(defun temple-lines (game view)
  "The temple menu as menu lines, two pages riding VIEW (a
TEMPLE-VIEW).  First the rates and — Who wishes healing? — one
numbered row per party member the priests can help; the roster pane
already shows everyone's health and purse, so a hale hero earns no
row, and the number is the hero's roster slot (the digit TEMPLE-ACT
expects), so a row is skipped, never renumbered.  With the patient
chosen: Who will pay? over one row per party member and their purse.
The view's NOTE (the payer page's Not enough Gold) is the last line."
  (let* ((loc (game-location game))
         (patient (and view (temple-view-patient view)))
         (note (and view (temple-view-note view))))
    (append
     (list (format nil "*** ~A ***" (location-title loc)) ""
           (format nil "Healing ~D gold a wound." (temple-price loc))
           ;; a scaling raising cannot be quoted as one number — the
           ;; rate stands in for it, and each row carries the hero's own
           (let ((rate (temple-raise-rate loc))
                 (base (temple-raise-base loc)))
             (cond ((zerop rate)
                    (format nil "Raising the fallen ~D gold." base))
                   ((zerop base)
                    (format nil "Raising the fallen ~D gold a hit point."
                            rate))
                   (t
                    (format nil "Raising the fallen ~D a hit point plus ~D."
                            rate base))))
           "")
     (if patient
         (append
          (list (format nil "Who will pay ~A's ~D gold?"
                        (hero-name patient) (temple-cost loc patient))
                "")
          (let ((i 0))
            (mapcar (lambda (h)
                      (incf i)
                      (menu-numbered
                       i (format nil "~D) ~A  ~D gp"
                                 i (hero-name h) (hero-gold h))))
                    (game-party game))))
         (let ((i 0)
               (rows '()))
           (dolist (h (game-party game))
             (incf i)
             ;; a hero earns a row when these priests have work to do —
             ;; wounds, or a condition they treat (TEMPLE-WORK-P, the
             ;; same question the patient pick asks)
             (when (temple-work-p loc h)
               (push (menu-numbered
                      i (format nil "~D) ~A~@[ (~A)~]  costs ~D"
                                i (hero-name h) (hero-condition-titles h)
                                (temple-cost loc h)))
                     rows)))
           (if rows
               (cons "Who wishes healing?" (nreverse rows))
               (list "No one here needs the priests."))))
     (when note (list "" note)))))

(defun temple-act (game view char)
  "Apply key CHAR to the temple menu (VIEW is its TEMPLE-VIEW): on
the first page a digit picks the patient (the fallen very much
included — that is what temples are for), on the payer page a digit
picks whose purse pays — a short purse buys what it can, and one too
short for any healing leaves the Not enough Gold notice.  Esc backs
off the payer page, else leaves.  Returns :LEFT when the party
leaves the location, else NIL."
  (let ((digit (digit-char-p char))
        (patient (and view (temple-view-patient view))))
    (when view (setf (temple-view-note view) nil))
    (cond ((and view digit (<= 1 digit (length (game-party game))))
           (let ((h (nth (1- digit) (game-party game))))
             (if patient
                 (multiple-value-bind (healed reason)
                     (temple-heal game patient h)
                   (declare (ignore healed))
                   (if (eq reason :poor)
                       (setf (temple-view-note view) "Not enough Gold")
                       ;; healed — or hale after another purse already
                       ;; paid: back to picking a patient either way
                       (setf (temple-view-patient view) nil)))
                 ;; work, not cost, makes a patient — a free shrine
                 ;; (:price 0) heals too, and a hale hero carrying a
                 ;; condition these priests treat is as much a patient
                 ;; as a wounded one
                 (if (temple-work-p (game-location game) h)
                     (setf (temple-view-patient view) h)
                     (%temple-turn-away game h))))
           nil)
          ((eql char #\Escape)
           (cond (patient
                  (setf (temple-view-patient view) nil)
                  nil)
                 (t
                  (leave-location game)
                  :left)))
          ((and (null patient) (member char '(#\l #\L #\q #\Q)))
           (leave-location game)
           :left)
          (t nil))))

;;; ---------------------------------------------------------------------
;;; The energy fount: spell points for sale, Roscoe's Energy Emporium
;;; by way of the public baths.  An :ENERGY location refills a
;;; caster's spell points at so many gold apiece (:PRICE); singers
;;; have the tavern, the fallen the temple.

(defun energy-price (location)
  "LOCATION's rate, gold per spell point restored (:PRICE, default 3)."
  (or (location-arg location :price) 3))

(defun energy-cost (location hero)
  "What LOCATION asks to refill HERO's spell points: the rate over the
missing points.  Zero for a full (or spell-less) hero."
  (* (energy-price location)
     (- (hero-max-sp hero) (hero-sp hero))))

(defun energy-restore (game hero)
  "Refill HERO's spell points for ENERGY-COST gold out of HERO's own
purse.  Returns T and emits :COIN and :ENERGY-RESTORED; says why and
returns NIL when HERO casts no spells, is down, brims already, or
cannot pay."
  (let* ((loc (game-location game))
         (cost (energy-cost loc hero)))
    (cond ((not (hero-caster-p hero))
           (say game "~A has no spell points to fill." (hero-name hero))
           nil)
          ((not (hero-alive-p hero))
           (say game "~A is beyond these waters -- the temple, perhaps."
                (hero-name hero))
           nil)
          ((zerop cost)
           (say game "~A brims with power already." (hero-name hero))
           nil)
          ((< (hero-gold hero) cost)
           (say game "~A cannot pay the ~D gold." (hero-name hero) cost)
           nil)
          (t
           (decf (hero-gold hero) cost)
           (setf (hero-sp hero) (hero-max-sp hero))
           (emit game :coin cost)
           (say game "Power floods back into ~A.  (~D gold)"
                (hero-name hero) cost)
           (emit game :energy-restored hero)
           t))))

(defun energy-lines (game)
  "The energy-fount menu as menu lines: the rate, then one numbered
row per party member — spell points for the casters, the purse, and
the cost of a refill."
  (let ((loc (game-location game)))
    (append
     (list (format nil "*** ~A ***" (location-title loc)) ""
           (format nil "Spell points, ~D gold apiece."
                   (energy-price loc))
           "")
     (let ((i 0))
       (mapcar (lambda (h)
                 (incf i)
                 (menu-numbered
                  i (if (hero-caster-p h)
                        (format nil "~D) ~A  SP ~D/~D  (~D gp)~
                                     ~@[  costs ~D~]"
                                i (hero-name h) (hero-sp h)
                                (hero-max-sp h) (hero-gold h)
                                (let ((c (energy-cost loc h)))
                                  (when (plusp c) c)))
                        (format nil "~D) ~A  no spells  (~D gp)"
                                i (hero-name h) (hero-gold h)))))
               (game-party game))))))

(defun energy-act (game char)
  "Apply key CHAR to the energy-fount menu: a digit refills that
hero's spell points, Esc leaves.  Returns :LEFT when the party leaves
the location, else NIL."
  (let ((digit (digit-char-p char)))
    (cond ((and digit (<= 1 digit (length (game-party game))))
           (energy-restore game (nth (1- digit) (game-party game)))
           nil)
          ((member char '(#\Escape #\l #\L #\q #\Q))
           (leave-location game)
           :left)
          (t nil))))

(defun make-location-view (game)
  "A fresh view for the current location's menu, of the kind that
menu needs: the shop model for :SHOP, the temple's for :TEMPLE, NIL
for kinds whose menus keep no state (and with no location at all).
The front-ends call this on :ENTER-LOCATION and hand the view to
LOCATION-LINES and LOCATION-ACT."
  (let ((loc (game-location game)))
    (when loc
      (case (location-kind loc)
        (:shop (make-shop-view))
        (:temple (make-temple-view))
        (t nil)))))

(defun location-lines (game view)
  "Menu lines for the current location: the shop model for :SHOP, the
tavern menu for :TAVERN, the temple for :TEMPLE, the energy fount for
:ENERGY, and for kinds the engine has no mechanics for (a :HOUSE and
friends) the Bard's Tale interior notice — the title over a lone
clickable EXIT."
  (let ((loc (game-location game)))
    (case (location-kind loc)
      (:shop (shop-lines game view))
      (:tavern (tavern-lines game))
      (:temple (temple-lines game view))
      (:energy (energy-lines game))
      (t (list (format nil "*** ~A ***" (location-title loc)) ""
               "There is nothing to do here."
               "" (menu-option #\Escape "EXIT"))))))

(defun location-act (game view char)
  "Apply key CHAR inside the current location (see SHOP-ACT,
TAVERN-ACT, TEMPLE-ACT and ENERGY-ACT)."
  (let ((loc (game-location game)))
    (case (location-kind loc)
      (:shop (shop-act game view char))
      (:tavern (tavern-act game char))
      (:temple (temple-act game view char))
      (:energy (energy-act game char))
      (t (when (member char '(#\Escape #\e #\E #\l #\L #\q #\Q))
           (leave-location game)
           :left)))))
