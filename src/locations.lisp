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
;;; (healing and raising the fallen, for gold), :ENERGY (spell
;;; points at so many gold apiece) and :GUILD (the Adventurers'
;;; Guild: characters made, parties formed — see the guild section
;;; below).  Unknown kinds still enter/leave cleanly — campaigns
;;; script them via the :ENTER-LOCATION event.
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

(defun location-banner (location)
  "The location page's title row: *** TITLE *** when that fits the
narrowest takeover column, else *** TITLE — the closing stars are
ornament, and ornament may not cost a row (a wrapped banner would
push the page's last rows off the lores screen)."
  (let ((full (format nil "*** ~A ***" (location-title location))))
    (if (<= (length full) +takeover-columns+)
        full
        (format nil "*** ~A" (location-title location)))))

(defun plaque-title (game)
  "What the view plaque under the first-person view says: the current
location's :PLAQUE arg while the party stands inside one that names
it — a short form of the title, cut to fit the plaque's width where
the title itself would not — else the zone's own MAP-TITLE."
  (let ((loc (game-location game)))
    (or (and loc (location-arg loc :plaque))
        (map-title (game-map game)))))

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
      (when (eq kind :guild)
        (let ((gold (location-arg loc :gold)))
          (unless (or (null gold) (integerp gold))
            (parse-dice gold))))    ; catch a bad purse at entry, loudly
      (let ((plaque (location-arg loc :plaque)))
        (unless (or (null plaque) (stringp plaque))
          (error "location ~S: :plaque ~S must be a string" title plaque)))
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

(defun %lone-exit-dir (game)
  "The single passable side of the party's cell, or NIL when the cell
has none or several.  This is the front door a location entered
without a step still obviously owns — a purpose-built building cell
is walled all round but for its one door."
  (let ((map (game-map game))
        (x (game-x game))
        (y (game-y game))
        (found nil))
    (dotimes (d 4 found)
      (when (wall-passable-p (cell-wall map x y d))
        (if found
            (return nil)
            (setf found d))))))

(defun leave-location (game)
  "Leave the current location.  When the party stepped in through a
door (the location's ENTRY-DIR), it steps back out onto the cell it
came from, facing away from the door — the Bard's Tale exit: leaving
a house puts you on the street before its front, not standing in the
doorway looking at the hearth.  The exit step costs time and maps
like any step, but does NOT re-trigger the street cell's special —
the party just came from there.  A location entered without a step
(TRAVEL, a script, the boot's start cell) still takes that exit when
its cell has exactly one passable side — the front door is not less
the way out for having been skipped on the way in (a game that
starts AT its guild leaves onto the street, see %LONE-EXIT-DIR) —
and leaves the party where it stands otherwise.  Emits
:LEAVE-LOCATION."
  (let ((loc (game-location game)))
    (when loc
      (setf (game-location game) nil)
      ;; as quiet as entering — the view coming back says it all
      (let* ((entry (location-entry-dir loc))
             (out (if entry
                      (dir-opposite entry)
                      (%lone-exit-dir game))))
        (when out
          (%step-out game out out t)))
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
         (list (location-banner loc) "")
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
             ;; the two-space gap before Gold: is where a page too
             ;; narrow for the one-liner breaks it (WRAP-MENU-LINE),
             ;; name and purse each keeping a whole row
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
             ;; the star marks the worn COPY, not the worn name — with
             ;; a duplicate in the pack only one row wears it, the same
             ;; rule as the pack page (EQUIPPED-INSTANCE-P), so the
             ;; window's start joins the row number to make the pack
             ;; position absolute
             (let ((start (menu-window (length (hero-items hero))
                                       (shop-view-top view))))
               (menu-scrolled-lines
                (hero-items hero) (shop-view-top view)
                (lambda (i name)
                  (menu-numbered i (format nil "~D) ~A~:[~;*~]~A~A  ~D gp"
                                           i (item-title name)
                                           (equipped-instance-p
                                            hero name (+ start i -1))
                                           (item-hand-marker name)
                                           (item-fit-marker hero name)
                                           (item-sell-price name))))))
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
     (list (location-banner loc) ""
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
                      (say game "The priests chant, and ~A rises - ~
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
     (list (location-banner loc) ""
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

(defstruct (energy-view (:constructor %make-energy-view))
  hero                ; the caster the waters will fill, or NIL while
                      ; the menu asks who wants refreshing
  note)               ; a notice for the menu's last line ("Not enough
                      ; Gold"), or NIL; cleared by the next key

(defun make-energy-view ()
  (%make-energy-view))

(defun energy-work-p (location hero)
  "Have LOCATION's waters anything to sell HERO — a living caster with
points to fill?  The menu's pick asks this one question, so a hero the
waters turn away is a hero they never had work for."
  (and (hero-caster-p hero)
       (hero-alive-p hero)
       (plusp (energy-cost location hero))))

(defun %energy-turn-away (game hero)
  "Say why the waters will not take HERO: no spell points at all, one
foot past them, or brimming already."
  (cond ((not (hero-caster-p hero))
         (say game "~A has no spell points to fill." (hero-name hero)))
        ((not (hero-alive-p hero))
         (say game "~A is beyond these waters -- the temple, perhaps."
              (hero-name hero)))
        (t
         (say game "~A brims with power already." (hero-name hero)))))

(defun energy-restore (game hero payer)
  "Fill HERO's spell points out of PAYER's purse for ENERGY-COST gold.
Returns T and emits :COIN and :ENERGY-RESTORED when the waters run;
otherwise returns (VALUES NIL REASON) — :DRY when there is nothing to
sell HERO at all (said aloud: no spells, beyond the waters, or
brimming), :POOR when PAYER's purse falls short (quiet: the fount menu
shows the notice, see ENERGY-LINES)."
  (let* ((loc (game-location game))
         (cost (energy-cost loc hero)))
    (cond ((not (energy-work-p loc hero))
           (%energy-turn-away game hero)
           (values nil :dry))
          ((< (hero-gold payer) cost)
           (values nil :poor))
          (t
           (decf (hero-gold payer) cost)
           (setf (hero-sp hero) (hero-max-sp hero))
           (emit game :coin cost)
           (say game "Power floods back into ~A.  (~D gold)"
                (hero-name hero) cost)
           (emit game :energy-restored hero)
           t))))

(defun energy-lines (game view)
  "The energy-fount menu as menu lines, two pages riding VIEW (an
ENERGY-VIEW).  First the rate and a bare prompt naming the digit range
— the roster pane already lists the party with their spell points and
their purses, so the pick page keeps no rows of its own (the shop's
who-is-shopping shape, and the Amiga front-end lets the roster rows
click as their digits here).  With the caster chosen: Who will pay?
over one row per party member and their purse.  The view's NOTE (the
payer page's Not enough Gold) is the last line."
  (let* ((loc (game-location game))
         (hero (and view (energy-view-hero view)))
         (note (and view (energy-view-note view))))
    (append
     (list (location-banner loc) ""
           (format nil "Spell points, ~D gold apiece."
                   (energy-price loc))
           "")
     (if hero
         (append
          (list (format nil "Who will pay ~A's ~D gold?"
                        (hero-name hero) (energy-cost loc hero))
                "")
          (let ((i 0))
            (mapcar (lambda (h)
                      (incf i)
                      (menu-numbered
                       i (format nil "~D) ~A  ~D gp"
                                 i (hero-name h) (hero-gold h))))
                    (game-party game))))
         (let ((n (length (game-party game))))
           (list (format nil "Who wants to refresh spell points?  ~
                              (1~@[-~D~])"
                         (when (> n 1) n)))))
     (when note (list "" note)))))

(defun energy-act (game view char)
  "Apply key CHAR to the energy-fount menu (VIEW is its ENERGY-VIEW):
on the first page a digit picks the caster to refresh — one the waters
have no work for is turned away aloud and the page stands — and on the
payer page a digit picks whose purse pays, a purse too short leaving
the Not enough Gold notice.  Esc backs off the payer page, else
leaves.  Returns :LEFT when the party leaves the location, else NIL."
  (let ((digit (digit-char-p char))
        (hero (and view (energy-view-hero view))))
    (when view (setf (energy-view-note view) nil))
    (cond ((and view digit (<= 1 digit (length (game-party game))))
           (let ((h (nth (1- digit) (game-party game))))
             (if hero
                 (multiple-value-bind (filled reason)
                     (energy-restore game hero h)
                   (declare (ignore filled))
                   (if (eq reason :poor)
                       (setf (energy-view-note view) "Not enough Gold")
                       ;; filled — or dry after another purse already
                       ;; paid: back to picking a caster either way
                       (setf (energy-view-hero view) nil)))
                 (if (energy-work-p (game-location game) h)
                     (setf (energy-view-hero view) h)
                     (%energy-turn-away game h))))
           nil)
          ((eql char #\Escape)
           (cond (hero
                  (setf (energy-view-hero view) nil)
                  nil)
                 (t
                  (leave-location game)
                  :left)))
          ((and (null hero) (member char '(#\l #\L #\q #\Q)))
           (leave-location game)
           :left)
          (t nil))))

;;; ---------------------------------------------------------------------
;;; The guild: the Adventurers' Guild of Bard's Tale tradition.  A
;;; :GUILD location is where characters are made and parties formed —
;;; and where a fresh game begins, when the campaign puts it on the
;;; map's start cell (the boot's TRIGGER-SPECIAL walks straight in).
;;; Heroes not marching wait in the game's ROSTER (see game.lisp):
;;; Create rolls a new one onto it, Add moves one into the party,
;;; Remove sends one back to the hall, Delete strikes a name for good.
;;; The main page also offers the save/load picker — the model cannot
;;; open a front-end page itself, so GUILD-ACT returns the instruction
;;; (:SAVES :SAVE) or (:SAVES :LOAD) and the front-end opens its
;;; picker over the guild page (the SAVE-MENU-ACT instruction pattern).
;;;
;;; (location TITLE :guild :gold DICE) — DICE (a dice string or an
;;; integer, default 0) is a new character's starting purse; campaign
;;; data decides.  With no living party the guild does not send the
;;; party out at all: the street's dangers are no place for nobody.

(defconstant +hero-name-limit+ 12
  "Longest hero name the guild's entry field accepts — the widest the
roster pane's name column shows whole.")

(defun hero-name-char-p (ch)
  "Characters a hero's name may hold: letters, digits, spaces, - and
_ (\"El Cid\" is a fine name; a hero is not a file)."
  (or (alphanumericp ch) (member ch '(#\Space #\- #\_))))

(defun guild-gold (location)
  "A new character's starting purse at LOCATION: the :GOLD arg (a dice
string or an integer; default 0 — campaign data decides)."
  (or (location-arg location :gold) 0))

(defun guild-name-taken-p (game name)
  "Is NAME already borne by someone in the party or on the roster?
Case-insensitive — two heroes a capital apart would be one hero to
every menu that picks by name."
  (and (find name (append (game-party game) (game-roster game))
             :key #'hero-name :test #'string-equal)
       t))

(defun guild-startable-classes (race)
  "The classes the guild offers a new character of RACE: the
:STARTABLE ones the race may take, in HERO-CLASSES' stable order."
  (remove-if-not (lambda (class) (race-allows-class-p race class))
                 (startable-hero-classes)))

(defstruct (guild-view (:constructor %make-guild-view))
  (mode :main)        ; :main, the :add/:remove/:delete/:confirm-delete
                      ; roster pages, or the creation walk
                      ; :race -> :class [-> :sex] -> :name -> :roll
  race                ; the creation walk's picks so far...
  class
  woman
  name                ; the name being typed (:name mode)
  rolled              ; the freshly rolled HERO awaiting Keep/Roll again
  pending             ; the roster hero awaiting the delete confirmation
  note                ; a notice for the page's last line, or NIL;
                      ; cleared by the next key (the temple's pattern)
  (top 0))            ; scroll offset into the current page's list

(defun make-guild-view ()
  (%make-guild-view))

(defun %guild-roster-row (i hero)
  "A roster page's row: the hero named with their class code and level
— the same shorthand the party pane's columns speak."
  (menu-numbered i (format nil "~D) ~A  ~A ~D"
                           i (hero-name hero) (hero-class-abbrev hero)
                           (hero-level hero))))

(defun %guild-rolled-lines (view)
  "The Keep-or-roll-again page's stat block: what the dice just gave,
compact — the full sheet is a party page and this hero is not in the
party yet."
  (let ((h (guild-view-rolled view)))
    (append
     (list (hero-name h)
           (format nil "~@[~A ~]~A" (hero-race-title h)
                   (hero-class-title h))
           (format nil "HP ~D  AC ~D~@[  SP ~D~]"
                   (hero-max-hp h) (hero-ac h)
                   (when (plusp (hero-max-sp h)) (hero-max-sp h)))
           (format nil "STR ~D DEX ~D IQ ~D"
                   (hero-str h) (hero-dex h) (hero-iq h))
           (format nil "CON ~D LCK ~D" (hero-con h) (hero-lck h))
           (format nil "Gold ~D gp" (hero-gold h))
           "")
     (list (menu-option #\k "Keep")
           (menu-option #\r "Roll again")))))

(defun guild-lines (game view)
  "The guild's menu as menu lines, one page per VIEW mode (a
GUILD-VIEW).  The main page counts the hall and offers the guild's
services; the roster pages list who waits (or marches) as numbered
rows; the creation walk asks race, class, portrait where the class
carries two, then the name (live echo, the save picker's entry
pattern), and shows the roll for keeping or rolling again.  The
view's NOTE is the last line."
  (let* ((loc (game-location game))
         (note (guild-view-note view)))
    (append
     (list (location-banner loc) "")
     (case (guild-view-mode view)
       (:main
        (append
         (list (let ((n (length (game-roster game))))
                 (if (plusp n)
                     (format nil "Adventurers in the hall: ~D" n)
                     "The hall stands empty.")))
         ;; the note takes this breathing row's place below: the main
         ;; page is the one tall enough that keeping both would push
         ;; the note off the lores page
         (unless note (list ""))
         (list (menu-option #\c "Create a character")
               (menu-option #\a "Add a member")
               (menu-option #\r "Remove a member")
               (menu-option #\d "Delete a character")
               (menu-option #\s "Save game")
               (menu-option #\l "Load game"))))
       (:add
        (cons "Who joins the party?"
              (menu-scrolled-lines (game-roster game)
                                   (guild-view-top view)
                                   #'%guild-roster-row
                                   +book-page-size+)))
       (:remove
        (cons "Who stays behind?"
              (let ((i 0))
                (mapcar (lambda (h)
                          (incf i)
                          (menu-numbered i (format nil "~D) ~A"
                                                   i (hero-name h))))
                        (game-party game)))))
       (:delete
        (cons "Whose name is struck?"
              (menu-scrolled-lines (game-roster game)
                                   (guild-view-top view)
                                   #'%guild-roster-row
                                   +book-page-size+)))
       (:confirm-delete
        (list (format nil "Strike ~A from the roster?"
                      (hero-name (guild-view-pending view)))
              "There is no coming back."
              ""
              (menu-option #\y "Yes, strike the name")
              (menu-option #\n "No, keep them")))
       ;; the pick pages carry a prompt and their rows and nothing
       ;; else, so they window at +BOOK-PAGE-SIZE+ like the spell
       ;; book: with the banner on one row, the eight startable
       ;; classes of the fullest race stand on the page whole
       (:race
        (cons "Choose a race:"
              (menu-scrolled-lines (races) (guild-view-top view)
                                   (lambda (i race)
                                     (menu-numbered
                                      i (format nil "~D) ~A"
                                               i (race-title race))))
                                   +book-page-size+)))
       (:class
        (cons "Choose a class:"
              (menu-scrolled-lines
               (guild-startable-classes (guild-view-race view))
               (guild-view-top view)
               (lambda (i class)
                 (menu-numbered
                  i (format nil "~D) ~A" i
                            (string-capitalize
                             (substitute #\Space #\- (string class))))))
               +book-page-size+)))
       (:sex
        ;; a class with two portraits asks whose face this is
        (list "Man or woman?"
              (menu-numbered 1 "1) A man")
              (menu-numbered 2 "2) A woman")))
       (:name
        ;; a text-entry modal, the save picker's pattern: the keys
        ;; mean typing here, so the page says so
        (list (format nil "Name: ~A_" (guild-view-name view))
              ""
              "Type a name; Return rolls"))
       (:roll (%guild-rolled-lines view)))
     (when note (list "" note)))))

(defun %guild-roll (game view)
  "Roll the creation walk's character: MAKE-HERO over the view's
picks, the purse from the location's :GOLD."
  (setf (guild-view-rolled view)
        (make-hero (guild-view-name view) (guild-view-class view)
                   :race (guild-view-race view)
                   :woman (guild-view-woman view)
                   :gold (guild-gold (game-location game)))))

(defun guild-act (game view char)
  "Apply key CHAR to the guild menu (VIEW is its GUILD-VIEW).  Digits
pick within the visible window of the page's list (u/d scroll it),
Esc steps back a page at a time and finally leaves — though never
with an empty party: the guild sends no one out alone.  Returns NIL,
:LEFT when the party steps out, or the front-end instruction
\(:SAVES :SAVE) / (:SAVES :LOAD) — the front-end opens its save/load
picker over the guild page and closes it back onto it."
  (let ((digit (digit-char-p char))
        (mode (guild-view-mode view)))
    (setf (guild-view-note view) nil)
    (flet ((to (mode)
             (setf (guild-view-mode view) mode
                   (guild-view-top view) 0)
             nil)
           ;; the pick pages window at +BOOK-PAGE-SIZE+ (see
           ;; GUILD-LINES) — the act's window math must be the same
           ;; page's, or a digit would pick beside what is drawn
           (scroll (n)
             (let ((top (menu-scroll (guild-view-top view) char n
                                     +book-page-size+)))
               (when top (setf (guild-view-top view) top)))
             nil))
      (case mode
        (:main
         (case char
           ((#\c #\C)
            (setf (guild-view-race view) nil
                  (guild-view-class view) nil
                  (guild-view-woman view) nil
                  (guild-view-name view) nil
                  (guild-view-rolled view) nil)
            (to :race))
           ((#\a #\A)
            (if (game-roster game)
                (to :add)
                (progn (setf (guild-view-note view)
                             "No one waits in the hall.")
                       nil)))
           ((#\r #\R)
            (if (game-party game)
                (to :remove)
                (progn (setf (guild-view-note view) "No one marches.")
                       nil)))
           ((#\d #\D)
            (if (game-roster game)
                (to :delete)
                (progn (setf (guild-view-note view)
                             "No one waits in the hall.")
                       nil)))
           ((#\s #\S) (list :saves :save))
           ((#\l #\L) (list :saves :load))
           ((#\Escape #\q #\Q)
            (if (game-party game)
                (progn (leave-location game) :left)
                (progn (setf (guild-view-note view)
                             "The guild sends no one out alone.")
                       nil)))
           (t nil)))
        (:add
         (cond (digit
                (let ((hero (menu-window-pick (game-roster game)
                                              (guild-view-top view)
                                              digit +book-page-size+)))
                  (when (and hero (join-party game hero))
                    (setf (game-roster game)
                          (remove hero (game-roster game) :count 1))
                    (unless (game-roster game)
                      (to :main))))
                nil)
               ((eql char #\Escape) (to :main))
               (t (scroll (length (game-roster game))))))
        (:remove
         (cond ((and digit (<= 1 digit (length (game-party game))))
                (let ((hero (nth (1- digit) (game-party game))))
                  (when (remove-from-party game hero)
                    (setf (game-roster game)
                          (append (game-roster game) (list hero)))
                    (say game "~A stays at the guild." (hero-name hero))
                    (unless (game-party game)
                      (to :main))))
                nil)
               ((eql char #\Escape) (to :main))
               (t nil)))
        (:delete
         (cond (digit
                (let ((hero (menu-window-pick (game-roster game)
                                              (guild-view-top view)
                                              digit +book-page-size+)))
                  (when hero
                    (setf (guild-view-pending view) hero)
                    (to :confirm-delete)))
                nil)
               ((eql char #\Escape) (to :main))
               (t (scroll (length (game-roster game))))))
        (:confirm-delete
         (case char
           ((#\y #\Y)
            (let ((hero (guild-view-pending view)))
              (setf (game-roster game)
                    (remove hero (game-roster game) :count 1)
                    (guild-view-pending view) nil)
              (say game "~A's name is struck from the roster."
                   (hero-name hero)))
            (to (if (game-roster game) :delete :main)))
           ((#\n #\N #\Escape)
            (setf (guild-view-pending view) nil)
            (to :delete))
           (t nil)))
        (:race
         (cond (digit
                (let ((race (menu-window-pick (races)
                                              (guild-view-top view)
                                              digit +book-page-size+)))
                  (when race
                    (setf (guild-view-race view) race)
                    (to :class)))
                nil)
               ((eql char #\Escape) (to :main))
               (t (scroll (length (races))))))
        (:class
         (let ((classes (guild-startable-classes (guild-view-race view))))
           (cond (digit
                  (let ((class (menu-window-pick classes
                                                 (guild-view-top view)
                                                 digit +book-page-size+)))
                    (when class
                      (setf (guild-view-class view) class
                            (guild-view-woman view) nil
                            (guild-view-name view) "")
                      (to (if (hero-class-property class :image-woman)
                              :sex
                              :name))))
                  nil)
                 ((eql char #\Escape) (to :race))
                 (t (scroll (length classes))))))
        (:sex
         (case digit
           (1 (setf (guild-view-woman view) nil) (to :name))
           (2 (setf (guild-view-woman view) t) (to :name))
           (t (when (eql char #\Escape) (to :class))
              nil)))
        (:name
         (let ((entry (guild-view-name view)))
           (cond ((or (eql char #\Return) (eql char #\Newline)
                      (eql char (code-char 13)))
                  (let ((name (string-trim " " entry)))
                    (cond ((zerop (length name))
                           (setf (guild-view-note view) "A name is needed."))
                          ((guild-name-taken-p game name)
                           (setf (guild-view-note view) "The name is taken."))
                          (t
                           (setf (guild-view-name view) name)
                           (%guild-roll game view)
                           (to :roll))))
                  nil)
                 ((eql char #\Escape)
                  (to (if (hero-class-property (guild-view-class view)
                                               :image-woman)
                          :sex
                          :class)))
                 ((or (eql char #\Backspace) (eql char (code-char 8)))
                  (when (plusp (length entry))
                    (setf (guild-view-name view)
                          (subseq entry 0 (1- (length entry)))))
                  nil)
                 ((and (characterp char)
                       (hero-name-char-p char)
                       ;; a name neither starts with a space nor
                       ;; outgrows the roster pane's name column
                       (not (and (zerop (length entry))
                                 (char= char #\Space)))
                       (< (length entry) +hero-name-limit+))
                  (setf (guild-view-name view)
                        (concatenate 'string entry (string char)))
                  nil)
                 (t nil))))
        (:roll
         (cond ((or (member char '(#\k #\K #\Return #\Newline))
                    (eql char (code-char 13)))
                (let ((hero (guild-view-rolled view)))
                  (setf (game-roster game)
                        (append (game-roster game) (list hero))
                        (guild-view-rolled view) nil)
                  (say game "~A signs the guild roster." (hero-name hero)))
                (to :main))
               ((member char '(#\r #\R))
                (%guild-roll game view)
                nil)
               ((eql char #\Escape)
                (setf (guild-view-rolled view) nil)
                (to :main))
               (t nil)))
        (t nil)))))

(defun make-location-view (game)
  "A fresh view for the current location's menu, of the kind that
menu needs: the shop model for :SHOP, the temple's for :TEMPLE, the
fount's for :ENERGY, the guild's for :GUILD, NIL for kinds whose
menus keep no state (and with no location at all).  The front-ends
call this on :ENTER-LOCATION and hand the view to LOCATION-LINES and
LOCATION-ACT."
  (let ((loc (game-location game)))
    (when loc
      (case (location-kind loc)
        (:shop (make-shop-view))
        (:temple (make-temple-view))
        (:energy (make-energy-view))
        (:guild (make-guild-view))
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
      (:energy (energy-lines game view))
      (:guild (guild-lines game view))
      (t (list (location-banner loc) ""
               "There is nothing to do here."
               "" (menu-option #\Escape "EXIT"))))))

(defun location-act (game view char)
  "Apply key CHAR inside the current location (see SHOP-ACT,
TAVERN-ACT, TEMPLE-ACT, ENERGY-ACT and GUILD-ACT).  Returns what the
kind's own ACT returns: NIL, :LEFT when the party leaves the location
— or an instruction the front-end executes, today the guild's
\(:SAVES :SAVE) / (:SAVES :LOAD) ask for the save/load picker."
  (let ((loc (game-location game)))
    (case (location-kind loc)
      (:shop (shop-act game view char))
      (:tavern (tavern-act game char))
      (:temple (temple-act game view char))
      (:energy (energy-act game view char))
      (:guild (guild-act game view char))
      (t (when (member char '(#\Escape #\e #\E #\l #\L #\q #\Q))
           (leave-location game)
           :left)))))
