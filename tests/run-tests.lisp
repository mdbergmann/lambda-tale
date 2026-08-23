;;; Lambda's Tale engine — test suite: map model, movement, knowledge,
;;; renderers (M0/M1); dice, events, specials, party, combat, save
;;; games (M2).  Story content lives in games (e.g. the Closure game
;;; next door, which has its own suite); the world these tests play is
;;; the minimal fixture under tests/world/.
;;; Run from the engine root:  make test

(load "src/load.lisp")

(in-package :tale)

;;; ---------------------------------------------------------------------
;;; Tiny test harness

(defvar *checks* 0)
(defvar *failures* 0)

(defun check (label expected actual)
  (incf *checks*)
  (unless (equal expected actual)
    (incf *failures*)
    (format t "FAIL ~A~%  expected: ~S~%  actual:   ~S~%" label expected actual)))

(defun check-true (label value)
  (check label t (not (not value))))

(defmacro check-error (label &body body)
  "Pass when BODY signals an error."
  `(check-true ,label
               (handler-case (progn ,@body nil)
                 (error () t))))

(defmacro with-rng ((&rest values) &body body)
  "Run BODY with *RNG* scripted: successive (roll N) calls return the
next of VALUES (mod N), then 0 forever."
  (let ((vals (gensym "VALS")))
    `(let* ((,vals (list ,@values))
            (*rng* (lambda (n) (mod (if ,vals (pop ,vals) 0) n))))
       ,@body)))

(defun watch-messages (game)
  "Subscribe to GAME's :MESSAGE events; returns a closure yielding the
messages so far (oldest first)."
  (let ((msgs '()))
    (on-event game :message
              (lambda (g text) (declare (ignore g)) (push text msgs)))
    (lambda () (reverse msgs))))

;;; ---------------------------------------------------------------------
;;; Test maps

;; 3x2 map: doors, features, fully walled border.
;;   (0,0) start, east open; (1,0)/(1,1) connected by a door;
;;   '<' feature (stairs up) at (2,1).
(defparameter *art*
"+-+-+-+
|@  | |
+ +D+ +
| |  <|
+-+-+-+")

;; 2x1 wrapping map with open borders (short second line pads to :open).
(defparameter *wrap-art*
"+ + +
|
+ + +")

;;; ---------------------------------------------------------------------
;;; Direction helpers

(check "dir-index keyword" 2 (dir-index :south))
(check "dir-index index passthrough" 3 (dir-index 3))
(check "dir-keyword" :west (dir-keyword 3))
(check "dir-opposite" +south+ (dir-opposite :north))
(check "turn right from west wraps to north" +north+ (turn-dir :west 1))
(check "turn left from north wraps to west" +west+ (turn-dir :north -1))
(check-error "dir-index rejects garbage" (dir-index :up))

;;; ---------------------------------------------------------------------
;;; Map parsing

(let ((m (parse-map *art* :name "test")))
  (check "width" 3 (dungeon-map-width m))
  (check "height" 2 (dungeon-map-height m))
  (check "start-x" 0 (dungeon-map-start-x m))
  (check "start-y" 0 (dungeon-map-start-y m))
  (check "start-facing" :north (dungeon-map-start-facing m))
  (check "no wrap by default" nil (dungeon-map-wrap m))

  (check "north border is wall" :wall (cell-wall m 0 0 :north))
  (check "west border is wall" :wall (cell-wall m 0 0 :west))
  (check "open east edge" :open (cell-wall m 0 0 :east))
  (check "open south edge" :open (cell-wall m 0 0 :south))
  (check "door south of (1,0)" :door (cell-wall m 1 0 :south))
  (check "same door north of (1,1)" :door (cell-wall m 1 1 :north))
  (check "interior wall east of (1,0)" :wall (cell-wall m 1 0 :east))
  (check "interior wall west of (2,0)" :wall (cell-wall m 2 0 :west))

  (check "feature at (2,1)" #\< (cell-feature m 2 1))
  (check "no feature at (0,0)" nil (cell-feature m 0 0))
  (check "start glyph not stored as feature" nil (cell-feature m 1 0))

  (check "wall not passable" nil (wall-passable-p :wall))
  (check-true "door passable" (wall-passable-p :door))
  (check-true "open passable" (wall-passable-p :open))

  (multiple-value-bind (nx ny) (neighbor m 1 0 :south)
    (check "neighbor south" '(1 1) (list nx ny)))
  (check "neighbor off-map is nil (no wrap)" nil (neighbor m 0 0 :west)))

;; :start-facing overrides the default facing; '>' in a cell is a plain
;; feature (stairs down), not a start glyph.
(let ((m (parse-map "+-+
|>|
+-+" :start-facing :east)))
  (check "start-facing argument" :east (dungeon-map-start-facing m))
  (check "'>' is a feature, not a start glyph" #\> (cell-feature m 0 0))
  (check "default start position" 0 (dungeon-map-start-x m)))

(check-error "invalid wall char rejected"
  (parse-map "+-+
|@x
+-+"))
(check-error "even-sized art rejected"
  (parse-map "++
++"))

;;; ---------------------------------------------------------------------
;;; Movement

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (check "game starts at start pos" '(0 0) (list (game-x g) (game-y g)))
  (check "game starts facing north" +north+ (game-facing g))

  (check "forward into wall blocked" :blocked (move-party g :forward))
  (check "blocked move keeps position" '(0 0) (list (game-x g) (game-y g)))

  (check "turn right" :east (turn-right g))
  (check "forward through open edge" :moved (move-party g :forward))
  (check "moved east" '(1 0) (list (game-x g) (game-y g)))

  (check "turn right again" :south (turn-right g))
  (check "forward through door reports :door" :door (move-party g :forward))
  (check "moved through door" '(1 1) (list (game-x g) (game-y g)))

  ;; Back-step keeps the facing.
  (check "back-step through door" :door (move-party g :back))
  (check "back-step position" '(1 0) (list (game-x g) (game-y g)))
  (check "back-step kept facing" +south+ (game-facing g))

  (check "turn left" :east (turn-left g))
  (check "turn around" :west (turn-around g)))

;;; ---------------------------------------------------------------------
;;; Wrapping maps

(let* ((m (parse-map *wrap-art* :wrap t))
       (g (new-game m)))
  (check "wrap map width" 2 (dungeon-map-width m))
  (check "wrap map height" 1 (dungeon-map-height m))
  (check-true "wrap flag" (dungeon-map-wrap m))

  ;; North edge open, height 1: wraps back to the same cell.
  (check "wrap north" :moved (move-party g :forward))
  (check "wrap north lands on same cell" '(0 0) (list (game-x g) (game-y g)))

  (check "west wall blocks despite wrap" :blocked
         (progn (turn-left g) (move-party g :forward)))

  (turn-around g)                       ; face east
  (check "move east" :moved (move-party g :forward))
  (check "at (1,0)" '(1 0) (list (game-x g) (game-y g)))
  ;; East edge of (1,0) is open; wrapping enters (0,0) even though (0,0)'s
  ;; west side is a wall — movement uses the current cell's wall only
  ;; (one-way walls are a feature, not a bug).
  (check "wrap east through one-way boundary" :moved (move-party g :forward))
  (check "wrapped to (0,0)" '(0 0) (list (game-x g) (game-y g))))

;; Same map without :wrap — the open border edge leads off-map: blocked.
(let* ((m (parse-map *wrap-art*))
       (g (new-game m)))
  (check "open border edge blocked without wrap" :blocked
         (move-party g :forward)))

;;; ---------------------------------------------------------------------
;;; Large maps and the minimap viewport (specs/ui-and-engine.md)

(defun %big-map-art (w h)
  "Art for a fully-walled WxH map with all interior edges open."
  (with-output-to-string (s)
    (dotimes (row (1+ (* 2 h)))
      (dotimes (col (1+ (* 2 w)))
        (write-char
         (cond ((and (evenp row) (evenp col)) #\+)
               ((evenp row)
                (if (or (= row 0) (= row (* 2 h))) #\- #\Space))
               ((evenp col)
                (if (or (= col 0) (= col (* 2 w))) #\| #\Space))
               (t #\Space))
         s))
      (write-char #\Newline s))))

;; 30x30 — the Bard's Tale I level size, the spec's minimum.
(let* ((m (parse-map (%big-map-art 30 30) :name "big30"))
       (g (new-game m)))
  (check "30x30 parses" '(30 30)
         (list (dungeon-map-width m) (dungeon-map-height m)))
  ;; viewport clamping: top-left corner, center, bottom-right corner
  (multiple-value-bind (x0 y0 w h) (map-viewport m 0 0 6 6)
    (check "viewport clamps at origin" '(0 0 6 6) (list x0 y0 w h)))
  (multiple-value-bind (x0 y0 w h) (map-viewport m 15 15 6 6)
    (check "viewport centers on the party" '(12 12 6 6) (list x0 y0 w h)))
  (multiple-value-bind (x0 y0 w h) (map-viewport m 29 29 6 6)
    (check "viewport clamps at the far corner" '(24 24 6 6)
           (list x0 y0 w h)))
  (multiple-value-bind (x0 y0 w h) (map-viewport m 29 0 6 6)
    (check "viewport clamps mixed edges" '(24 0 6 6) (list x0 y0 w h)))
  ;; walk the top corridor east and check movement + knowledge scale
  (turn-right g)
  (dotimes (i 10) (move-party g :forward))
  (check "movement across a big map" '(10 0) (list (game-x g) (game-y g)))
  (check-true "knowledge recorded far from origin"
              (cell-explored-p (game-knowledge g) 10 0)))

;; A viewport request larger than the map yields the whole map.
(let ((m (parse-map *art* :name "small")))
  (multiple-value-bind (x0 y0 w h) (map-viewport m 1 0 6 6)
    (check "viewport of a small map is the whole map" '(0 0 3 2)
           (list x0 y0 w h))))

;;; The automap page's sizing policy (MAP-PAGE-WINDOW).  A front-end's
;;; feature glyphs need a floor of pixels to draw in — the Amiga's is
;;; the microfont's 6px small advance — so a page too short for the
;;; whole map keeps a legible cell and windows the map instead of
;;; shrinking the cells past the point where the stairs, the doors and
;;; the legend's markers stop drawing at all.
;;;
;;; The numbers below are the lores automap page's own: 217 pixels
;;; across (the content span less the legend's reserved column and the
;;; scrollbar gutter) and 158 down (the content column less the
;;; two-line footer) on the 200-line layout, against 214 down on the
;;; 256-line one the layout used to be.
(let ((m (parse-map (%big-map-art 30 30) :name "big30")))
  ;; the 256-line page: 30 rows at 7px fit, so the map shows whole
  (multiple-value-bind (cell vw vh) (map-page-window m 217 214 6)
    (check "a page with the room shows the whole map" '(7 30 30)
           (list cell vw vh)))
  ;; the 200-line page: fitting 30 rows into 158px asks for 5px cells,
  ;; under the glyph floor — so the cell stays what the WIDTH affords
  ;; and the page windows 22 of the 30 rows
  (multiple-value-bind (cell vw vh) (map-page-window m 217 158 6)
    (check "a short page keeps a legible cell and windows the map"
           '(7 30 22) (list cell vw vh)))
  ;; ... never below the floor, whatever the page's height
  (multiple-value-bind (cell vw vh) (map-page-window m 217 40 6)
    (check "a very short page still draws legible cells" '(7 30 5)
           (list cell vw vh)))
  ;; a narrow page is the one case the cell may not reach the floor:
  ;; there is nothing to give, and a window that also scrolled
  ;; sideways would be a maze to read
  (multiple-value-bind (cell vw vh) (map-page-window m 90 158 6)
    (check "a narrow page falls back to the floor and windows both ways"
           '(6 15 26) (list cell vw vh)))
  ;; the cap holds a small map's cells to a sane size
  (multiple-value-bind (cell vw vh) (map-page-window m 700 700 6)
    (check "a roomy page stops growing at the cap" '(16 30 30)
           (list cell vw vh))))

;; A map that fits shows whole and never scrolls; one that windows
;; scrolls a window at a time, clamped to the map's own rows.
(check "a whole map does not scroll" nil (map-page-scroll 0 #\d 30 30))
(check "a windowed map scrolls a window down" 10
       (map-page-scroll 0 #\d 30 10))
(check "and another window down" 20 (map-page-scroll 10 #\d 30 10))
(check "the last window down is clamped to the map" 20
       (map-page-scroll 20 #\d 30 10))
(check "scrolling up comes back a window" 10 (map-page-scroll 20 #\u 30 10))
(check "the first window up is clamped at the top" 0
       (map-page-scroll 5 #\u 30 10))
(check "upper case scrolls too" 10 (map-page-scroll 0 #\D 30 10))
(check "a key that is not a scroll key moves nothing" nil
       (map-page-scroll 0 #\f 30 10))
(check "a non-character moves nothing" nil (map-page-scroll 0 :esc 30 10))
;; the town's own numbers: 22 rows of 30, so one press already lands on
;; the last window rather than 22 rows past the map's foot
(check "a deep window's first scroll is its last" 8
       (map-page-scroll 0 #\d 30 22))

;; Region rendering (the full map view's clamped window): a 6x6 region
;; is 13 art lines high, and the party arrow sits inside it.
(let* ((m (parse-map (%big-map-art 30 30) :name "big30"))
       (g (new-game m))
       (lines (%split-lines
               (multiple-value-bind (x0 y0 w h)
                   (map-viewport m (game-x g) (game-y g) 6 6)
                 (render-dungeon m :px (game-x g) :py (game-y g)
                                   :facing (game-facing g)
                                   :x0 x0 :y0 y0 :w w :h h)))))
  (check "region render is 6 cells high" 13 (length lines))
  (check "region render top border" "+-+-+-+-+-+-+" (first lines))
  (check "region render party arrow" "|^" (second lines)))

;; The region window follows the party: at the far corner the arrow
;; renders at the region's bottom-right cell, and cells outside the
;; region are not drawn.
(let* ((m (parse-map (%big-map-art 30 30) :name "big30"))
       (g (new-game m)))
  (setf (game-x g) 29 (game-y g) 29)
  (observe g)
  (let ((lines (%split-lines
                (multiple-value-bind (x0 y0 w h)
                    (map-viewport m 29 29 6 6)
                  (render-dungeon m :px 29 :py 29
                                    :facing (game-facing g)
                                    :x0 x0 :y0 y0 :w w :h h)))))
    (check "region render height at far corner" 13 (length lines))
    (check "region arrow at far corner" #\^
           (char (nth 11 lines) 11))))

;; 64x64: full save/load round-trip at flexible-map scale.
(let* ((m (parse-map (%big-map-art 64 64) :name "big64"))
       (g (new-game m)))
  (check "64x64 parses" '(64 64)
         (list (dungeon-map-width m) (dungeon-map-height m)))
  (turn-right g)
  (dotimes (i 5) (move-party g :forward))
  ;; save needs a map FILE to reference: write the art out
  (with-open-file (s "tests/tmp-big.map"
                     :direction :output :if-exists :supersede)
    (write-string (%big-map-art 64 64) s))
  (let ((g2 (new-game (load-map-file "tests/tmp-big.map"))))
    (turn-right g2)
    (dotimes (i 5) (move-party g2 :forward))
    (save-game g2 "tests/tmp-big.sav")
    (let ((g3 (load-game "tests/tmp-big.sav")))
      (check "64x64 save round-trips position" '(5 0)
             (list (game-x g3) (game-y g3)))
      (check-true "64x64 save round-trips knowledge"
                  (cell-explored-p (game-knowledge g3) 3 0))
      (check "64x64 unvisited cells stay unknown" nil
             (cell-explored-p (game-knowledge g3) 3 5))))
  (delete-file "tests/tmp-big.map")
  (delete-file "tests/tmp-big.sav"))

;; 128x128: the spec's upper flexibility bound — parse, move, viewport.
(let* ((m (parse-map (%big-map-art 128 128) :name "big128"))
       (g (new-game m)))
  (check "128x128 parses" '(128 128)
         (list (dungeon-map-width m) (dungeon-map-height m)))
  (setf (game-x g) 100 (game-y g) 64)
  (observe g)
  (multiple-value-bind (x0 y0 w h) (map-viewport m 100 64 6 6)
    (check "128x128 viewport" '(97 61 6 6) (list x0 y0 w h)))
  (move-party g :forward)
  (check "128x128 movement" '(100 63) (list (game-x g) (game-y g))))

;;; ---------------------------------------------------------------------
;;; First-person view geometry

(let* ((m (parse-map *art* :name "test"))
       (v (compute-view m 0 0 :north)))
  (check "view blocked at depth 0" 1 (length v))
  (let ((s (first v)))
    (check "slice front" :wall (view-slice-front s))
    (check "slice left" :wall (view-slice-left s))
    (check "slice right" :open (view-slice-right s))
    (check "right side cell" '(1 0)
           (list (view-slice-rx s) (view-slice-ry s)))
    (check "right side front wall" :wall (view-slice-right-front s))
    (check "no left side cell behind wall" nil (view-slice-lx s))))

(let* ((m (parse-map *art* :name "test"))
       (v (compute-view m 0 0 :east)))
  (check "corridor view depth" 2 (length v))
  (let ((s0 (first v))
        (s1 (second v)))
    (check "near slice front open" :open (view-slice-front s0))
    (check "near slice right cell" '(0 1)
           (list (view-slice-rx s0) (view-slice-ry s0)))
    (check "far slice center cell" '(1 0)
           (list (view-slice-cx s1) (view-slice-cy s1)))
    (check "far slice front wall" :wall (view-slice-front s1))
    (check "far slice right door" :door (view-slice-right s1))
    (check "closed side door not seen through" nil (view-slice-rx s1))))

;; A door straight ahead blocks the view like a wall.
(let* ((m (parse-map *art* :name "test"))
       (v (compute-view m 1 0 :south)))
  (check "door blocks view" 1 (length v))
  (check "front door in slice" :door (view-slice-front (first v))))

;; An endless (wrapping) corridor is capped at +view-depth+ slices.
(let ((m (parse-map *wrap-art* :wrap t)))
  (check "wrap corridor capped at view depth"
         +view-depth+ (length (compute-view m 0 0 :north))))

(check "view-planes plane 1" '(6 3 26 13) (aref (view-planes 33 17) 1))
(check "view-planes plane 0 is viewport" '(0 0 32 16)
       (aref (view-planes 33 17) 0))

;;; ---------------------------------------------------------------------
;;; Wall-piece slots and the blit list (M3)

(check "wall-piece-names covers all kinds and depths"
       (* +view-depth+ 10) (length (wall-piece-names)))

(let ((planes (view-planes 240 130)))
  ;; the fixed slots at the game's FP viewport size
  (check "front slot at depth 0" '(48 26 144 78)
         (wall-piece-rect planes '(:front 0)))
  (check "front-door shares the front slot"
         (wall-piece-rect planes '(:front 0))
         (wall-piece-rect planes '(:front-door 0)))
  (check "left side slot spans the full column" '(0 0 49 130)
         (wall-piece-rect planes '(:side 0 :l)))
  (check "left flank slot is the side band at wall height" '(0 26 49 78)
         (wall-piece-rect planes '(:flank 0 :l)))
  ;; left/right slots mirror around the viewport center
  (let ((l (wall-piece-rect planes '(:side 1 :l)))
        (r (wall-piece-rect planes '(:side 1 :r))))
    (check "side slots mirror"
           (list (first l) (third l))
           (list (- 239 (first r) (1- (third r))) (third r))))
  ;; every piece the view can ask for fits inside the viewport
  (check "all piece slots lie inside the viewport" nil
         (remove-if (lambda (piece)
                      (destructuring-bind (x y w h)
                          (wall-piece-rect planes piece)
                        (and (<= 0 x) (<= 0 y) (< 0 w) (< 0 h)
                             (<= (+ x w) 240) (<= (+ y h) 130))))
                    (wall-piece-names))))

;; the same slots at the lores profile's 120x100 viewport (2/5 of the
;; 320px screen's content span goes to the view, 3/5 to the log; the
;; 100px height is what keeps the whole layout inside NTSC's 200 lines)
(let ((planes (view-planes 120 100)))
  (check "lores view-planes plane 1" '(24 20 95 79) (aref planes 1))
  (check "lores front slot at depth 0" '(24 20 72 60)
         (wall-piece-rect planes '(:front 0)))
  (check "lores left side slot spans the full column" '(0 0 25 100)
         (wall-piece-rect planes '(:side 0 :l)))
  (check "lores left flank slot is the side band at wall height"
         '(0 20 25 60)
         (wall-piece-rect planes '(:flank 0 :l)))
  (check "lores piece slots lie inside the viewport" nil
         (remove-if (lambda (piece)
                      (destructuring-bind (x y w h)
                          (wall-piece-rect planes piece)
                        (and (<= 0 x) (<= 0 y) (< 0 w) (< 0 h)
                             (<= (+ x w) 120) (<= (+ y h) 100))))
                    (wall-piece-names))))

;; The blit list mirrors the display-list wall logic: same map spots as
;; the display-list tests above.
(let* ((m (parse-map *art* :name "test"))
       (planes (view-planes 240 130)))
  (check "blit list: walled dead end"
         '((:side 0 :l) (:flank 0 :r) (:front 0))
         (mapcar #'first (view-blit-list (compute-view m 0 0 :north) planes)))
  (check "blit list: corridor with side door, far to near"
         '((:side 1 :l) (:side-door 1 :r) (:front 1)
           (:side 0 :l) (:flank 0 :r))
         (mapcar #'first (view-blit-list (compute-view m 0 0 :east) planes)))
  ;; (1,0) facing south: east wall left, the open start cell right
  ;; (open beyond -> no piece), the door dead ahead
  (check "blit list: front door blocks the view"
         '((:side 0 :l) (:front-door 0))
         (mapcar #'first (view-blit-list (compute-view m 1 0 :south) planes)))
  ;; each record carries its wall style, its slot rect and the source
  ;; offset the blit starts at (0 for every piece drawn whole)
  (check "blit records carry their slot rects" nil
         (remove-if (lambda (rec)
                      (destructuring-bind (piece style x y w h sx) rec
                        (and (integerp style)
                             (equal (list x y w h)
                                    (wall-piece-rect planes piece))
                             (zerop sx))))
                    (view-blit-list (compute-view m 0 0 :east) planes))))

;;; ---------------------------------------------------------------------
;;; Flank geometry: a flank is the NEIGHBOR cell's front wall seen
;;; through an open side, so it stands at the same distance as the
;;; front wall of its depth and gets that wall's full perspective
;;; width — not the narrow near-to-far strip of the side band, which
;;; drew a house across an open side about half as wide as it should
;;; be.  What the party actually sees of it is cropped by the walls in
;;; front of it (FLANK-VISIBLE-X), and the blit record's SX says where
;;; in the piece the visible part starts.

(let ((planes (view-planes 120 100)))
  ;; depth 0: a whole cell is wider than the viewport edge allows, so
  ;; the slot stays the side band — the old geometry, unchanged
  (check "lores flank slot at depth 0 fills the side band" '(0 20 25 60)
         (wall-piece-rect planes '(:flank 0 :l)))
  ;; deeper in, the slot is one cell wide at the wall's own distance
  (dotimes (d +view-depth+)
    (destructuring-bind (qx0 qy0 qx1 qy1) (aref planes (1+ d))
      (destructuring-bind (lx ly lw lh) (wall-piece-rect planes
                                                         (list :flank d :l))
        (destructuring-bind (rx ry rw rh) (wall-piece-rect
                                           planes (list :flank d :r))
          (check (format nil "depth-~D flank is a cell wide at its own ~
plane, clipped to the viewport" d)
                 (list (max 0 (- qx0 (- qx1 qx0))) qy0 (1+ (- qy1 qy0)))
                 (list lx ly lh))
          ;; the flank meets the front wall's own edge column, which
          ;; the front piece then draws over
          (check (format nil "depth-~D flank abuts the front wall" d)
                 (list qx0 qx1)
                 (list (+ lx lw -1) rx))
          (check (format nil "depth-~D flanks mirror" d)
                 (list lw lh) (list rw rh))
          (check (format nil "depth-~D flank shares the front wall's ~
height" d)
                 (list qy0 (1+ (- qy1 qy0))) (list ry rh)))))))

;; 3x2 map, party at (0,0) facing east: the side beside it is open with
;; nothing beyond, the next cell's side opens on a wall — so the flank
;; at depth 1 has no near wall in front of it and is seen whole.
(let* ((m (parse-map
"+-+-+-+
|@    |
+ + +-+
|   | |
+-+-+-+" :name "flank-open"))
       (planes (view-planes 120 100))
       (blits (view-blit-list (compute-view m 0 0 :east) planes))
       (flank (find '(:flank 1 :r) blits :key #'first :test #'equal)))
  (check "nothing is drawn where the near side opens on open ground"
         nil (find '(:flank 0 :r) blits :key #'first :test #'equal))
  (check-true "a flank behind an open side is drawn" flank)
  (destructuring-bind (piece style x y w h sx) flank
    (declare (ignore piece style))
    (check "an unblocked flank keeps its whole cell width"
           (wall-piece-rect planes '(:flank 1 :r)) (list x y w h))
    (check "an unblocked flank blits from its own left edge" 0 sx)))

;; The same wall with the near side WALLED: the near wall hides all but
;; the strip between its planes, and the blit crops to it — the fixed
;; slot the renderer always used, so a walled street is unchanged.
(let* ((m (parse-map
"+-+-+-+
|@    |
+-+ +-+
|   | |
+-+-+-+" :name "flank-blocked"))
       (planes (view-planes 120 100))
       (blits (view-blit-list (compute-view m 0 0 :east) planes))
       (flank (find '(:flank 1 :r) blits :key #'first :test #'equal)))
  (check-true "a flank behind a walled near side is still drawn" flank)
  (destructuring-bind (piece style x y w h sx) flank
    (declare (ignore piece style y h))
    (destructuring-bind (px0 py0 px1 py1) (aref planes 1)
      (declare (ignore px0 py0 py1))
      (destructuring-bind (qx0 qy0 qx1 qy1) (aref planes 2)
        (declare (ignore qx0 qy0 qy1))
        (check "a blocked flank crops to the strip past the near wall"
               (list qx1 (1+ (- px1 qx1))) (list x w))))
    ;; cropped on its right, so the piece still blits from its own x 0
    (check "a cropped right flank blits from its own left edge" 0 sx)))

;; Mirrored, on the LEFT: the near wall hides the flank's outer part,
;; so the blit starts further into the piece — the wall the party sees
;; is the strip beside the corridor, never a squashed whole.
(let* ((m (parse-map
"+-+-+-+
|   | |
+-+ +-+
|@    |
+-+-+-+" :name "flank-left"))
       (planes (view-planes 120 100))
       (blits (view-blit-list (compute-view m 0 1 :east) planes))
       (flank (find '(:flank 1 :l) blits :key #'first :test #'equal)))
  (check-true "a left flank behind a near wall is drawn" flank)
  (destructuring-bind (piece style x y w h sx) flank
    (declare (ignore piece style y h))
    (destructuring-bind (px0 py0 px1 py1) (aref planes 1)
      (declare (ignore py0 px1 py1))
      (destructuring-bind (qx0 qy0 qx1 qy1) (aref planes 2)
        (declare (ignore qy0 qx1 qy1))
        (check "a left flank crops to the strip past the near wall"
               (list px0 (1+ (- qx0 px0))) (list x w))
        (destructuring-bind (fx fy fw fh) (wall-piece-rect planes
                                                           '(:flank 1 :l))
          (declare (ignore fy fw fh))
          (check "its source offset skips the hidden part"
                 (- px0 fx) sx))))))

;;; ---------------------------------------------------------------------
;;; Flank RUNS (*DRAW-FLANKS*): a distant row of houses spans many
;;; cells, but the slice model knew only the corridor's immediate
;;; neighbors — three houses on a horizon with room for sixteen.  The
;;; runs walk outward from the corridor while the sides stay open and
;;; repeat the flank piece one cell width per step; the knob says how
;;; far, and it is a RENDERING knob like *DRAW-DEPTH*.

;; A market square: a row of seven houses, a gap splitting off the
;; westmost (two buildings), a door in the middle mass, and the party
;; three cells back on open ground.
(defparameter *row-art*
"+-+-+-+-+-+-+-+
| | | | | | | |
+-+ +-+-+D+-+-+
|             |
+ + + + + + + +
|             |
+ + + + + + + +
|      @      |
+-+-+-+-+-+-+-+")

(let* ((m (parse-map *row-art* :name "row"))
       (a (%wall-style m 0 0))          ; the lone west house
       (b (%wall-style m 2 0))          ; the long mass with the door
       (v (compute-view m 3 3 :north +view-depth+ 8))
       (s (third v)))
  (check "row: the view stops at the house row" 3 (length v))
  ;; the runs read the row: house, gap, house to the left; door and
  ;; two more cells of the same mass to the right, ending at the edge
  (check "row: left run fronts" '(:wall :open :wall)
         (mapcar #'car (view-slice-left-fronts s)))
  (check "row: left run styles" (list b nil a)
         (mapcar #'cdr (view-slice-left-fronts s)))
  (check "row: right run fronts" '(:door :wall :wall)
         (mapcar #'car (view-slice-right-fronts s)))
  (check "row: one mass, one style along the run" (list b b b)
         (mapcar #'cdr (view-slice-right-fronts s)))
  ;; FLANKS bounds the walk; the scalar fields never depend on it
  (let ((short (third (compute-view m 3 3 :north +view-depth+ 2))))
    (check "a shorter walk cuts the run" 2
           (length (view-slice-left-fronts short))))
  (let ((none (third (compute-view m 3 3 :north +view-depth+ 0))))
    (check "no walk, no run" nil (view-slice-left-fronts none))
    (check "the immediate neighbor fields survive a zero walk" '(2 1 :wall)
           (list (view-slice-lx none) (view-slice-ly none)
                 (view-slice-left-front none))))
  ;; the blit list deals the run out as repeated flank pieces, the
  ;; outermost first, each shifted one cell width and cut at the
  ;; viewport edge (SX mapping the cut into the piece bitmap)
  (let ((planes (view-planes 240 130)))
    (check "row: the run fills the horizon"
           (list (list '(:flank 2 :l) a 0 54 23 22 17)
                 (list '(:flank 2 :l) b 61 54 40 22 0)
                 (list '(:flank 2 :r) b 217 54 23 22 0)
                 (list '(:flank 2 :r) b 178 54 40 22 0)
                 (list '(:flank-door 2 :r) b 139 54 40 22 0)
                 (list '(:front 2) b 100 54 40 22 0))
           (view-blit-list v planes))
    ;; the default knob is the classic single neighbor — the same
    ;; three-house skyline as before the runs existed
    (check "the default draw width shows the classic three houses"
           '((:flank 2 :l) (:flank-door 2 :r) (:front 2))
           (mapcar #'first
                   (view-blit-list (compute-view m 3 3 :north) planes)))
    ;; 0 is the original Bard's Tale look: the one house dead ahead
    (let ((*draw-flanks* 0))
      (check "draw width 0 leaves the lone far house"
             '((:front 2))
             (mapcar #'first
                     (view-blit-list (compute-view m 3 3 :north) planes))))
    ;; the wireframe draws from the same runs
    (check-true "the wireframe view draws the run too"
                (> (length (view-display-list v planes))
                   (length (view-display-list
                            (compute-view m 3 3 :north +view-depth+ 0)
                            planes))))))

;; A closed side ends the walk: the run never slips through a wall.
(let* ((m (parse-map *art* :name "run-stop"))
       (s (first (compute-view m 0 0 :north +view-depth+ 8))))
  (check "a wall beside the corridor leaves no run" nil
         (view-slice-left-fronts s))
  (check "a closed side one cell out ends the run" 1
         (length (view-slice-right-fronts s))))

;; The knob is a RENDERING knob: however wide the machine draws, the
;; automap learns only what OBSERVE always recorded — corridor cells
;; and their immediate neighbors.
(let* ((*draw-flanks* 8)
       (g (new-game (parse-map *row-art* :name "row-map"))))
  (check-true "the immediate neighbor's front is mapped"
              (wall-known-p (game-knowledge g) 2 1 +north+))
  (check "a drawn run cell is never mapped" nil
         (wall-known-p (game-knowledge g) 0 1 +north+)))

;;; ---------------------------------------------------------------------
;;; Wall styles: the blitted view varies tile-pack piece variants per
;;; building (%WALL-STYLE / the STYLE field of the blit records)

(let* ((m (parse-map *art* :name "test"))
       (planes (view-planes 240 130)))
  ;; deterministic: the same view always deals the same styles
  (check "wall styles are deterministic"
         (mapcar #'second (view-blit-list (compute-view m 0 0 :east) planes))
         (mapcar #'second (view-blit-list (compute-view m 0 0 :east) planes)))
  ;; the style keys on the BUILDING cell (beyond the wall), so the
  ;; same wall keeps its style as the party approaches: the front wall
  ;; seen from afar and from up close carries one style
  (let ((far (view-blit-list (compute-view m 0 0 :east) planes))
        (near (view-blit-list (compute-view m 1 0 :east) planes)))
    (check "a building keeps its style as the party approaches"
           (second (assoc '(:front 1) far :test #'equal))
           (second (assoc '(:front 0) near :test #'equal))))
  ;; a LOCATION op's :style pins the building's variant explicitly
  (setf (cell-special m 1 1)
        '((location "The Cooper's" :house :style 7
           :image "gfx/interior-1.iff")))
  (let* ((g (new-game m)))
    (setf (game-x g) 1 (game-y g) 0 (game-facing g) +south+)
    (let ((blits (view-blit-list
                  (compute-view m (game-x g) (game-y g) (game-facing g))
                  planes)))
      (check "a location's :style styles its door piece"
             7 (second (assoc '(:front-door 0) blits :test #'equal)))))
  (setf (cell-special m 1 1) nil))

;;; A building is a MASS, not a cell: every wall of one walled-in
;;; region answers with the same style, so a block-long house wears one
;;; look instead of changing from stone to plaster halfway along its
;;; own front (which is what a per-cell style hash did).

;; A street with a block of houses on each side: rows 0 and 2 are
;; solid mass, row 1 is the street the party walks.
(defparameter *street-art*
"+-+-+-+-+-+
| | | | | |
+-+-+-+-+-+
|@        |
+-+-+-+-+-+
| | | | | |
+-+-+-+-+-+")

(let ((m (parse-map *street-art* :name "street")))
  (check-true "a walled-in cell is building mass" (%cell-solid-p m 3 0))
  (check-true "a cell the party can stand in is not"
              (not (%cell-solid-p m 1 1)))
  ;; the whole block answers with one style, however far along it the
  ;; wall is — the house does not change its look mid-front
  (check "one mass, one style"
         (list (%wall-style m 0 0) (%wall-style m 0 0))
         (list (%wall-style m 2 0) (%wall-style m 4 0)))
  ;; the block across the street is its own building
  (check-true "separate masses may differ"
              (/= (%wall-style m 0 0) (%wall-style m 0 2)))
  ;; a walkable cell keeps the old per-cell hash (dungeon rock behind a
  ;; map edge, and anything a campaign has not walled in)
  (check "a walkable cell falls back to its coordinate hash"
         (%coord-style 3 1) (%wall-style m 3 1)))

;; A LOCATION op anywhere in the mass pins the whole building's style,
;; so the house's other wall cells wear the look the campaign chose for
;; its facade picture.
(let ((m (parse-map *street-art* :name "pinned")))
  (setf (cell-special m 2 0)
        '((location "The Weaver's" :house :style 5
           :facade "gfx/house-1.iff")))
  (check "a location's :style pins its whole building" '(5 5 5)
         (list (%wall-style m 0 0) (%wall-style m 2 0) (%wall-style m 4 0)))
  (check-true "the block across the street is not pinned with it"
              (/= 5 (%wall-style m 0 2)))
  ;; attaching an op drops the cache, so the pin goes when the op does
  (setf (cell-special m 2 0) nil)
  (check-true "dropping the op drops the pin"
              (/= 5 (%wall-style m 0 0))))

;; The view sees it: walking the street, both blocks keep their style
;; at every depth the party looks along.
(let* ((m (parse-map *street-art* :name "street-view"))
       (planes (view-planes 120 100))
       (blits (view-blit-list (compute-view m 0 1 :east) planes))
       (left (remove-if-not (lambda (r) (eq :l (third (first r)))) blits))
       (right (remove-if-not (lambda (r) (eq :r (third (first r)))) blits)))
  (check-true "the street shows both blocks" (and left right))
  (check "the whole left-hand block wears one style" 1
         (length (remove-duplicates (mapcar #'second left))))
  (check "the whole right-hand block wears one style" 1
         (length (remove-duplicates (mapcar #'second right)))))

;;; ---------------------------------------------------------------------
;;; Backdrop slots (ceiling/floor) and the tile-pack manifest

(destructuring-bind (ceiling floor) (backdrop-rects (view-planes 240 130))
  (check "ceiling backdrop slot" '(0 0 240 65) ceiling)
  (check "floor backdrop slot" '(0 65 240 65) floor))

(destructuring-bind (ceiling floor) (backdrop-rects (view-planes 120 100))
  (check "lores ceiling backdrop slot" '(0 0 120 50) ceiling)
  (check "lores floor backdrop slot" '(0 50 120 50) floor))

;; the two slots tile any viewport exactly, split at the horizon
(destructuring-bind (ceiling floor) (backdrop-rects (view-planes 33 17))
  (destructuring-bind (cx cy cw ch) ceiling
    (destructuring-bind (fx fy fw fh) floor
      (check "small-viewport backdrops start at the top-left" '(0 0)
             (list cx cy))
      (check "small-viewport backdrops span the width" '(33 33)
             (list cw fw))
      (check "floor starts where the ceiling ends" (+ cy ch) fy)
      (check "backdrops tile the viewport height" 17 (+ ch fh)))))

;; FIT-TITLE: the plaque under the view clips a zone title wider than
;; the (profile-tunable, since 2026-07-19 narrower) view column instead
;; of overrunning into the log — regression for the 160->120 lores
;; shrink.  Measured with a topaz-8-like 8px/char ruler at the lores
;; plaque width.
(let ((px8 (lambda (s) (* 8 (length s)))))
  (check "fit-title passes a fitting name through unchanged"
         "The Cellar" (fit-title "The Cellar" px8 118))
  (check "fit-title drops trailing characters until the name fits"
         "A Very Long Lo"
         (fit-title "A Very Long Location Name That Overflows" px8 118))
  (check-true "fit-title results always fit the given width"
              (<= (funcall px8 (fit-title
                                "A Very Long Location Name That Overflows"
                                px8 118))
                  118))
  (check "fit-title never shrinks a name below one character"
         "W" (fit-title "W" px8 4)))

;; TITLE-CASE: display titles keep their authored capitalization.
;; STRING-CAPITALIZE was wrong for the map legend and plaque on two
;; counts: it starts a new "word" at any non-alphanumeric character
;; ("Wolfgar's Arms" -> "Wolfgar'S Arms") and it downcases interior
;; capitals the author wrote deliberately.
(check "title-case passes an authored title through unchanged"
       "Wolfgar's Arms & Armour" (title-case "Wolfgar's Arms & Armour"))
(check "title-case upcases the first letter of each word"
       "The Old Cellar" (title-case "the old cellar"))
(check "title-case does not start a word at an apostrophe"
       "The Adventurer's Rest" (title-case "the adventurer's rest"))
(check "title-case keeps interior capitals"
       "McGuffin's" (title-case "mcGuffin's"))
(check "title-case survives the empty string" "" (title-case ""))

(check "gfx-dir defaults to the engine's lores pack"
       (engine-path "data/gfx/") *gfx-dir*)

(let ((manifest (with-output-to-string (s) (print-tile-manifest s))))
  (check "manifest lists every pack file"
         (+ 4 (length (wall-piece-names)))
         (print-tile-manifest (make-broadcast-stream)))
  (check-true "manifest names the wall pieces"
              (search "side-door-2-l.iff" manifest))
  ;; the manifest is read at whichever REPL asks for it, the Amiga's
  ;; included, and that font has no glyphs past ASCII
  (check "the manifest is ASCII throughout" '()
         (remove-if (lambda (c) (< (char-code c) 128))
                    (coerce manifest 'list)))
  ;; both backdrop pairs: a zone takes one or the other, and a pack
  ;; author who only ever hears about ceiling/floor cannot paint a street
  (check-true "manifest names the dark zone's backdrops"
              (and (search "ceiling.iff" manifest)
                   (search "floor.iff" manifest)))
  (check-true "manifest names the open zone's backdrops"
              (and (search "sky.iff" manifest)
                   (search "ground.iff" manifest)))
  (check-true "manifest says only pens 5 and 6 follow the day bands"
              (search "day bands" manifest))
  (check-true "manifest lists the pens this pack owns"
              (search "5 6 7 8 9 10 11 12 13 14 15 16 20 21 22 23" manifest))
  (check-true "manifest states what the engine keeps"
              (and (search "pens 17-19 the mouse pointer" manifest)
                   (search "pens 24-31 the shared figure core" manifest)))
  (check-true "manifest tells travelling art which pens it may use"
              (and (search "Travelling art" manifest)
                   (search "1 2 3 4 24 25 26 27 28 29 30 31" manifest))))

;;; ---------------------------------------------------------------------
;;; Display profiles (src/profiles.lisp): the per-target bundles of
;;; screen geometry, viewport, tile pack and layout tuning.

(check "find-display-profile resolves :lores" *lores-profile*
       (find-display-profile :lores))
(check "find-display-profile resolves :hires" *hires-profile*
       (find-display-profile :hires))
(check "find-display-profile passes a profile through" *lores-profile*
       (find-display-profile *lores-profile*))
(check-true "find-display-profile rejects an unknown name"
            (handler-case (progn (find-display-profile :vga) nil)
              (error () t)))

(check "the default profile's pack is the default gfx-dir"
       (display-profile-gfx-dir *display-profile*) *gfx-dir*)
(check "the default profile's viewport is the default viewport"
       (list (display-profile-fp-width *display-profile*)
             (display-profile-fp-height *display-profile*))
       (list *fp-view-width* *fp-view-height*))

(check "the default profile's draw depth is the default draw depth"
       (display-profile-draw-depth *display-profile*) *draw-depth*)

;; :lores opens 200 lines, not PAL's 256, so one screen serves PAL and
;; NTSC alike — an NTSC machine has no 256-line mode to fall back on.
;; The window mode must match it: both displays are meant to lay out
;; identically, and %AMIGA-LAYOUT is the same code for both.
(check ":lores is a 320x200 screen, NTSC and PAL alike"
       '(320 200)
       (list (display-profile-screen-width *lores-profile*)
             (display-profile-screen-height *lores-profile*)))
(dolist (p *display-profiles*)
  (check (format nil "~S's window matches its screen"
                 (display-profile-name p))
         (list (display-profile-screen-width p)
               (display-profile-screen-height p))
         (list (display-profile-win-width p)
               (display-profile-win-height p))))
;; SCREEN-HEIGHT-FOR: the layout height is fixed, the screen grows to
;; the display it landed on.  The display database answers with the
;; mode's real height (AMIGA.INTUITION:DISPLAY-MODE-HEIGHT), so there
;; is no PAL/NTSC branch to test — just the clamp, which is pure
;; arithmetic and belongs here rather than on the Amiga.
(check ":lores on a PAL display opens the full 256" 256
       (screen-height-for *lores-profile* 256))
(check ":lores on an NTSC display opens 200" 200
       (screen-height-for *lores-profile* 200))
(check ":lores never grows past its cap on a tall RTG display" 256
       (screen-height-for *lores-profile* 480))
(check ":lores never shrinks below its layout" 200
       (screen-height-for *lores-profile* 128))
(check ":lores falls back to the layout when the database is silent" 200
       (screen-height-for *lores-profile* nil))
;; :hires is already the full PAL height and declares no cap, so it
;; opens its own size whatever the display says
(dolist (h '(200 256 480 nil))
  (check (format nil ":hires ignores a ~A-row display" h)
         256 (screen-height-for *hires-profile* h)))
;; whatever comes back, the layout must still fit inside it — that is
;; the invariant the window relies on when it clamps to the layout
(dolist (p *display-profiles*)
  (dolist (h '(200 256 480 128 nil))
    (check-true (format nil "~S's layout fits its screen on a ~A-row display"
                        (display-profile-name p) h)
                (<= (display-profile-screen-height p)
                    (screen-height-for p h)))))

;; SCREEN-ASK-HEIGHT: what BestModeIDA is asked for, which is NOT the
;; layout height.  Handing it :lores' 200 rows on a machine that has
;; both a chipset display and an RTG board makes 320x200 an exact hit
;; on an RTG mode, so the game lands on the graphics card and its
;; pixels get resampled instead of shown; asking for the tallest
;; display the profile would take (256) puts PAL's own lores mode back
;; in front.  A machine with no 320x256 answers with its best fit
;; regardless, and SCREEN-HEIGHT-FOR clamps that back to the layout.
(check ":lores asks the database for its full 256, not the layout's 200"
       256 (screen-ask-height *lores-profile*))
(check ":hires asks for its own height (it declares no cap)"
       256 (screen-ask-height *hires-profile*))
(dolist (p *display-profiles*)
  ;; the ask is never SHORTER than the layout — a mode that cannot show
  ;; the layout is never the one to prefer
  (check-true (format nil "~S asks for at least its layout"
                      (display-profile-name p))
              (>= (screen-ask-height p) (display-profile-screen-height p)))
  ;; and never taller than a screen it would actually open, so the mode
  ;; asked for and the screen opened cannot describe different displays
  (check (format nil "~S's ask is a height it would open"
                 (display-profile-name p))
         (screen-ask-height p)
         (screen-height-for p (screen-ask-height p))))

;; The view's drop shadow hangs into the gap under the plaque, so it
;; must stay strictly shallower than the gap or it collides with the
;; roster's CHARACTER header — which is what a 3px shadow against a
;; 4px gap did, reading as one heavy band rather than a shadow.
(check-true "the view's shadow clears the roster header"
            (< +view-shadow+ +roster-gap+))
;; Counted in the same inclusive rows %CHROME-FRAMES draws: the shadow's
;; last row is PB + +VIEW-SHADOW+, the header's first is
;; PB + +ROSTER-GAP+, so the clear grey between them is the difference
;; less the header row itself.
(check "the shadow leaves exactly one clear row above the header" 1
       (- +roster-gap+ +view-shadow+ 1))

;; The viewport and the furniture under it have to fit that screen: the
;; chrome pad, the view at its full asset height, the plaque, and the
;; roster's header plus +PARTY-LIMIT+ solid-set rows, ending on or
;; above the layout's last usable row.  Overshoot and the layout does
;; not shrink gracefully — it drops the view to the wireframe (see
;; *LORES-PROFILE*).  Topaz 8 is the game's own font
;; (%WITH-GAME-FONT), so its 8px glyph box and 10px leaded line are
;; known here rather than measured.
;;
;; This walks the same top-down chain %AMIGA-LAYOUT does, in the same
;; inclusive pixel rows, so the two cannot disagree: the Amiga suite
;; checks the live layout against these very numbers.
(let* ((row-h 8) (line-h 10)
       (p *lores-profile*)
       (by (display-profile-pad-y p))          ; content top row
       (bottom (- (display-profile-screen-height p)
                  (display-profile-pad-y p)))  ; last usable row
       (plaque-y (+ by (display-profile-fp-height p) 1))
       (plaque-b (+ plaque-y line-h 2))
       (party-y (+ plaque-b +roster-gap+ row-h))
       (last-row (+ party-y (* row-h +party-limit+) -1)))
  (check-true (format nil ":lores fills its 200 lines without overflowing ~
(roster ends on row ~D of ~D)" last-row bottom)
              (<= last-row bottom))
  ;; and it is not wasteful either: one more roster row would not fit,
  ;; so nothing above the roster is quietly hogging pixels
  (check-true ":lores leaves no room for another roster row"
              (> (+ last-row row-h) bottom))
  ;; the column comes out exact — row 7 lands ON the last usable row.
  ;; If this ever loosens, the pixel went somewhere; find out where
  ;; before spending it.
  (check ":lores roster row 7 ends on the last usable row"
         bottom last-row)
  ;; The message column's own budget, walked the same way
  ;; %AMIGA-LAYOUT walks it: the column runs from BY to the plaque's
  ;; bottom, the effect strip takes its foot (bottom row flush with
  ;; the plaque's), and the white page ends four pixels of gap above
  ;; the strip.  What is left is what the log, the shop and the
  ;; character sheet have to say their piece in, and it is measured
  ;; in whole small-face rows — a page short of one is a page that
  ;; drops its last option (see FIT-MENU-LINES).
  (let* ((col-h (- (1+ plaque-b) by))
         (band-h (display-profile-band-height p))
         (page-b (- (+ by col-h) band-h 4))
         (rows (floor (- (- page-b by) 2) +microfont-line-height+)))
    ;; the 20px strip is paid for out of dead seams (the profile's
    ;; comment itemizes them), NOT out of the page — the rows check
    ;; below is the receipt.  One more strip pixel would take a row.
    (check ":lores spends the seam harvest on the effect strip" 20 band-h)
    (check ":lores gives the message page eleven small-face rows"
           11 rows)))

;; *HIRES-PROFILE*'s comment claims the same kind of receipt as
;; lores' eleven rows above: fourteen small-face rows, out of its
;; taller 130px viewport and its own 24px strip — walked the same way.
(let* ((line-h 10)
       (p *hires-profile*)
       (by (display-profile-pad-y p))
       (plaque-y (+ by (display-profile-fp-height p) 1))
       (plaque-b (+ plaque-y line-h 2))
       (col-h (- (1+ plaque-b) by))
       (band-h (display-profile-band-height p))
       (page-b (- (+ by col-h) band-h 4))
       (rows (floor (- (- page-b by) 2) +microfont-line-height+)))
  (check ":hires spends its band-height on the effect strip" 24 band-h)
  (check ":hires gives the message page fourteen small-face rows"
         14 rows))

;; The help page's own budget, walked the way %HELP-PAGE-BOX walks it:
;; the reference draws in the microfont's small face (topaz held
;; barely half of it on the 200-line layout), the box runs from BY+8
;; down to the content bottom, and a reference taller than the box
;; windows and scrolls (u/d, or the scrollbar).
(let* ((p *lores-profile*)
       (by (display-profile-pad-y p))
       (bottom (- (display-profile-screen-height p)
                  (display-profile-pad-y p)))
       (lines (help-lines t))
       (py (+ by 8))
       (ph (min (- bottom py 4)
                (+ (* +microfont-line-height+ (length lines)) 12)))
       (rows (floor (- ph 8) +microfont-line-height+)))
  (check ":lores gives the help page twenty small-face rows" 20 rows)
  ;; the whole reference is two windows at most: one page turn (d)
  ;; reaches its tail
  (check-true "one page turn reaches the reference's tail"
              (>= (* 2 rows) (length lines)))
  ;; the longest line names the page's width, and the content affords
  ;; it — no help line may truncate
  (check-true "the longest help line fits the lores content"
              (<= (+ 16 6 (* +microfont-small-advance+
                             (reduce #'max lines :key #'length)))
                  (- (display-profile-screen-width p)
                     (* 2 (display-profile-pad-x p))
                     4))))

;; Every profile must declare a draw distance the plane set can serve.
(dolist (p *display-profiles*)
  (check-true (format nil "~S declares a sane draw depth"
                      (display-profile-name p))
              (let ((d (display-profile-draw-depth p)))
                (and (integerp d) (<= 1 d +view-depth+)))))

;; The per-target defaults, pinned: the small viewport affords the full
;; view, the one that blits twice the area gives up its deepest level.
(check ":lores draws the full view" +view-depth+
       (display-profile-draw-depth *lores-profile*))
(check ":hires gives up its deepest level" 3
       (display-profile-draw-depth *hires-profile*))

;; Draw width follows the same profile-default scheme.
(check "the default profile's draw width is the default draw width"
       (display-profile-draw-flanks *display-profile*) *draw-flanks*)
(dolist (p *display-profiles*)
  (check-true (format nil "~S declares a sane draw width"
                      (display-profile-name p))
              (let ((w (display-profile-draw-flanks p)))
                (and (integerp w) (<= 0 w +view-flanks+)))))
;; both ship the classic single flank; the knob buys the wider street
(check ":lores draws the classic single flank" 1
       (display-profile-draw-flanks *lores-profile*))
(check ":hires draws the classic single flank" 1
       (display-profile-draw-flanks *hires-profile*))

;; The roster's number columns are right-aligned: ROSTER-CELL pushes
;; short values towards the field's right edge so the digits (and the
;; heading over them) line up down the table.
(check "a full-width roster value starts at its column" 15
       (roster-cell 15 "112"))
(check "a two-character roster value gives up one cell" 16
       (roster-cell 15 "10"))
(check "a one-character roster value gives up two cells" 17
       (roster-cell 15 "4"))
(check "a negative armor class still right-aligns" 16
       (roster-cell 15 "-2"))
(check "a heading right-aligns like the values under it" 16
       (roster-cell 15 "AC"))
(check "an oversized value starts at its column, never before it" 15
       (roster-cell 15 "1234"))
(check "an explicit field width overrides the default" 18
       (roster-cell 15 "AC" 5))
(check "the empty string sits at the field's right edge" 18
       (roster-cell 15 ""))

;; Every profile must leave each numeric column room for the full
;; field, or a wide value would collide with the column after it.
(dolist (p *display-profiles*)
  (let ((cols (display-profile-roster-cols p))
        (order '(:ac :hit :hpts :spl :spts :cl))
        (tight '()))
    (loop for (key next) on order
          while next
          do (when (< (- (getf cols next) (getf cols key))
                      +roster-num-cells+)
               (push key tight)))
    (check (format nil "~S leaves every number column its full field"
                   (display-profile-name p))
           nil tight)))

(let ((outer-w *fp-view-width*)
      (outer-dir *gfx-dir*)
      (outer-depth *draw-depth*)
      (outer-flanks *draw-flanks*))
  (with-display-profile (:hires)
    (check "with-display-profile binds the profile" :hires
           (display-profile-name *display-profile*))
    (check "with-display-profile binds the viewport" '(240 130)
           (list *fp-view-width* *fp-view-height*))
    (check "with-display-profile binds the pack dir"
           (display-profile-gfx-dir *hires-profile*) *gfx-dir*)
    (check "with-display-profile binds the draw depth"
           (display-profile-draw-depth *hires-profile*) *draw-depth*)
    (check "with-display-profile binds the draw width"
           (display-profile-draw-flanks *hires-profile*) *draw-flanks*))
  (check "with-display-profile restores the viewport"
         outer-w *fp-view-width*)
  (check "with-display-profile restores the pack dir"
         outer-dir *gfx-dir*)
  (check "with-display-profile restores the draw depth"
         outer-depth *draw-depth*)
  (check "with-display-profile restores the draw width"
         outer-flanks *draw-flanks*))

;; The profile supplies the DEFAULT only: a binding made inside the
;; macro is what PLAY-AMIGA's :DRAW-DEPTH does, and it wins.
(with-display-profile (:hires)
  (let ((*draw-depth* 2))
    (check "a draw depth bound inside the profile wins" 2 *draw-depth*))
  (let ((*draw-flanks* 8))
    (check "a draw width bound inside the profile wins" 8 *draw-flanks*)))

(with-display-profile (:hires)
  (let ((manifest (with-output-to-string (s) (print-tile-manifest s))))
    (check-true "hires manifest names its viewport"
                (search "240x130 viewport" manifest))
    (check-true "hires manifest states the 16-color palette contract"
                (search "5 6 7 8 9 10 11 12 13 14 15" manifest))
    ;; 16 colors have no room for the core, so hires is a wall-pack
    ;; target only — its travelling art is limited to the UI pens.
    (check-true "hires manifest claims no figure core"
                (not (search "figure core" manifest)))))

;;; ---------------------------------------------------------------------
;;; Cookie-cut mask bytes (the Amiga transparent-blit source): a 1 bit
;;; per opaque pixel, MSB first, rows padded to a 16-pixel word.

(multiple-value-bind (m bpr)
    (mask-bytes 10 2 (let ((p (make-array 20 :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
                       (setf (aref p 2) 1 (aref p 3) 1 (aref p 8) 1)
                       p))
  (check "mask row is word-aligned" 2 bpr)
  (check "mask covers every row" 4 (length m))
  (check "mask row0 byte0 marks pixels 2,3" #x30 (aref m 0))
  (check "mask row0 byte1 marks pixel 8" #x80 (aref m 1))
  (check "mask clears an all-transparent row" 0 (+ (aref m 2) (aref m 3))))

;; the transparent key need not be pen 0 (pen 3 here) — and pen 0 is not
;; special: it counts as opaque when it isn't the key
(check "mask honors a non-zero transparent key" #xEF
       (aref (mask-bytes 8 1 #(0 1 2 3 4 5 6 7) 3) 0))

;; A width whose row padding runs past the last pixel group (17 px: 3
;; groups of pixels, but 4 bytes per word-aligned row) must leave the
;; padding byte clear — the mask is blitted at full row width.
(multiple-value-bind (m bpr)
    (mask-bytes 17 1 (let ((p (make-array 17 :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
                       (setf (aref p 16) 1) ; the lone pixel of group 2
                       p))
  (check "padded mask row is word-aligned" 4 bpr)
  (check "mask marks the last pixel before the padding" #x80 (aref m 2))
  (check "mask leaves the row padding clear" 0 (aref m 3)))

(check-true "image-transparent-p spots the key"
            (image-transparent-p (make-image 2 2 2)))       ; all pen 0
(check-true "image-transparent-p is nil when fully painted"
            (not (image-transparent-p
                  (let ((img (make-image 2 2 2)))
                    (dotimes (y 2 img) (dotimes (x 2)
                                         (setf (pixel-ref img x y) 1)))))))

;;; ---------------------------------------------------------------------
;;; Knowledge

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (k (game-knowledge g)))
  (check-true "start cell explored" (cell-explored-p k 0 0))
  (check "unvisited cell not explored" nil (cell-explored-p k 1 0))
  (check-true "start cell walls known" (wall-known-p k 0 0 :north))
  ;; The east side of the start cell is open, so the neighbor's front wall
  ;; is visible from the start — and recorded.
  (check-true "side cell front wall seen through opening"
              (wall-known-p k 1 0 :north))
  (check "out-of-view walls unknown" nil (wall-known-p k 1 1 :north))

  (turn-right g)
  (move-party g :forward)
  (check-true "explored after moving" (cell-explored-p k 1 0))
  (check-true "walls known after moving" (wall-known-p k 1 0 :south)))

;;; ---------------------------------------------------------------------
;;; Rendering

;; Omniscient render round-trips the source art (start glyph is not a
;; feature, so its cell renders blank).
(let ((m (parse-map *art* :name "test")))
  (check "omniscient render round-trips art"
         "+-+-+-+
|   | |
+ +D+ +
| |  <|
+-+-+-+"
         (render-dungeon m)))

;; Party arrow in the full view.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (turn-right g)
  (move-party g :forward)
  (check "full render with party arrow"
         "+-+-+-+
|  >| |
+ +D+ +
| |  <|
+-+-+-+"
         (render-game g :full t)))

;; Knowledge-filtered render of a fresh game: the start cell's walls plus
;; the neighbor's front wall (seen through the open east side) and the
;; party arrow.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (check "knowledge render shows explored cell and seen walls"
         "+-+-+
|^
+

"
         (render-game g)))

;; After walking through the door the door and new walls appear; the
;; unexplored feature cell (2,1) stays hidden.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (turn-right g)
  (move-party g :forward)
  (turn-right g)
  (move-party g :forward)
  (let ((view (render-game g)))
    (check-true "door visible after passing it" (search "D" view))
    (check "hidden feature not rendered" nil (search "<" view))))

;; Found locations draw their legend marker on their cell — the map
;; shows WHERE the "1" of the legend is, not just that it exists.  The
;; marker covers the cell's feature glyph; full mode marks every place.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (setf (cell-special m 2 1) '((location "Wolfgar's Arms" :shop)))
  (check "full render draws the legend marker on its cell"
         "+-+-+-+
|^  | |
+ +D+ +
| |  1|
+-+-+-+"
         (render-game g :full t))
  (check "an unfound location draws no marker" nil
         (search "1" (render-game g)))
  (know-cell (game-knowledge g) 2 1)
  (check-true "a found location draws its marker"
              (search "1" (render-game g)))
  (check "the marker covers the cell's feature glyph" nil
         (search "<" (render-game g))))

;;; ---------------------------------------------------------------------
;;; First-person ASCII renderer

;; Facing north at the start of *art*: solid left wall (trapezoid), open
;; right side showing the neighbor's front wall, solid front at plane 1.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (lines (%split-lines (render-first-person g))))
  (check "fp viewport height" 17 (length lines))
  (check "fp front wall and right opening line"
         "|     +-------------------+-----+"
         (nth 3 lines))
  (check-true "fp left wall receding edge" (find #\\ (nth 1 lines))))

;; A door straight ahead renders a 'D' marker centered on the front wall.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (turn-right g)
  (move-party g :forward)
  (turn-right g)                        ; at (1,0) facing the door south
  (let ((lines (%split-lines (render-first-person g))))
    (check "fp door marker centered" #\D (char (nth 8 lines) 16))))

;;; ---------------------------------------------------------------------
;;; Sample map loads (the committed fixture world, tests/world/)

(let ((m (load-map-file "tests/world/keep.map")))
  (check "keep width" 5 (dungeon-map-width m))
  (check "keep height" 1 (dungeon-map-height m))
  (check "keep stairs down" #\> (cell-feature m 4 0)))
(let ((m (load-map-file "tests/world/crypt.map")))
  (check "crypt width" 3 (dungeon-map-width m))
  (check "crypt height" 1 (dungeon-map-height m))
  (check "crypt ladder up" #\< (cell-feature m 2 0)))

;;; ---------------------------------------------------------------------
;;; The map cache sidecar (.mapc): written on parse, byte-format
;;; round-trip, actually consulted when newer than the map, and
;;; invalidated the moment the map is edited.

(let ((path "tests/tmp-cache.map")
      (art "+-+-+-+
|@ D  |
+-+-+ +
|<|  >|
+-+-+-+
(zone :kind :city :title \"cachetown\" :gfx \"gfx-town/\")
(special (1 0) (message \"hi\"))
"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string art s))
  ;; the sidecar only wins while strictly newer than the map, so let the
  ;; clock tick past the map's write second before the parse creates it
  (sleep 1.1)
  (let ((m1 (load-map-file path)))
    (check-true "map parse leaves a .mapc sidecar"
                (not (null (probe-file (%map-cache-path path)))))
    ;; decode the sidecar directly — every art-derived field round-trips
    (let* ((buf (with-open-file (s (%map-cache-path path)
                                   :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s)
                                       :element-type '(unsigned-byte 8))))
                    (read-sequence v s)
                    v)))
           (m2 (%decode-map-cache buf path nil :north)))
      (check-true "cache round-trips the wall grid"
                  (equalp (dungeon-map-walls m1) (dungeon-map-walls m2)))
      (check-true "cache round-trips the features"
                  (equalp (dungeon-map-features m1)
                          (dungeon-map-features m2)))
      (check "cache round-trips the start cell"
             (list (dungeon-map-start-x m1) (dungeon-map-start-y m1))
             (list (dungeon-map-start-x m2) (dungeon-map-start-y m2)))
      (check "cache reload re-reads the zone form" "cachetown"
             (dungeon-map-title m2))
      (check "cache reload re-reads the specials" '((message "hi"))
             (cell-special m2 1 0)))
    ;; a load with the sidecar newer than the map must actually use it:
    ;; patch one wall byte in the sidecar (wall -> door) and watch the
    ;; patched value come back
    (let* ((cpath (%map-cache-path path))
           (bytes (with-open-file (s cpath :element-type '(unsigned-byte 8))
                    (let ((v (make-array (file-length s)
                                         :element-type '(unsigned-byte 8))))
                      (read-sequence v s)
                      v))))
      (check "cache patch target is the north wall of (0,0)"
             (%wall-code (cell-wall m1 0 0 :north)) (aref bytes 16))
      (setf (aref bytes 16) 2)          ; :wall/:open -> :door
      (with-open-file (s cpath :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence bytes s))
      (check "a newer sidecar is used" :door
             (cell-wall (load-map-file path) 0 0 :north)))
    ;; editing the map invalidates the sidecar (same-second edits
    ;; included: the cache must be STRICTLY newer than the map)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string art s))
    (check "an edited map beats the sidecar"
           (cell-wall m1 0 0 :north)
           (cell-wall (load-map-file path) 0 0 :north))
    ;; a corrupt sidecar is rejected loudly by the decoder and silently
    ;; (fall back to parsing) by the loader
    (check-error "corrupt cache magic is rejected"
      (%decode-map-cache
       (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7)
       path nil :north))
    (check "a garbage sidecar falls back to the parse"
           (dungeon-map-width m1)
           (progn
             (with-open-file (s (%map-cache-path path)
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
               (write-sequence (make-array 40 :element-type '(unsigned-byte 8)
                                             :initial-element 7)
                               s))
             (dungeon-map-width (load-map-file path)))))
  (delete-file path)
  (when (probe-file (%map-cache-path path))
    (delete-file (%map-cache-path path))))

;;; ---------------------------------------------------------------------
;;; Dice

(multiple-value-bind (c s b) (parse-dice "2d6+1")
  (check "parse-dice 2d6+1" '(2 6 1) (list c s b)))
(multiple-value-bind (c s b) (parse-dice "1d8")
  (check "parse-dice 1d8" '(1 8 0) (list c s b)))
(multiple-value-bind (c s b) (parse-dice "3d4-1")
  (check "parse-dice 3d4-1" '(3 4 -1) (list c s b)))
(multiple-value-bind (c s b) (parse-dice 5)
  (check "parse-dice integer constant" '(0 0 5) (list c s b)))
(check-error "parse-dice rejects garbage" (parse-dice "banana"))
(check-error "parse-dice rejects zero dice" (parse-dice "0d6"))
(check-error "parse-dice rejects zero sides" (parse-dice "1d0"))

(with-rng (3 4)
  (check "roll-dice 2d6+1 scripted" 10 (roll-dice "2d6+1")))
(with-rng ()
  (check "roll-dice exhausted script rolls ones" 1 (roll-dice "1d8")))
(check "roll-dice integer is constant" 7 (roll-dice 7))
(with-rng (13)
  (check "scripted roll wraps mod n" 3 (roll 10)))

(multiple-value-bind (lo hi) (dice-range "2d6+1")
  (check "dice-range 2d6+1 spans low to high" '(3 13) (list lo hi)))
(multiple-value-bind (lo hi) (dice-range 5)
  (check "dice-range on a constant is a single point" '(5 5) (list lo hi)))
(multiple-value-bind (lo hi) (dice-range "1d1")
  (check "dice-range on 1d1 is a single point" '(1 1) (list lo hi)))
(check "dice-range-text spans as MIN-MAX" "4-16" (dice-range-text "4d4"))
(check "dice-range-text collapses a constant to a bare number"
       "5" (dice-range-text 5))
(check "dice-range-text collapses 1d1 to a bare number"
       "1" (dice-range-text "1d1"))

;;; ---------------------------------------------------------------------
;;; Events and story flags

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (order '()))
  (on-event g :ping (lambda (game x) (declare (ignore game))
                      (push (list :a x) order)))
  (on-event g :ping (lambda (game x) (declare (ignore game))
                      (push (list :b x) order)))
  (emit g :ping 7)
  (check "handlers run in subscription order" '((:a 7) (:b 7))
         (nreverse order))
  (check "emit without subscribers is quiet" nil
         (handler-case (progn (emit g :nobody-listens) nil)
           (error () :boom))))

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (say g "hello ~D" 5)
  (check "say formats into a :message event" '("hello 5") (funcall msgs)))

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (check "unset flag is nil" nil (flag g :quest))
  (set-flag g :quest)
  (check "set-flag defaults to t" t (flag g :quest))
  (set-flag g :quest 42)
  (check "set-flag with value" 42 (flag g :quest))
  (set-flag g '(:door "cellar" 1) :open)
  (check "flags use equal keys" :open (flag g '(:door "cellar" 1)))
  (clear-flag g :quest)
  (check "clear-flag" nil (flag g :quest)))

;; The message log: the Bard's Tale text column's backing store.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (log (attach-message-log g :limit 3)))
  (check "fresh log is empty" '() (log-recent log 10))
  (say g "one")
  (say g "two")
  (check "log-recent returns oldest first" '("one" "two")
         (log-recent log 10))
  (check "log-recent trailing lines only" '("two") (log-recent log 1))
  (say g "three")
  (say g "four")
  (check "log ring drops the oldest beyond the limit"
         '("two" "three" "four") (log-recent log 10))
  (log-message log "five")
  (check "log-message appends directly" '("three" "four" "five")
         (log-recent log 10)))

;; EXPIRE-MESSAGES: the board clears itself of old news — the
;; front-ends call it from their idle beat and redraw when it returns
;; true.  NOW is a parameter, so the tests age lines without waiting.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (log (attach-message-log g))
       (later (+ (get-internal-real-time)
                 (* (1+ *message-ttl*) internal-time-units-per-second))))
  (say g "old news")
  (check "a fresh line survives the sweep" nil
         (expire-messages log))
  (check "the fresh line stays on the board" '("old news")
         (log-recent log 10))
  (check-true "an over-aged line is swept" (expire-messages log later))
  (check "the board is clear after the sweep" '() (log-recent log 10))
  (check "a sweep with nothing to drop reports so" nil
         (expire-messages log later))
  (let ((*message-ttl* nil))
    (say g "kept")
    (check "TTL nil never sweeps" nil (expire-messages log later))
    (check "TTL nil keeps the line" '("kept") (log-recent log 10))))

;; LOG-LENGTH / LOG-SINCE: the combat round's own transcript page —
;; the front-end marks the log at round start and draws only what the
;; round said.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (log (attach-message-log g)))
  (check "log-length counts the lines" 0 (log-length log))
  (say g "before")
  (check "log-length follows the log" 1 (log-length log))
  (let ((mark (log-length log)))
    (say g "-- Round 1 --")
    (say g "a hit")
    (check "log-since shows the round only, oldest first"
           '("-- Round 1 --" "a hit") (log-since log mark))
    (check "log-since at the tip is empty" '()
           (log-since log (log-length log)))))

;; Word wrap for the text column (the Amiga log wraps long messages).
(check "wrap: short text passes through" '("short") (wrap-text "short" 10))
(check "wrap: exact width does not wrap" '("12345") (wrap-text "12345" 5))
(check "wrap: empty string yields one empty line" '("") (wrap-text "" 10))
(check "wrap: breaks at word boundaries"
       '("El Cid hits the" "giant rat for 2" "damage.")
       (wrap-text "El Cid hits the giant rat for 2 damage." 16))
(check "wrap: space at the boundary"
       '("one two" "three") (wrap-text "one two three" 7))
(check "wrap: long word breaks hard"
       '("aaaa" "aaaa" "aa") (wrap-text "aaaaaaaaaa" 4))
(check "wrap: width floor of 1" '("a" "b") (wrap-text "ab" 0))
(check-true "wrap: every line fits the width"
            (every (lambda (line) (<= (length line) 12))
                   (wrap-text "the quick brown fox jumps over the lazy dog" 12)))

;; wrap-message: a leading blank line separates each message; content
;; wraps to the full width with no prefix.
(check "wrap-message: single line led by a blank"
       '("" "short") (wrap-message "short" 10))
(check "wrap-message: continuation lines are full width, no indent"
       '("" "one two" "three") (wrap-message "one two three" 9))
(check-true "wrap-message: every line fits the width"
            (every (lambda (line) (<= (length line) 16))
                   (wrap-message "El Cid hits the giant rat for 2 damage." 16)))
(check "wrap-message: narrow width floor"
       '("" "a" "b") (wrap-message "ab" 0))

;; Structured menu lines: an option line carries the key that picks it
;; (MENU-OPTION / MENU-NUMBERED), plain lines stay strings, and the
;; footer hints' bracket tokens locate as clickable spans
;; (MENU-KEY-SPANS) — the pointing front-end turns clicks on either
;; into key presses.
(check "menu-option pairs text with its key" '("3) row" . #\3)
       (menu-option #\3 "3) row"))
(check "menu-line-text unwraps an option line" "3) row"
       (menu-line-text (menu-option #\3 "3) row")))
(check "menu-line-text passes a plain line through" "plain"
       (menu-line-text "plain"))
(check "menu-line-key reads the option key" #\7
       (menu-line-key (menu-option #\7 "x")))
(check "menu-line-key of a plain line is NIL" nil (menu-line-key "x"))
(check "menu-numbered attaches the digit" '("2) b" . #\2)
       (menu-numbered 2 "2) b"))
(check "menu-numbered past nine stays plain" "10) c"
       (menu-numbered 10 "10) c"))
(check "menu-texts strips a mixed list" '("a" "1) b" "c")
       (menu-texts (list "a" (menu-numbered 1 "1) b") "c")))
(check "wrap-menu-line carries the key onto every row"
       '(("1) one two" . #\1) ("three" . #\1))
       (wrap-menu-line (menu-option #\1 "1) one two three") 10))
(check "wrap-menu-line leaves plain rows plain"
       '("one two" "three") (wrap-menu-line "one two three" 7))
;; menu-key-spans: the "[s] sell  [Esc] back" footer convention
(check "spans: single-char and Esc tokens, ranges skipped"
       '((11 19 #\s) (21 31 #\Escape))
       (menu-key-spans "[1-9] buy  [s] sell  [Esc] back"))
(check "spans: a span runs over its words to the line's end"
       '((0 21 #\d))
       (menu-key-spans "[d] down the trapdoor"))
(check "spans: Return commits, Esc cancels"
       '((0 13 #\Return) (15 27 #\Escape))
       (menu-key-spans "[Return] save  [Esc] cancel"))
(check "spans: no brackets, no spans" '() (menu-key-spans "plain text"))
(check "spans: an unmatched bracket yields nothing" '()
       (menu-key-spans "an [unclosed token"))
(check "spans: [n] new name parses" '((17 29 #\n))
       (menu-key-spans "[1-9] overwrite  [n] new name"))
(check "spans: BT-style [S]ell hints parse, span over the word"
       '((0 6 #\S) (8 19 #\G))
       (menu-key-spans "[S]ell  [G]old pool"))

;; Footer hint wrapping: a hint row breaks only BETWEEN options (at
;; the two-space gaps), never inside one — no more "[G]old" on one row
;; and "pool" on the next.
(check "hint-line-p spots a bracket hint row" t
       (hint-line-p "[G]old pool"))
(check "hint-line-p leaves plain prose alone" nil
       (hint-line-p "Who is shopping?"))
(check "hint-line-p leaves option lines alone" nil
       (hint-line-p (menu-option #\g "[G]old pool")))
(check "hint wrap keeps whole options per row"
       '("[1-9] buy  [S]ell" "[G]old pool  [Esc] back")
       (wrap-hint-line "[1-9] buy  [S]ell  [G]old pool  [Esc] back" 27))
(check "hint wrap passes a fitting line through"
       '("[1-9] buy  [S]ell")
       (wrap-hint-line "[1-9] buy  [S]ell" 27))
(check "hint wrap goes one per row when each gap overflows"
       '("[G]old pool" "[Esc] back")
       (wrap-hint-line "[G]old pool  [Esc] back" 14))
(check "wrap-menu-line routes hint rows through the hint wrap"
       '("[G]old pool" "[Esc] back")
       (wrap-menu-line "[G]old pool  [Esc] back" 14))

;; FIT-MENU-LINES: the takeover page's fitting policy, pure so the
;; host suite can pin it.  One option per row while the page has the
;; rows; under pressure the hint rows pack (whole options only), then
;; the blank spacers go.
(let ((lines '("*** Shop ***" "" "1) Sword" ""
               "[1-9] buy" "[S]ell" "[Esc] back")))
  (check "a roomy page keeps the vertical options"
         '("*** Shop ***" "" "1) Sword" ""
           "[1-9] buy" "[S]ell" "[Esc] back")
         (menu-texts (fit-menu-lines lines 12 27)))
  (check "a tight page packs the hint rows first"
         '("*** Shop ***" "" "1) Sword" ""
           "[1-9] buy  [S]ell" "[Esc] back")
         (menu-texts (fit-menu-lines lines 6 27)))
  (check "a tighter page then drops the spacers"
         '("*** Shop ***" "1) Sword"
           "[1-9] buy  [S]ell" "[Esc] back")
         (menu-texts (fit-menu-lines lines 4 27))))

;; The last squeeze: the OPTION rows (the bracketless convention the
;; pages actually use now) pack onto shared rows too, once packing the
;; hints and dropping the spacers has not been enough.  Without this a
;; short page loses its foot — and the foot is where the navigation is.
(let ((lines (list "*** Sheet ***" ""
                   (menu-option #\p "Pool gold") ""
                   (menu-option #\t "Trade gold") ""
                   (menu-option #\o "Order party") ""
                   (menu-next-option))))
  (check "a roomy page leaves the option rows alone"
         '("*** Sheet ***" "" "Pool gold" "" "Trade gold" ""
           "Order party" "" "           NEXT")
         (menu-texts (fit-menu-lines lines 12 27)))
  ;; spacers first, options still one per row while that fits
  (check "a tight page drops the spacers before it packs"
         '("*** Sheet ***" "Pool gold" "Trade gold"
           "Order party" "           NEXT")
         (menu-texts (fit-menu-lines lines 5 27)))
  ;; a page one row short packs no more than that row buys: the head
  ;; of the run shares, and the foot — NEXT — keeps standing alone
  (check "a tighter page packs only what the overflow demands"
         '("*** Sheet ***" "Pool gold  Trade gold"
           "Order party" "           NEXT")
         (menu-texts (fit-menu-lines lines 4 27)))
  ;; and a page shorter still packs the whole run — NEXT gives up the
  ;; padding that centered it and rides along rather than falling off
  ;; the foot
  (check "a shorter page still packs the options, NEXT included"
         '("*** Sheet ***" "Pool gold  Trade gold" "Order party  NEXT")
         (menu-texts (fit-menu-lines lines 3 27)))
  ;; the packed row still picks per option: each keeps its own span
  (let ((packed (second (fit-menu-lines lines 4 27))))
    (check "a packed row is no longer a whole-row option" nil
           (menu-line-key packed))
    (check "a packed row carries a span per option"
           '((0 9 #\p) (11 21 #\t))
           (menu-line-spans packed))
    (check "a plain line carries no spans" nil (menu-line-spans "x"))
    (check "an unpacked option carries no spans" nil
           (menu-line-spans (menu-option #\p "Pool gold")))))

;; A numbered list row is never packed: its number reads down the
;; column against its neighbours, and a stock item run together with
;; the page's commands would read as one of them.
(let ((lines (list "*** Shop ***"
                   (menu-numbered 1 "1) Sword  30 gp")
                   (menu-numbered 2 "2) Mace  60 gp")
                   (menu-option #\s "Sell")
                   (menu-option #\p "Pool gold"))))
  (check "the numbered rows keep their own rows, the commands share one"
         '("*** Shop ***" "1) Sword  30 gp" "2) Mace  60 gp"
           "Sell  Pool gold")
         (menu-texts (fit-menu-lines lines 4 27))))

;; With more than one run the squeeze is taken in page order: the run
;; nearer the head pays first, and the foot keeps its rows while the
;; saving above covers the overflow.
(let ((lines (list (menu-option #\a "Ale") (menu-option #\b "Bread")
                   "1) Sword"
                   (menu-option #\s "Sell") (menu-next-option))))
  (check "an earlier run pays before the foot is touched"
         '("Ale  Bread" "1) Sword" "Sell" "           NEXT")
         (menu-texts (fit-menu-lines lines 4 27))))

;; A lone option row is left as it is: NEXT standing by itself keeps
;; the padding that centers it on the takeover column, even on a page
;; too short to hold it — the overflow falls on the plain rows at the
;; head instead (%DROP-INFO-ROWS, below).
(check "a run of one option is not packed"
       '("           NEXT" "B")
       (menu-texts (fit-menu-lines (list "A" "" (menu-next-option) "" "B")
                                   2 27)))

;; The last resort behind all three squeezes: a line that WRAPPED — a
;; long header over a narrow page, a stock row whose title and price
;; run past it — can push the page over even after the spacers went
;; and the commands packed.  The overflow then drops plain
;; informational rows from the head (the title, the header) rather
;; than losing the picks or the command foot off the bottom edge.
(let ((lines (list "*** Shop ***"
                   "A very long header line that wraps"
                   (menu-numbered 1 "1) Sword  30 gp")
                   (menu-option #\s "Sell")
                   (menu-option #\p "Pool gold"))))
  (check "still overflowing, the plain head rows go before the foot"
         '("that wraps" "1) Sword  30 gp" "Sell  Pool gold")
         (menu-texts (fit-menu-lines lines 3 27))))
;; ... but a bracket-hint row is navigation, not garnish: it stands
;; while the plain rows pay
(check "a hint row is not dropped"
       '("info" "[Esc] back")
       (menu-texts (fit-menu-lines (list "*** Shop ***" "info" "[Esc] back")
                                   2 27)))

;; A plain informational line that overflows and carries a two-space
;; gap breaks at the gap — the shop header splits into its name and
;; purse halves, whole — while one that fits passes through untouched.
(check "an overflowing gapped line breaks at its gap"
       '("Wolfhardt the Bold buys." "Gold: 12345 gp")
       (menu-texts (fit-menu-lines
                    (list "Wolfhardt the Bold buys.  Gold: 12345 gp")
                    4 26)))
(check "a fitting gapped line keeps its row"
       '("Kestrel buys.  Gold: 250 gp")
       (menu-texts (fit-menu-lines
                    (list "Kestrel buys.  Gold: 250 gp")
                    4 27)))

;; The two pages that drove this — the lores takeover column is 27
;; cells wide (+TAKEOVER-COLUMNS+) and 11 rows tall on the 200-line
;; layout.  Both must still reach their last option: the shop's Pool
;; gold and, above all, the sheet's NEXT, which is the only way on
;; round the carousel.
(let ((rows 11)
      (width +takeover-columns+))
  (labels ((page-keys (fitted)
             ;; every key the fitted page can still be picked by —
             ;; whole-row options and the options inside packed rows
             (mapcan (lambda (line)
                       (let ((key (menu-line-key line)))
                         (if key
                             (list key)
                             (mapcar #'third (menu-line-spans line)))))
                     fitted))
           (fits-p (lines)
             (let ((fitted (fit-menu-lines lines rows width)))
               (and (<= (length fitted) rows)
                    (member (menu-line-key (car (last lines)))
                            (page-keys fitted))
                    t))))
    ;; the shop's buy page at a full seven-item window
    (check-true "the lores shop page keeps its last option"
                (fits-p (append
                         (list "*** The Armoury ***" ""
                               "Kestrel buys.  Gold: 250 gp" "")
                         (loop for i from 1 to +menu-page-size+
                               collect (menu-numbered
                                        i (format nil "~D) Longsword  120 gp" i)))
                         (list ""
                               (menu-option #\s "Sell")
                               (menu-option #\i "Inspect")
                               (menu-option #\p "Pool gold")))))
    ;; the character sheet at its fullest: a stat block filling the
    ;; window AND both ladders open (a level due and an art to take up)
    (check-true "the lores sheet page keeps its NEXT row"
                (fits-p (append
                         (loop for i from 1 to +sheet-page-size+
                               collect (format nil "STR ~D DEX ~D IQ ~D" i i i))
                         (list ""
                               (menu-option #\l "Level up") ""
                               (menu-option #\c "Change class") ""
                               (menu-option #\p "Pool gold") ""
                               (menu-option #\t "Trade gold") ""
                               (menu-option #\o "Order party") ""
                               (menu-next-option)))))))

;; The same shop page at its worst: the stock scrolled (its scrollbar
;; costs a cell — 26), a hero name that breaks the header in two, and
;; one stock row wrapped by a long title and price.  The squeezes and
;; the head drop together must still leave every command reachable.
(let* ((rows 11)
       (fitted (fit-menu-lines
                (append
                 (list "*** The Armoury ***" ""
                       "Wolfhardt the Bold buys.  Gold: 12345 gp" "")
                 (loop for i from 1 to +menu-page-size+
                       collect (menu-numbered
                                i (format nil "~D) ~A  ~D gp" i
                                          (if (= i 4)
                                              "Stone Servant Fgn"
                                              "Longsword")
                                          (if (= i 4) 1250 120))))
                 (list ""
                       (menu-option #\s "Sell")
                       (menu-option #\i "Inspect")
                       (menu-option #\p "Pool gold")))
                rows (1- +takeover-columns+)))
       (keys (mapcan (lambda (line)
                       (let ((key (menu-line-key line)))
                         (if key
                             (list key)
                             (mapcar #'third (menu-line-spans line)))))
                     fitted)))
  (check-true "the wrapped stock page fits its rows"
              (<= (length fitted) rows))
  (check-true "every stock digit still shows"
              (subsetp '(#\1 #\2 #\3 #\4 #\5 #\6 #\7) keys))
  (check-true "Sell, Inspect and Pool gold all still stand"
              (subsetp '(#\s #\i #\p) keys)))

;; Menu scrolling: a list longer than +MENU-PAGE-SIZE+ (7) windows to
;; a full page of rows — no rows spent on marker hints; the scroll
;; geometry goes to the front-ends' scrollbars through *MENU-SCROLL*.
;; The same MENU-WINDOW math drives the renderers and the digit
;; picks, so they cannot disagree.
(multiple-value-bind (start end above below) (menu-window 7 0)
  (check "a page-sized list shows whole" '(0 7) (list start end))
  (check "a page-sized list hides nothing" '(nil nil)
         (list above below)))
(multiple-value-bind (start end above below) (menu-window 3 9)
  (check "a short list ignores the offset" '(0 3) (list start end))
  (check "a short list hides nothing" '(nil nil) (list above below)))
(multiple-value-bind (start end above below) (menu-window 12 0)
  (check "a deep list windows to a full page" '(0 7) (list start end))
  (check "hidden rows below only at the top" '(nil t)
         (list above (and below t))))
(multiple-value-bind (start end above below) (menu-window 12 3)
  (check "mid-list window" '(3 10) (list start end))
  (check "hidden rows on both sides mid-list" '(t t)
         (list (and above t) (and below t))))
(multiple-value-bind (start end above below) (menu-window 12 99)
  (check "the offset clamps to the tail" '(5 12) (list start end))
  (check "hidden rows above only at the bottom" '(t nil)
         (list (and above t) below)))
(multiple-value-bind (start end) (menu-window 12 -4)
  (check "a negative offset clamps to the head" '(0 7) (list start end)))
(multiple-value-bind (start end) (menu-window 10 0 8)
  (check "the page size is a parameter" '(0 8) (list start end)))
;; digits pick within the visible window
(let ((items '(a b c d e f g h i j k l)))
  (check "digit 1 picks the window's first row" 'f
         (menu-window-pick items 5 1))
  (check "digit 7 picks the window's last row" 'l
         (menu-window-pick items 5 7))
  (check "a digit past the window picks nothing" nil
         (menu-window-pick items 0 8))
  (check "the pick returns the absolute index" 7
         (nth-value 1 (menu-window-pick items 5 3)))
  (check "an unscrolled deep list picks from the head" 'e
         (menu-window-pick items 0 5)))
(let ((items '(a b)))
  (check "a short list picks directly" 'b (menu-window-pick items 0 2))
  (check "a digit past a short list picks nothing" nil
         (menu-window-pick items 0 3)))
;; u/d move the window a full page; other keys and short lists say NIL
(check "d scrolls a window down" 5 (menu-scroll 0 #\d 12))
(check "d clamps at the tail" 5 (menu-scroll 3 #\d 12))
(check "d at the tail stays" 5 (menu-scroll 5 #\d 12))
(check "u scrolls a window up" 6 (menu-scroll 13 #\u 20))
(check "u clamps at the head" 0 (menu-scroll 2 #\u 12))
(check "U is u" 0 (menu-scroll 0 #\U 12))
(check "a non-scroll key is not a scroll" nil (menu-scroll 0 #\x 12))
(check "a non-character is not a scroll" nil (menu-scroll 0 :esc 12))
(check "a short list never scrolls" nil (menu-scroll 0 #\d 7))
;; the rendered window: a full page of rows, the geometry reported
;; through *MENU-SCROLL* for the scrollbar
(let ((items '("A" "B" "C" "D" "E" "F" "G" "H" "I")))
  (let ((lines (menu-scrolled-lines
                items 0
                (lambda (i x) (menu-numbered i (format nil "~D) ~A" i x))))))
    (check "top window: a full page of rows, no marker rows"
           '("1) A" "2) B" "3) C" "4) D" "5) E" "6) F" "7) G")
           (menu-texts lines))
    (check "the geometry reports the window" '(0 7 9) *menu-scroll*)
    (check "option rows renumber within the window" #\1
           (menu-line-key (first lines))))
  (let ((lines (menu-scrolled-lines
                items 4
                (lambda (i x) (menu-numbered i (format nil "~D) ~A" i x))))))
    (check "tail window: the clamped last page"
           '("1) C" "2) D" "3) E" "4) F" "5) G" "6) H" "7) I")
           (menu-texts lines))
    (check "the tail geometry" '(2 9 9) *menu-scroll*)))
(check "a short list renders whole" '("1) A" "2) B")
       (menu-texts (menu-scrolled-lines
                    '("A" "B") 0
                    (lambda (i x)
                      (menu-numbered i (format nil "~D) ~A" i x))))))
(check "a short list reports no scroll geometry" nil *menu-scroll*)

;; Compass-rose geometry (the UI's facing indicator).
(destructuring-bind (needle letters) (compass-points +north+ 100 50 20)
  (check "compass needle points north" '(100 50 100 38) needle)
  (check "compass north letter" '(#\N 100 30 t) (first letters))
  (check "compass east letter" '(#\E 120 50 nil) (second letters))
  (check "compass south letter" '(#\S 100 70 nil) (third letters))
  (check "compass west letter" '(#\W 80 50 nil) (fourth letters)))
(destructuring-bind (needle letters) (compass-points +west+ 0 0 10)
  (check "compass needle points west" '(0 0 -2 0) needle)
  (check "only the facing letter is highlighted"
         '(nil nil nil t) (mapcar #'fourth letters)))
(destructuring-bind (needle letters) (compass-points +east+ 10 10 4)
  (declare (ignore letters))
  (check "compass needle keeps a minimum length" '(10 10 12 10) needle))

;; Active effects (the UI's spell strip): records with an optional
;; expiry on the game clock and a payload plist of engine facts.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (check "fresh game has no active effects" '() (game-effects g))
  (add-effect g "shield")
  (add-effect g "lamp")
  (check "effects accumulate in order" '("shield" "lamp")
         (mapcar #'effect-name (game-effects g)))
  (add-effect g "shield")
  (check "re-adding keeps one entry in place" '("shield" "lamp")
         (mapcar #'effect-name (game-effects g)))
  (check "an undated effect has no expiry" nil
         (effect-expires-at (find-effect g "shield")))
  (remove-effect g "shield")
  (check "remove-effect" '("lamp") (mapcar #'effect-name (game-effects g)))
  (remove-effect g "not-there")
  (check "removing an absent effect is quiet" '("lamp")
         (mapcar #'effect-name (game-effects g)))
  ;; durations and payloads
  (add-effect g "mage flame" :duration 60 :payload '(:light t))
  (check "duration sets the expiry on the clock"
         (+ (game-time g) 60)
         (effect-expires-at (find-effect g "mage flame")))
  (check-true "payload is stored"
              (getf (effect-payload (find-effect g "mage flame")) :light))
  (check-true "a :light payload lights the party" (light-active-p g))
  (add-effect g "mage flame" :duration 10 :payload '(:light t))
  (check "re-adding refreshes the expiry"
         (+ (game-time g) 10)
         (effect-expires-at (find-effect g "mage flame")))
  (check "effect-label downcases for the strip" "mage flame"
         (effect-label (find-effect g "mage flame")))
  (add-effect g "stone skin" :duration 30 :payload '(:ac 2))
  (add-effect g "blessing" :payload '(:ac 1))
  (check ":ac payloads sum into the party bonus" 3 (effects-ac-bonus g))
  (check "lamp carries no :ac payload" nil
         (getf (effect-payload (find-effect g "lamp")) :ac))
  ;; icon images and the :compass payload
  (check "effects carry no image by default" nil
         (effect-image (find-effect g "blessing")))
  (check "without a :compass payload the party is lost" nil
         (compass-active-p g))
  (add-effect g "wayfinder" :duration 20 :payload '(:compass t)
                            :image "fx-rose.iff")
  (check "add-effect stores the icon image" "fx-rose.iff"
         (effect-image (find-effect g "wayfinder")))
  (check-true "a :compass payload orients the party" (compass-active-p g))
  (add-effect g "wayfinder" :duration 20 :payload '(:compass t)
                            :image "fx-rose2.iff")
  (check "re-adding refreshes the image" "fx-rose2.iff"
         (effect-image (find-effect g "wayfinder")))
  (remove-effect g "wayfinder")
  (check "a removed compass leaves the party lost again" nil
         (compass-active-p g)))

;; apply-effect-spec: the timed-effect vocabulary spells and usable
;; items speak, funneled into one applier.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (apply-effect-spec g "wolf skin" '(:buff-ac 2 :duration 30))
  (check "a :buff-ac spec becomes an :ac effect" 2 (effects-ac-bonus g))
  (check "the spec's duration sets the expiry" (+ (game-time g) 30)
         (effect-expires-at (find-effect g "wolf skin")))
  (apply-effect-spec g "lantern" '(:light t :duration 10)
                     :image "fx-light.iff")
  (check-true "a :light spec lights the party" (light-active-p g))
  (check "the applier stores the image" "fx-light.iff"
         (effect-image (find-effect g "lantern")))
  (apply-effect-spec g "wayfinder" '(:compass t :duration 10))
  (check-true "a :compass spec orients the party" (compass-active-p g))
  (check-error "a spec naming no timed effect is rejected"
    (apply-effect-spec g "bogus" '(:frobnicate t :duration 5))))

;;; ---------------------------------------------------------------------
;;; Game time: the clock, day and night, darkness

;; The clock: action costs and the display line.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (check "a fresh game starts at *new-game-minutes*"
         *new-game-minutes* (game-time g))
  (check "fresh clock line" "Day 1, 08:00" (clock-line g))
  (turn-right g)                       ; face east
  (check "a turn costs one minute" (+ *new-game-minutes* 1) (game-time g))
  (check "step east" :moved (move-party g :forward))
  (check "a step costs one minute" (+ *new-game-minutes* 2) (game-time g))
  (turn-left g)                        ; face north into the border wall
  (let ((before (game-time g)))
    (check "a blocked step is blocked" :blocked (move-party g :forward))
    (check "a blocked step costs nothing" before (game-time g)))
  (setf (game-time g) (+ +minutes-per-day+ (* 13 60) 5))
  (check "clock line formats day and zero-padded time"
         "Day 2, 13:05" (clock-line g)))

;; Daylight boundaries: [06:00, 22:00).
(check-true "05:59 is night" (not (daylight-p 359)))
(check-true "06:00 is day" (daylight-p 360))
(check-true "21:59 is day" (daylight-p 1319))
(check-true "22:00 is night" (not (daylight-p 1320)))
(check-true "daylight wraps across days"
            (daylight-p (+ +minutes-per-day+ 360)))

;; The five day-bands tile the clock and align to the daylight window.
(check "00:00 is night"        :night     (time-of-day 0))
(check "05:59 is night"        :night     (time-of-day 359))
(check "06:00 is morning"      :morning   (time-of-day 360))
(check "09:59 is morning"      :morning   (time-of-day 599))
(check "10:00 is noon"         :noon      (time-of-day 600))
(check "13:59 is noon"         :noon      (time-of-day 839))
(check "14:00 is afternoon"    :afternoon (time-of-day 840))
(check "17:59 is afternoon"    :afternoon (time-of-day 1079))
(check "18:00 is evening"      :evening   (time-of-day 1080))
(check "20:30 is evening"      :evening   (time-of-day 1230))
(check "21:59 is evening"      :evening   (time-of-day 1319))
(check "22:00 is night"        :night     (time-of-day 1320))
(check "23:59 is night"        :night     (time-of-day 1439))
(check "bands wrap across days" :morning  (time-of-day (+ +minutes-per-day+ 360)))
(check-true "the daylight bands are exactly the daylight window"
            (loop for m from 0 below +minutes-per-day+
                  always (eq (daylight-p m)
                             (not (eq (time-of-day m) :night)))))
(check "band display name" "Morning" (time-of-day-name :morning))
(let* ((m (parse-map *art* :name "test")) (g (new-game m)))
  (setf (game-time g) (+ (* 3 60) 0))   ; 03:00
  (check "night line" "It's Night." (time-of-day-line g))
  (setf (game-time g) (+ (* 7 60) 0))   ; 07:00
  (check "morning line" "It's Morning." (time-of-day-line g))
  (check "band from the game" :morning (game-time-of-day g)))

;; advance-time emits :time-band when the band turns (not only across
;; the daylight boundary).
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (bands '()))
  (on-event g :time-band (lambda (game band) (declare (ignore game))
                           (push band bands)))
  (setf (game-time g) 599)              ; 09:59, morning
  (advance-time g)                      ; -> 10:00, noon
  (check "crossing 10:00 emits :time-band noon" '(:noon) bands)
  (advance-time g)                      ; 10:01, still noon
  (check "no band turn, no event" '(:noon) bands)
  (setf (game-time g) 1319)             ; 21:59, evening
  (advance-time g)                      ; -> 22:00, night
  (check "crossing 22:00 emits :time-band night" '(:night :noon) bands))

;;; ---------------------------------------------------------------------
;;; Day-time sky and ground colour (see palette.lisp).

;; Noon is the base untouched; a NIL base uses the engine default.
(check "noon sky is the zone base" '(102 170 204)
       (sky-color-for '(102 170 204) :noon))
(check "noon ground is the zone base" '(80 60 40)
       (ground-color-for '(80 60 40) :noon))
(check "a NIL sky base uses *default-sky*" *default-sky*
       (sky-color-for nil :noon))
(check "a NIL ground base uses *default-ground*" *default-ground*
       (ground-color-for nil :noon))

;; The sky brightens toward morning and sinks to near-black at night.
(flet ((lum (rgb) (reduce #'+ rgb)))
  (let ((base '(102 170 204)))
    (check-true "morning sky is brighter than noon"
                (> (lum (sky-color-for base :morning))
                   (lum (sky-color-for base :noon))))
    (check-true "evening sky is dimmer than noon"
                (< (lum (sky-color-for base :evening))
                   (lum (sky-color-for base :noon))))
    (check-true "night sky is the dimmest band"
                (< (lum (sky-color-for base :night))
                   (lum (sky-color-for base :evening))))
    (check-true "night sky is near black"
                (< (lum (sky-color-for base :night)) 120))
    ;; the ground darkens the same way and never leaves 0-255
    (check-true "night ground is darker than noon"
                (< (lum (ground-color-for base :night))
                   (lum (ground-color-for base :noon))))
    (check-true "every band stays in gamut"
                (loop for band in '(:morning :noon :afternoon :evening :night)
                      always (every (lambda (c) (<= 0 c 255))
                                    (append (sky-color-for base band)
                                            (ground-color-for base band)))))
    ;; a red alien sky still goes dark at night — the tint is relative
    (check-true "a declared red sky still darkens at night"
                (< (lum (sky-color-for '(204 34 34) :night))
                   (lum (sky-color-for '(204 34 34) :noon))))))

;; A zone declares its sky/ground; a list or a vector both parse, and
;; the value survives onto the map.  Bad colours are rejected loudly.
(let ((m (parse-map *art* :name "test")))
  (check "no ZONE colour leaves sky NIL (engine default applies)"
         nil (dungeon-map-sky m)))
(let ((m (parse-map *art* :name "sky-zone")))
  (%parse-map-forms m "(zone :sky (10 20 30) :ground #(40 50 60))" "sky-zone")
  (check "zone :sky parses a list" '(10 20 30) (dungeon-map-sky m))
  (check "zone :ground parses a vector to a list" '(40 50 60)
         (dungeon-map-ground m)))
(check-error "a two-component sky is rejected"
  (%parse-map-forms (parse-map *art* :name "bad") "(zone :sky (10 20))" "bad"))
(check-error "an out-of-range component is rejected"
  (%parse-map-forms (parse-map *art* :name "bad") "(zone :sky (10 20 300))"
                    "bad"))

;; ZONE-PEN-COLORS: which colours the two pack registers take, and —
;; the part that earns the function — which registers to leave alone.
;; The front end writes exactly what this returns (%APPLY-ZONE-PALETTE),
;; so a NIL here is "the pack's own colour stands", not "black".
(let ((m (parse-map *art* :name "outdoor")))
  (%parse-map-forms m "(zone :sky (10 20 30) :ground (40 50 60))" "outdoor")
  ;; out of doors, noon is the declared base exactly...
  (multiple-value-bind (sky ground) (zone-pen-colors m :noon)
    (check "an outdoor zone's noon sky is what it declared" '(10 20 30) sky)
    (check "an outdoor zone's noon ground is what it declared"
           '(40 50 60) ground))
  ;; ...and every other band is the band blend, not the base
  (multiple-value-bind (sky ground) (zone-pen-colors m :night)
    (check "an outdoor zone's night sky is blended" (sky-color-for '(10 20 30) :night) sky)
    (check "an outdoor zone's night ground is blended"
           (ground-color-for '(40 50 60) :night) ground)))
(let ((m (parse-map *art* :name "outdoor-bare")))
  ;; an outdoor zone that declares nothing still gets a sky: the engine
  ;; default, blended.  Out here NIL would be a black hole overhead.
  (multiple-value-bind (sky ground) (zone-pen-colors m :noon)
    (check-true "a bare outdoor zone still gets a sky" (and sky t))
    (check-true "a bare outdoor zone still gets a ground" (and ground t))
    (check "the bare outdoor sky is the engine default" *default-sky* sky)
    (check "the bare outdoor ground is the engine default" *default-ground* ground)))
;; Underground: a dark zone's declared colours reach the registers —
;; this is what the whole change is for — but unblended, because there
;; is no hour down there for a ceiling to follow.
(let ((m (parse-map *art* :name "dark-zone")))
  (%parse-map-forms m "(zone :dark t :sky (10 20 30) :ground (40 50 60))"
                    "dark-zone")
  (dolist (band '(:noon :night :morning :evening))
    (multiple-value-bind (sky ground) (zone-pen-colors m band)
      (check (format nil "a dark zone's ceiling ignores the ~A band" band)
             '(10 20 30) sky)
      (check (format nil "a dark zone's floor ignores the ~A band" band)
             '(40 50 60) ground))))
;; ...and a dark zone that declares nothing must get NIL, not a default.
;; Its pack painted those two pens; handing back *DEFAULT-SKY* here
;; would repaint the ceiling art of every dungeon that never asked.
(let ((m (parse-map *art* :name "dark-bare")))
  (%parse-map-forms m "(zone :dark t)" "dark-bare")
  (multiple-value-bind (sky ground) (zone-pen-colors m :noon)
    (check "a bare dark zone leaves the pack's ceiling alone" nil sky)
    (check "a bare dark zone leaves the pack's floor alone" nil ground)))
;; The two are independent: a zone may colour its floor and say nothing
;; about its roof, and the roof must keep the pack's.
(let ((m (parse-map *art* :name "dark-half")))
  (%parse-map-forms m "(zone :dark 2 :ground (40 50 60))" "dark-half")
  (multiple-value-bind (sky ground) (zone-pen-colors m :noon)
    (check "a dark zone may colour its floor alone" '(40 50 60) ground)
    (check "and its untouched roof keeps the pack's colour" nil sky)))

;;; ---------------------------------------------------------------------
;;; The living-world idle clock (pure arithmetic; see time.lisp).

(let ((u internal-time-units-per-second))
  ;; disabled: no idle progression, whatever the elapsed time
  (let ((*idle-clock-rate* nil))
    (check "idle off buys no minutes" 0 (idle-minutes-elapsed (* 10 u)))
    (check "idle off costs nothing" 0 (idle-minutes-cost 10)))
  ;; brisk: 4 game-minutes per real second
  (let ((*idle-clock-rate* 4))
    (check "one real second is four game-minutes" 4 (idle-minutes-elapsed u))
    (check "two real seconds is eight game-minutes"
           8 (idle-minutes-elapsed (* 2 u)))
    (check "just under a minute's worth buys nothing"
           0 (idle-minutes-elapsed (floor (1- u) 4)))
    (check "four minutes cost one real second" u (idle-minutes-cost 4))
    (check "eight minutes cost two real seconds" (* 2 u) (idle-minutes-cost 8))
    ;; the consumed real time never exceeds the elapsed (the idle base
    ;; cannot outrun the clock), leaving a sub-minute remainder to carry
    (let* ((elapsed (+ (* 2 u) 7))
           (mins (idle-minutes-elapsed elapsed))
           (cost (idle-minutes-cost mins)))
      (check "2s+ε at brisk is eight whole minutes" 8 mins)
      (check-true "consumed time never exceeds elapsed" (<= cost elapsed))
      (check-true "the carried remainder is under one minute's cost"
                  (< (- elapsed cost) (idle-minutes-cost 1)))))
  ;; the rate is a plain special: ambient and demo scale linearly
  (let ((*idle-clock-rate* 1))
    (check "ambient: one game-minute per second" 1 (idle-minutes-elapsed u)))
  (let ((*idle-clock-rate* 20))
    (check "demo: twenty game-minutes per second"
           20 (idle-minutes-elapsed u)))
  ;; a non-positive rate is treated as off
  (let ((*idle-clock-rate* 0))
    (check "a zero rate is off" 0 (idle-minutes-elapsed (* 5 u)))))

;; End to end: standing idle turns the day-band without a step.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (bands '())
       (*idle-clock-rate* 4))
  (on-event g :time-band (lambda (game band) (declare (ignore game))
                           (push band bands)))
  (setf (game-time g) 599)                    ; 09:59, morning
  ;; one real second of standing at 4 game-min/s buys four game-minutes,
  ;; a single boundary crossing (09:59 -> 10:03, into noon)
  (advance-time g (idle-minutes-elapsed internal-time-units-per-second))
  (check "standing four game-minutes reaches 10:03" 603 (game-time g))
  (check "the band turned to noon while idle" :noon (game-time-of-day g))
  (check "the idle turn fired :time-band" '(:noon) bands))

;; advance-time: a single large jump (a long idle stall, or a future
;; rest op) must not skip the boundaries in between — every day-band
;; turn, and every :SUNRISE/:SUNSET crossed along the way, still fires.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (bands '())
       (events '())
       (msgs (watch-messages g)))
  (on-event g :time-band (lambda (game band) (declare (ignore game))
                           (push band bands)))
  (on-event g :sunset (lambda (game) (declare (ignore game))
                        (push :sunset events)))
  (on-event g :sunrise (lambda (game) (declare (ignore game))
                         (push :sunrise events)))
  (setf (game-time g) 599)                    ; day 1, 09:59, morning
  ;; 1450 minutes crosses six boundaries: noon, afternoon, evening,
  ;; night (sunset) on day 1, then morning (sunrise), noon on day 2
  (advance-time g 1450)
  (check "a multi-day jump lands on the right minute" 2049 (game-time g))
  (check "the jump ends in day 2's noon band" :noon (game-time-of-day g))
  (check "every band turn fires, oldest first"
         '(:noon :morning :night :evening :afternoon :noon) bands)
  (check "sunset then sunrise both fire, none skipped"
         '(:sunrise :sunset) events)
  (check-true "night falls in the log despite the single big jump"
              (member "Night falls." (funcall msgs) :test #'equal))
  (check-true "the sun rises in the log despite the single big jump"
              (member "The sun rises." (funcall msgs) :test #'equal))
  ;; the quiet turns are announced through the jump too, not only sun-up
  ;; and sun-down
  (check-true "noon is announced through the jump"
              (member "The sun climbs high." (funcall msgs) :test #'equal))
  (check-true "afternoon is announced through the jump"
              (member "The afternoon wears on." (funcall msgs) :test #'equal))
  (check-true "dusk is announced through the jump"
              (member "Dusk gathers." (funcall msgs) :test #'equal)))

;; Each single band turn announces exactly its own line (see
;; *TIME-BAND-MESSAGES*) — the newest message after crossing the boundary.
(dolist (case '((360 . "The sun rises.")
                (600 . "The sun climbs high.")
                (840 . "The afternoon wears on.")
                (1080 . "Dusk gathers.")
                (1320 . "Night falls.")))
  (let* ((m (parse-map *art* :name "test"))
         (g (new-game m))
         (msgs (watch-messages g)))
    (setf (game-time g) (1- (car case)))   ; one minute before the turn
    (advance-time g)
    (check (format nil "crossing minute ~D announces its band line"
                   (car case))
           (cdr case)
           (car (last (funcall msgs))))))

;; advance-time: boundary events and effect expiry.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g))
       (events '()))
  (on-event g :sunset (lambda (game) (declare (ignore game))
                        (push :sunset events)))
  (on-event g :sunrise (lambda (game) (declare (ignore game))
                         (push :sunrise events)))
  (setf (game-time g) 1319)
  (advance-time g)
  (check "crossing 22:00 emits :sunset" '(:sunset) events)
  (check-true "night falls in the log"
              (member "Night falls." (funcall msgs) :test #'equal))
  (setf (game-time g) (+ +minutes-per-day+ 359))
  (advance-time g)
  (check "crossing 06:00 emits :sunrise" '(:sunrise :sunset) events)
  (check-true "the sun rises in the log"
              (member "The sun rises." (funcall msgs) :test #'equal))
  ;; a timed effect expires with a message and an event
  (let ((expired '()))
    (on-event g :effect-expired
              (lambda (game name) (declare (ignore game))
                (push name expired)))
    (add-effect g "mage flame" :duration 5 :payload '(:light t))
    (advance-time g 3)
    (check "an unexpired effect stays" '("mage flame")
           (mapcar #'effect-name (game-effects g)))
    (advance-time g 2)
    (check "the effect expires on time" '() (game-effects g))
    (check "expiry emits :effect-expired" '("mage flame") expired)
    (check-true "expiry is announced"
                (member "Mage flame wears off." (funcall msgs)
                        :test #'equal))))

;; Darkness: night and :dark zones shrink the view (and the automap) to
;; one cell; a light effect restores it.
(defparameter *corridor-art*
"+-+-+-+-+
|@      |
+-+-+-+-+")

(let* ((m (parse-map *corridor-art* :name "dark-test" :start-facing :east))
       (g (new-game m)))
  (check "daylight: full view depth" +view-depth+ (game-view-depth g))
  (check-true "daylight outdoors is not dark" (not (game-dark-p g)))
  (setf (game-time g) 1320)            ; 22:00 — night
  (check-true "night outdoors is dark" (game-dark-p g))
  ;; outdoors at night there is moonlight: a few cells, not the blind
  ;; one of a lightless dungeon, and never more than the daytime depth
  (check "night outdoors: moonlight, not blind" *moonlight-depth*
         (game-view-depth g))
  (check "moonlight truncates compute-view to its depth"
         (min *moonlight-depth* +view-depth+)
         (length (compute-view (game-map g) (game-x g) (game-y g)
                               (game-facing g) (game-view-depth g))))
  (let ((*moonlight-depth* 1))
    (check "moonlight 1 is a pitch-black night" 1 (game-view-depth g)))
  (let ((*moonlight-depth* 99))
    (check "moonlight is capped at +view-depth+" +view-depth+
           (game-view-depth g)))
  (add-effect g "torchlight" :payload '(:light t))
  (check-true "a light effect defeats the night" (not (game-dark-p g)))
  (check "lit night: full view depth" +view-depth+ (game-view-depth g))
  (remove-effect g "torchlight")
  (check "light gone: moonlit again" *moonlight-depth* (game-view-depth g)))

;; The automap honors darkness, and moonlight widens it.  A long corridor
;; runs east from (0,0); the party is born at night (NEW-GAME's first
;; OBSERVE already maps).  Knowing cell C's far (east) wall needs depth
;; C+1, so cell 1 tells moonlight from a pitch-black night and cell 3
;; tells moonlight (depth 3) from a light (the full +view-depth+ 4).
(flet ((born-at-night (moon)
         (let ((*new-game-minutes* 1320)     ; 22:00
               (*moonlight-depth* moon))
           (new-game (parse-map "+-+-+-+-+-+-+-+
|@            |
+-+-+-+-+-+-+-+"
                                :name "dark-map" :start-facing :east)))))
  (let ((blind (born-at-night 1))
        (moonlit (born-at-night 3)))
    (check-true "night automap: the standing cell is always known"
                (cell-explored-p (game-knowledge blind) 0 0))
    (check-true "a pitch-black night maps nothing one cell ahead"
                (not (wall-known-p (game-knowledge blind) 1 0 +east+)))
    (check-true "moonlight maps the cell one ahead"
                (wall-known-p (game-knowledge moonlit) 1 0 +east+))
    (check-true "moonlight does not reach three cells ahead"
                (not (wall-known-p (game-knowledge moonlit) 3 0 +east+)))
    (add-effect moonlit "torchlight" :payload '(:light t))
    (observe moonlit)
    (check-true "a light reaches three cells ahead, past the moonlight"
                (wall-known-p (game-knowledge moonlit) 3 0 +east+))))

;; A (zone :dark t) zone is dark at any hour.
(let ((path "tests/tmp-dark.map"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string "+-+-+
|@  |
+-+-+
(zone :kind :dungeon :title \"the crypt\" :dark t)
" s))
  (let* ((m (load-map-file path))
         (g (new-game m)))
    (check-true "zone :dark parses" (dungeon-map-dark m))
    (check-true "a :dark zone is dark at noon"
                (progn (setf (game-time g) 720) (game-dark-p g)))
    (check "a plain :dark t zone sees one cell" 1 (game-view-depth g))
    (add-effect g "mage flame" :payload '(:light t))
    (check-true "light works underground too" (not (game-dark-p g))))
  (delete-file path))

;; (zone :dark N) — dark at any hour, but with N cells of sight (the
;; Closure cellar plays with 3): a light effect still buys the full
;; view depth, and N is capped at +VIEW-DEPTH+.
(let* ((m (parse-map "+-+-+-+-+-+-+
|@          |
+-+-+-+-+-+-+"
                     :name "dim-test" :start-facing :east))
       (g (new-game m)))
  (setf (dungeon-map-dark m) 3)         ; as (zone :dark 3) stores it
  (setf (game-time g) 720)              ; noon — still dark underground
  (check-true "a :dark 3 zone is dark" (game-dark-p g))
  (check ":dark 3 sees three cells" 3 (game-view-depth g))
  (check ":dark 3 truncates compute-view to three" 3
         (length (compute-view (game-map g) (game-x g) (game-y g)
                               (game-facing g) (game-view-depth g))))
  (add-effect g "torchlight" :payload '(:light t))
  (check "a light buys the full view depth" +view-depth+
         (game-view-depth g))
  (remove-effect g "torchlight")
  (setf (dungeon-map-dark m) 99)
  (check ":dark above +view-depth+ is capped" +view-depth+
         (game-view-depth g)))

;;; Draw distance (*DRAW-DEPTH*): the speed knob for slower machines.
;;; It caps what the view DRAWS; it must never touch what the party
;;; sees (GAME-VIEW-DEPTH) or maps (OBSERVE).

(let* ((m (parse-map "+-+-+-+-+-+-+
|@          |
+-+-+-+-+-+-+"
                     :name "draw-depth" :start-facing :east))
       (g (new-game m)))
  (check "by default the view draws everything the party sees"
         (game-view-depth g) (render-view-depth g))
  (let ((*draw-depth* 2))
    (check "a lowered draw depth caps the drawn view" 2
           (render-view-depth g))
    (check "the drawn view still sees the full distance" +view-depth+
           (game-view-depth g))
    (check "a lowered draw depth truncates compute-view" 2
           (length (compute-view (game-map g) (game-x g) (game-y g)
                                 (game-facing g) (render-view-depth g)))))
  ;; darkness and draw distance: whichever is tighter wins, and neither
  ;; can talk the other up
  (setf (dungeon-map-dark m) 3)
  (setf (game-time g) 720)              ; noon, but underground
  (let ((*draw-depth* +view-depth+))
    (check "darkness wins when it is the tighter of the two" 3
           (render-view-depth g)))
  (let ((*draw-depth* 2))
    (check "draw distance wins when it is the tighter of the two" 2
           (render-view-depth g)))
  (setf (dungeon-map-dark m) nil)
  ;; out-of-range settings degrade instead of indexing past the planes
  (let ((*draw-depth* 0))
    (check "a draw depth below one still draws one cell" 1
           (render-view-depth g)))
  (let ((*draw-depth* 99))
    (check "a draw depth above +view-depth+ is capped" +view-depth+
           (render-view-depth g))))

;; Draw width degrades the same way — but 0 is a VALID setting (no
;; flanks at all), where draw depth must keep its one cell.
(let ((*draw-flanks* -3))
  (check "a draw width below none draws none" 0 (%draw-flanks)))
(let ((*draw-flanks* 99))
  (check "a draw width past the viewport edge is capped" +view-flanks+
         (%draw-flanks)))

;; The knob is a RENDERING cap: the automap must record everything the
;; light allows, whatever the machine draws.  NEW-GAME's first OBSERVE
;; runs under the lowered setting.
(let* ((g (let ((*draw-depth* 1))
            (new-game (parse-map *corridor-art*
                                 :name "draw-depth-map"
                                 :start-facing :east)))))
  (check-true "the shortest draw distance still maps two cells ahead"
              (wall-known-p (game-knowledge g) 2 0 +east+)))

;; The pack contract is the FULL set whatever the machine draws — only
;; the runtime loader asks for less.
(check "wall-piece-names defaults to the full asset set"
       (* +view-depth+ 10) (length (wall-piece-names)))
(check "wall-piece-names at a lower depth covers only those levels"
       20 (length (wall-piece-names 2)))
(check-true "a depth-limited asset set names no deeper piece"
            (every (lambda (piece) (< (second piece) 2))
                   (wall-piece-names 2)))
(let ((*draw-depth* 2))
  (check "the full set is unchanged by a lowered draw depth"
         (* +view-depth+ 10) (length (wall-piece-names))))

;; The :dark integer round-trips through the map file, and a bad value
;; is rejected with a message naming the map.
(let ((path "tests/tmp-dim.map"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string "+-+-+
|@  |
+-+-+
(zone :kind :dungeon :title \"the cellar\" :dark 3)
" s))
  (check "zone :dark 3 parses as the integer" 3
         (dungeon-map-dark (load-map-file path)))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string "+-+-+
|@  |
+-+-+
(zone :dark :pitch-black)
" s))
  (check-error "zone :dark rejects a non-integer non-T value"
    (load-map-file path))
  (delete-file path))

;; at-night / at-day specials: pure clock tests.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0)
        '((at-night (message "Eyes glitter in the dark."))
          (at-day (message "The lane lies quiet."))))
  (turn-right g)                       ; face east
  (setf (game-time g) 719)             ; the step lands at noon
  (move-party g :forward)
  (check "at-day runs by day" '("The lane lies quiet.") (funcall msgs))
  (move-party g :back)                 ; back-step keeps facing east
  (setf (game-time g) 1369)            ; the step lands well into night
  (move-party g :forward)
  (check-true "at-night runs by night"
              (member "Eyes glitter in the dark." (funcall msgs)
                      :test #'equal))
  (check-true "at-day stays quiet by night"
              (= 1 (count "The lane lies quiet." (funcall msgs)
                          :test #'equal))))

;; Movement emits :enter-cell and :blocked.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (entered '())
       (blocked '()))
  (on-event g :enter-cell (lambda (game x y) (declare (ignore game))
                            (push (list x y) entered)))
  (on-event g :blocked (lambda (game dir) (declare (ignore game))
                         (push dir blocked)))
  (move-party g :forward)               ; north wall
  (turn-right g)
  (move-party g :forward)               ; east to (1,0)
  (check "blocked event carries direction" '(:north) blocked)
  (check "enter-cell event carries coordinates" '((1 0)) (nreverse entered)))

;;; ---------------------------------------------------------------------
;;; Cell specials

;; message + set-flag on entry.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((message "It is dark here.")
                               (set-flag :dark)))
  (turn-right g)
  (move-party g :forward)
  (check "special message on entry" '("It is dark here.") (funcall msgs))
  (check "special set a flag" t (flag g :dark)))

;; trigger-special fires the start cell by hand (after wiring handlers).
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 0 0) '((message "You are at the start.")))
  (trigger-special g)
  (check "trigger-special runs the standing cell"
         '("You are at the start.") (funcall msgs)))

;; once runs only the first time, ever.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((once (message "first time"))))
  (turn-right g)
  (move-party g :forward)
  (move-party g :back)
  (move-party g :forward)
  (check "once fires a single time" '("first time") (funcall msgs)))

;; when-flag / unless-flag branch on story flags.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((when-flag :key (message "yes"))
                               (unless-flag :key (message "no"))))
  (turn-right g)
  (move-party g :forward)
  (check "unless-flag branch without flag" '("no") (funcall msgs))
  (set-flag g :key)
  (move-party g :back)
  (move-party g :forward)
  (check "when-flag branch with flag" '("no" "yes") (funcall msgs)))

;; teleport relocates, faces, records knowledge and chains the target's
;; special.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((teleport 2 1 :south)))
  (setf (cell-special m 2 1) '((message "arrived")))
  (turn-right g)
  (move-party g :forward)
  (check "teleport position" '(2 1) (list (game-x g) (game-y g)))
  (check "teleport facing" +south+ (game-facing g))
  (check "teleport chains target special" '("arrived") (funcall msgs))
  (check-true "teleport target explored"
              (cell-explored-p (game-knowledge g) 2 1)))

;; a teleport loop trips the recursion guard.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (setf (cell-special m 0 0) '((teleport 1 0)))
  (setf (cell-special m 1 0) '((teleport 0 0)))
  (check-error "teleport loop is caught" (trigger-special g))
  ;; and names the map it looped in, in text an Amiga shell can draw:
  ;; a map's own data raises this one, so it is read where it happens
  (check-true "and says which map, in ASCII"
              (let ((said (handler-case (trigger-special g)
                            (error (e) (princ-to-string e)))))
                (and (search "teleport loop in the map data?" said)
                     (every (lambda (c) (< (char-code c) 128)) said)))))

;; teleport off the map is rejected.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (setf (cell-special m 0 0) '((teleport 9 9)))
  (check-error "teleport off-map is rejected" (trigger-special g)))

;; spin turns the party to a random facing, silently.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((spin)))
  (turn-right g)
  (with-rng (2)
    (move-party g :forward))
  (check "spinner facing" +south+ (game-facing g))
  (check "spinner is silent" '() (funcall msgs)))

;; gold goes to the leading hero, and says so.
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5))
       (b (%make-hero :name "Bob" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((gold "2d4")))
  (turn-right g)
  (with-rng (3) (move-party g :forward))
  (check "the gold op says what the party found"
         (list (format nil "The party finds ~D gold!" (hero-gold a)))
         (funcall msgs))
  (check-true "and it went to the leading hero alone"
              (and (plusp (hero-gold a)) (zerop (hero-gold b)))))

;;; The item ops: what a map can put into a pack, take out of one, and
;;; ask about.  WHEN-FLAG remembers that the party once could do a
;;; thing; these four are about what it is carrying right now.

;; :startable NIL keeps the pack-mule out of the guild's creation page,
;; whose tests take the first eight startable classes as they come.
(define-hero-class :packer :hp-dice "1d8" :damage "1d4" :ac 9
                   :startable nil)
(define-item 'test-relic)
(define-item 'test-charm)

;; give-item: the find goes to the first LIVING hero with pack room,
;; and the op names both the prize and its taker.
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5))
       (b (%make-hero :name "Bob" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((give-item test-relic)))
  (turn-right g)
  (move-party g :forward)
  (check "give-item names the find and its taker"
         '("The party finds Test Relic!" "Ann takes it.") (funcall msgs))
  (check "the first hero carries it" '(test-relic) (hero-items a))
  (check "and nobody else does" '() (hero-items b)))

;; a full pack is stepped over — the next hero takes it, and the op
;; leaves GIVE-ITEM's own word about the full one standing
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5
                      :items (make-list +inventory-limit+
                                        :initial-element 'test-charm)))
       (b (%make-hero :name "Bob" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((give-item test-relic)))
  (turn-right g)
  (move-party g :forward)
  (check "a full pack is named and passed over"
         '("The party finds Test Relic!" "Ann's pack is full."
           "Bob takes it.")
         (funcall msgs))
  (check "the full pack is untouched" +inventory-limit+ (length (hero-items a)))
  (check "the next hero took it" '(test-relic) (hero-items b)))

;; every pack full: the find is left where it lay, and nobody has it
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5
                      :items (make-list +inventory-limit+
                                        :initial-element 'test-charm)))
       (g (new-game m :party (list a))))
  (setf (cell-special m 1 0) '((give-item test-relic)))
  (turn-right g)
  (move-party g :forward)
  (check "nothing was taken" +inventory-limit+ (length (hero-items a)))
  (check-true "and the relic is not in the pack"
              (not (hero-carrying-p a 'test-relic))))

;; the fallen do not pick things up
(let* ((m (parse-map *art* :name "test"))
       (dead (%make-hero :name "Mor" :class :packer :hp 0 :max-hp 5))
       (b (%make-hero :name "Bob" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list dead b))))
  (setf (cell-special m 1 0) '((give-item test-relic)))
  (turn-right g)
  (move-party g :forward)
  (check "a fallen hero's hands stay empty" '() (hero-items dead))
  (check "the living one takes it" '(test-relic) (hero-items b)))

;; when-item / unless-item branch on what is carried, not on a flag
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list a)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((when-item test-relic (message "open"))
                               (unless-item test-relic (message "locked"))))
  (turn-right g)
  (move-party g :forward)
  (check "unless-item branch with an empty pack" '("locked") (funcall msgs))
  (give-item g a 'test-relic)
  (move-party g :back)
  (move-party g :forward)
  ;; the watcher accumulates, so the second reading carries the first
  (check "when-item branch once it is carried"
         '("locked" "open") (funcall msgs)))

;; a key in a fallen hero's pack still counts — the party has it
(let* ((m (parse-map *art* :name "test"))
       (dead (%make-hero :name "Mor" :class :packer :hp 0 :max-hp 5
                         :items (list 'test-relic)))
       (b (%make-hero :name "Bob" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list dead b)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((when-item test-relic (message "open"))))
  (turn-right g)
  (move-party g :forward)
  (check "a gate opens for a key its bearer died holding"
         '("open") (funcall msgs))
  (check-true "which is what PARTY-CARRYING-P says too"
              (party-carrying-p g 'test-relic)))

;; take-item spends one copy, from the first carrier in party order,
;; and takes the last one out of the hands that wore it
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5
                      :items (list 'test-relic 'test-relic)
                      :equipped (list 'test-relic)))
       (b (%make-hero :name "Bob" :class :packer :hp 5 :max-hp 5
                      :items (list 'test-relic)))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((take-item test-relic)))
  (turn-right g)
  (move-party g :forward)
  (check "the first carrier gives one up" '(test-relic) (hero-items a))
  (check "the spare went, so the worn copy stays worn"
         '(test-relic) (hero-equipped a))
  (check "the other carrier is untouched" '(test-relic) (hero-items b))
  (check "spending a key is silent" '() (funcall msgs))
  (move-party g :back)
  (move-party g :forward)
  (check "the second helping empties the pack" '() (hero-items a))
  (check "and the hands with it" '() (hero-equipped a)))

;; spending what nobody carries is a no-op, not an error
(let* ((m (parse-map *art* :name "test"))
       (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5))
       (g (new-game m :party (list a)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((take-item test-relic)))
  (turn-right g)
  (move-party g :forward)
  (check "take-item on an empty party says nothing" '() (funcall msgs))
  (check "and changes nothing" '() (hero-items a)))

;; an unregistered item name is loud in every one of the four
(dolist (ops '(((give-item test-nonesuch))
               ((take-item test-nonesuch))
               ((when-item test-nonesuch (message "x")))
               ((unless-item test-nonesuch (message "x")))))
  (let* ((m (parse-map *art* :name "test"))
         (a (%make-hero :name "Ann" :class :packer :hp 5 :max-hp 5))
         (g (new-game m :party (list a))))
    (setf (cell-special m 0 0) ops)
    (check-error (format nil "~A of an unknown item is an error"
                         (first (first ops)))
                 (trigger-special g))))

;; unknown ops and malformed ops are loud.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (setf (cell-special m 0 0) '((frobnicate 1 2)))
  (check-error "unknown special op is an error" (trigger-special g))
  (setf (cell-special m 0 0) '(42))
  (check-error "non-list special op is an error" (trigger-special g)))

;;; ---------------------------------------------------------------------
;;; Heroes and the party

(define-hero-class :tester :hp-dice "1d8+2" :damage "1d6" :ac 8)

(let ((h (with-rng (5) (make-hero "Bob" :tester))))
  (check "hero name" "Bob" (hero-name h))
  (check "hero class" :tester (hero-class h))
  (check "hero hp from class hit dice" 8 (hero-max-hp h))
  (check "hero starts at full hp" 8 (hero-hp h))
  (check "hero str rolled 3d6" 3 (hero-str h))
  (check "hero ac from class" 8 (hero-ac h))
  (check "hero damage from class" "1d6" (hero-damage h))
  (check "hero level 1" 1 (hero-level h))
  (check-true "fresh hero is alive" (hero-alive-p h)))

(check-error "make-hero rejects unknown class" (make-hero "X" :nonesuch))

;; Character sheet (the party-UI stat block): pure text, rendered by
;; the Amiga :sheet page and tested here from the same source.
(define-hero-class :war-mage :hp-dice "1d6" :damage "1d4" :ac 8)
(check "hero-class-title spaces and capitalizes" "War Mage"
       (hero-class-title (%make-hero :name "z" :class :war-mage)))
(let* ((h (%make-hero :name "El Cid" :class :war-mage :level 3 :xp 1200
                      :max-hp 11 :hp 9 :str 15 :dex 12 :iq 9 :con 14
                      :lck 10 :ac 8 :gold 250))
       (lines (hero-summary-lines h)))
  (check "sheet has six lines" 6 (length lines))
  (check "sheet name/class line" "El Cid the War Mage" (first lines))
  (check "sheet level/xp line" "Level 3  XP 1200" (second lines))
  ;; the sheet's AC is the roster's: HERO-EFFECTIVE-AC, not the base
  ;; slot — here the DEX 12 gift takes the base 8 to 7
  (check "sheet hp/ac line" "HP 9/11  AC 7" (third lines))
  (check "sheet primary stats line" "STR 15 DEX 12 IQ 9" (fourth lines))
  (check "sheet secondary stats line" "CON 14 LCK 10" (fifth lines))
  (check "sheet gold line, standing" "Gold 250 gp" (sixth lines))
  ;; the pack is not on the sheet — it lists on its own page
  ;; (EQUIP-LINES, the sheet's 'i')
  (check "the sheet carries no pack line" nil
         (find-if (lambda (s) (search "Pack" s)) lines)))
;; a downed hero is flagged on the gold line
(check "sheet marks a downed hero" "Gold 0 gp (down)"
       (sixth (hero-summary-lines
               (%make-hero :name "x" :class :war-mage :hp 0))))
;; the block's width discipline: every line stays within 20 cells
;; even at worst-case values — three-digit points, a negative AC, a
;; rich dead hero — well inside the lores takeover's 27 small-face
;; cells, so the block never wraps mid-figure
(let ((h (%make-hero :name "Maximus" :race :dwarf :class :war-mage
                     :level 99 :xp 999999 :max-hp 999 :hp 0
                     :str 18 :dex 18 :iq 18 :con 18 :lck 18
                     :ac -12 :gold 99999)))
  (check "worst-case sheet lines fit the lores takeover column" nil
         (find-if (lambda (s) (> (length s) 20))
                  (hero-summary-lines h))))

;; The character-sheet page (HERO-SHEET-LINES): the summary block and
;; the key hints, a blank line between — no header; the roster pane
;; already shows who is who.  The front-ends (the Amiga message-area
;; takeover, the host :sheet mode) draw these verbatim.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (with-rng ()
                               (list (make-hero "A" :tester)
                                     (make-hero "B" :tester)))))
       (lines (hero-sheet-lines g 1)))
  (check "sheet page opens straight on the summary block" "B the Tester"
         (first lines))
  ;; the sheet names only its own keys, bracket-free, each row a
  ;; clickable option with a blank line between them; the digit pick,
  ;; scrolling and Esc live on the help screen.  The page closes with
  ;; the carousel's NEXT row — the pack lost its Inventory row to it
  (check "sheet page ends with the sheet's own keys, aired one per row"
         (list (menu-option #\p "Pool gold")
               ""
               (menu-option #\t "Trade gold")
               ""
               (menu-option #\o "Order party")
               ""
               (menu-next-option))
         (last lines 7))
  ;; the NEXT row centers its word on the lores takeover column
  (check "the NEXT row is the n key, centered"
         (menu-option #\n "           NEXT")
         (menu-next-option))
  ;; ORDERING true is the marching-order pick: the hints give way to
  ;; the where-to prompt
  (check "ordering sheet asks where to move the hero"
         '("" "Move B where?")
         (last (hero-sheet-lines g 1 0 t) 2))
  ;; a banked level puts the rise on the sheet — 'l' heads the keys,
  ;; and only while one is actually due
  (setf (hero-xp (second (game-party g))) 100)
  (check "a banked level heads the sheet keys with l"
         (list ""
               (menu-option #\l "Level up")
               ""
               (menu-option #\p "Pool gold")
               ""
               (menu-option #\t "Trade gold")
               ""
               (menu-option #\o "Order party")
               ""
               (menu-next-option))
         (last (hero-sheet-lines g 1) 10))
  (setf (hero-xp (second (game-party g))) 0)
  (check "the l key leaves with the flag"
         (list (menu-option #\p "Pool gold")
               ""
               (menu-option #\t "Trade gold")
               ""
               (menu-option #\o "Order party")
               ""
               (menu-next-option))
         (last (hero-sheet-lines g 1) 7)))

;; Class portraits: DEFINE-HERO-CLASS :IMAGE resolves map-relative
;; (the effect-icon rule); a class without one has no portrait.
;; MAKE-HERO stamps the portrait onto the hero at creation — :WOMAN
;; picks a class's second portrait (:IMAGE-WOMAN) — and the face is
;; the person's, not the job's: CHANGE-CLASS leaves it alone.
(define-hero-class :t-faced :image "gfx/face.iff")
(define-hero-class :t-two-faced :image "gfx/man.iff"
                                :image-woman "gfx/woman.iff"
                                :change-at 1 :change-group :t-guise)
(define-hero-class :t-new-guise :image "gfx/other.iff"
                                :change-at 1 :change-group :t-guise)
(define-hero-class :t-faceless :change-at 1 :change-group :t-guise)
(let* ((m (parse-map *art* :name "world/deep/test"))
       (g (new-game m :party (with-rng ()
                               (list (make-hero "F" :t-faced)
                                     (make-hero "A" :tester))))))
  (check "the portrait file is class data" "gfx/face.iff"
         (hero-image (first (game-party g))))
  (check "the portrait resolves beside the map" "world/deep/gfx/face.iff"
         (hero-image-path g (first (game-party g))))
  (check "a class without :image has no portrait" nil
         (hero-image-path g (second (game-party g))))
  (check "creation stamped the portrait onto the hero" "gfx/face.iff"
         (hero-portrait (first (game-party g)))))
(check "without :woman the default portrait is stamped" "gfx/man.iff"
       (hero-image (with-rng () (make-hero "Mordec" :t-two-faced))))
(check-error ":woman on a class without a second portrait is refused"
  (make-hero "X" :t-faced :woman t))
(check "a hero from before portraits stuck falls back to the class"
       "gfx/face.iff"
       (hero-image (%make-hero :name "old" :class :t-faced)))
(let* ((h (with-rng () (make-hero "Mab" :t-two-faced :woman t)))
       (m (parse-map *art* :name "test"))
       (g (new-game m :party (list h))))
  (check "with :woman the woman's portrait is stamped" "gfx/woman.iff"
         (hero-image h))
  (change-class g h :t-new-guise)
  (check "the stamped face outlives a class change" "gfx/woman.iff"
         (hero-image h)))
(let* ((h (with-rng () (make-hero "Grey" :t-faceless)))
       (m (parse-map *art* :name "test"))
       (g (new-game m :party (list h))))
  (check "a portrait-less class stamps :none, not nil" :none
         (hero-portrait h))
  (check "a portrait-less class starts with no image" nil
         (hero-image h))
  (change-class g h :t-new-guise)
  (check "changing into a class with a portrait grows no new face" nil
         (hero-image h)))

;;; ---------------------------------------------------------------------
;;; Races (ability-score modifiers + which classes a race may take)

;; A test race and two test classes exercise the mechanics
;; deterministically, independent of the shipped ruleset.
(define-hero-class :r-fighter :hp-dice "1d10" :damage "1d8" :ac 8)
(define-hero-class :r-mage    :hp-dice "1d6"  :damage "1d4" :ac 10 :caster t)
(define-race :r-orc :str 3 :iq -2 :con 1 :classes '(:r-fighter)
  :description "test race")

;; DEFINE-RACE registers the race and its data.
(check "find-race returns the race" :r-orc (race-name (find-race :r-orc)))
(check-error "find-race errors on an unknown race" (find-race :r-nobody))
(check-true "races lists the registered race" (member :r-orc (races)))

;; MAKE-HERO applies the racial ability modifiers to the 3d6 rolls
;; without drawing extra dice.  Scripted rolls: the first draw feeds the
;; hp die (1d10 -> 6), then each 3d6 sees three 2s -> die faces of 3 ->
;; 9 per stat.  r-orc lands +3 str, -2 iq, +1 con on those 9s.
(let ((h (with-rng (5  2 2 2  2 2 2  2 2 2  2 2 2  2 2 2)
           (make-hero "Orc" :r-fighter :race :r-orc))))
  (check "race recorded on the hero" :r-orc (hero-race h))
  (check "hp rolled before the abilities (roll order intact)" 6
         (hero-max-hp h))
  (check "race adds to strength"      12 (hero-str h))   ; 9 + 3
  (check "race leaves dexterity alone"  9 (hero-dex h))
  (check "race lowers intelligence"     7 (hero-iq h))   ; 9 - 2
  (check "race raises constitution"    10 (hero-con h))  ; 9 + 1
  (check "race leaves luck alone"       9 (hero-lck h)))

;; A modifier cannot push a rolled score outside 1..18.
(let ((h (with-rng (5  5 5 5  0 0 0  0 0 0  0 0 0  0 0 0)  ; str 18, rest 3
           (make-hero "Brute" :r-fighter :race :r-orc))))
  (check "racial bonus is clamped at 18" 18 (hero-str h))  ; 18 + 3 -> 18
  (check "racial penalty is clamped at 1" 1 (hero-iq h)))  ; 3 - 2 -> 1

;; MAKE-HERO rejects a race that does not permit the class; an allowed
;; pairing (or a raceless hero) works and records the race.
(check-error "race forbids an off-list class"
  (make-hero "Bad" :r-mage :race :r-orc))
;; and says which races may, in text an Amiga shell can draw — the font
;; there has no glyphs past ASCII, and a design error is read where it
;; is raised
(check-true "and names the races that may"
            (let ((said (handler-case (make-hero "Bad" :r-mage :race :r-orc)
                          (error (e) (princ-to-string e)))))
              (and (search "A R-Orc cannot be a R Mage - the race may be:" said)
                   (every (lambda (c) (< (char-code c) 128)) said))))
(check "race permits an on-list class" :r-orc
       (hero-race (make-hero "Good" :r-fighter :race :r-orc)))
(check-true "a raceless hero still works" (make-hero "Free" :r-mage))

;; A race that restricts nothing (NIL :classes) takes any class.
(define-race :r-human :classes nil :description "unrestricted test race")
(check "an unrestricted race lists no classes" nil
       (race-classes (find-race :r-human)))
(check-true "an unrestricted race takes any class"
            (race-allows-class-p :r-human :r-mage))

;; RACE-TITLE is pure formatting (no registry lookup needed).
(check "race-title hyphenates half-elf" "Half-Elf" (race-title :half-elf))
(check "race-title capitalizes dwarf"   "Dwarf"    (race-title :dwarf))

;; The character sheet spells out "Race Class" under the bare name —
;; "Name the Race Class" on one line could overrun the lores takeover
;; column; a raceless hero stays "Name the Class" on one line.
;; (:tester is a registered class, so HERO-SUMMARY-LINES can read its
;; caster/singer flags.)
(let ((lines (hero-summary-lines
              (%make-hero :name "Grod" :race :dwarf :class :tester))))
  (check "sheet opens on the bare name" "Grod"
         (first lines))
  (check "race and class share the second line" "Dwarf Tester"
         (second lines)))
(check "sheet names the class when there is no race" "Nym the Tester"
       (first (hero-summary-lines
               (%make-hero :name "Nym" :class :tester))))

(check "stat-bonus 10" 0 (stat-bonus 10))
(check "stat-bonus 12" 1 (stat-bonus 12))
(check "stat-bonus 15" 2 (stat-bonus 15))
(check "stat-bonus 9" -1 (stat-bonus 9))
(check "stat-bonus 3" -4 (stat-bonus 3))

;;; The experience ladder.  With no campaign table registered the
;;; engine's own gentle curve answers — the fixture world plays on it.
(check "level 1 is free" 0 (xp-for-level 1))
(check "xp-for-level 2" 100 (xp-for-level 2))
(check "xp-for-level 3" 300 (xp-for-level 3))
(check "the engine curve is 50 x L x (L-1)" 7800 (xp-for-level 13))

;;; A campaign's own ladder: the table answers as far as it reaches,
;;; *XP-GROWTH* compounds beyond its end.  Bound here, so the rest of
;;; the suite goes on playing the engine's curve.
(let ((*xp-table* nil)
      (*xp-growth* 3/2))
  (check "define-xp-table returns its totals" '(100 400 1000)
         (define-xp-table '(100 400 1000) :growth 2))
  (check "level 1 is still free" 0 (xp-for-level 1))
  (check "the table's first entry is level 2's total" 100 (xp-for-level 2))
  (check "and its last is the last level it reaches" 1000 (xp-for-level 4))
  (check "one past the end costs the growth" 2000 (xp-for-level 5))
  (check "and it compounds, once per level" 4000 (xp-for-level 6))
  ;; a rational growth stays exact until the closing ROUND: 100 x
  ;; (6/5)^3 is 172.8, and only the answer is rounded
  (define-xp-table '(100) :growth 6/5)
  (check "a rational growth rounds only at the end" 173 (xp-for-level 5))
  ;; a hero standing past the table's end still has a next rung to
  ;; reach for — the ladder never simply stops
  (define-xp-table '(100 400))
  (let ((h (%make-hero :name "Kel" :class :tester :level 9 :xp 0)))
    (check-true "a hero above the table is not yet ready"
                (not (hero-level-up-pending-p h)))
    (setf (hero-xp h) (xp-for-level 10))
    (check-true "and rises on the compounded threshold"
                (hero-level-up-pending-p h))))
(check "the campaign table did not outlive its binding" nil *xp-table*)
(check "nor its growth" 3/2 *xp-growth*)
(check "so the engine curve answers again" 100 (xp-for-level 2))

;; a ladder that stalls, turns downhill, or is not a ladder at all
(check-error "define-xp-table rejects an empty table"
             (define-xp-table '()))
(check-error "define-xp-table rejects a non-list"
             (define-xp-table 100))
(check-error "define-xp-table rejects a non-integer total"
             (define-xp-table '(100 2.5 400)))
(check-error "define-xp-table rejects a total at zero"
             (define-xp-table '(0 100)))
(check-error "define-xp-table rejects a negative total"
             (define-xp-table '(100 -400)))
(check-error "define-xp-table rejects totals that stall"
             (define-xp-table '(100 400 400)))
(check-error "define-xp-table rejects totals that turn back down"
             (define-xp-table '(100 400 300)))
(check-error "define-xp-table rejects a growth of 1"
             (define-xp-table '(100) :growth 1))
(check-error "define-xp-table rejects a growth below 1"
             (define-xp-table '(100) :growth 1/2))
(check-error "define-xp-table rejects a growth that is not a number"
             (define-xp-table '(100) :growth :fast))
(check "and none of that touched the engine's curve" nil *xp-table*)

;; Party queries, damage and healing.
(let* ((m (parse-map *art* :name "test"))
       (heroes (with-rng () (list (make-hero "A" :tester)
                                  (make-hero "B" :tester)
                                  (make-hero "C" :tester)
                                  (make-hero "D" :tester))))
       (g (new-game m :party heroes))
       (msgs (watch-messages g))
       (died '())
       (wiped nil))
  (on-event g :hero-died (lambda (game h) (declare (ignore game))
                           (push (hero-name h) died)))
  (on-event g :party-defeated (lambda (game) (declare (ignore game))
                                (setf wiped t)))
  (check "party of four alive" 4 (length (alive-heroes g)))
  (check "front ranks are the first three" '("A" "B" "C")
         (mapcar #'hero-name (front-ranks g)))
  (damage-hero g (first heroes) 999)
  (check "damage clamps hp at zero" 0 (hero-hp (first heroes)))
  (check "hero-died event" '("A") died)
  (check "front ranks skip the fallen" '("B" "C" "D")
         (mapcar #'hero-name (front-ranks g)))
  (check "three heroes standing" 3 (length (alive-heroes g)))
  (check "party not wiped yet" nil wiped)
  (dolist (h (rest heroes))
    (damage-hero g h 999))
  (check-true "party-defeated event after the last hero" wiped)
  (check "party-alive-p when wiped" nil (party-alive-p g))
  (check-true "fall message emitted" (search "FALLS" (first (funcall msgs)))))

;; The roster holds up to +party-limit+ (7) members: 6 heroes + 1 guest.
(check "party limit is seven" 7 +party-limit+)
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (with-rng () (list (make-hero "A" :tester)))))
       (msgs (watch-messages g))
       (joined '()))
  (on-event g :party-joined (lambda (game h) (declare (ignore game))
                              (push (hero-name h) joined)))
  (check "party starts below the limit" nil (party-full-p g))
  (dotimes (i 6)
    (check-true (format nil "join-party accepts member ~D" (+ 2 i))
                (with-rng () (join-party g (make-hero (format nil "H~D" i)
                                                      :tester)))))
  (check "party at the limit" 7 (length (game-party g)))
  (check-true "party-full-p at the limit" (party-full-p g))
  (check ":party-joined emitted per join" 6 (length joined))
  (check "join-party refuses the 8th" nil
         (with-rng () (join-party g (make-hero "Late" :tester))))
  (check "refused hero not added" 7 (length (game-party g)))
  (check-true "join message emitted"
              (find-if (lambda (s) (search "joins the party" s))
                       (funcall msgs)))
  ;; exact, because the text itself is the check: every character a
  ;; message carries is drawn by the Amiga's font, which has no glyphs
  ;; past ASCII — a UTF-8 dash slipped in here arrives as three of
  ;; garbage on the board
  (check-true "full message emitted"
              (member "The party is full - Late cannot join."
                      (funcall msgs) :test #'string=))
  ;; combat still works with a full roster: front ranks stay three
  (check "front ranks with a full party" 3 (length (front-ranks g))))

;; Marching order: MOVE-HERO moves a roster slot to a new place, the
;; others closing ranks — who stands in the front ranks is the point
;; (the character sheet offers it on 'o').
(let* ((m (parse-map *art* :name "test"))
       (heroes (with-rng () (list (make-hero "A" :tester)
                                  (make-hero "B" :tester)
                                  (make-hero "C" :tester)
                                  (make-hero "D" :tester))))
       (g (new-game m :party heroes))
       (msgs (watch-messages g)))
  (check-true "move-hero sends a hero back" (move-hero g 0 2))
  (check "the others close ranks" '("B" "C" "A" "D")
         (mapcar #'hero-name (game-party g)))
  (check-true "the move is announced"
              (find-if (lambda (s)
                         (search "A now marches in position 3" s))
                       (funcall msgs)))
  (check-true "move-hero brings a hero forward" (move-hero g 3 0))
  (check "the whole order shifts" '("D" "B" "C" "A")
         (mapcar #'hero-name (game-party g)))
  (check "the front ranks follow the new order" '("D" "B" "C")
         (mapcar #'hero-name (front-ranks g)))
  (check "a move to the hero's own slot is no move" nil (move-hero g 1 1))
  (check "an out-of-range target slot is refused" nil (move-hero g 0 4))
  (check "an out-of-range source slot is refused" nil (move-hero g -1 0))
  (check "refusals leave the order alone" '("D" "B" "C" "A")
         (mapcar #'hero-name (game-party g))))

(let* ((m (parse-map *art* :name "test"))
       (h (with-rng () (make-hero "A" :tester)))  ; 3 max hp
       (g (new-game m :party (list h))))
  (damage-hero g h 2)
  (check "heal-hero returns hp gained" 1 (with-rng (0) (heal-hero g h 1)))
  (check "heal-hero caps at max" 3 (progn (heal-hero g h 99) (hero-hp h))))

;; Pool gold: the party's purses land on one hero (the Bard's Tale
;; command — the shop pages and the character sheet offer it on 'g').
(let* ((m (parse-map *art* :name "test"))
       (heroes (with-rng () (list (make-hero "A" :tester)
                                  (make-hero "B" :tester)
                                  (make-hero "C" :tester))))
       (g (new-game m :party heroes))
       (msgs (watch-messages g)))
  (setf (hero-gold (first heroes)) 10
        (hero-gold (second heroes)) 25
        (hero-gold (third heroes)) 7)
  (check "pool-gold returns the amount gained" 35
         (pool-gold g (third heroes)))
  (check "the pooling hero holds everything" 42
         (hero-gold (third heroes)))
  (check "the other purses are emptied" '(0 0)
         (list (hero-gold (first heroes)) (hero-gold (second heroes))))
  (check-true "pooling is announced"
              (find-if (lambda (s) (search "pools its gold" s))
                       (funcall msgs)))
  (check "pooling again gains nothing" 0 (pool-gold g (third heroes)))
  (check "and leaves the gold in place" 42 (hero-gold (third heroes)))
  (check-true "the empty pool says so"
              (find-if (lambda (s) (search "Nobody else has gold" s))
                       (funcall msgs)))
  ;; a downed hero's purse pools too — gold is no use to the fallen
  (setf (hero-gold (first heroes)) 5)
  (damage-hero g (first heroes) 999)
  (check "a downed hero's purse pools too" 5
         (pool-gold g (third heroes))))

;; Trade gold: one purse hands a sum to another — the character
;; sheet's 't', POOL-GOLD's counterpart for splitting a purse instead
;; of piling it.
(let* ((m (parse-map *art* :name "test"))
       (heroes (with-rng () (list (make-hero "A" :tester)
                                  (make-hero "B" :tester))))
       (g (new-game m :party heroes))
       (msgs (watch-messages g))
       (a (first heroes))
       (b (second heroes)))
  (setf (hero-gold a) 40
        (hero-gold b) 3)
  (check-true "trade-gold moves the sum" (trade-gold g a b 15))
  (check "the giver paid" 25 (hero-gold a))
  (check "the receiver holds it" 18 (hero-gold b))
  (check-true "the hand-over is announced"
              (find-if (lambda (s) (search "hands 15 gold" s))
                       (funcall msgs)))
  (check "a short purse refuses" nil (trade-gold g a b 99))
  (check-true "and says why"
              (find-if (lambda (s) (search "does not have 99" s))
                       (funcall msgs)))
  (check "refusals move nothing" '(25 18)
         (list (hero-gold a) (hero-gold b)))
  (check "trading with oneself is refused" nil (trade-gold g a a 5))
  (check "a zero sum is refused" nil (trade-gold g a b 0))
  (check "a negative sum is refused" nil (trade-gold g a b -5))
  ;; the fallen both give and receive, as with pooled gold
  (damage-hero g b 999)
  (check-true "a downed hero still receives" (trade-gold g a b 5))
  (check-true "and still gives" (trade-gold g b a 5)))

;; The trade page (the sheet's 't' — TRADE-LINES/TRADE-ACT): a digit
;; picks who receives, then the sum is typed — digits, Backspace,
;; Return — and Esc backs out a page at a time.
(let* ((m (parse-map *art* :name "test"))
       (heroes (with-rng () (list (make-hero "A" :tester)
                                  (make-hero "B" :tester))))
       (g (new-game m :party heroes))
       (msgs (watch-messages g))
       (v (make-trade-view (first heroes))))
  (setf (hero-gold (first heroes)) 30)
  (check-true "the pick page asks who receives"
              (member "Give gold to whom?" (menu-texts (trade-lines g v))
                      :test #'equal))
  (check-true "the giver's row is marked"
              (find-if (lambda (s) (search "(giver)" s))
                       (menu-texts (trade-lines g v))))
  (check "a recipient row carries its digit" #\2
         (menu-line-key
          (find-if (lambda (line)
                     (search "2) B" (menu-line-text line)))
                   (trade-lines g v))))
  (check "picking the giver goes nowhere" nil (trade-act g v #\1))
  (check "the view stays on the pick page" nil (trade-view-to v))
  (check-true "and says the gold stays put"
              (find-if (lambda (s) (search "already holds" s))
                       (funcall msgs)))
  (check "a digit picks the recipient" nil (trade-act g v #\2))
  (check "the amount page opens on them" "B"
         (hero-name (trade-view-to v)))
  (check "Esc steps back to the pick page" nil (trade-act g v #\Escape))
  (check "the recipient is unpicked" nil (trade-view-to v))
  ;; pick again; type 125, rub the 5 out, Return: 12 gold moves
  (trade-act g v #\2)
  (trade-act g v #\1)
  (trade-act g v #\2)
  (trade-act g v #\5)
  (check "digits build the sum" "125" (trade-view-amount v))
  (check "Backspace deletes" "12"
         (progn (trade-act g v #\Backspace) (trade-view-amount v)))
  (check-true "the amount page shows the sum being typed"
              (find-if (lambda (s) (search "12_" s))
                       (menu-texts (trade-lines g v))))
  (check "Return trades" :done (trade-act g v #\Return))
  (check "the sum arrived" 12 (hero-gold (second heroes)))
  (check "the giver paid" 18 (hero-gold (first heroes))))

;; the trade page's edges: an unpayable sum starts the entry over, an
;; empty Return steps back, the sum cannot outgrow its row
(let* ((m (parse-map *art* :name "test"))
       (heroes (with-rng () (list (make-hero "A" :tester)
                                  (make-hero "B" :tester))))
       (g (new-game m :party heroes))
       (v (make-trade-view (first heroes))))
  (setf (hero-gold (first heroes)) 5)
  (trade-act g v #\2)
  (trade-act g v #\9)
  (trade-act g v #\9)
  (check "an unpayable sum keeps the page open" nil
         (trade-act g v #\Return))
  (check "and the entry starts over" "" (trade-view-amount v))
  (check "a bare Return steps back to the pick page" nil
         (trade-act g v #\Return))
  (check "the recipient is unpicked again" nil (trade-view-to v))
  (trade-act g v #\2)
  (dotimes (i 9) (trade-act g v #\7))
  (check "the sum stops at six digits" "777777" (trade-view-amount v))
  (trade-act g v #\Escape)
  (check "Esc then closes the page" :cancelled (trade-act g v #\Escape)))

;; The host roster pane carries the Bard's Tale columns the Amiga
;; table shows: effective armor class and spell points beside HP and
;; gold.  (Host front-end only — amiga-ui draws its own table.)
#-amigaos
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng () (make-hero "Ann" :tester)))
       (g (new-game m :party (list h))))
  (setf (hero-gold h) 42)
  (let ((pane (%party-pane g)))
    (check-true "roster shows armor class" (search "AC  8" pane))
    (check-true "roster shows spell points" (search "SP   0/  0" pane))
    (check-true "roster shows gold" (search "42 gp" pane))
    (check-true "no level banked, no flag" (search "1 Ann" pane)))
  ;; the banked-level flag: ^ in the spare column before the name
  ;; (the Amiga table's white up-arrow)
  (setf (hero-xp h) 100)
  (check-true "a banked level raises the ^ flag"
              (search "1^Ann" (%party-pane g))))

;; Leveling: xp only banks — crossing a threshold announces the
;; readiness and raises the roster flag, and the rise itself waits
;; for ADVANCE-LEVEL (the character sheet's 'l'), one level per call.
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng (5) (make-hero "A" :tester)))  ; 8 max hp
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (check-true "a fresh hero has no level banked"
              (not (hero-level-up-pending-p h)))
  (check "no rise due: advance-level declines" nil (advance-level g h))
  (award-xp g h 100)
  (check "xp banks, the level waits" 1 (hero-level h))
  (check-true "the threshold raises the flag" (hero-level-up-pending-p h))
  (check-true "the readiness is announced"
              (search "ready for the next level" (first (funcall msgs))))
  (with-rng (4) (advance-level g h))
  (check "advance-level takes the level" 2 (hero-level h))
  (check "level-up adds rolled hp" 15 (hero-max-hp h))
  (check-true "level-up message"
              (find-if (lambda (s) (search "level 2" s)) (funcall msgs)))
  (check-true "the flag drops with the rise"
              (not (hero-level-up-pending-p h)))
  (award-xp g h 500)                    ; 600 xp: levels 3 and 4 banked
  (check "a doubly-crossed threshold announces once, not twice"
         2 (count-if (lambda (s) (search "ready for the next level" s))
                     (funcall msgs)))
  (with-rng (4) (advance-level g h))
  (check "one level per call" 3 (hero-level h))
  (check-true "the second level still waits" (hero-level-up-pending-p h))
  (check "the next call takes the next one" 4
         (with-rng (4) (advance-level g h))))

;; The rise's own page: the character sheet takeover hides the log the
;; rise's messages land in, so a front-end marks the log, takes the
;; level, and shows what was said since (LOG-SINCE the mark) as a page
;; of its own — LEVEL-NOTES-LINES — turning back to the sheet on the
;; next key or the NEXT row's click.
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng (5) (make-hero "A" :tester)))
       (g (new-game m :party (list h)))
       (log (attach-message-log g)))
  (award-xp g h 100)
  (let ((mark (log-length log)))
    (with-rng (4) (advance-level g h))
    (let ((lines (level-notes-lines (log-since log mark))))
      (check-true "the rise's page tells the new level"
                  (search "rises to level 2" (first lines)))
      (check "a spacer parts the notes from the closing row" ""
             (menu-line-text (nth (- (length lines) 2) lines)))
      (check "the page closes with the carousel's NEXT row" #\n
             (menu-line-key (first (last lines)))))))

;; Bard's Tale stat growth: each level-up draws a d18 per stat in
;; str dex iq con lck order after the hit die — a draw at or above the
;; score is a gain, so gains thin out toward the 18 cap — but at most
;; ONE ability rises per level: the first success pays out and the
;; later draws are spent unheeded (still five draws every level, so
;; scripted rolls stay fixed).
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng (5) (make-hero "B" :tester)))   ; all abilities 3
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  ;; hit die 4, then str 17 (rises), dex 0 (stays), iq 3 (would rise,
  ;; but str took the level's one), con 2, lck 17 (likewise unheeded)
  (award-xp g h 100)
  (with-rng (4 17 0 3 2 17) (advance-level g h))
  (check "str rose on the level-up" 4 (hero-str h))
  (check "dex stayed" 3 (hero-dex h))
  (check "iq stayed — one rise per level" 3 (hero-iq h))
  (check "con stayed" 3 (hero-con h))
  (check "lck stayed — one rise per level" 3 (hero-lck h))
  (check-true "the gain is announced"
              (member "B's STR rises to 4!" (funcall msgs) :test #'equal))
  (check "and announced alone" 1
         (count-if (lambda (s) (and (search "rises to" s)
                                    (not (search "rises to level" s))))
                   (funcall msgs)))
  ;; a failed early draw leaves the level's one rise to a later stat
  (award-xp g h 200)                               ; to level 3
  (with-rng (4 0 0 17 0 0) (advance-level g h))
  (check "the rise falls to the first success" 4 (hero-iq h))
  (check "str keeps its score" 4 (hero-str h))
  ;; the cap: a d18 draws 0-17, so a score of 18 can never rise
  (setf (hero-str h) 18)
  (award-xp g h 300)                               ; to level 4
  (with-rng (0 17 0 0 0 0) (advance-level g h))
  (check "a score of 18 never rises" 18 (hero-str h)))

;; STAT-GIFT is the Bard's Tale kindness: high scores reward hit
;; points and armor, low scores never punish them.
(check "stat-gift is the bonus for a high score" 2 (stat-gift 14))
(check "stat-gift never punishes a low one" 0 (stat-gift 3))

;; CON pays into hp at creation and on every level-up.
(let* ((m (parse-map *art* :name "test"))
       ;; hit die 6, abilities str/dex/iq 3, con 18, lck 3
       (h (with-rng (5 0 0 0 0 0 0 0 0 0 5 5 5) (make-hero "C" :tester)))
       (g (new-game m :party (list h))))
  (check "con 18" 18 (hero-con h))
  (check "creation hp carry the con gift" 12 (hero-max-hp h))
  (award-xp g h 100)
  (with-rng (4) (advance-level g h))
  (check "the level's hp gain carries it too" 23 (hero-max-hp h)))

;; DEX pays into the effective armor class.
(let ((h (with-rng (5 0 0 0 5 5 5) (make-hero "D" :tester))))
  (check "dex 18" 18 (hero-dex h))
  (check "the dex gift lowers the effective ac" 4 (hero-effective-ac h)))

;; :AC-PER-LEVEL — the monk's art: natural armor tightens by one every
;; N levels beyond the first, floored at Bard's Tale's -10.
(check-error "ac-per-level must be a positive integer"
  (define-hero-class :t-bogus-monk :ac-per-level 0))
(define-hero-class :t-monk :hp-dice "1d8+2" :damage "1d8" :ac 7
                           :ac-per-level 1)
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng (5) (make-hero "M" :t-monk)))
       (g (new-game m :party (list h))))
  (check "the monk starts at the class ac" 7 (hero-ac h))
  (award-xp g h 100)
  (with-rng () (advance-level g h))
  (check "each level tightens the monk's guard" 6 (hero-ac h))
  (award-xp g h 500)                    ; levels 3 and 4
  (with-rng () (advance-level g h))
  (with-rng () (advance-level g h))
  (check "every level, not every other" 4 (hero-ac h))
  (setf (hero-ac h) -10)
  (award-xp g h 400)                    ; level 5
  (with-rng () (advance-level g h))
  (check "natural armor floors at -10" -10 (hero-ac h)))

;;; ---------------------------------------------------------------------
;;; Changing class — the second ladder.  An art is left at its
;;; :CHANGE-AT level, freezes there, and goes on granting exactly the
;;; spells it had opened; the new art starts over at level 1 while the
;;; body keeps what it earned.

(check-error "change-at must be a positive integer"
  (define-hero-class :t-bogus-art :change-at 0))
(check-error "change-group must be a symbol"
  (define-hero-class :t-bogus-art :change-group 7))
(check-error ":requires wants (CLASS . LEVEL) pairs"
  (define-hero-class :t-bogus-art :requires '(:t-first)))
(check-error ":requires wants a positive level"
  (define-hero-class :t-bogus-art :requires '((:t-first . 0))))

;; Three arts of one family: the first two are ordinary and
;; interchangeable, the third is closed to new characters and asks for
;; both of the others.
(define-hero-class :t-first  :hp-dice "1d6" :damage "1d4" :ac 10
                             :caster t :change-at 3 :change-group :t-art)
(define-hero-class :t-second :hp-dice "1d6" :damage "1d6" :ac 10
                             :caster t :change-at 3 :change-group :t-art)
(define-hero-class :t-third  :hp-dice "1d6" :damage "1d4" :ac 10
                             :caster t :startable nil
                             :change-at 3 :change-group :t-art
                             :requires '((:t-first . 3) (:t-second . 3)))
;; and a fourth outside the family and with no :CHANGE-AT at all — an
;; art for life, and no destination for anyone either
(define-hero-class :t-forlife :hp-dice "1d8" :damage "1d6" :ac 9)

(check-true ":startable defaults to true"
            (hero-class-property :t-first :startable))
(check-true "a :startable nil class is off the creation list"
            (not (member :t-third (startable-hero-classes))))
(check-true "an ordinary class is on it"
            (member :t-first (startable-hero-classes)))
(check-error "make-hero refuses a class closed to new characters"
  (make-hero "Nope" :t-third))

(defun %art-hero (name class)
  "A deterministic level-1 caster of CLASS: hit die 4, all abilities 10."
  (let ((h (with-rng (3 7 7 7 7 7) (make-hero name class))))
    (setf (hero-str h) 10 (hero-dex h) 10 (hero-iq h) 10
          (hero-con h) 10 (hero-lck h) 10)
    h))

;; Spells the arts hand out, one per level, so a frozen rating is
;; visible as "these and no more".
(define-spell 't-first-1 :title "first one" :level 1 :cost 1
  :classes '(:t-first) :heal 1)
(define-spell 't-first-3 :title "first three" :level 3 :cost 1
  :classes '(:t-first) :heal 1)
(define-spell 't-first-5 :title "first five" :level 5 :cost 1
  :classes '(:t-first) :heal 1)
(define-spell 't-second-1 :title "second one" :level 1 :cost 1
  :classes '(:t-second) :heal 1)
(define-spell 't-shared-1 :title "shared one" :level 1 :cost 1
  :classes '(:t-first :t-second) :heal 1)

;; The refusals, one at a time.
(let ((h (%art-hero "Sy" :t-first)))
  (check "an art below its :change-at will not be left"
         "Needs level 3 here." (class-change-refusal h :t-second))
  (check "nothing is open yet" '() (hero-class-change-targets h))
  (check-true "and the sheet offers no 'c'"
              (not (hero-can-change-class-p h)))
  (setf (hero-level h) 3)
  (check "at :change-at the sister art opens" nil
         (class-change-refusal h :t-second))
  (check "the class already worn is refused"
         "Already that." (class-change-refusal h :t-first))
  (check "an art outside the family is no destination"
         "Not that kind of art." (class-change-refusal h :t-forlife))
  (check "the top art still wants the other rating"
         "T Second 3 first." (class-change-refusal h :t-third))
  (check "so only the sister art is offered" '(:t-second)
         (hero-class-change-targets h))
  (setf (hero-hp h) 0)
  (check "a fallen hero changes nothing"
         "Not while down." (class-change-refusal h :t-second))
  (check-error "and change-class refuses outright"
    (change-class nil h :t-second)))

;; A class for life is exactly that, however high the level.
(let ((h (%art-hero "Stone" :t-forlife)))
  (setf (hero-level h) 20)
  (check "no :change-at, nothing open" '() (hero-class-change-targets h))
  (check "and it says so"
         "This art is for life." (class-change-refusal h :t-first)))

;; The change itself: what freezes, what starts over, what carries.
(let* ((m (parse-map *art* :name "test"))
       (h (%art-hero "Vex" :t-first))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (hero-level h) 3 (hero-xp h) 300 (hero-gold h) 42)
  (setf (hero-max-hp h) 30 (hero-hp h) 30)
  (setf (hero-max-sp h) 12 (hero-sp h) 12)
  (check-true "the first art's level-3 spells are known"
              (spell-known-p h 't-first-3))
  (check-true "its level-5 spells are not" (not (spell-known-p h 't-first-5)))
  (check ":t-second returns the change" :t-second (change-class g h :t-second))
  (check "the new art starts at level 1" 1 (hero-level h))
  (check "with no experience" 0 (hero-xp h))
  (check "the class is the new one" :t-second (hero-class h))
  (check "the art left behind froze at the level it was left"
         3 (hero-class-level h :t-first))
  (check "the new art's rating is the hero's level"
         1 (hero-class-level h :t-second))
  (check "an art never worn rates 0" 0 (hero-class-level h :t-third))
  (check-true "the old art is remembered" (hero-held-class-p h :t-first))
  (check-true "hit points carry across" (= 30 (hero-max-hp h)))
  (check-true "spell points carry across" (= 12 (hero-max-sp h)))
  (check-true "gold carries across" (= 42 (hero-gold h)))
  (check "the bare attack becomes the new art's" "1d6" (hero-damage h))
  (check-true "the change is announced"
              (find-if (lambda (s) (search "takes up the t second" s))
                       (funcall msgs)))
  ;; the frozen art keeps granting what it opened, and never more
  (check-true "the left art's level-1 spell is still known"
              (spell-known-p h 't-first-1))
  (check-true "and its level-3 spell too" (spell-known-p h 't-first-3))
  (check-true "but the level it never reached stays shut"
              (not (spell-known-p h 't-first-5)))
  (check-true "the new art's level-1 spell arrives"
              (spell-known-p h 't-second-1))
  (check-true "a spell both arts share is known once"
              (= 1 (count 't-shared-1 (spells-for-hero h))))
  ;; and the art cannot be taken up a second time
  (setf (hero-level h) 3)
  (check "an art once worn is closed for good"
         "Been there once." (class-change-refusal h :t-first)))

;; Levelling after a change never lets the earned spell points fall.
(let* ((m (parse-map *art* :name "test"))
       (h (%art-hero "Purse" :t-first))
       (g (new-game m :party (list h))))
  (setf (hero-level h) 3 (hero-max-sp h) 12 (hero-sp h) 12)
  (change-class g h :t-second)
  (check "the change keeps the earned purse" 12 (hero-max-sp h))
  (award-xp g h 100)
  (with-rng (4) (advance-level g h))
  (check "and a level-1 art's arithmetic cannot shrink it"
         12 (hero-max-sp h)))

;; The top art: reachable only once both ratings are in hand, and only
;; by changing class.
(let* ((m (parse-map *art* :name "test"))
       (h (%art-hero "Arch" :t-first))
       (g (new-game m :party (list h))))
  (setf (hero-level h) 3)
  (check "one rating short, the top art stays shut"
         "T Second 3 first." (class-change-refusal h :t-third))
  (change-class g h :t-second)
  (check "and freshly changed, the new art must be walked first"
         "Needs level 3 here." (class-change-refusal h :t-third))
  (setf (hero-level h) 3)               ; the second art reaches 3 too
  (check "both ratings in hand, it opens" nil
         (class-change-refusal h :t-third))
  (check "and it is what is offered" '(:t-third)
         (hero-class-change-targets h))
  (change-class g h :t-third)
  (check "the second art froze on the way out"
         3 (hero-class-level h :t-second))
  (check "and the first is still where it was left"
         3 (hero-class-level h :t-first)))

;; A race that does not permit the art refuses it, change or no change.
(define-race :t-narrow :classes '(:t-first))
(let ((h (%art-hero "Nar" :t-first)))
  (setf (hero-race h) :t-narrow (hero-level h) 3)
  (check "the race has the last word"
         "No T-Narrow does that." (class-change-refusal h :t-second)))

;; The sheet: the Arts row appears only once an art has been left, and
;; the 'c' hint only while something is open.
(let* ((m (parse-map *art* :name "test"))
       (h (%art-hero "Page" :t-first))
       (g (new-game m :party (list h))))
  (check "one art, no Arts row" nil
         (find-if (lambda (s) (search "Arts" s)) (hero-summary-lines h)))
  (check-true "and no 'c' on the page"
              (not (find-if (lambda (l) (search "Change class"
                                                (menu-line-text l)))
                            (hero-sheet-lines g 0))))
  (setf (hero-level h) 3)
  (check-true "at the threshold the 'c' hint appears"
              (find-if (lambda (l) (search "Change class" (menu-line-text l)))
                       (hero-sheet-lines g 0)))
  ;; the pick page: the offered arts, numbered, in place of the hints
  (check-true "the pick page asks which art"
              (find-if (lambda (l) (search "Take up which art?"
                                           (menu-line-text l)))
                       (hero-sheet-lines g 0 0 nil t)))
  (check-true "and numbers the offer"
              (find-if (lambda (l) (search "1) T Second" (menu-line-text l)))
                       (hero-sheet-lines g 0 0 nil t)))
  (change-class g h :t-second)
  (check "both arts now show with their ratings" '("Arts TF3 TS1")
         (remove-if-not (lambda (s) (search "Arts" s))
                        (hero-summary-lines h))))

;; The Arts rows wrap at three arts so the sheet's 20 cells hold.
(let ((h (%art-hero "Wide" :t-first)))
  (setf (hero-class-levels h) '((:t-forlife . 13) (:t-third . 13)
                                (:t-second . 13))
        (hero-level h) 13)
  (check "four arts take two rows"
         '("Arts TS13 TT13 TF13" "Arts TF13")
         (remove-if-not (lambda (s) (search "Arts" s))
                        (hero-summary-lines h)))
  (check-true "and neither row runs past the sheet's 20 cells"
              (every (lambda (s) (<= (length s) 20))
                     (hero-summary-lines h))))

;;; ---------------------------------------------------------------------
;;; Combat

(define-monster "test rat"
  :level 1 :hp-dice 3 :ac 10 :damage "1d2" :xp 10 :gold 6)

(check-error "unknown monster type" (find-monster-type "grue"))

(defun %combat-hero (&optional (name "Alva"))
  "A deterministic level-1 :tester hero: 8 hp, str 10 (no bonus)."
  (let ((h (with-rng (5) (make-hero name :tester))))
    (setf (hero-str h) 10)
    h))

;; start-combat: dice group counts, banner, event, exclusivity.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g))
       (started '()))
  (on-event g :combat-start (lambda (game monsters) (declare (ignore game))
                              (setf started (length monsters))))
  (with-rng (2)
    (start-combat g '(("test rat" "1d3+1"))))
  (check "dice group count spawns monsters" 4 started)
  (check "combat groups count the living" '(("test rat" . 4))
         (mapcar (lambda (grp)
                   (cons (monster-type-name (first grp)) (second grp)))
                 (combat-groups (game-combat g))))
  (check "combat banner" '("You face 4 test rats!") (funcall msgs))
  (check-error "no moving during combat" (move-party g :forward))
  (check-error "no nested combat" (start-combat g '(("test rat" 1)))))

;; Monster portraits: DEFINE-MONSTER :IMAGE is type data, and the
;; fight's picture resolves map-relative (the effect-icon rule) — the
;; Amiga front-end shows it in the view column while the round-orders
;; page takes over the message area.
(define-monster "test ogre" :hp-dice 3 :damage "1d2"
                            :image "gfx/mon-ogre.iff")
(check "the portrait file is monster data" "gfx/mon-ogre.iff"
       (monster-type-image (find-monster-type "test ogre")))
(check "a monster without :image has no portrait" nil
       (monster-type-image (find-monster-type "test rat")))

(let* ((m (parse-map *art* :name "world/deep/test"))
       (g (new-game m :party (list (%combat-hero)))))
  (check "no combat, no enemy portrait" nil (combat-image-path g))
  (start-combat g '(("test ogre" 1)))
  (check "the enemy portrait is the fight's picture" "gfx/mon-ogre.iff"
         (combat-enemy-image (game-combat g)))
  (check "the enemy portrait resolves beside the map"
         "world/deep/gfx/mon-ogre.iff" (combat-image-path g)))

;; Encounter order picks the portrait, and the fallen give it up: a
;; leading group with no picture never hides the one behind it.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (start-combat g '(("test rat" 1) ("test ogre" 1)))
  (check "a picture-less leading group falls through" "gfx/mon-ogre.iff"
         (combat-enemy-image (game-combat g)))
  (setf (monster-hp (second (combat-monsters (game-combat g)))) 0)
  (check "a fallen monster gives its portrait up" nil
         (combat-enemy-image (game-combat g)))
  (check "and the fight then has no picture" nil (combat-image-path g)))

;;; ---------------------------------------------------------------------
;;; Wandering monsters (the zone's random encounters)

(defun %group-names (g)
  "The current fight's groups as (NAME . COUNT) conses."
  (mapcar (lambda (grp) (cons (monster-type-name (first grp)) (second grp)))
          (combat-groups (game-combat g))))

;; Zone keys land in the map slots.
(let ((m (parse-map *art* :name "test")))
  (%apply-map-form m '(zone :encounters (("test rat" "1d3" 3)
                                         ("test ogre" 1))
                            :encounter-chance 10
                            :night-encounters (("test ogre" 2))
                            :night-encounter-chance 30)
                   "test")
  (check "zone encounter table parsed"
         '(("test rat" "1d3" 3) ("test ogre" 1))
         (dungeon-map-encounters m))
  (check "zone encounter chance parsed" 10 (dungeon-map-encounter-chance m))
  (check "zone night table parsed" '(("test ogre" 2))
         (dungeon-map-night-encounters m))
  (check "zone night chance parsed" 30
         (dungeon-map-night-encounter-chance m)))

;; Bad zone encounter data fails at map load, not at play time.
(let ((m (parse-map *art* :name "test")))
  (check-error "encounter chance 0 rejected"
    (%apply-map-form m '(zone :encounter-chance 0) "test"))
  (check-error "encounter chance above 100 rejected"
    (%apply-map-form m '(zone :night-encounter-chance 101) "test"))
  (check-error "encounter table must be a list of entries"
    (%apply-map-form m '(zone :encounters "test rat") "test"))
  (check-error "encounter entry needs a monster name string"
    (%apply-map-form m '(zone :encounters ((test-rat 1))) "test"))
  (check-error "encounter entry dice must parse"
    (%apply-map-form m '(zone :encounters (("test rat" "d6"))) "test"))
  (check-error "encounter weight must be a positive integer"
    (%apply-map-form m '(zone :night-encounters (("test rat" 1 0))) "test"))
  (check-error "an encounter distance must be feet"
    (%apply-map-form m '(zone :encounters (("test rat" 1 1 :far))) "test"))
  (check-error "and a positive number of them"
    (%apply-map-form m '(zone :encounters (("test rat" 1 1 0))) "test"))
  (check-error "a fifth element is not an entry"
    (%apply-map-form m '(zone :encounters (("test rat" 1 1 30 7))) "test")))

;; A zone may say how far off its wanderers are met — the fourth
;; element — and the fight opens there.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 1 1 30))
                            :encounter-chance 100)
                   "test")
  (setf (game-map g) m)
  (check "the table carries the distance"
         '(("test rat" 1 1 30)) (dungeon-map-encounters m))
  (with-rng (0) (maybe-wandering-encounter g))
  (check "and the wanderers are met that far off" 30
         (combat-distance (game-combat g))))

;; Which chance and table apply when: day, night, the :night-* fallbacks
;; and the sunless underground.
(let ((m (parse-map *art* :name "test")))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 10
                            :night-encounters (("test ogre" 2))
                            :night-encounter-chance 30)
                   "test")
  (multiple-value-bind (chance table) (%zone-encounter-check m 480)
    (check "day check uses the base chance" 10 chance)
    (check "day check uses the base table" '(("test rat" 2)) table))
  (multiple-value-bind (chance table) (%zone-encounter-check m 1380)
    (check "night check uses the night chance" 30 chance)
    (check "night check uses the night table" '(("test ogre" 2)) table)))
(let ((m (parse-map *art* :name "test")))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 5
                            :night-encounter-chance 20)
                   "test")
  (multiple-value-bind (chance table) (%zone-encounter-check m 0)
    (check "a lone night chance raises the rate on the base table"
           20 chance)
    (check "the base table serves the night then" '(("test rat" 2)) table)))
(let ((m (parse-map *art* :name "test")))
  (%apply-map-form m '(zone :dark t
                            :encounters (("test rat" 2))
                            :encounter-chance 5
                            :night-encounters (("test ogre" 1))
                            :night-encounter-chance 50)
                   "test")
  (multiple-value-bind (chance table) (%zone-encounter-check m 0)
    (check "a dark zone keeps its base chance at midnight" 5 chance)
    (check "no night table underground" '(("test rat" 2)) table)))
(check "no table, no check" nil
       (%zone-encounter-check (parse-map *art* :name "test") 0))

;; The weighted pick.
(let ((table '(("test rat" 1 3) ("test ogre" 1))))
  (check "a roll inside the first weight picks the first entry" "test rat"
         (with-rng (2) (first (%pick-encounter table))))
  (check "a roll past the first weight picks the second entry" "test ogre"
         (with-rng (3) (first (%pick-encounter table)))))
(check "a single-entry table draws no random number" "test rat"
       (let ((*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
         (first (%pick-encounter '(("test rat" 2))))))

;; The step roll: chance window, *ENCOUNTER-RATE* scaling, the kill
;; switch, and no roll while a fight already runs.
(check "the default encounter rate plays chances as authored" 1
       *encounter-rate*)
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 10)
                   "test")
  (check-true "a roll under the chance fires"
              (with-rng (9) (maybe-wandering-encounter g)))
  (check "the wandering group" '(("test rat" . 2)) (%group-names g))
  (setf (game-combat g) nil)
  (check "a roll at the chance stays quiet" nil
         (with-rng (10) (maybe-wandering-encounter g)))
  (let ((*encounter-rate* 1/2))
    (check "a rational rate halves the chance" nil
           (with-rng (9) (maybe-wandering-encounter g)))
    (check-true "the halved chance still fires under it"
                (with-rng (4) (maybe-wandering-encounter g))))
  (setf (game-combat g) nil)
  (let ((*encounter-rate* 3))
    (check-true "a raised rate widens the window"
                (with-rng (25) (maybe-wandering-encounter g))))
  (setf (game-combat g) nil)
  (let ((*encounter-rate* nil)
        (*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (check "rate NIL disables wandering monsters" nil
           (maybe-wandering-encounter g)))
  (let ((*encounter-rate* 0)
        (*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (check "rate 0 disables wandering monsters" nil
           (maybe-wandering-encounter g)))
  (with-rng (9) (maybe-wandering-encounter g))
  (let ((*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (check "an ongoing fight skips the check" nil
           (maybe-wandering-encounter g)))
  (setf (game-combat g) nil))

;; A step draws the roll, and the step's own minute decides the table:
;; at 21:59 one step lands on 22:00 — the sunset-crossing step already
;; rolls against the night pair (25 is over the day 10, under the
;; night 30).
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 10
                            :night-encounters (("test ogre" 2))
                            :night-encounter-chance 30)
                   "test")
  (setf (game-time g) 1319
        (game-facing g) +east+)
  (with-rng (25) (move-party g))
  (check "the sunset-crossing step rolls the night table"
         '(("test ogre" . 2)) (%group-names g)))

;; The living world hunts the idle too: MAYBE-IDLE-ENCOUNTER accrues
;; the heartbeat's minutes on the game and rolls the zone's wandering
;; check once per full *IDLE-ENCOUNTER-MINUTES*, the remainder
;; carrying forward.  A running fight drains periods without a draw,
;; and a NIL dial turns the vigil off entirely.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 10)
                   "test")
  (check "a fresh game keeps no vigil" 0 (game-idle-encounter-clock g))
  (let ((*rng* (lambda (n) (error "rolled ~D" n))))
    (check "under the period no die is cast" nil
           (maybe-idle-encounter g 29)))
  (check "the minutes accrued" 29 (game-idle-encounter-clock g))
  ;; one more minute completes the half-hour: one roll, over the
  ;; chance — the streets stay quiet and the accrual is spent
  (check "a quiet half-hour passes" nil
         (with-rng (99) (maybe-idle-encounter g 1)))
  (check "the accrual was spent" 0 (game-idle-encounter-clock g))
  ;; 45 more: one period fires under the chance, 15 minutes carry
  (check-true "the vigil springs the fight"
              (with-rng (5) (maybe-idle-encounter g 45)))
  (check-true "combat is on" (game-combat g))
  (check "the remainder carries forward" 15
         (game-idle-encounter-clock g))
  ;; mid-fight the periods drain silently — no random number drawn
  (let ((*rng* (lambda (n) (error "rolled ~D" n))))
    (check "a running fight silences the vigil" nil
           (maybe-idle-encounter g 75)))
  (check "yet its periods drained" 0 (game-idle-encounter-clock g))
  (setf (game-combat g) nil)
  (let ((*idle-encounter-minutes* nil)
        (*rng* (lambda (n) (error "rolled ~D" n))))
    (check "a NIL dial turns the vigil off" nil
           (maybe-idle-encounter g 500)))
  (check "and nothing accrues while it is off" 0
         (game-idle-encounter-clock g)))

;; A zone may keep its own idle vigil — or none: :IDLE-ENCOUNTER-MINUTES
;; overrides the global dial, an explicit NIL makes loitering safe, and
;; a zone that names neither follows *IDLE-ENCOUNTER-MINUTES*.
(check "a zone names no idle period by default" :default
       (dungeon-map-idle-encounter-minutes (parse-map *art* :name "test")))
(check-error ":idle-encounter-minutes rejects garbage"
  (%apply-map-form (parse-map *art* :name "test")
                   '(zone :idle-encounter-minutes :often) "test"))
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 10
                            :idle-encounter-minutes 10)
                   "test")
  (check "the zone's own period parses" 10
         (dungeon-map-idle-encounter-minutes m))
  ;; the zone's short vigil beats the global 30: ten minutes suffice
  (check-true "the zone's own period rules"
              (with-rng (5) (maybe-idle-encounter g 10))))
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 2))
                            :encounter-chance 10
                            :idle-encounter-minutes nil)
                   "test")
  (check "an explicit NIL parses" nil
         (dungeon-map-idle-encounter-minutes m))
  (let ((*rng* (lambda (n) (error "rolled ~D" n))))
    (check "a friendly zone never hunts its loiterers" nil
           (maybe-idle-encounter g 500)))
  (check "and accrues nothing" 0 (game-idle-encounter-clock g)))

;; A cell's scripted (ENCOUNTER ...) preempts the wandering roll — no
;; random number is drawn behind a fight the story already started.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test ogre" 1))
                            :encounter-chance 100)
                   "test")
  (setf (cell-special m 1 0) '((encounter ("test rat" 2)))
        (game-facing g) +east+)
  (let ((*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (move-party g))
  (check "the scripted fight preempts the wandering roll"
         '(("test rat" . 2)) (%group-names g)))

;; A clean kill: hero hits, monster dies, rewards are handed out.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (ended '()))
  (on-event g :combat-end (lambda (game result) (declare (ignore game))
                            (push result ended)))
  (start-combat g '(("test rat" 1)))
  (check "single-monster banner" '("You face 1 test rat!") (funcall msgs))
  (check "victory round" :victory (with-rng (10 2) (combat-round g)))
  (check "combat cleared after victory" nil (game-combat g))
  (check "combat-end event" '(:victory) ended)
  (check "xp awarded" 10 (hero-xp h))
  (check "gold awarded" 6 (hero-gold h))
  ;; the killing blow names its damage — a spell's or an arrow's worth
  ;; shows even when it ends the fight
  (check-true "slain message names the damage"
              (find "Alva SLAYS the test rat for 3 points!"
                    (funcall msgs) :test #'equal))
  (check-true "victory message"
              (find-if (lambda (s) (search "Victory" s)) (funcall msgs))))

;; The Bard's Tale payout: the pot is never divided.  Two rats add up
;; to 20 xp, and their flat 6 gold apiece to 12 — and each hero left
;; standing banks that whole sum, so a hero fights no poorer for
;; having company.  The one who went down takes neither.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ann"))
       (b (%combat-hero "Bo"))
       (c (%combat-hero "Cyd"))
       (g (new-game m :party (list a b c)))
       (msgs (watch-messages g)))
  (setf (hero-hp b) 0)             ; Bo is down when the last rat falls
  (start-combat g '(("test rat" 2)))
  (dolist (mon (combat-monsters (game-combat g)))
    (setf (monster-hp mon) 0))
  (%award-victory g (game-combat g))
  (check "the whole xp, not a share, to each hero standing"
         '(20 20) (list (hero-xp a) (hero-xp c)))
  (check "and the whole gold to each of them as well"
         '(12 12) (list (hero-gold a) (hero-gold c)))
  (check "the fallen hero takes neither"
         '(0 0) (list (hero-xp b) (hero-gold b)))
  (check-true "the victory line speaks the sum each hero takes"
              (find "Victory!  Each hero wins 20 experience and 12 gold."
                    (funcall msgs) :test #'equal)))

;; Miss, get hit back; defending raises AC for the round.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1)))
  ;; hero d20=1 misses; rat targets hero, d20=12 hits ac 8, 1d2 -> 2.
  (check "ongoing round" :ongoing (with-rng (0 0 11 1) (combat-round g)))
  (check "monster damage landed" 6 (hero-hp h))
  ;; defending: ac 8-4=4 needs 16+; rat d20=14 now misses.
  (check "defend round ongoing" :ongoing
         (with-rng (0 13) (combat-round g '(:defend))))
  (check "defender untouched" 6 (hero-hp h))
  (check-true "monster miss message"
              (find-if (lambda (s) (search "misses Alva" s)) (funcall msgs)))
  ;; without defending the same monster roll (d20=14 vs ac 8) hits.
  (check "same roll hits when not defending" :ongoing
         (with-rng (0 0 13 0) (combat-round g)))
  (check "hit for 1d2 minimum" 5 (hero-hp h)))

;; Defeat: the last hero falls to a monster.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (wiped nil))
  (on-event g :party-defeated (lambda (game) (declare (ignore game))
                                (setf wiped t)))
  (start-combat g '(("test rat" 1)))
  (setf (hero-hp h) 1)
  (check "defeat round" :defeat (with-rng (0 0 11 1) (combat-round g)))
  (check "combat cleared after defeat" nil (game-combat g))
  (check-true "party-defeated emitted in combat" wiped))

;; Fleeing: even odds; failure hands the monsters a free round.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (start-combat g '(("test rat" 1)))
  (check "flee success" :fled (with-rng (10) (attempt-flee g)))
  (check "combat cleared after fleeing" nil (game-combat g)))
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1)))
  (check "flee failure is ongoing" :ongoing
         (with-rng (60 0 11 1) (attempt-flee g)))
  (check "free round hurt the party" 6 (hero-hp h))
  (check-true "no escape message"
              (find-if (lambda (s) (search "No escape" s)) (funcall msgs))))

(check-error "combat-round without combat"
  (combat-round (new-game (parse-map *art*))))
(check-error "attempt-flee without combat"
  (attempt-flee (new-game (parse-map *art*))))

;; Monster loot: a type may carry an :ITEM into the fight; after the
;; victory each fallen carrier rolls its :ITEM-CHANCE and the FIRST
;; success hands the item over — one find per fight at most.
(define-item 'test-fang)
(define-monster "test looter"
  :hp-dice 3 :ac 10 :damage "1d2" :xp 5 :gold 0
  :item 'test-fang :item-chance 50)

(check "a monster carries no item unless told to" nil
       (monster-type-item (find-monster-type "test rat")))
(check "the item chance defaults to a sure drop" 100
       (monster-type-item-chance
        (%make-monster-type :name "x" :item 'test-fang)))

;; The drop lands: kill the carrier, roll under the chance, take it.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (start-combat g '(("test looter" 1)))
  ;; d20=10 hits, 1d6=3 slays; the loot roll 49 lands under chance 50
  (check "the carrier falls" :victory (with-rng (10 2 49) (combat-round g)))
  (check-true "the find is spoken"
              (find "The test looter carried a Test Fang!"
                    (funcall msgs) :test #'equal))
  (check-true "and taken"
              (find "Alva takes it." (funcall msgs) :test #'equal))
  (check-true "the item sits in the pack"
              (hero-carrying-p h 'test-fang)))

;; The chance can fail — then nothing is found or said.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (start-combat g '(("test looter" 1)))
  (check "the carrier falls anyway" :victory
         (with-rng (10 2 75) (combat-round g)))
  (check "an unlucky roll finds nothing" nil
         (find-if (lambda (s) (search "carried" s)) (funcall msgs)))
  (check "the pack stays empty" nil (hero-carrying-p h 'test-fang)))

;; Two carriers are a better chance at the same single prize, not a
;; shower of prizes: the first success stops the rolling.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (start-combat g '(("test looter" 2)))
  (check "round one fells the first carrier" :ongoing
         (with-rng (10 2 0 0) (combat-round g)))
  ;; the loot roll 3 succeeds on the first carrier; were the second
  ;; rolled too, the exhausted script's 0 would also drop — the single
  ;; fang below proves it never was
  (check "round two wins" :victory (with-rng (10 2 3) (combat-round g)))
  (check "one find per fight" 1 (count 'test-fang (hero-items h))))

;; With every pack full the find is left behind, whole.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (hero-items h) (make-list +inventory-limit+
                                  :initial-element 'test-fang))
  (start-combat g '(("test looter" 1)))
  (check "the fight is won" :victory (with-rng (10 2 0) (combat-round g)))
  (check-true "the find is spoken"
              (find "The test looter carried a Test Fang!"
                    (funcall msgs) :test #'equal))
  (check-true "but the pack is full"
              (find "Alva's pack is full." (funcall msgs) :test #'equal))
  ;; the filler is fangs too, so count, not carry: the pack must hold
  ;; exactly what it held — the find never squeezed in as one more
  (check "and the fang stays behind" +inventory-limit+
         (length (hero-items h))))

;; The victory picture: campaign data, resolved beside the map like a
;; monster portrait; NIL until a campaign names one.
(let* ((m (parse-map *art* :name "world/deep/test"))
       (g (new-game m :party (list (%combat-hero)))))
  (check "no victory picture by default" nil (victory-image-path g))
  (let ((*victory-image* "gfx/treasure.iff"))
    (check "the victory picture resolves beside the map"
           "world/deep/gfx/treasure.iff" (victory-image-path g))))

;; Reach cuts both ways: the front ranks are the heroes monsters can
;; hit AND the only heroes who can trade melee blows back; a hero
;; behind them attacks only with a strung bow-and-arrow pair.
(define-item 'test-bow :kind :bow)
(define-item 'test-arrows :kind :arrow :damage "1d4")
(define-item 'test-selfbow :kind :bow :damage "1d3")
(define-item 'test-blunts :kind :arrow)

(let* ((m (parse-map *art* :name "test"))
       (heroes (list (%combat-hero "A") (%combat-hero "B")
                     (%combat-hero "C") (%combat-hero "D")))
       (g (new-game m :party heroes)))
  (check "the front ranks stand in reach" '(t t t)
         (mapcar (lambda (h) (hero-in-reach-p g h)) (subseq heroes 0 3)))
  (check "the fourth hero stands behind them" nil
         (hero-in-reach-p g (fourth heroes)))
  (damage-hero g (first heroes) 999)
  (check-true "a fallen front hero pulls the fourth into reach"
              (hero-in-reach-p g (fourth heroes))))

;; HERO-MISSILE-DICE wants the whole pair equipped; the arrows carry
;; the dice, the bow stands in when they name none.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (check "no bow, no missile dice" nil (hero-missile-dice h))
  (dolist (name '(test-bow test-arrows test-selfbow test-blunts))
    (give-item g h name))
  (equip-item g h 'test-bow)
  (check "a bow without arrows shoots nothing" nil (hero-missile-dice h))
  (equip-item g h 'test-arrows)
  (check "bow and arrows make the pair" "1d4" (hero-missile-dice h))
  (unequip-item g h 'test-bow)
  (check "arrows without a bow shoot nothing" nil (hero-missile-dice h))
  (equip-item g h 'test-selfbow)
  (check "the arrows' dice outrank the bow's" "1d4" (hero-missile-dice h))
  (equip-item g h 'test-blunts)         ; same kind: replaces the arrows
  (check "dice-less arrows fall back to the bow's" "1d3"
         (hero-missile-dice h))
  (equip-item g h 'test-bow)            ; no dice anywhere in the pair
  (check "a dice-less pair shoots nothing" nil (hero-missile-dice h)))

;; The reach rule in the round: a bare back-rank :attack is out of
;; reach and wastes the action; strung, the same hero shoots over the
;; front ranks' heads — DEX aims the shot and the arrows alone carry
;; the damage, no STR behind a bowstring.
(let* ((m (parse-map *art* :name "test"))
       (heroes (list (%combat-hero "A") (%combat-hero "B")
                     (%combat-hero "C") (%combat-hero "D")))
       (d (fourth heroes))
       (g (new-game m :party heroes))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1)))    ; 3 hp, ac 10
  ;; d20=20 would slay outright — proof the out-of-reach D never rolls
  ;; it: the 19 lands on the rat's target pick (19 mod 3 -> B), whose
  ;; defence (ac 4) turns the d20=6 away.  D's action is spent saying so.
  (check "a bare back-rank attack wastes the action" :ongoing
         (with-rng (19 5) (combat-round g '(:defend :defend :defend))))
  (check-true "and says why"
              (find-if (lambda (s) (search "D cannot reach the enemy" s))
                       (funcall msgs)))
  (check "the rat stands untouched" 3
         (monster-hp (first (combat-monsters (game-combat g)))))
  ;; string D's bow: DEX 16 (+3) aims, STR 4 (-3) must not weigh in —
  ;; d20=6 hits ac 10 only through the DEX bonus (1+5+1+3 = 10), and
  ;; the 1d4=4 slays the 3-hp rat only left un-dragged-down by STR.
  (give-item g d 'test-bow)
  (give-item g d 'test-arrows)
  (equip-item g d 'test-bow)
  (equip-item g d 'test-arrows)
  (setf (hero-str d) 4 (hero-dex d) 16)
  (check "the strung back rank shoots the round home" :victory
         (with-rng (5 3) (combat-round g '(:defend :defend :defend))))
  (check-true "the arrow slays in the transcript"
              (find-if (lambda (s) (search "D SLAYS the test rat" s))
                       (funcall msgs))))

;; The orders page polices reach up front: A for a bare back-rank
;; hero says so and keeps asking; strung, the same hero's A records.
(let* ((m (parse-map *art* :name "test"))
       (heroes (list (%combat-hero "A") (%combat-hero "B")
                     (%combat-hero "C") (%combat-hero "D")))
       (d (fourth heroes))
       (g (new-game m :party heroes))
       (msgs (watch-messages g))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 1)))
  (combat-orders-act g view #\f)        ; the party engages
  (dotimes (i 3) (combat-orders-act g view #\a))
  (check "the back-rank hero is at hand" "D"
         (hero-name (combat-orders-hero g view)))
  ;; the page already dropped the Attack row for the bare back rank —
  ;; but a scripted A still draws the spoken refusal below
  (check "the bare back rank's page drops Attack"
         '("Defend")
         (last (menu-texts (combat-orders-lines g view)) 1))
  (check "a refused attack returns nil" nil (combat-orders-act g view #\a))
  (check "the refusal keeps asking the same hero" "D"
         (hero-name (combat-orders-hero g view)))
  (check-true "and says why"
              (find-if (lambda (s) (search "D cannot reach the enemy" s))
                       (funcall msgs)))
  (check "defending still stands open" nil (combat-orders-act g view #\d))
  (check-true "the four picks reach the review"
              (combat-orders-review view))
  ;; redo with a strung bow: the same hero's A now records — the
  ;; engagement stands through the redo, no second engage page
  (combat-orders-act g view #\n)
  (give-item g d 'test-bow)
  (give-item g d 'test-arrows)
  (equip-item g d 'test-bow)
  (equip-item g d 'test-arrows)
  (dotimes (i 3) (combat-orders-act g view #\a))
  (check "the strung back rank's page offers Attack"
         '("Attack" "Defend")
         (last (menu-texts (combat-orders-lines g view)) 2))
  (combat-orders-act g view #\a)
  (check "a strung back-rank attack records"
         '(:fight (:attack :attack :attack :attack))
         (combat-orders-act g view #\y)))

;;; ---------------------------------------------------------------------
;;; Distance: where the groups stand, what reaches them, how they close.

;; The opening line-up: the first group at melee, each later one
;; *COMBAT-GROUP-SPACING* further back, and COMBAT-GROUPS reads them
;; back nearest first with the distance on every row.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1) ("test ogre" 2) ("test rat" 1)))
  (check "the groups line up from melee backwards" '(10 30 50)
         (mapcar #'third (combat-groups (game-combat g))))
  (check "two groups of one kind stay two rows"
         '(("test rat" . 1) ("test ogre" . 2) ("test rat" . 1))
         (%group-names g))
  (check "the nearest group is what reach is measured against" 10
         (combat-distance (game-combat g)))
  (check "the banner says how far off the standing groups are"
         '("You face 1 test rat and 2 test ogres at 30 feet and 1 test rat at 50 feet!")
         (funcall msgs)))

;; An encounter may put its group where it likes, and the banner then
;; opens on the distance alone.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 2 40)))
  (check "an encounter names its own distance" 40
         (combat-distance (game-combat g)))
  (check "and the banner opens on it"
         '("You face 2 test rats at 40 feet!") (funcall msgs)))

;; Closing: the far group walks one *COMBAT-CLOSE-STEP* nearer at the
;; end of every round and stops at melee, saying so both ways.  Until
;; it arrives nobody trades blows: it cannot swing, and the front rank
;; has nothing to swing at.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero "A"))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1 30)))
  (check "a distant group is out of melee" nil (hero-can-attack-p g h))
  (check "and the round passes with nothing landed" :ongoing
         (combat-round g '(:attack)))
  (check-true "the hero says it cannot reach"
              (find-if (lambda (s) (search "A cannot reach the enemy" s))
                       (funcall msgs)))
  (check "the group has walked a step in" 20
         (combat-distance (game-combat g)))
  (check "the rat stands untouched" 3
         (monster-hp (first (combat-monsters (game-combat g)))))
  (combat-round g '(:defend))
  (check "a second round brings it to melee" 10
         (combat-distance (game-combat g)))
  (check-true "and the arrival is announced, one rat closing alone"
              (find-if (lambda (s) (search "The 1 test rat closes in!" s))
                       (funcall msgs)))
  (check-true "now the front rank can swing" (hero-can-attack-p g h)))

;; A line of several walks in the plural, and the orders page carries
;; each group's remaining feet in its own narrow column.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero "A"))))
       (msgs (watch-messages g))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 2 30)))
  (check "the orders page shows the feet still to cover"
         '("  2 test rats (30')")
         (remove-if-not (lambda (s) (search "test rat" s))
                        (combat-orders-lines g view)))
  (combat-round g '(:defend))
  (check-true "a line of two advances in the plural"
              (find "The 2 test rats advance to 20 feet." (funcall msgs)
                    :test #'string=))
  (combat-round g '(:defend))
  (check-true "and closes in the plural"
              (find "The 2 test rats close in!" (funcall msgs)
                    :test #'string=))
  (check "a group at melee names no distance on the page"
         '("  2 test rats")
         (remove-if-not (lambda (s) (search "test rat" s))
                        (combat-orders-lines g view))))

;; A pinned skirmish line: with *COMBAT-CLOSE-STEP* off, nothing ever
;; closes and only reach decides the fight.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (*combat-close-step* nil))
  (start-combat g '(("test rat" 1 30)))
  (combat-round g '(:defend))
  (check "a nailed-down group holds its ground" 30
         (combat-distance (game-combat g))))

;; A measured missile carries exactly as far as it says: the same
;; back-rank hero shoots the group at 30 feet and falls short of the
;; one at 50.  An unmeasured missile carries however far it must.
(define-item 'test-longbow :kind :bow)
(define-item 'test-flight :kind :arrow :damage "1d4" :reach 30)

(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero "A"))
       (g (new-game m :party (list h))))
  (dolist (name '(test-longbow test-flight test-arrows))
    (give-item g h name))
  (equip-item g h 'test-longbow)
  (equip-item g h 'test-flight)
  (check "the arrows carry their own reach" 30 (hero-missile-reach h))
  (start-combat g '(("test rat" 1 30)))
  (check-true "a flight that just reaches may shoot"
              (hero-can-attack-p g h))
  (setf (monster-distance (first (combat-monsters (game-combat g)))) 50)
  (check "a foot too far and the shot is refused" nil
         (hero-can-attack-p g h))
  (equip-item g h 'test-arrows)          ; same kind: replaces the flight
  (check "arrows the campaign never measured have no reach" nil
         (hero-missile-reach h))
  (check-true "and they carry however far the fight asks"
              (hero-can-attack-p g h)))

;; A thrown weapon is a missile in its own right: a :REACH on a weapon
;; shoots from the back rank with no bow beside it, and a strung pair
;; still wins when the hero carries both.
(define-item 'test-shuriken :kind :weapon :damage "1d6" :reach 40)

(let* ((m (parse-map *art* :name "test"))
       (heroes (list (%combat-hero "A") (%combat-hero "B")
                     (%combat-hero "C") (%combat-hero "D")))
       (d (fourth heroes))
       (g (new-game m :party heroes)))
  (give-item g d 'test-shuriken)
  (check "an unequipped throw is no missile" nil (hero-missile-dice d))
  (equip-item g d 'test-shuriken)
  (check "a thrown weapon needs no bow" "1d6" (hero-missile-dice d))
  (check "and throws as far as it says" 40 (hero-missile-reach d))
  (start-combat g '(("test rat" 1 40)))
  (check-true "so the back rank throws" (hero-can-attack-p g d))
  (dolist (name '(test-longbow test-flight))
    (give-item g d name))
  (equip-item g d 'test-longbow)
  (equip-item g d 'test-flight)
  (check "a strung pair wins over the throw" "1d4" (hero-missile-dice d))
  (check "reach follows the shot in hand" 30 (hero-missile-reach d)))

;; Melee comes first: a front-rank hero with a bow swings at the group
;; on top of them rather than shooting over it.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero "A"))
       (g (new-game m :party (list h))))
  (dolist (name '(test-longbow test-flight))
    (give-item g h name))
  (equip-item g h 'test-longbow)
  (equip-item g h 'test-flight)
  (start-combat g '(("test rat" 1)))
  (check-true "at melee the front rank swings"
              (eq (hero-strike-function g h) #'%hero-attack))
  (setf (monster-distance (first (combat-monsters (game-combat g)))) 30)
  (check-true "standing off, the same hero shoots"
              (eq (hero-strike-function g h) #'%hero-shoot)))

;;; The enemy's own reach: a type given a :MISSILE answers from where
;;; it stands instead of spending the round walking, and a type given a
;;; :SPEED covers the ground at its own gait.

(define-monster "test archer"
  :level 1 :hp-dice 3 :ac 10 :damage "1d2"
  :missile "1d2" :missile-reach 30)

(check "the missile is monster data" "1d2"
       (monster-type-missile (find-monster-type "test archer")))
(check "and carries a measured reach" 30
       (monster-type-missile-reach (find-monster-type "test archer")))
(check "whose word for a landed shot defaults" "SHOOTS"
       (monster-type-missile-verb (find-monster-type "test archer")))
(check "a plain fighter carries no missile" nil
       (monster-type-missile (find-monster-type "test rat")))
(check "and no gait of its own" nil
       (monster-type-speed (find-monster-type "test rat")))

;; A shot flies over the ranks: the archer standing off at 30 feet
;; finds the FOURTH hero, whom no melee blow of its could reach.
(let* ((m (parse-map *art* :name "test"))
       (heroes (list (%combat-hero "A") (%combat-hero "B")
                     (%combat-hero "C") (%combat-hero "D")))
       (d (fourth heroes))
       (g (new-game m :party heroes))
       (msgs (watch-messages g)))
  (start-combat g '(("test archer" 1 30)))
  (check "the back rank is out of melee reach" nil (hero-in-reach-p g d))
  (check-true "but the archer answers from where it stands"
              (monster-can-shoot-p
               (first (combat-monsters (game-combat g)))))
  (with-rng (3 19 1)
    (combat-round g '(:defend :defend :defend :defend)))
  (check-true "and the shot finds the back rank"
              (find "The test archer SHOOTS D for 2 damage." (funcall msgs)
                    :test #'string=))
  (check "which cost D hit points the front rank never paid for" 6
         (hero-hp d))
  (check "the line still covered its ground" 20
         (combat-distance (game-combat g))))

;; A missile that falls short is no answer at all: the same archer met
;; at 40 feet spends its round walking, and shoots only once the ground
;; between is inside its reach.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero "A"))))
       (msgs (watch-messages g)))
  (start-combat g '(("test archer" 1 40)))
  (check "the shot falls ten feet short" nil
         (monster-can-shoot-p (first (combat-monsters (game-combat g)))))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check "so the round is the ground it covers" 30
         (combat-distance (game-combat g)))
  (check-true "and nothing was loosed"
              (notany (lambda (s) (search "SHOOTS" s)) (funcall msgs)))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check-true "the next round it stands in its own range and shoots"
              (find "The test archer SHOOTS A for 2 damage." (funcall msgs)
                    :test #'string=)))

;; Melee comes first for the enemy too: a group that has closed swings
;; its :DAMAGE rather than shooting over a rank it is standing on.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero "A"))))
       (msgs (watch-messages g)))
  (start-combat g '(("test archer" 1)))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check-true "at melee it HITS instead"
              (find "The test archer HITS A for 2 damage." (funcall msgs)
                    :test #'string=)))

;; An unmeasured missile carries however far the fight asks — the hero
;; side's rule — and its word is the campaign's to choose.  What a
;; landed shot carries besides damage is what a landed blow carries.
(define-monster "test howler"
  :level 1 :hp-dice 3 :ac 10 :damage "1d2"
  :missile "1d2" :missile-verb "WAILS AT" :inflicts '((:poison 100)))

(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero "A"))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (start-combat g '(("test howler" 1 50)))
  (check-true "a missile the campaign never measured always reaches"
              (monster-can-shoot-p (first (combat-monsters (game-combat g)))))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check-true "and speaks in the campaign's own word"
              (find "The test howler WAILS AT A for 2 damage." (funcall msgs)
                    :test #'string=))
  (check-true "a venom rides the shot as it rides the blow"
              (hero-ailment-p h :poison)))

;; A gait of its own: :SPEED is the feet a type covers in a round, and
;; the charger crosses in two what the shambler would take four over.
(define-monster "test charger"
  :level 1 :hp-dice 3 :ac 10 :damage "1d2" :speed 30)

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero "A"))))
       (msgs (watch-messages g)))
  (start-combat g '(("test charger" 1 50)))
  (check "the type carries its own gait" 30
         (monster-step (first (combat-monsters (game-combat g)))))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check "and covers it in one round" 20 (combat-distance (game-combat g)))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check "the second brings it to melee" 10
         (combat-distance (game-combat g)))
  (check-true "arriving in the words every group arrives in"
              (find "The 1 test charger closes in!" (funcall msgs)
                    :test #'string=)))

;; :SPEED 0 with a missile is the enemy that stands off and shoots: it
;; never closes, and the party must out-shoot it or run.
(define-monster "test lurker"
  :level 1 :hp-dice 3 :ac 10 :damage "1d2"
  :missile "1d2" :missile-verb "SPITS AT" :speed 0)

(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero "A"))))
       (msgs (watch-messages g)))
  (start-combat g '(("test lurker" 1 40)))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check "it holds the ground it was met on" 40
         (combat-distance (game-combat g)))
  (check-true "and answers from it"
              (find "The test lurker SPITS AT A for 2 damage." (funcall msgs)
                    :test #'string=)))

;; The dial is the default, not the law: with *COMBAT-CLOSE-STEP* off
;; the plain line is pinned where the fight found it while a type with
;; a gait of its own still walks.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero "A"))))
       (*combat-close-step* nil))
  (start-combat g '(("test rat" 1 30) ("test charger" 1 50)))
  (check "a type with no speed of its own follows the dial" nil
         (monster-step (first (combat-monsters (game-combat g)))))
  (with-rng (0 19 1) (combat-round g '(:defend)))
  (check "so the pinned line holds" 30
         (monster-distance (first (combat-monsters (game-combat g)))))
  (check "and the charger keeps its own pace" 20
         (monster-distance (second (combat-monsters (game-combat g))))))

;; Bad monster data fails at registration, not mid-fight.
(check-error "a reach wants a missile to carry it"
  (define-monster "test bad" :missile-reach 30))
(check-error "and the reach is feet, a positive number of them"
  (define-monster "test bad" :missile "1d2" :missile-reach 0))
(check-error "missile dice must parse"
  (define-monster "test bad" :missile "d6"))
(check-error "the missile's word must be one"
  (define-monster "test bad" :missile "1d2" :missile-verb ""))
(check-error "and a speed is feet per round, never backwards"
  (define-monster "test bad" :speed -10))
(check-error "an unregistered failure leaves no type behind"
  (find-monster-type "test bad"))

;; The strikes land on the NEAREST group, not on the encounter's first.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (start-combat g '(("test rat" 1 50) ("test ogre" 1 10)))
  (check "the nearest monster is what a blow finds" "test ogre"
         (monster-type-name (monster-kind (nearest-monster (game-combat g)))))
  (check "and MONSTERS-IN-REACH keeps to its feet" '("test ogre")
         (mapcar (lambda (mo) (monster-type-name (monster-kind mo)))
                 (monsters-in-reach (game-combat g) 30)))
  (check "an unmeasured reach takes the whole field" 2
         (length (monsters-in-reach (game-combat g) nil))))

;; The encounter special starts combat and skips the remaining ops.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((encounter ("test rat" 1))
                               (message "never shown")))
  (turn-right g)
  (check "moving onto an encounter still reports the step" :moved
         (move-party g :forward))
  (check-true "encounter started combat" (game-combat g))
  (check "ops after encounter are skipped" '("You face 1 test rat!")
         (funcall msgs)))

;;; ---------------------------------------------------------------------
;;; Spells: casters, spell points, DEFINE-SPELL, casting, the cast menu

(define-hero-class :t-mage :hp-dice "1d6+3" :damage "1d4" :ac 10 :caster t)

(defun %combat-mage (&optional (name "Zzgo"))
  "A deterministic level-1 :t-mage: 7 hp, iq 18 (+4 bonus, 6 sp)."
  (let ((h (with-rng (3  0 0 0  0 0 0  5 5 5) (make-hero name :t-mage))))
    (setf (hero-str h) 10)
    h))

;; Casters and spell points.
(let ((mage (%combat-mage))
      (grunt (%combat-hero)))
  (check-true "a :caster class hero is a caster" (hero-caster-p mage))
  (check "caster sp: 2 per level + iq bonus" 6 (hero-max-sp mage))
  (check "caster starts at full sp" 6 (hero-sp mage))
  (check-true "a plain class hero is no caster" (not (hero-caster-p grunt)))
  (check "non-caster has no sp" 0 (hero-max-sp grunt))
  (check "caster sheet shows sp on its own line" "SP 6/6"
         (fourth (hero-summary-lines mage)))
  (check-true "non-caster sheet stays sp-free"
              (not (find-if (lambda (s) (search "SP " s))
                            (hero-summary-lines grunt)))))

;; Leveling grows sp like hp: the new maximum arrives ready to burn.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-mage))
       (g (new-game m :party (list h))))
  (setf (hero-sp h) 1)
  (award-xp g h 100)
  (with-rng (2) (advance-level g h))    ; level 2: max-sp 2*2+4 = 8
  (check "level-up raises max sp" 8 (hero-max-sp h))
  (check "level-up adds the growth to current sp" 3 (hero-sp h)))

;; DEFINE-SPELL validation and the registry.
(define-spell 'test-bolt  :cost 2 :level 1 :classes '(:t-mage)
  :damage "1d4")
(define-spell 'test-mend  :cost 2 :level 1 :classes '(:t-mage)
  :heal "1d8")
(define-spell 'test-shield :cost 3 :level 1 :classes '(:t-mage)
  :buff-ac 2 :duration 30)
(define-spell 'test-flame :cost 1 :level 1 :classes '(:t-mage)
  :light t :duration 60)
(define-spell 'test-lore  :cost 1 :level 3 :classes '(:t-mage)
  :heal "1d4")
(define-spell 'test-needle :cost 1 :level 1 :classes '(:t-mage)
  :compass t :duration 45 :image "fx-needle.iff")

(check "spell-title downcases the name" "test bolt" (spell-title 'test-bolt))
(check-error "unknown spell rejected" (find-spell-type 'test-nonesuch))
(check-error "define-spell rejects an unknown effect key"
  (define-spell 'test-bogus :sparkle t))
(check-error "define-spell needs an effect"
  (define-spell 'test-bogus :cost 1))
(check-error "a timed spell needs a duration"
  (define-spell 'test-bogus :buff-ac 1))
(check-error "a compass spell needs a duration too"
  (define-spell 'test-bogus :compass t))
(check-error "duration must be a positive integer"
  (define-spell 'test-bogus :light t :duration -5))

;; Knowledge gates: class, level, caster-ness.
(let ((mage (%combat-mage))
      (grunt (%combat-hero)))
  (check-true "mage knows a class spell" (spell-known-p mage 'test-bolt))
  (check-true "non-caster knows nothing" (not (spell-known-p grunt 'test-bolt)))
  (check-true "level gate holds" (not (spell-known-p mage 'test-lore)))
  (setf (hero-level mage) 3)
  (check-true "level gate opens" (spell-known-p mage 'test-lore))
  (check "known spells in registration order"
         '(test-bolt test-mend test-shield test-flame test-lore test-needle)
         (spells-for-hero mage)))

;; The spellbook lives on the sheet carousel's spells/songs page
;; (MAGIC-LINES) — SPELLS-FOR-HERO under a Spells: head, each row
;; numbered so a digit opens its card, the carousel's NEXT row closing
;; the page.  There is no learning step: a fresh level's spells simply
;; appear with the rise.  A plain hero has no such page at all
;; (HERO-MAGIC-P gates it in the front-ends), and the stat block
;; itself stays book-free.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (grunt (%combat-hero))
       (g (new-game m :party (list mage grunt)))
       (v (make-magic-view mage)))
  (check-true "the caster has a spells/songs page" (hero-magic-p mage))
  (check-true "the plain hero has none" (not (hero-magic-p grunt)))
  (check "the stat block carries no spellbook" nil
         (member "Spells:" (hero-summary-lines mage) :test #'equal))
  (check "the entries are the known spells, tagged"
         '((:spell . test-bolt) (:spell . test-mend) (:spell . test-shield)
           (:spell . test-flame) (:spell . test-needle))
         (magic-entries mage))
  (check "the page numbers the book under its head"
         '("Spells:" "1) test bolt" "2) test mend" "3) test shield"
           "4) test flame" "5) test needle")
         (menu-texts (butlast (magic-lines g v) 2)))
  (check "each row carries its pick digit" #\3
         (menu-line-key (fourth (magic-lines g v))))
  (check "the page closes with the NEXT row"
         (list "" (menu-next-option))
         (last (magic-lines g v) 2))
  (setf (hero-level mage) 3)
  (check-true "a new level's spells appear on the page"
              (find-if (lambda (s) (search "test lore" s))
                       (menu-texts (magic-lines g v))))
  ;; six spells known at level 3: the book fits +BOOK-PAGE-SIZE+ whole,
  ;; so it does not scroll
  (check "a short book does not scroll" nil
         (progn (magic-lines g v) *menu-scroll*))
  (check "and d moves nothing" nil (magic-act g v #\d))
  (check "an empty book says so" '("The book is empty.")
         (menu-texts (butlast (magic-lines g (make-magic-view grunt)) 2))))

;; The spell card (a digit on the page — there is no separate inspect
;; mode, the numbered rows ARE the inspection): the tier and what the
;; cast costs against the caster's own points, the incantation, the
;; spellbook's range and duration words, then what the spell does —
;; and the key that casts it right there.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (v (make-magic-view mage)))
  (check "a digit opens that entry's card" '(:spell . test-mend)
         (progn (magic-act g v #\2)
                (magic-view-pending v)))
  (let ((texts (menu-texts (magic-lines g v))))
    (check "the card heads with the title" "*** test mend ***"
           (first texts))
    (check-true "it names the tier and the cost against the purse"
                (member "Tier 1   2 sp (of 6)" texts :test #'equal))
    (check-true "it reads the effect out of the spec"
                (member "Heals 1-8" texts :test #'equal))
    (check-true "and offers the cast"
                (member (menu-option #\c "Cast it") (magic-lines g v)
                        :test #'equal)))
  (check "Esc goes back to the list" nil
         (progn (magic-act g v #\Escape)
                (magic-view-pending v)))
  ;; a campaign's own words win over the derived line
  (define-spell 'test-worded :cost 1 :level 1 :classes '(:t-mage)
    :damage "1d4" :description "It stings the eyes.")
  (check "a :description speaks instead of the effect"
         '("*** test worded ***" "" "Tier 1   1 sp (of 6)" ""
           "It stings the eyes.")
         (menu-texts (spell-card-lines mage 'test-worded)))
  (check "spell-description reads it back" "It stings the eyes."
         (spell-description 'test-worded))
  (check "a spell without one has none" nil
         (spell-description 'test-mend))
  (check-error "define-spell rejects a non-string :description"
    (define-spell 'test-bogus :damage "1d4" :description :nope)))

;; BEGIN-CAST — the card's 'c', a cast started outside the cast menu.
;; Three outcomes: a spell that needs aiming hands back a CAST-VIEW the
;; front-end goes on with (already holding caster and spell, so the
;; menu opens straight on the target question); one that needs no
;; aiming resolves on the spot; a refusal says why and resolves
;; nothing.  The sheet's page turns them into :CAST / :DONE.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage (%combat-hero))))
       (msgs (watch-messages g)))
  ;; test-mend heals a chosen hero: the aim is still to come
  (let ((view (begin-cast g mage 'test-mend)))
    (check-true "an aiming spell hands back a cast view" (cast-view-p view))
    (check "with the caster already picked" mage (cast-view-hero view))
    (check "and the spell already picked" 'test-mend (cast-view-spell view))
    (check "the page reports it as a handoff" :cast
           (let ((v (make-magic-view mage)))
             (magic-act g v #\2)          ; test-mend's card
             (first (magic-act g v #\c))))
    (check "no points are spent until it lands" 6 (hero-sp mage)))
  ;; test-flame is a light spell: nothing to aim at, so it resolves
  (check "a spell that needs no aim resolves on the spot" nil
         (begin-cast g mage 'test-flame))
  (check-true "and it cost the points" (< (hero-sp mage) 6))
  (check-true "the log carries the cast"
              (find-if (lambda (s) (search "test flame" s)) (funcall msgs)))
  ;; drained of points, the same spell is refused — with a reason
  (setf (hero-sp mage) 0)
  (check "an unaffordable spell resolves nothing" nil
         (begin-cast g mage 'test-flame))
  (check-true "and says why"
              (find-if (lambda (s) (search "cannot cast" s))
                       (funcall msgs))))

(define-spell 'test-warp :cost 1 :level 1 :classes '(:t-mage)
  :teleport 3)

;; A refusal on the card keeps the card up and puts the reason ON it:
;; the takeover owns the whole page, so a logged line would go unread,
;; and closing the sheet to show it would lose the player's place.
;; SPELL-REFUSAL names the reason in card-width words.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (v (make-magic-view mage)))
  (check "a castable spell has no refusal" nil
         (spell-refusal g mage 'test-flame))
  (setf (hero-sp mage) 0)
  (check "an empty purse names itself" "Not enough spell points."
         (spell-refusal g mage 'test-flame))
  ;; test-bolt is a damage spell: it wants a fight
  (setf (hero-sp mage) 6)
  (check "a battle spell wants a fight" "Only in a fight."
         (spell-refusal g mage 'test-bolt))
  ;; test-lore is level 3; a level-1 mage never learned it
  (check "an unlearned spell names itself" "Not in the book."
         (spell-refusal g mage 'test-lore))
  ;; test-warp is a real (integer) teleport: mid-fight there is no
  ;; walking away through folded space
  (start-combat g '(("test rat" 1)))
  (check "a real teleport wants no fight" "Not in a fight."
         (spell-refusal g mage 'test-warp))
  (setf (game-combat g) nil)
  ;; the card carries it, and the page stays put
  (setf (hero-sp mage) 0)
  (magic-act g v #\4)                   ; test-flame's card
  (check "the refused cast keeps the card" nil (magic-act g v #\c))
  (check "the card is still up" '(:spell . test-flame)
         (magic-view-pending v))
  (check "with the reason on it" "Not enough spell points."
         (magic-view-refusal v))
  (check-true "and the page shows it"
              (member "Not enough spell points."
                      (menu-texts (magic-lines g v)) :test #'equal))
  (check "no points were spent" 0 (hero-sp mage))
  ;; the next key clears it — the reason has been read
  (magic-act g v #\Escape)
  (check "a later press clears the reason" nil (magic-view-refusal v))
  ;; with points again the same card casts
  (setf (hero-sp mage) 6)
  (magic-act g v #\4)
  (check "the affordable cast goes off" :done (magic-act g v #\c))
  (check-true "and it cost the points" (< (hero-sp mage) 6)))

;; MAGIC-ACT's card-level 'c' must not confuse BEGIN-CAST's two NIL
;; outcomes: a refusal has to keep the card open so the player reads
;; why and tries another spell, not fall through to :DONE and close
;; the whole sheet as if a cast had resolved.  The refusal reaches
;; them on the card rather than through the log — the check above
;; covers the whole path; what matters here is that the page stays
;; quiet, since a line logged under a takeover is a line never seen.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (v (make-magic-view mage))
       (msgs (watch-messages g)))
  (setf (hero-sp mage) 0)
  (magic-act g v #\4)                  ; test-flame's card
  (check "an unaffordable cast leaves the card open" nil
         (magic-act g v #\c))
  (check "the card is still showing" '(:spell . test-flame)
         (magic-view-pending v))
  (check "the refusal does not go to the unread log" nil
         (find-if (lambda (s) (search "cannot cast" s)) (funcall msgs)))
  (check "it stands on the card instead" "Not enough spell points."
         (magic-view-refusal v))
  (check "no points were spent" 0 (hero-sp mage)))

;; The rise itself names the spells it brings — the book is
;; level-gated, so LEVEL-UP diffs it around the bump and announces
;; each arrival; a level that brings none stays quiet about spells.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-mage))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (award-xp g h 100)
  (with-rng () (advance-level g h))     ; level 2: no new spells
  (check "a level without new spells stays quiet about the book" nil
         (find-if (lambda (s) (search "learns" s)) (funcall msgs)))
  (award-xp g h 200)
  (with-rng () (advance-level g h))     ; level 3: test-lore arrives
  (check-true "the rise names the arriving spell"
              (member "Zzgo learns test lore!" (funcall msgs)
                      :test #'equal)))

;; Cast refusals: each says why, costs nothing, returns NIL.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (check "damage spell out of combat refused" nil
         (cast-spell g mage 'test-bolt))
  (check-true "out-of-combat refusal says why"
              (find-if (lambda (s) (search "nothing to strike" s))
                       (funcall msgs)))
  (setf (hero-sp mage) 1)
  (check "unaffordable spell refused" nil (cast-spell g mage 'test-mend))
  (check-true "no-sp refusal says why"
              (find-if (lambda (s) (search "lacks the spell points" s))
                       (funcall msgs)))
  (check "refusals cost no sp" 1 (hero-sp mage))
  (check "unknown-to-the-hero spell refused" nil
         (cast-spell g (%combat-hero) 'test-bolt)))

;; Damage cast in combat: auto-hits the first living monster.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g))
       (cast '()))
  (on-event g :spell-cast (lambda (game h name) (declare (ignore game))
                            (push (list (hero-name h) name) cast)))
  (start-combat g '(("test rat" 1)))    ; 3 hp
  (check-true "bolt slays the rat"
              (with-rng (2) (cast-spell g mage 'test-bolt)))  ; 1d4 -> 3
  (check "cast paid its sp" 4 (hero-sp mage))
  (check ":spell-cast emitted" '(("Zzgo" test-bolt)) cast)
  (check-true "cast announced"
              (find-if (lambda (s) (search "casts test bolt" s))
                       (funcall msgs)))
  ;; ... and names the spell's damage, like every killing blow
  (check-true "spell kill reads like a kill and names its damage"
              (find "Zzgo SLAYS the test rat for 3 points!"
                    (funcall msgs) :test #'equal)))

;; Heal targets a chosen hero; buffs and light become timed effects.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt mage))))
  (damage-hero g grunt 5)
  (check-true "mend heals the chosen hero"
              (with-rng (3) (cast-spell g mage 'test-mend grunt)))
  (check "heal landed on the target" 7 (hero-hp grunt))
  (check-true "shield casts" (cast-spell g mage 'test-shield))
  (check "shield lowers the party's effective ac" 6
         (hero-effective-ac grunt g))
  (check "plain ac untouched without game context" 8
         (hero-effective-ac grunt))
  (check "shield is a timed effect"
         (+ (game-time g) 30)
         (effect-expires-at (find-effect g "test shield")))
  (check-true "flame casts" (cast-spell g mage 'test-flame))
  (check-true "flame lights the party" (light-active-p g))
  (advance-time g 30)
  (check "expired shield stops shielding" 8 (hero-effective-ac grunt g))
  (check-true "flame still burns" (light-active-p g))
  (check "recasting an active effect pays and succeeds" 0
         (progn (setf (hero-sp mage) 1)
                (cast-spell g mage 'test-flame)
                (hero-sp mage)))
  (check "recast refreshed the flame's expiry"
         (+ (game-time g) 60)
         (effect-expires-at (find-effect g "test flame"))))

;; A compass spell: the party knows its facing only while it burns.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (check "the party starts without a compass" nil (compass-active-p g))
  (check-true "needle casts" (cast-spell g mage 'test-needle))
  (check-true "the party knows its facing" (compass-active-p g))
  (check "the effect carries the spell's icon" "fx-needle.iff"
         (effect-image (find-effect g "test needle")))
  (check "the compass is a timed effect" (+ (game-time g) 45)
         (effect-expires-at (find-effect g "test needle")))
  (advance-time g 45)
  (check "an expired compass leaves the party lost" nil
         (compass-active-p g)))

;;; ---------------------------------------------------------------------
;;; Bard songs: singers and tunes, one song at a time, the tavern

(define-hero-class :t-bard :hp-dice "1d8" :damage "1d4" :ac 9 :singer t)
(define-song 'test-march :buff-ac 2 :duration 20)
(define-song 'test-gleam :light t :duration 20 :image "fx-gleam.iff")
(define-song 'test-dirge :level 3 :compass t :duration 20
  :notes "canon: a marching dirge (to come)")

(check "song-title downcases the name" "test march" (song-title 'test-march))
(check "song :notes rides along as data" "canon: a marching dirge (to come)"
       (song-type-notes (find-song-type 'test-dirge)))
(check "no song :notes reads NIL" nil
       (song-type-notes (find-song-type 'test-march)))
(check-error "define-song rejects non-string :notes"
  (define-song 'test-bogus :light t :duration 5 :notes '(:very :loud)))
(check-error "unknown song rejected" (find-song-type 'test-nonesuch))
(check-error "define-song rejects an instant effect"
  (define-song 'test-bogus :damage "1d4" :duration 5))
(check-error "define-song needs an effect"
  (define-song 'test-bogus :duration 5))
(check-error "a song needs a duration"
  (define-song 'test-bogus :light t))
(check-error "song duration must be a positive integer"
  (define-song 'test-bogus :light t :duration -5))

;; Singers and tunes: one charge per level, rested singers start full.
(let ((bard (with-rng () (make-hero "Mel" :t-bard)))
      (grunt (%combat-hero)))
  (check-true "the bard is a singer" (hero-singer-p bard))
  (check "a fresh bard holds one tune" 1 (hero-tunes bard))
  (check "max tunes follow the level" 1 (hero-max-tunes bard))
  (setf (hero-level bard) 3)
  (check "max tunes grow with the level" 3 (hero-max-tunes bard))
  (setf (hero-level bard) 1)
  (check-true "the grunt is no singer" (not (hero-singer-p grunt)))
  (check "non-singers hold no tunes" 0 (hero-max-tunes grunt))
  (check-true "the bard knows the level-1 songs"
              (song-known-p bard 'test-march))
  (check-true "the level gate holds" (not (song-known-p bard 'test-dirge)))
  (check-true "non-singers know nothing" (not (song-known-p grunt 'test-march)))
  (check "known songs in registration order" '(test-march test-gleam)
         (songs-for-hero bard))
  (check "the singer's sheet shows the tunes on their own line"
         "Tunes 1/1"
         (fourth (hero-summary-lines bard))))

;; SING-SONG: refusals say why; a song is a timed :SONG-marked effect
;; and a new song displaces the old (one tune at a time).
(let* ((m (parse-map *art* :name "test"))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt bard)))
       (msgs (watch-messages g))
       (sung '()))
  (on-event g :song-sung
            (lambda (game hero name) (declare (ignore game hero))
              (push name sung)))
  (check "a non-singer cannot sing" nil (sing-song g grunt 'test-march))
  (check-true "does-not-know message"
              (find-if (lambda (s) (search "does not know" s))
                       (funcall msgs)))
  (check "no song plays yet" nil (current-song g))
  (check-true "the bard strikes up the march"
              (sing-song g bard 'test-march))
  (check "the tune was spent" 0 (hero-tunes bard))
  (check "the march is the current song" "test march"
         (effect-name (current-song g)))
  (check-true "the song effect carries the :song marker"
              (getf (effect-payload (current-song g)) :song))
  (check "the march shields the party" 2 (effects-ac-bonus g))
  (check "the march is timed" (+ (game-time g) 20)
         (effect-expires-at (current-song g)))
  (check ":song-sung emitted" '(test-march) sung)
  ;; out of tunes: the tavern beckons
  (check "no tunes, no song" nil (sing-song g bard 'test-gleam))
  (check-true "no-tunes message names the tavern"
              (member "Mel has no tunes left - the tavern would help."
                      (funcall msgs) :test #'string=))
  (check-true "the march still plays" (current-song g))
  ;; a new song displaces the old
  (setf (hero-tunes bard) 1)
  (check-true "the bard strikes up the gleam"
              (sing-song g bard 'test-gleam))
  (check "the gleam displaced the march" "test gleam"
         (effect-name (current-song g)))
  (check "only one song plays" 1
         (count-if (lambda (e) (getf (effect-payload e) :song))
                   (game-effects g)))
  (check "the march's shield left with it" 0 (effects-ac-bonus g))
  (check-true "the gleam lights the party" (light-active-p g))
  (check "the song carries its icon" "fx-gleam.iff"
         (effect-image (current-song g)))
  ;; songs fade on the clock like any timed effect
  (advance-time g 20)
  (check "the faded song is gone" nil (current-song g))
  (check-true "the fade is announced"
              (find-if (lambda (s) (search "wears off" s))
                       (funcall msgs))))

;; The music needs an instrument in hand: a :SINGER class may name a
;; :SINGS-WITH item kind, and then a singer with empty hands plays
;; nothing.  Carrying is not enough — the kind must be EQUIPPED.
(define-hero-class :t-piper :hp-dice "1d8" :damage "1d4" :ac 9
                            :singer t :sings-with :instrument)
(define-item 't-lute :kind :instrument :price 40)
(define-item 't-drum :kind :instrument :price 60)

(check-error "sings-with must name an item kind"
  (define-hero-class :t-bogus :singer t :sings-with :kazoo))
(check-error "sings-with needs a singer to sing"
  (define-hero-class :t-bogus :sings-with :instrument))
(check ":sings-with reads back off the class" :instrument
       (hero-class-property :t-piper :sings-with))

(let* ((m (parse-map *art* :name "test"))
       (piper (with-rng () (make-hero "Pip" :t-piper)))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (g (new-game m :party (list piper bard)))
       (msgs (watch-messages g)))
  (check "the piper's class names its tool" :instrument
         (hero-sings-with piper))
  (check "a bare-handed class names none" nil (hero-sings-with bard))
  (check "empty hands hold no instrument" nil (hero-song-tool piper))
  ;; no instrument: refused, and the tune is not spent
  (check "no instrument, no song" nil (sing-song g piper 'test-march))
  (check "the tune was not spent" 1 (hero-tunes piper))
  ;; exact: the message's own characters are part of the check (the
  ;; Amiga font has no glyphs past ASCII)
  (check-true "the refusal names the instrument"
              (member "Pip has no instrument in hand - the music needs one."
                      (funcall msgs) :test #'string=))
  (check "no song plays" nil (current-song g))
  (check "the card's reason is short" "No instrument in hand."
         (song-refusal piper 'test-march))
  (check "and it is not playable" nil (song-playable-p piper 'test-march))
  ;; carrying the lute is not playing it
  (give-item g piper 't-lute)
  (check "a packed lute is still no instrument in hand" nil
         (hero-song-tool piper))
  (check "so the song is still refused" nil (sing-song g piper 'test-march))
  ;; equipped: the music plays
  (equip-item g piper 't-lute)
  (check "the equipped lute is the tool" 't-lute (hero-song-tool piper))
  (check-true "and now the piper plays" (song-playable-p piper 'test-march))
  (check-true "the piper strikes up the march"
              (sing-song g piper 'test-march))
  (check "now the tune was spent" 0 (hero-tunes piper))
  (check "the march plays" "test march" (effect-name (current-song g)))
  ;; the second instrument replaces the first in the same slot, and the
  ;; music carries on — any item of the kind will do
  (give-item g piper 't-drum)
  (equip-item g piper 't-drum)
  (check "the drum took the instrument slot" 't-drum (hero-song-tool piper))
  ;; a class with no :SINGS-WITH sings bare-handed, as before
  (check "the bare-handed bard needs nothing" nil (hero-song-tool bard))
  (check-true "and sings all the same" (sing-song g bard 'test-gleam)))

;; Refusal precedence: the book first, then the instrument, then the
;; tunes — the reason a player can act on comes first.
(let* ((m (parse-map *art* :name "test"))
       (piper (with-rng () (make-hero "Pip" :t-piper)))
       (g (new-game m :party (list piper))))
  (declare (ignore g))
  (setf (hero-tunes piper) 0)
  (check "an unknown song beats the empty hands" "Not in the book."
         (song-refusal piper 'test-dirge))
  (check "empty hands beat the empty throat" "No instrument in hand."
         (song-refusal piper 'test-march)))

;; The song card pays the spell card's courtesy: a refusal keeps the
;; card up and stands on the page instead of vanishing into the log.
(let* ((m (parse-map *art* :name "test"))
       (piper (with-rng () (make-hero "Pip" :t-piper)))
       (g (new-game m :party (list piper)))
       (v (make-magic-view piper)))
  (magic-act g v #\1)                   ; the first song's card
  (check "the song card is up" '(:song . test-march) (magic-view-pending v))
  (check "the refused play keeps the card" nil (magic-act g v #\p))
  (check "the card is still up" '(:song . test-march)
         (magic-view-pending v))
  (check "with the reason on it" "No instrument in hand."
         (magic-view-refusal v))
  (check-true "and the page shows it"
              (member "No instrument in hand."
                      (menu-texts (magic-lines g v)) :test #'equal))
  (check "the tune was not spent" 1 (hero-tunes piper))
  (give-item g piper 't-lute)
  (equip-item g piper 't-lute)
  (check "with the lute in hand the card plays" :done (magic-act g v #\p))
  (check "and now the tune is spent" 0 (hero-tunes piper)))

;; combat-round accepts (:sing SONG) beside :attack and (:cast ...).
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (g (new-game m :party (list grunt bard))))
  (start-combat g '(("test rat" 1)))
  ;; the grunt slays the rat (d20=11 hits, 1d6=3) while the bard sings
  (check "a sung round wins" :victory
         (with-rng (10 2)
           (combat-round g (list :attack '(:sing test-march)))))
  (check "the combat tune was spent" 0 (hero-tunes bard))
  (check-true "the march outlives the fight" (current-song g)))

;; The sing menu: pick the singer, then the song (the CAST-VIEW pattern).
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (g (new-game m :party (list grunt bard))))
  (let ((v (make-sing-view)))
    (check "Esc at the top cancels" :cancelled (sing-act g v #\Escape)))
  (let ((v (make-sing-view)))
    (check-true "the menu opens on the singer pick"
                (member "Who plays?" (sing-lines g v) :test #'equal))
    (sing-act g v #\1)
    (check "a non-singer is not picked" nil (sing-view-hero v))
    (sing-act g v #\2)
    (check "the bard picked" bard (sing-view-hero v))
    (check-true "the song page lists the march"
                (find-if (lambda (s) (search "test march" s))
                         (menu-texts (sing-lines g v))))
    (check "the song row carries its pick key" #\1
           (menu-line-key
            (find-if (lambda (line)
                       (search "test march" (menu-line-text line)))
                     (sing-lines g v))))
    (sing-act g v #\Escape)
    (check "Esc backs out to the singer pick" nil (sing-view-hero v))
    (sing-act g v #\2)
    (check "picking the song resolves the menu" :done (sing-act g v #\1))
    (check "the march plays" "test march"
           (effect-name (current-song g)))))

;; The tavern: drinks refill a singer's tunes; :down holds the way
;; below (the Bard's Tale trapdoor).
(let* ((m (parse-map *art* :name "test"))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt bard)))
       (msgs (watch-messages g)))
  (setf (hero-gold grunt) 10
        (hero-gold bard) 2
        (hero-tunes bard) 0)
  (enter-location g '("The Rusty Flagon" :tavern :price 5))
  (check "the price is the location's" 5
         (tavern-price (game-location g)))
  (check-true "the menu shows the price"
              (find-if (lambda (s) (search "5 gold" s))
                       (menu-texts (tavern-lines g))))
  (check-true "no trapdoor line without :down"
              (not (find-if (lambda (s) (search "trapdoor" s))
                            (menu-texts (tavern-lines g)))))
  (check "a drink row carries its hero's digit" #\2
         (menu-line-key
          (find-if (lambda (line)
                     (search "2) " (menu-line-text line)))
                   (tavern-lines g))))
  (check "a poor hero is refused" nil (buy-drink g bard))
  (check-true "cannot-afford message"
              (find-if (lambda (s) (search "cannot afford" s))
                       (funcall msgs)))
  (check "refusals keep the gold" 2 (hero-gold bard))
  (check-true "the grunt drinks" (buy-drink g grunt))
  (check "the ale cost five gold" 5 (hero-gold grunt))
  (check "ale grants no tunes to a non-singer" 0 (hero-tunes grunt))
  (setf (hero-gold bard) 5)
  (check-true "the bard drinks through the menu"
              (progn (tavern-act g #\2) (zerop (hero-gold bard))))
  (check "the tunes came flooding back" 1 (hero-tunes bard))
  (check-true "the refill is announced"
              (find-if (lambda (s) (search "flooding back" s))
                       (funcall msgs)))
  (check "Esc leaves the tavern" :left (tavern-act g #\Escape))
  (check "the tavern is left behind" nil (game-location g)))

;; The default drink price, and the trapdoor down.
(with-open-file (s "tests/tmp-down.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+
|@|
+-+

(zone :kind :dungeon :title \"the snug\")
" s))
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (enter-location g '("Trapdoor Inn" :tavern :down "tests/tmp-down.map"))
  (check "a drink is three gold by default" 3
         (tavern-price (game-location g)))
  (check-true "the trapdoor is offered as a clickable option"
              (member (menu-option #\d "Down the trapdoor")
                      (tavern-lines g) :test #'equal))
  (check "the trapdoor drops through" :left (tavern-act g #\d))
  (check "the trapdoor landed below" "the snug" (map-title (game-map g)))
  (check "the location is left behind" nil (game-location g)))
(delete-file "tests/tmp-down.map")

;;; ---------------------------------------------------------------------
;;; Ailments — the four conditions a hero carries until something lifts
;;; them.  The engine owns the states and what each does in play; who
;;; hands them out and what a cure costs is campaign data.

(check "the ailment vocabulary, in display and cure order"
       '(:poison :insanity :paralysis :stone) *ailments*)
(check-true "an ailment is what AILMENT-P says it is"
            (and (every #'ailment-p *ailments*)
                 (not (ailment-p :poisoned))
                 (not (ailment-p :sniffles))))
(check "each ailment reads as an adjective"
       '("poisoned" "insane" "paralysed" "stone")
       (mapcar #'ailment-title *ailments*))
(check "and as a noun, for what a cleansing lifts"
       '("poison" "insanity" "paralysis" "stone")
       (mapcar #'ailment-noun *ailments*))

;; Afflicting: said once, emitted once, and idempotent — an ailment does
;; not deepen, and the carried list keeps *AILMENTS* order however the
;; conditions arrived.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (cues '()))
  (on-event g :afflict (lambda (game hero ailment)
                         (declare (ignore game))
                         (push (list (hero-name hero) ailment) cues)))
  (check "a fresh hero carries nothing" nil (hero-ailments h))
  (check-true "the first affliction takes" (afflict-hero g h :paralysis))
  (check "and is spoken" '("Alva is paralysed!") (funcall msgs))
  (check "and cued" '(("Alva" :paralysis)) cues)
  (check "a second helping of the same changes nothing" nil
         (afflict-hero g h :paralysis))
  (check "no second cue" 1 (length cues))
  (afflict-hero g h :poison)
  (check "the carried list keeps the vocabulary's order, not arrival's"
         '(:poison :paralysis) (hero-ailments h))
  (check-true "and each reads back on its own"
              (and (hero-ailment-p h :poison)
                   (hero-ailment-p h :paralysis)
                   (not (hero-ailment-p h :stone))))
  (check-error "an unknown ailment is a campaign error"
    (afflict-hero g h :sniffles))
  ;; curing: quiet on its own, and only what it is asked for
  (check-true "curing lifts it" (cure-ailment g h :paralysis))
  (check "and it is gone" '(:poison) (hero-ailments h))
  (check "curing what is not carried lifts nothing" nil
         (cure-ailment g h :paralysis))
  (afflict-hero g h :insanity)
  (check "CURE-HERO lifts only the named, and says which"
         '(:poison) (cure-hero g h '(:poison :stone)))
  (check "the unnamed stays" '(:insanity) (hero-ailments h))
  (check-true "the cleansing is spoken"
              (find "Alva is cleansed of poison." (funcall msgs)
                    :test #'equal))
  (check "and with nothing to lift, CURE-HERO lifts none" nil
         (cure-hero g h '(:poison)))
  (check "no argument means every ailment" '(:insanity) (cure-hero g h))
  (check "the hero is clean" nil (hero-ailments h)))

;; What the front-ends read off a hero: the condition string (both
;; front-ends' parenthetical) and the roster table's four-letter code,
;; which names the WORST thing about the hero — death first, then stone,
;; paralysis, insanity, poison.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero "Ava"))
       (g (new-game m :party (list h))))
  (check "a hale hero has no condition string" nil
         (hero-condition-titles h))
  (check "and no code" nil (hero-condition-code h))
  (afflict-hero g h :poison)
  (check "one condition" "poisoned" (hero-condition-titles h))
  (check "poison's code" "POIS" (hero-condition-code h))
  (afflict-hero g h :insanity)
  (check "two, in vocabulary order" "poisoned, insane"
         (hero-condition-titles h))
  (check "the worse of the two shows" "INSA" (hero-condition-code h))
  (afflict-hero g h :paralysis)
  (check "paralysis outranks both" "PARA" (hero-condition-code h))
  (afflict-hero g h :stone)
  (check "stone outranks all three" "STON" (hero-condition-code h))
  (check "and the string carries every one, in order"
         "poisoned, insane, paralysed, stone" (hero-condition-titles h))
  (setf (hero-hp h) 0)
  (check "death leads the string" "down, poisoned, insane, paralysed, stone"
         (hero-condition-titles h))
  (check "and takes the code" "DEAD" (hero-condition-code h))
  ;; the sheet names them one to a line — four at once would run past
  ;; its 20 cells
  (setf (hero-hp h) 8
        (hero-ailments h) '(:poison :stone))
  (let ((lines (hero-summary-lines h)))
    (check-true "the sheet names the conditions, one per line"
                (and (member "Poisoned" lines :test #'equal)
                     (member "Stone" lines :test #'equal)))
    (check-true "and every line still fits the sheet's 20 cells"
                (every (lambda (l) (<= (length l) 20)) lines))))

;; Who can act: the paralysed and the stone cannot, the insane can (the
;; madness acts, just not as ordered), the dead never.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ann"))
       (b (%combat-hero "Bo"))
       (c (%combat-hero "Cyd"))
       (g (new-game m :party (list a b c))))
  (check "everyone acts to begin with" '("Ann" "Bo" "Cyd")
         (mapcar #'hero-name (acting-heroes g)))
  (afflict-hero g b :insanity)
  (check "madness still acts" '("Ann" "Bo" "Cyd")
         (mapcar #'hero-name (acting-heroes g)))
  (afflict-hero g b :paralysis)
  (check-true "paralysis is helplessness" (hero-helpless-p b))
  (check "and drops out of the acting list" '("Ann" "Cyd")
         (mapcar #'hero-name (acting-heroes g)))
  (afflict-hero g a :stone)
  (check-true "so is stone" (hero-helpless-p a))
  (check "one hero left standing to act" '("Cyd")
         (mapcar #'hero-name (acting-heroes g)))
  (check-true "and the party can still act" (party-can-act-p g))
  (setf (hero-hp c) 0)
  (check "a fallen hero acts no more" nil (acting-heroes g))
  (check "the party cannot act" nil (party-can-act-p g))
  (check-true "yet it is not dead — two heroes still live"
              (party-alive-p g)))

;; Poison: *POISON-BITE* hit points off every poisoned hero still
;; standing, on every party step and every combat round, and it kills.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ann"))
       (b (%combat-hero "Bo"))
       (c (%combat-hero "Cyd"))
       (g (new-game m :party (list a b c)))
       (msgs (watch-messages g)))
  (afflict-hero g a :poison)
  (afflict-hero g c :poison)
  (setf (hero-hp c) 0)                   ; the fallen feel no venom
  (check "the bite finds the poisoned who stand" 1 (poison-bite g))
  (check "and costs them *POISON-BITE*" 7 (hero-hp a))
  (check "the unpoisoned are untouched" 8 (hero-hp b))
  (check "the fallen stay where they lie" 0 (hero-hp c))
  (check-true "the bite is spoken"
              (find "The poison bites Ann." (funcall msgs) :test #'equal))
  ;; a step costs the venom's due — MOVE-PARTY, after the clock
  (setf (game-facing g) +east+)
  (move-party g)
  (check "a step feeds the poison" 6 (hero-hp a))
  ;; and it can kill: the bite goes through DAMAGE-HERO like any harm
  (setf (hero-hp a) 1)
  (let ((died '()))
    (on-event g :hero-died (lambda (game hero) (declare (ignore game))
                             (push (hero-name hero) died)))
    (poison-bite g)
    (check "the last hit point goes" 0 (hero-hp a))
    (check "and the death is cued" '("Ann") died)))

;; The helpless in a fight: never asked for an order, never spending
;; one, named once a round — and the orders line up with the heroes who
;; actually give them, so a statue in the middle of the party does not
;; shift anybody's action.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ann"))
       (b (%combat-hero "Bo"))
       (c (%combat-hero "Cyd"))
       (g (new-game m :party (list a b c)))
       (msgs (watch-messages g))
       (view nil))
  (afflict-hero g b :paralysis)
  (start-combat g '(("test rat" 1)))
  (setf view (make-combat-orders))
  (check "the orders page asks the first hero who can act" "Ann"
         (hero-name (combat-orders-hero g view)))
  (setf (combat-orders-chosen view) (list (cons a :defend)))
  (check "and skips the statue for the next" "Cyd"
         (hero-name (combat-orders-hero g view)))
  (setf (combat-orders-chosen view)
        (list (cons a :defend) (cons c :attack)))
  (check "two orders are all this party has to give" nil
         (combat-orders-hero g view))
  ;; the round: Ann's defence lands on Ann, Cyd's attack on the rat —
  ;; both swings miss (d20 = 1) so the fight is still standing to be
  ;; inspected afterwards
  (check "the round runs on two orders for three heroes" :ongoing
         (with-rng (0 0 0) (combat-round g '(:defend :attack))))
  (check "the defence went to the hero who ordered it" '("Ann")
         (mapcar #'hero-name (combat-defenders (game-combat g))))
  (check-true "the paralysed hero is named, once"
              (= 1 (length (remove-if-not
                            (lambda (s)
                              (equal s "Bo is paralysed and cannot move."))
                            (funcall msgs)))))
  (setf (game-combat g) nil))

;; Frozen stiff is beaten: with nobody able to swing, the fight ends as
;; a defeat instead of running rounds forever.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (ended '()))
  (on-event g :combat-end (lambda (game result) (declare (ignore game))
                            (push result ended)))
  (afflict-hero g h :stone)
  (start-combat g '(("test rat" 1)))
  ;; the rat picks its only target and misses (d20 = 1); nobody answers
  (check "a helpless party loses the fight" :defeat
         (with-rng (0 0) (combat-round g)))
  (check "the defeat is cued" '(:defeat) ended)
  (check-true "and named for what it was"
              (find "The party stands helpless..." (funcall msgs)
                    :test #'equal))
  (check-true "the hero is alive all the same" (hero-alive-p h)))

;; Insanity: the madness spends the round on a companion, and voids
;; whatever was ordered — including a defence.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ann"))
       (b (%combat-hero "Bo"))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g)))
  (afflict-hero g a :insanity)
  ;; one companion to pick (roll 1 -> 0), then 1d6 = 3+1 = 4 damage,
  ;; str 10 adding nothing
  (with-rng (0 3) (%insane-strike g a))
  (check "the madness strikes the companion" 4 (hero-hp b))
  (check-true "and says so"
              (find "Ann, raving, STRIKES Bo for 4 damage!" (funcall msgs)
                    :test #'equal))
  (setf (hero-hp b) 0)                   ; nobody left to strike
  (with-rng () (%insane-strike g a))
  (check-true "with no companion standing it rages at the air"
              (find "Ann rages at the air." (funcall msgs) :test #'equal))
  ;; in a round, an insane hero's :defend never reaches the defenders
  (setf (hero-hp b) 8)
  (start-combat g '(("test rat" 1)))
  (with-rng (0 3 0 0) (combat-round g '(:defend :defend)))
  (check "madness cannot defend — only the sane hero does" '("Bo")
         (mapcar #'hero-name (combat-defenders (game-combat g))))
  (setf (game-combat g) nil))

;; Inflicting: a monster's :INFLICTS table is what its blows carry
;; besides damage, rolled per landed hit.
(check-error ":inflicts refuses an unknown ailment"
  (define-monster "test typo" :inflicts '((:poisoned 10))))
(check-error ":inflicts refuses a chance that is not a percent"
  (define-monster "test typo" :inflicts '((:poison 0))))
(check-error ":inflicts refuses a malformed entry"
  (define-monster "test typo" :inflicts '((:poison 10 :stone))))
(check "a monster carries no ailments unless told to" nil
       (monster-type-inflicts (find-monster-type "test rat")))

(define-monster "test viper"
  :level 1 :hp-dice 3 :ac 10 :damage "1d2" :xp 5 :gold 0
  :inflicts '((:poison 50) (:paralysis 25)))

(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (check "the table reads back" '((:poison 50) (:paralysis 25))
         (monster-type-inflicts (find-monster-type "test viper")))
  (start-combat g '(("test viper" 1)))
  ;; the viper picks its target, hits (defending drops ac 8 to 4, so
  ;; d20 = 16 lands), rolls 1 damage, then rolls the ailments in
  ;; vocabulary order: 49 lands under poison's 50, 30 misses
  ;; paralysis' 25
  (with-rng (0 15 0 49 30) (combat-round g '(:defend)))
  (check "the landed blow carried its poison" '(:poison)
         (hero-ailments h))
  (check-true "and said so"
              (find "Alva is poisoned!" (funcall msgs) :test #'equal))
  (setf (game-combat g) nil))

;; A blow that misses carries nothing, and a blow that kills carries
;; nothing either — the ailments are the living's to carry.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (start-combat g '(("test viper" 1)))
  (with-rng (0 0) (combat-round g '(:defend)))     ; d20 = 1, a miss
  (check "a missed blow leaves the hero clean" nil (hero-ailments h))
  (setf (hero-hp h) 1)
  (with-rng (0 15 0 0 0) (combat-round g '(:defend)))
  (check "the blow felled the hero" 0 (hero-hp h))
  (check "and the dead caught nothing" nil (hero-ailments h))
  (setf (game-combat g) nil))

;; Temples: healers for hire — so many gold per missing hit point,
;; a flat fee on top to raise the fallen.  The menu asks twice: who
;; wishes healing, then who will pay — any purse may cover any
;; patient, and a short purse buys wound by wound what it can.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (b (%combat-hero "Bo"))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g))
       (v nil))
  (setf (hero-gold a) 100
        (hero-gold b) 2)                   ; short of even one wound
  (damage-hero g a 5)                      ; 3/8 — five wounds
  (enter-location g '("The Test Chapel" :temple :price 3 :raise 40))
  (setf v (make-location-view g))
  (check-true "the temple gets a temple view" (temple-view-p v))
  (check "the rates are the location's" '(3 40)
         (list (temple-price (game-location g))
               (temple-raise-fee (game-location g) a)))
  (check "a flat raising asks the same of any hero" '(40 0)
         (list (temple-raise-base (game-location g))
               (temple-raise-rate (game-location g))))
  (check "the cost is the rate over the wounds" 15
         (temple-cost (game-location g) a))
  (check "an unhurt hero costs nothing" 0
         (temple-cost (game-location g) b))
  (check "location-lines serves the temple menu"
         (menu-texts (temple-lines g v))
         (menu-texts (location-lines g v)))
  (check-true "the menu shows the healing rate"
              (find "Healing 3 gold a wound."
                    (menu-texts (temple-lines g v)) :test #'equal))
  (check-true "and quotes a flat raising as the one number it is"
              (find "Raising the fallen 40 gold."
                    (menu-texts (temple-lines g v)) :test #'equal))
  (check-true "the menu asks who wishes healing"
              (find "Who wishes healing?" (menu-texts (temple-lines g v))
                    :test #'equal))
  ;; the roster pane already shows health and purse — a temple row
  ;; carries only who and what the priests ask
  (check-true "a hurt hero's row shows the cost alone"
              (find-if (lambda (s) (search "1) Ava  costs 15" s))
                       (menu-texts (temple-lines g v))))
  (check "an unhurt hero earns no row" nil
         (find-if (lambda (s) (search "Bo" s))
                  (menu-texts (temple-lines g v))))
  (check "a hero row carries its digit" #\1
         (menu-line-key
          (find-if (lambda (line)
                     (search "Ava" (menu-line-text line)))
                   (temple-lines g v))))
  ;; picking the patient turns the page to the payer question
  (temple-act g v #\1)
  (check "the digit picks the patient" a (temple-view-patient v))
  (check-true "the menu asks who will pay, naming the cost"
              (find-if (lambda (s) (search "Who will pay Ava's 15 gold?" s))
                       (menu-texts (temple-lines g v))))
  (check-true "every purse gets a payer row"
              (and (find-if (lambda (s) (search "1) Ava  100 gp" s))
                            (menu-texts (temple-lines g v)))
                   (find-if (lambda (s) (search "2) Bo  2 gp" s))
                            (menu-texts (temple-lines g v)))))
  (check-true "Esc backs off the payer page"
              (progn (temple-act g v #\Escape)
                     (null (temple-view-patient v))))
  (check "backing off healed nothing" 3 (hero-hp a))
  ;; a payer too poor for even one wound: the notice, not a heal
  (temple-act g v #\1)                     ; Ava wishes healing
  (temple-act g v #\2)                     ; Bo's 2 gp buy no 3-gold wound
  (check-true "a purse short of one wound leaves the notice last"
              (equal "Not enough Gold"
                     (first (last (menu-texts (temple-lines g v))))))
  (check "the notice keeps the gold" 2 (hero-gold b))
  (check "and the wounds" 3 (hero-hp a))
  (check-true "the notice keeps the payer page open"
              (temple-view-patient v))
  (check-true "the next key clears the notice"
              (progn (temple-act g v #\1)  ; Ava pays herself
                     (null (temple-view-note v))))
  (check-true "the payer digit heals the patient" (= (hero-hp a) 8))
  (check "the payer was charged" 85 (hero-gold a))
  (check "a done deal turns back to the patient page" nil
         (temple-view-patient v))
  (check-true "the mending is announced"
              (find-if (lambda (s) (search "mend Ava's wounds" s))
                       (funcall msgs)))
  (check "an unhurt patient is turned away" nil
         (progn (temple-act g v #\1) (temple-view-patient v)))
  (check-true "needs-no-healing message"
              (find-if (lambda (s) (search "needs no healing" s))
                       (funcall msgs)))
  ;; another purse may pay: Bo falls — the 40 fee brings him back at one
  ;; hit point, the seven above it are wounds at 3 — and Ava covers it all
  (damage-hero g b 999)
  (check "raising adds the fee, and bills only the wounds above 1 hp" 61
         (temple-cost (game-location g) b))
  (check-true "the fallen hero's row says down, numbered by roster slot"
              (find-if (lambda (s)
                         (search "2) Bo (down)  costs 61" s))
                       (menu-texts (temple-lines g v))))
  (temple-act g v #\2)                     ; Bo wishes healing
  (temple-act g v #\1)                     ; Ava pays
  (check-true "another purse raises the fallen" (hero-alive-p b))
  (check "back on their feet, whole" 8 (hero-hp b))
  (check "the payer covered fee and wounds" 24 (hero-gold a))
  (check "the patient's own purse is untouched" 2 (hero-gold b))
  (check-true "the raising is announced"
              (find-if (lambda (s) (search "Bo rises, whole again" s))
                       (funcall msgs)))
  (check-true "a hale party gets the no-one-needs line"
              (find-if (lambda (s)
                         (search "No one here needs the priests" s))
                       (menu-texts (temple-lines g v))))
  (check "Esc leaves the temple" :left (temple-act g v #\Escape))
  (check "the temple is left behind" nil (game-location g)))

;; The short purse buys what it can: wounds one by one, and a raising
;; only once the fee plus the first wound is covered.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (g (new-game m :party (list a)))
       (msgs (watch-messages g))
       (v nil))
  (damage-hero g a 5)                      ; 3/8 — five wounds
  (setf (hero-gold a) 11)                  ; three wounds and change
  (enter-location g '("The Test Chapel" :temple :price 3 :raise 40))
  (setf v (make-location-view g))
  (temple-act g v #\1)                     ; Ava wishes healing
  (check-true "a short purse buys what it can"
              (progn (temple-act g v #\1) (= (hero-hp a) 6)))
  (check "three wounds cost nine, the change stays" 2 (hero-gold a))
  (check-true "the part mend is announced"
              (find-if (lambda (s) (search "mend some of Ava's wounds" s))
                       (funcall msgs)))
  ;; the fallen: the fee gates the raising, and buys the life alone —
  ;; 39 is short of the 40 and buys nothing at all, 40 exactly brings
  ;; her back at a single hit point with no wound mended
  (damage-hero g a 999)
  (setf (hero-gold a) 39)                  ; a gold short of the fee
  (temple-act g v #\1)
  (temple-act g v #\1)
  (check-true "a purse short of the fee raises no one"
              (and (not (hero-alive-p a))
                   (equal "Not enough Gold"
                          (first (last (menu-texts (temple-lines g v)))))))
  (check "the refusal keeps the gold" 39 (hero-gold a))
  (setf (hero-gold a) 40)                  ; the fee, and not a wound more
  (temple-act g v #\1)
  (check-true "the fee alone raises her, at one hit point"
              (progn (temple-act g v #\1)
                     (and (hero-alive-p a) (= (hero-hp a) 1))))
  (check "the raising took everything" 0 (hero-gold a))
  (check-true "the part raise is announced"
              (find-if (lambda (s) (search "Ava rises" s))
                       (funcall msgs)))
  ;; and the health above that hit point is ordinary wound trade: 12
  ;; gold buys four of her seven remaining wounds
  (setf (hero-gold a) 12)
  (temple-act g v #\1)
  (temple-act g v #\1)
  (check "four wounds bought at three" 5 (hero-hp a))
  (check "the change stays" 0 (hero-gold a))
  (temple-act g v #\Escape))

;; A free-healing shrine (:price 0) buys every wound without dividing
;; by zero; a flat raise fee, if any, still gates a raising.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (g (new-game m :party (list a)))
       (v nil))
  (damage-hero g a 5)                      ; 3/8 — five wounds
  (setf (hero-gold a) 0)
  (enter-location g '("The Free Shrine" :temple :price 0 :raise 40))
  (setf v (make-location-view g))
  (temple-act g v #\1)                     ; Ava wishes healing
  (check-true "a free shrine heals with no gold at all"
              (progn (temple-act g v #\1) (= (hero-hp a) 8)))
  (check "the shrine takes no gold" 0 (hero-gold a))
  (temple-act g v #\Escape))

;; A purse that covers the raising but not every wound after it: the
;; hero comes back hurt, and the priests say so in their own line.  The
;; text is checked whole, because every character of it is drawn by the
;; Amiga's font, which has no glyphs past ASCII.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (g (new-game m :party (list a)))
       (msgs (watch-messages g))
       (v nil))
  (damage-hero g a 999)                    ; down
  (setf (hero-gold a) 60)                  ; 40 for the life, 20 of wounds
  (enter-location g '("The Fallen's Rest" :temple :price 10 :raise 40))
  (setf v (make-location-view g))
  (temple-act g v #\1)                     ; Ava wishes raising
  (temple-act g v #\1)                     ; and pays for it herself
  (check-true "the raising brings her back" (hero-alive-p a))
  (check "but only as far as the purse reached" 3 (hero-hp a))
  (check "and the priests say the wounds remain"
         "The priests chant, and Ava rises - wounds remain.  (60 gold)"
         (find-if (lambda (s) (search "priests chant" s)) (funcall msgs)))
  (temple-act g v #\Escape))

;; the default temple rates: two gold a wound, fifty flat for a
;; raising — and no per-hit-point scaling unless a map asks for it
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))                  ; 8 max hp
       (g (new-game m :party (list h))))
  (enter-location g '("The Wayside Shrine" :temple))
  (check "healing is two gold a wound by default" 2
         (temple-price (game-location g)))
  (check "raising is fifty by default" 50
         (temple-raise-fee (game-location g) h))
  (check "and flat by default — the hero's health does not enter it" 0
         (temple-raise-rate (game-location g)))
  (check "location-act routes the temple" :left
         (location-act g nil #\Escape)))

;; A raising fee that scales with the patient (:RAISE-PER-HP, the
;; Bard's Tale rule Closure uses): so much per full hit point on top of
;; the flat :RAISE, so a hardy hero is dearer to bring back than a
;; frail one.  Ava's 8 max hp at 10 apiece plus 10 flat = 90.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (b (%combat-hero "Bo"))
       (g (new-game m :party (list a b)))
       (v nil))
  (setf (hero-max-hp b) 20)                ; a hardier hero, dearer to raise
  (enter-location g '("The Test Temple" :temple :price 10
                      :raise 10 :raise-per-hp 10))
  (setf v (make-location-view g))
  (check "the two parts of the fee read back" '(10 10)
         (list (temple-raise-base (game-location g))
               (temple-raise-rate (game-location g))))
  (check "the fee is the flat fee plus the rate over full hit points" 90
         (temple-raise-fee (game-location g) a))
  (check "a hardier hero costs more to raise" 210
         (temple-raise-fee (game-location g) b))
  ;; down and whole again: the scaled fee for the life, then 10 a wound
  ;; over the seven hit points above the one the fee returns
  (damage-hero g a 999)
  (check "the raising cost is the fee plus the wounds above 1 hp" 160
         (temple-cost (game-location g) a))
  (check-true "the menu quotes the rate, not a single fee"
              (find "Raising the fallen 10 a hit point plus 10."
                    (menu-texts (temple-lines g v)) :test #'equal))
  (check-true "and the fallen hero's row carries their own price"
              (find-if (lambda (s) (search "1) Ava (down)  costs 160" s))
                       (menu-texts (temple-lines g v))))
  (setf (hero-gold b) 160)
  (temple-act g v #\1)                     ; Ava wishes healing
  (temple-act g v #\2)                     ; Bo's purse covers it
  (check-true "the scaled raising brings her back whole"
              (and (hero-alive-p a) (= (hero-hp a) 8)))
  (check "and took the whole scaled price" 0 (hero-gold b))
  (temple-act g v #\Escape))

;; Closure's shape of the rule (BT's own): no flat part at all, just so
;; much a hit point — 10 apiece over a hero's full health, and the menu
;; quotes the rate alone.  Ava's 8 max hp raise for 80, and the seven
;; hit points over the one the fee returns cost 10 each on top.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (g (new-game m :party (list a)))
       (v nil))
  (enter-location g '("The Test Sanctum" :temple :price 10
                      :raise 0 :raise-per-hp 10))
  (setf v (make-location-view g))
  (check "no flat part, only the rate" '(0 10)
         (list (temple-raise-base (game-location g))
               (temple-raise-rate (game-location g))))
  (check "the fee is the rate over full hit points" 80
         (temple-raise-fee (game-location g) a))
  (damage-hero g a 999)
  (check "and whole again costs that plus the seven wounds" 150
         (temple-cost (game-location g) a))
  (check-true "the menu quotes a rate with no flat part to name"
              (find "Raising the fallen 10 gold a hit point."
                    (menu-texts (temple-lines g v)) :test #'equal))
  (setf (hero-gold a) 80)                  ; her own purse, the fee exactly
  (temple-act g v #\1)
  (temple-act g v #\1)
  (check-true "the fee alone stands her up at one hit point"
              (and (hero-alive-p a) (= (hero-hp a) 1)))
  (check "and it took the fee" 0 (hero-gold a))
  (temple-act g v #\Escape))

;; The priests also lift conditions, at a flat price per condition
;; (:CURES) — and only the ones this house knows how to treat.
(check-error ":cures refuses anything that is not an ailment"
  (let* ((m (parse-map *art* :name "test"))
         (g (new-game m :party (list (%combat-hero)))))
    (enter-location g '("The Bad Chapel" :temple :cures ((:sniffles 10))))
    (temple-cures (game-location g))))

;; a temple treats nothing at all unless its map says so
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (enter-location g '("The Bare Chapel" :temple))
  (check "no :cures table, no cures" nil (temple-cures (game-location g)))
  (check "and no price for any condition" nil
         (temple-cure-price (game-location g) :poison))
  (leave-location g))

(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))            ; 8 max hp
       (b (%combat-hero "Bo"))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g))
       (v nil))
  (enter-location g '("The Test Infirmary" :temple :price 10 :raise 40
                      :cures ((:poison 60) (:paralysis 100))))
  (setf v (make-location-view g))
  (check "the table reads back" '((:poison 60) (:paralysis 100))
         (temple-cures (game-location g)))
  (check "each condition has its own price" '(60 100 nil)
         (list (temple-cure-price (game-location g) :poison)
               (temple-cure-price (game-location g) :paralysis)
               (temple-cure-price (game-location g) :stone)))
  ;; what these priests can do for this patient, in vocabulary order
  (afflict-hero g a :paralysis)
  (afflict-hero g a :poison)
  (afflict-hero g a :stone)               ; untreated here
  (check "only the conditions this house treats, in order"
         '((:poison 60) (:paralysis 100))
         (temple-curable (game-location g) a))
  (check "and the cost is their sum — the untreated one is not in it" 160
         (temple-cost (game-location g) a))
  (check-true "a hale but ailing hero still earns a row, naming it all"
              (find "1) Ava (poisoned, paralysed, stone)  costs 160"
                    (menu-texts (temple-lines g v)) :test #'equal))
  ;; the buying order: cures before wounds, each cure whole or not at all
  (damage-hero g a 3)                     ; 5/8 — three wounds at 10
  (check "wounds join the bill" 190 (temple-cost (game-location g) a))
  (setf (hero-gold b) 80)                 ; poison's 60, and two wounds
  (temple-act g v #\1)                    ; Ava wishes healing
  (temple-act g v #\2)                    ; Bo's purse pays
  (check "the cheaper cure was bought, the dearer skipped"
         '(:paralysis :stone) (hero-ailments a))
  (check-true "the cleansing names what it lifted"
              (find "Ava is cleansed of poison." (funcall msgs)
                    :test #'equal))
  (check "and the change bought wounds at ten apiece" 7 (hero-hp a))
  (check "the purse is spent to the copper" 0 (hero-gold b))
  ;; a cure with nothing else to buy speaks its own receipt
  (setf (hero-hp a) (hero-max-hp a)
        (hero-gold b) 100)
  (temple-act g v #\1)
  (temple-act g v #\2)
  (check "the paralysis lifted on its own" '(:stone) (hero-ailments a))
  (check-true "and the receipt says what it cost"
              (find "The priests ask 100 gold for it." (funcall msgs)
                    :test #'equal))
  ;; what is left is untreated here: the priests turn her away
  (check "nothing these priests can sell" nil
         (temple-curable (game-location g) a))
  (temple-act g v #\1)
  (check-true "and they say as much"
              (find "These priests cannot lift what Ava carries."
                    (funcall msgs) :test #'equal))
  (check "the turned-away patient does not open the payer page" nil
         (temple-view-patient v))
  (check "and a hero with only an untreated condition earns no row" nil
         (find-if (lambda (s) (search "Ava" s))
                  (menu-texts (temple-lines g v))))
  (temple-act g v #\Escape))

;; The fallen are raised before anything else is sold them: a purse that
;; covers a cure but not the fee buys nothing at all — there is no
;; cleansing a corpse.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))
       (g (new-game m :party (list a)))
       (v nil))
  (enter-location g '("The Test Infirmary" :temple :price 10 :raise 40
                      :cures ((:poison 60))))
  (setf v (make-location-view g))
  (afflict-hero g a :poison)
  (damage-hero g a 99)
  (check "the bill is fee, cure and the wounds above one hit point" 170
         (temple-cost (game-location g) a))
  (setf (hero-gold a) 39)                 ; a gold short of the raising
  (temple-act g v #\1)
  (temple-act g v #\1)
  (check-true "short of the fee, nothing at all is bought"
              (and (not (hero-alive-p a))
                   (hero-ailment-p a :poison)
                   (equal "Not enough Gold"
                          (first (last (menu-texts (temple-lines g v)))))))
  (check "the purse is untouched" 39 (hero-gold a))
  ;; fee and cure, with nothing left for wounds
  (setf (hero-gold a) 100)
  (temple-act g v #\1)
  (temple-act g v #\1)
  (check-true "she rises, cleansed, and still hurt"
              (and (hero-alive-p a) (= 1 (hero-hp a))
                   (null (hero-ailments a))))
  (check "the fee and the cure took it all" 0 (hero-gold a))
  (temple-act g v #\Escape))

;; The energy fount: spell points at so many gold apiece, living
;; casters only — singers have the tavern, the fallen the temple.  Two
;; pages, the temple's shape: who wants refreshing, then who pays.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))              ; "Alva", no spell points
       (wiz (%combat-hero "Wanda"))
       (g (new-game m :party (list grunt wiz)))
       (msgs (watch-messages g))
       (v (progn (enter-location g '("The Test Well" :energy :price 4))
                 (make-location-view g))))
  (setf (hero-max-sp wiz) 6                ; a caster by the numbers
        (hero-sp wiz) 1
        (hero-gold wiz) 20
        (hero-gold grunt) 50)
  (check "the rate is the location's" 4
         (energy-price (game-location g)))
  (check "the cost is the rate over the missing points" 20
         (energy-cost (game-location g) wiz))
  (check-true "the fount's view is an energy-view" (energy-view-p v))
  (check "location-lines serves the fount menu"
         (menu-texts (energy-lines g v))
         (menu-texts (location-lines g v)))
  (check-true "the menu shows the rate"
              (find-if (lambda (s) (search "4 gold apiece" s))
                       (menu-texts (energy-lines g v))))
  ;; the pick page is a bare prompt: the roster pane below already
  ;; lists the party, their spell points and their purses
  (check-true "the pick page asks who wants refreshing"
              (find-if (lambda (s)
                         (search "Who wants to refresh spell points?  (1-2)"
                                 s))
                       (menu-texts (energy-lines g v))))
  (check "and lists no party rows of its own" nil
         (find-if (lambda (s) (search "Wanda" s))
                  (menu-texts (energy-lines g v))))
  (check "the waters have work for a drained caster" t
         (energy-work-p (game-location g) wiz))
  (check "and none for a hero with no spell points at all" nil
         (energy-work-p (game-location g) grunt))
  ;; a spell-less pick is turned away aloud, and the page stands
  (check "picking the grunt keeps the pick page" nil (energy-act g v #\1))
  (check "no caster was chosen" nil (energy-view-hero v))
  (check-true "no-spell-points message"
              (find-if (lambda (s)
                         (search "has no spell points to fill" s))
                       (funcall msgs)))
  (check "spurned, the grunt keeps his gold" 50 (hero-gold grunt))
  ;; the caster picked, the menu asks for a purse
  (check "picking the caster opens the payer page" nil (energy-act g v #\2))
  (check "the caster is on the view" wiz (energy-view-hero v))
  (check-true "the payer page quotes the cost"
              (find-if (lambda (s) (search "Who will pay Wanda's 20 gold?" s))
                       (menu-texts (energy-lines g v))))
  (check-true "and rows every purse in the party"
              (let ((rows (menu-texts (energy-lines g v))))
                (and (find-if (lambda (s) (search "1) Alva  50 gp" s)) rows)
                     (find-if (lambda (s) (search "2) Wanda  20 gp" s))
                              rows))))
  ;; Esc backs off the payer page rather than leaving the fount
  (check "Esc backs off the payer page" nil (energy-act g v #\Escape))
  (check "the caster is let go" nil (energy-view-hero v))
  (check-true "and the fount is still standing" (game-location g))
  ;; a purse too short leaves the notice and spends nothing
  (energy-act g v #\2)                     ; Wanda again
  (setf (hero-gold grunt) 3)
  (check "a short purse is refused" nil (energy-act g v #\1))
  (check "Not enough Gold" "Not enough Gold" (energy-view-note v))
  (check-true "the notice reaches the menu"
              (find-if (lambda (s) (search "Not enough Gold" s))
                       (menu-texts (energy-lines g v))))
  (check "the refusal keeps the points dry" 1 (hero-sp wiz))
  (check "and the purse full" 3 (hero-gold grunt))
  (check-true "the caster stays picked" (eq wiz (energy-view-hero v)))
  ;; another purse pays for her, and the notice clears
  (setf (hero-gold grunt) 50)
  (check "a fat purse fills her" nil (energy-act g v #\1))
  (check "the notice is gone" nil (energy-view-note v))
  (check "the caster brims" 6 (hero-sp wiz))
  (check "the payer paid, not the caster" 30 (hero-gold grunt))
  (check "the caster's own purse is untouched" 20 (hero-gold wiz))
  (check "and the menu is back on the pick page" nil (energy-view-hero v))
  (check-true "the surge is announced"
              (find-if (lambda (s) (search "Power floods back into Wanda" s))
                       (funcall msgs)))
  ;; a brimming caster, and the fallen
  (check "a full caster is turned away" nil (energy-act g v #\2))
  (check-true "brims-already message"
              (find-if (lambda (s) (search "brims with power" s))
                       (funcall msgs)))
  (setf (hero-sp wiz) 0)
  (damage-hero g wiz 999)
  (check "the fallen are beyond the waters" nil
         (energy-restore g wiz grunt))
  (check-true "and sent to the temple instead"
              (find-if (lambda (s) (search "the temple, perhaps" s))
                       (funcall msgs)))
  (check "the fallen cost the payer nothing" 30 (hero-gold grunt))
  (check "Esc leaves the fount from the pick page" :left
         (energy-act g v #\Escape))
  (check "the fount is left behind" nil (game-location g)))

;; the default rate: three gold a point
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (enter-location g '("Roscoe's Energy Emporium" :energy))
  (check "three gold apiece by default" 3
         (energy-price (game-location g)))
  (check "a lone party's prompt names no range" t
         (and (find-if (lambda (s)
                         (search "Who wants to refresh spell points?  (1)" s))
                       (menu-texts (energy-lines g (make-energy-view))))
              t))
  (check "location-act routes the fount" :left
         (location-act g nil #\Escape)))

;;; ---------------------------------------------------------------------
;;; The guild (:GUILD) — the Adventurers' Guild: characters made,
;;; parties formed, the roster of heroes waiting in the hall.

;; a class with both portraits (the creation walk's man-or-woman page)
;; and a race whose class list keeps the class page short and known
(define-hero-class :g-faced :image "gfx/g-man.iff"
                            :image-woman "gfx/g-woman.iff")
(define-race :g-folk :str 1 :classes '(:g-faced :tester)
  :description "guild test folk")

;; NEW-GAME carries the roster; a fresh game without one has none.
(let ((m (parse-map *art* :name "test")))
  (check "a fresh game's roster is empty" nil
         (game-roster (new-game m)))
  (let ((h (%combat-hero "Idle")))
    (check "new-game takes a :roster" (list h)
           (game-roster (new-game m :roster (list h))))))

;; REMOVE-FROM-PARTY is JOIN-PARTY's inverse.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ann"))
       (b (%combat-hero "Ben"))
       (c (%combat-hero "Col"))
       (g (new-game m :party (list a b c)))
       (left '()))
  (on-event g :party-left
            (lambda (game hero) (declare (ignore game))
              (push (hero-name hero) left)))
  (check "removal returns the hero" b (remove-from-party g b))
  (check "the others close ranks" (list a c) (game-party g))
  (check ":party-left emitted" '("Ben") left)
  (check "a stranger is not removed" nil (remove-from-party g b))
  (check "and the party stands" (list a c) (game-party g)))

;; The guild's main page and the add/remove flow: Add moves a waiting
;; hero into the party, Remove sends one back to the hall; the guild
;; sends no empty party out.
(let* ((m (parse-map *art* :name "test"))
       (hall (%combat-hero "Waiting"))
       (g (new-game m :roster (list hall)))
       (msgs (watch-messages g)))
  (enter-location g '("The Test Guild" :guild :gold 10))
  (let ((v (make-location-view g)))
    (check-true "the guild's view is a guild-view" (guild-view-p v))
    (check-true "the main page counts the hall"
                (find-if (lambda (s)
                           (search "Adventurers in the hall: 1" s))
                         (menu-texts (guild-lines g v))))
    (check "the Add row carries its key" #\a
           (menu-line-key
            (find-if (lambda (line)
                       (equal "Add a member" (menu-line-text line)))
                     (guild-lines g v))))
    ;; nobody marches yet: Remove is refused with a note, and the
    ;; door stays shut on an empty party
    (guild-act g v #\r)
    (check-true "Remove with no party leaves a note"
                (find-if (lambda (s) (search "No one marches." s))
                         (menu-texts (guild-lines g v))))
    ;; the refusal is wider than the narrowest takeover column, so it
    ;; comes back as a notice instruction — a page of its own — rather
    ;; than a note row the crowded menu would reflow around
    (let ((r (guild-act g v #\Escape)))
      (check "Esc with no party asks for a notice page"
             '(:notice "The guild sends no one out alone.") r)
      (check-true "the refusal really is too wide for a note row"
                  (> (length (second r)) +takeover-columns+))
      (check "the notice page is the banner over the sentence"
             (list (location-banner (game-location g)) "" (second r))
             (notice-lines g (second r))))
    (check-true "the guild's own page carries no such note"
                (notany (lambda (s) (search "sends no one out alone" s))
                        (menu-texts (guild-lines g v))))
    (check-true "still inside" (game-location g))
    ;; Add: the hall's row names the hero the party pane's way
    (guild-act g v #\a)
    (check-true "the add page rows the waiting hero"
                (find-if (lambda (s) (search "1) Waiting  TE 1" s))
                         (menu-texts (guild-lines g v))))
    (guild-act g v #\1)
    (check "the hero joined the party" (list hall) (game-party g))
    (check "and left the hall" nil (game-roster g))
    (check-true "the join is announced"
                (find-if (lambda (s) (search "joins the party" s))
                         (funcall msgs)))
    (check "an emptied hall folds back to the main page" :main
           (guild-view-mode v))
    ;; the hall drained into the party: the main page says whose hall
    ;; it is, not "empty" — a loaded game walking back in must not
    ;; read as heroes lost
    (check-true "a drained hall names the party"
                (find-if (lambda (s)
                           (search "Only your party stands here." s))
                         (menu-texts (guild-lines g v))))
    (check "the Leave row carries Escape" #\Escape
           (menu-line-key
            (find-if (lambda (line)
                       (equal "Leave the guild" (menu-line-text line)))
                     (guild-lines g v))))
    ;; Add with an empty hall is refused with a note
    (guild-act g v #\a)
    (check-true "Add with an empty hall leaves a note"
                (find-if (lambda (s)
                           (search "No one waits in the hall." s))
                         (menu-texts (guild-lines g v))))
    (let ((lines (guild-lines g v)))
      (check-true "the noted main page keeps the lores takeover rows"
                  (<= (length lines) +takeover-rows+))
      (check "and the note is its last line" "No one waits in the hall."
             (menu-line-text (car (last lines)))))
    ;; Remove: back to the hall, in the hall's own order
    (guild-act g v #\r)
    (check-true "the remove page rows the party"
                (find-if (lambda (s) (search "1) Waiting" s))
                         (menu-texts (guild-lines g v))))
    (guild-act g v #\1)
    (check "the hero stays behind" (list hall) (game-roster g))
    (check "the party is bare again" nil (game-party g))
    (check-true "the parting is announced"
                (find-if (lambda (s) (search "stays at the guild" s))
                         (funcall msgs)))
    ;; with a member marching, Esc leaves — on this cell (two open
    ;; sides, entered without a step) the party stays where it stands
    (guild-act g v #\a)
    (guild-act g v #\1)
    (check "Esc with a party leaves" :left (guild-act g v #\Escape))
    (check "the guild is left behind" nil (game-location g))
    (check "several open sides leave the party standing" '(0 0)
           (list (game-x g) (game-y g)))))

;; a guild with neither a party nor a waiting soul: the hall really
;; does stand empty
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (enter-location g '("The Test Guild" :guild))
  (let ((v (make-location-view g)))
    (check-true "a bare guild's hall stands empty"
                (find-if (lambda (s) (search "The hall stands empty." s))
                         (menu-texts (guild-lines g v))))))

;; A full party refuses an eighth: JOIN-PARTY's own rule, spoken
;; through the guild's add page.
(let* ((m (parse-map *art* :name "test"))
       (seven (loop for i below +party-limit+
                    collect (%combat-hero (format nil "M~D" i))))
       (hall (%combat-hero "Eighth"))
       (g (new-game m :party seven :roster (list hall)))
       (msgs (watch-messages g)))
  (enter-location g '("The Test Guild" :guild))
  (let ((v (make-location-view g)))
    (guild-act g v #\a)
    (guild-act g v #\1)
    (check "the eighth is refused" (list hall) (game-roster g))
    (check "the party stands at seven" seven (game-party g))
    (check-true "the refusal is spoken"
                (find-if (lambda (s) (search "party is full" s))
                         (funcall msgs)))))

;; The creation walk: race, class, name, the roll kept or rolled
;; again.  The dice are scripted, so every number on the page is
;; known: an empty script rolls 1s and 3s (hp 1d8+2 -> 3; each 3d6
;; stat 3; :g-folk's +1 STR makes 4), and the purse dice roll their
;; floor.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (msgs (watch-messages g)))
  (enter-location g '("The Test Guild" :guild :gold "1d4+5"))
  (let ((v (make-location-view g)))
    (guild-act g v #\c)
    (check "c opens the race page" :race (guild-view-mode v))
    (check-true "the races list by their titles"
                (find-if (lambda (s) (search "G-Folk" s))
                         (menu-texts (guild-lines g v))))
    (guild-act g v (digit-char (1+ (position :g-folk (races)))))
    (check "a digit picks the race" :g-folk (guild-view-race v))
    (check "and turns to the class page" :class (guild-view-mode v))
    ;; the class page offers the race's startable classes in the
    ;; registry's stable order: :g-faced then :tester
    (check-true "the class page rows the race's classes"
                (find-if (lambda (s) (search "2) Tester" s))
                         (menu-texts (guild-lines g v))))
    (guild-act g v #\2)
    (check "a one-portrait class goes straight to the name" :name
           (guild-view-mode v))
    ;; the name field: live echo, Backspace, the taken/blank refusals
    (guild-act g v #\A)
    (guild-act g v #\v)
    (guild-act g v #\v)
    (guild-act g v #\Backspace)
    (guild-act g v #\a)
    (check-true "the name echoes live"
                (find-if (lambda (s) (search "Name: Ava_" s))
                         (menu-texts (guild-lines g v))))
    (with-rng ()
      (guild-act g v #\Return))
    (check "Return rolls the character" :roll (guild-view-mode v))
    (check-true "the roll page shows the scripted dice"
                (let ((texts (menu-texts (guild-lines g v))))
                  (and (find-if (lambda (s) (search "Ava" s)) texts)
                       (find-if (lambda (s)
                                  (search "G-Folk Tester" s)) texts)
                       (find-if (lambda (s)
                                  (search "HP 3  AC 8" s)) texts)
                       (find-if (lambda (s)
                                  (search "STR 4 DEX 3 IQ 3" s)) texts)
                       (find-if (lambda (s) (search "Gold 6 gp" s))
                                texts))))
    ;; Roll again draws fresh dice — a scripted 4,4,4 STR reads 15+1
    (with-rng (0 4 4 4)
      (guild-act g v #\r))
    (check "the reroll is a fresh roll" 16
           (hero-str (guild-view-rolled v)))
    (check "k keeps the roll onto the roster" '("Ava")
           (progn (guild-act g v #\k)
                  (mapcar #'hero-name (game-roster g))))
    (check "keeping lands back on the main page" :main
           (guild-view-mode v))
    (check-true "the signing is announced"
                (find-if (lambda (s) (search "signs the guild roster" s))
                         (funcall msgs)))
    (check "the kept hero rolled the guild's gold" 6
           (hero-gold (first (game-roster g))))
    ;; a second Ava is refused by name; a blank name is refused flat
    (guild-act g v #\c)
    (guild-act g v (digit-char (1+ (position :g-folk (races)))))
    (guild-act g v #\2)
    (guild-act g v #\A)
    (guild-act g v #\v)
    (guild-act g v #\a)
    (guild-act g v #\Return)
    (check "a taken name does not roll" :name (guild-view-mode v))
    (check-true "and says so"
                (find-if (lambda (s) (search "The name is taken." s))
                         (menu-texts (guild-lines g v))))
    (dotimes (i 3) (guild-act g v #\Backspace))
    (guild-act g v #\Return)
    (check-true "a blank name asks for one"
                (find-if (lambda (s) (search "A name is needed." s))
                         (menu-texts (guild-lines g v))))
    ;; the walk backs out a page at a time and finally cancels
    (guild-act g v #\Escape)
    (check "Esc from the name steps back to the class" :class
           (guild-view-mode v))
    (guild-act g v #\Escape)
    (check "Esc from the class steps back to the race" :race
           (guild-view-mode v))
    (guild-act g v #\Escape)
    (check "Esc from the race cancels the walk" :main
           (guild-view-mode v))
    ;; a two-portrait class asks man or woman, and the pick stamps
    ;; the portrait
    (guild-act g v #\c)
    (guild-act g v (digit-char (1+ (position :g-folk (races)))))
    (guild-act g v #\1)
    (check "a two-portrait class asks whose face" :sex
           (guild-view-mode v))
    (guild-act g v #\2)
    (guild-act g v #\B)
    (guild-act g v #\e)
    (guild-act g v #\a)
    (with-rng ()
      (guild-act g v #\Return))
    (check "the woman's portrait is stamped" "gfx/g-woman.iff"
           (hero-portrait (guild-view-rolled v)))
    ;; Esc on the roll page discards the roll
    (guild-act g v #\Escape)
    (check "Esc discards the roll" nil (guild-view-rolled v))
    (check "the discarded never signed" '("Ava")
           (mapcar #'hero-name (game-roster g)))
    ;; Delete: y strikes the name behind a confirmation, n spares it
    (guild-act g v #\d)
    (guild-act g v #\1)
    (check "the delete pick asks first" :confirm-delete
           (guild-view-mode v))
    (check-true "the confirmation names the hero"
                (find-if (lambda (s)
                           (search "Strike Ava from the roster?" s))
                         (menu-texts (guild-lines g v))))
    (guild-act g v #\n)
    (check "n spares the name" '("Ava")
           (mapcar #'hero-name (game-roster g)))
    (guild-act g v #\1)
    (guild-act g v #\y)
    (check "y strikes the name" nil (game-roster g))
    (check-true "the striking is announced"
                (find-if (lambda (s)
                           (search "struck from the roster" s))
                         (funcall msgs)))
    ;; the save/load rows hand the front-end its instruction
    (check "s asks for the save picker" '(:saves :save)
           (guild-act g v #\s))
    (check "l asks for the load picker" '(:saves :load)
           (guild-act g v #\l))))

;; A bad purse is a design error caught at the door, the bad-stock
;; rule.
(let ((g (new-game (parse-map *art* :name "test"))))
  (check-error "a bad :gold is caught at entry"
    (enter-location g '("The Test Guild" :guild :gold "no dice"))))

;; The page banner: *** TITLE *** while it fits the narrowest
;; takeover column, else the closing stars go rather than cost the
;; page a row (a wrapped banner pushed the last rows off the lores
;; screen).
(let ((g (new-game (parse-map *art* :name "test"))))
  (enter-location g '("Short Hall" :hut))
  (check "a short title keeps its closing stars" "*** Short Hall ***"
         (menu-line-text (first (location-lines g nil))))
  (leave-location g)
  ;; nineteen characters is the widest that still wears both: 27
  (enter-location g '("Nineteen Characters" :hut))
  (check "the widest full banner is the takeover column exactly"
         +takeover-columns+
         (length (menu-line-text (first (location-lines g nil)))))
  (leave-location g)
  (enter-location g '("The Long Winter Rest" :hut))
  (check "a longer title trades the closing stars for its one row"
         "*** The Long Winter Rest"
         (menu-line-text (first (location-lines g nil)))))

;; The view plaque names the place the party stands in: the zone
;; outside, a location's own :PLAQUE inside one that carries it.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (check "the plaque is the zone's outside" (map-title m)
         (plaque-title g))
  (enter-location g '("The Adventurers' Guild" :guild
                      :plaque "The Guild"))
  (check "a location's :plaque takes the plaque over" "The Guild"
         (plaque-title g))
  (leave-location g)
  (check "leaving hands the plaque back to the zone" (map-title m)
         (plaque-title g))
  (enter-location g '("Nameless Hut" :hut))
  (check "a location without :plaque keeps the zone's" (map-title m)
         (plaque-title g))
  (leave-location g)
  (check-error "a bad :plaque is caught at entry"
    (enter-location g '("X" :hut :plaque 7))))

;; The creation walk's pick pages window at +BOOK-PAGE-SIZE+: the
;; eight startable classes of the fullest race stand on the page
;; whole — prompt and rows are all those pages carry — and the digit
;; window is the drawn window.
(check-true "the suite has eight startable classes to offer"
            (>= (length (startable-hero-classes)) 8))
(define-race :g-wide :classes (subseq (startable-hero-classes) 0 8)
  :description "eight-art guild test folk")
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (enter-location g '("The Test Guild" :guild))
  (let ((v (make-location-view g))
        (classes (guild-startable-classes :g-wide)))
    (guild-act g v #\c)
    (guild-act g v (digit-char (1+ (position :g-wide (races)))))
    (check "the eight-art race turns to the class page" :class
           (guild-view-mode v))
    (let ((lines (guild-lines g v)))
      (check-true "all eight classes stand on the page"
                  (find-if (lambda (s) (search "8) " s))
                           (menu-texts lines)))
      (check "eight classes fit the page whole (no scroll window)" nil
             *menu-scroll*)
      (check-true "the class page keeps inside the lores rows"
                  (<= (length lines) +takeover-rows+)))
    (guild-act g v #\8)
    (check "digit 8 picks the eighth class" (nth 7 classes)
           (guild-view-class v))))

;; The main page's note takes the breathing row's place, so the page
;; never outgrows the lores takeover even while it speaks.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m)))
  (enter-location g '("The Test Guild" :guild))
  (let ((v (make-location-view g)))
    (guild-act g v #\a)                 ; an empty hall leaves a note
    (check-true "the main page's note is on the page"
                (find-if (lambda (s)
                           (search "No one waits in the hall." s))
                         (menu-texts (guild-lines g v))))
    (check-true "and the noted main page keeps inside the lores rows"
                (<= (length (guild-lines g v)) +takeover-rows+))))

;; A game that starts AT its guild: the start cell's special walks in
;; without a step, and leaving still exits through the cell's one
;; door.
(with-open-file (s "tests/tmp-guild.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@D |
+-+-+

(zone :kind :city :title \"Boot Guild\")

(special (0 0)
  (location \"The Boot Guild\" :guild :gold 10))
" s))
(let* ((m (load-map-file "tests/tmp-guild.map"))
       (hall (%combat-hero "First"))
       (g (new-game m :roster (list hall))))
  (trigger-special g)
  (check "the boot walks into the guild" "The Boot Guild"
         (location-title (game-location g)))
  (check "a boot entry has no entry-dir" nil
         (location-entry-dir (game-location g)))
  (let ((v (make-location-view g)))
    (guild-act g v #\a)
    (guild-act g v #\1)
    (check "leaving takes the lone door" :left (guild-act g v #\Escape))
    (check "the exit lands on the street" '(1 0)
           (list (game-x g) (game-y g)))
    (check "facing away from the door" :east
           (dir-keyword (game-facing g)))
    (check "a back-step re-enters the guild" :door (move-party g :back))
    (check-true "re-entry is modal again" (game-location g))))

;; Save games carry the roster (v9) and the location the party stood
;; inside: a game saved at the guild's desk wakes at the guild's desk.
(let* ((m (load-map-file "tests/tmp-guild.map"))
       (march (%combat-hero "March"))
       (hall (%combat-hero "Hall"))
       (g (new-game m :party (list march) :roster (list hall))))
  (trigger-special g)
  (save-game g "tests/tmp-guild.sav")
  (let ((loaded (load-game "tests/tmp-guild.sav")))
    (check "the roster survives the save" '("Hall")
           (mapcar #'hero-name (game-roster loaded)))
    (check "the party survives beside it" '("March")
           (mapcar #'hero-name (game-party loaded)))
    (check "the save wakes inside the guild" "The Boot Guild"
           (location-title (game-location loaded)))
    (check "the restored entry is the saved one" nil
           (location-entry-dir (game-location loaded)))
    ;; the restored location leaves like the original: out the door
    (let ((v (make-location-view loaded)))
      (check "the restored guild leaves cleanly" :left
             (guild-act loaded v #\Escape))
      (check "onto the street" '(1 0)
             (list (game-x loaded) (game-y loaded))))))
(delete-file "tests/tmp-guild.sav")

;; A pre-v9 save has no roster and no location keys: an empty hall,
;; a party on the street — GETF's defaults, nothing more.
(with-open-file (s "tests/tmp-guild-v8.sav" :direction :output
                   :if-exists :supersede)
  (let ((*package* (find-package :tale)))
    (prin1 '(:lambda-tale-save 8 :map-file "tests/tmp-guild.map"
             :x 1 :y 0 :facing 1 :time 480
             :party ((:name "Old" :class :tester)))
           s)
    (terpri s)))
(let ((loaded (load-game "tests/tmp-guild-v8.sav")))
  (check "a v8 save loads with an empty hall" nil (game-roster loaded))
  (check "and its party intact" '("Old")
         (mapcar #'hero-name (game-party loaded)))
  (check "and stays on the street" nil (game-location loaded)))
(delete-file "tests/tmp-guild-v8.sav")
(delete-file "tests/tmp-guild.map")

;;; ---------------------------------------------------------------------
;;; EFFECT-SUMMARY-LINES / EFFECT-DURATION-TEXT — the *EFFECT-PHRASES*
;;; table read straight, one key at a time, no spell or cast needed.

;; instant keys
(check "damage quotes the dice span" '("Damage 2-8")
       (effect-summary-lines '(:damage "2d4")))
(check "damage-per-level quotes the dice span" '("Damage 1-4 a level")
       (effect-summary-lines '(:damage-per-level "1d4")))
(check "damage-group quotes the dice span" '("Group damage 4-10")
       (effect-summary-lines '(:damage-group "2d4+2")))
(check "damage-all quotes the dice span" '("Damage 10-13 to all")
       (effect-summary-lines '(:damage-all "1d4+9")))
(check "slay quotes the percent" '("Fells a foe (50%)")
       (effect-summary-lines '(:slay 50)))
(check "push-foes is a flat sentence" '("Hurls the foes back")
       (effect-summary-lines '(:push-foes t)))
(check "halt-foes is a flat sentence" '("Freezes the foes")
       (effect-summary-lines '(:halt-foes t)))
(check "calm is a flat sentence" '("Soothes the foes")
       (effect-summary-lines '(:calm t)))
(check "heal-party quotes the dice span" '("Heals all 1-4")
       (effect-summary-lines '(:heal-party "1d4")))
(check "a :full heal speaks the word" '("Heals fully")
       (effect-summary-lines '(:heal :full)))
(check "a :full heal-party speaks the word" '("Heals all fully")
       (effect-summary-lines '(:heal-party :full)))
(check "resurrect is a flat sentence" '("Raises the fallen")
       (effect-summary-lines '(:resurrect t)))
(check "cure lists the ailments" '("Cures poison, insanity")
       (effect-summary-lines '(:cure (:poison :insanity))))
(check "scry is a flat sentence" '("Tells where you are")
       (effect-summary-lines '(:scry t)))
(check "disarm-traps quotes the reach" '("Disarms traps (3 sq)")
       (effect-summary-lines '(:disarm-traps 3)))
(check "a real teleport quotes the squares" '("Folds space (4 sq)")
       (effect-summary-lines '(:teleport 4)))
(check "a homing teleport names no square count" '("Takes you to a known place")
       (effect-summary-lines '(:teleport t)))
(check "summon names the ally" '("Summons test wolf")
       (effect-summary-lines '(:summon "test wolf")))

;; timed keys
(check "light is a flat sentence" '("Light")
       (effect-summary-lines '(:light t)))
(check "night-vision is a flat sentence" '("Sight in the dark")
       (effect-summary-lines '(:night-vision t)))
(check "reveal is a flat sentence" '("Magical sight")
       (effect-summary-lines '(:reveal t)))
(check "compass is a flat sentence" '("Shows your facing")
       (effect-summary-lines '(:compass t)))
(check "levitate is a flat sentence" '("Floats over traps")
       (effect-summary-lines '(:levitate t)))
(check "buff-damage quotes the bonus" '("Damage +3")
       (effect-summary-lines '(:buff-damage 3)))
(check "save-bonus quotes the bonus" '("Saving rolls +2")
       (effect-summary-lines '(:save-bonus 2)))
(check "regen-sp quotes the multiplier" '("SP return x2")
       (effect-summary-lines '(:regen-sp 2)))
(check "extra-attacks quotes the count" '("+1 strike a round")
       (effect-summary-lines '(:extra-attacks 1)))
(check "combat-heal quotes the dice span" '("Heals 1-4 a round")
       (effect-summary-lines '(:combat-heal "1d4")))
(check "foes-ac is a flat sentence" '("Foes easier to hit")
       (effect-summary-lines '(:foes-ac 1)))
(check "foes-attack is a flat sentence" '("Foes hit less often")
       (effect-summary-lines '(:foes-attack 1)))

;; EFFECT-DURATION-TEXT on its own
(check "no :duration says nothing" nil
       (effect-duration-text '(:buff-ac 2)))
(check "an :indefinite duration" "until dispelled"
       (effect-duration-text '(:buff-ac 2 :duration :indefinite)))
(check "a single minute is singular" "for a minute"
       (effect-duration-text '(:buff-ac 2 :duration 1)))
(check "several minutes are plural" "for 60 minutes"
       (effect-duration-text '(:buff-ac 2 :duration 60)))

;; the duration closes the summary, but only when there is a phrase to
;; close after
(check "the summary closes with the duration when the spec carries one"
       '("AC 2 better" "for 60 minutes")
       (effect-summary-lines '(:buff-ac 2 :duration 60)))
(check "a duration with no phrase lines stays silent" nil
       (effect-summary-lines '(:duration 60)))
(check "a spec naming nothing gives NIL" nil (effect-summary-lines nil))

;;; ---------------------------------------------------------------------
;;; The extended effect vocabulary: spell metadata, combined effects,
;;; the new instant kinds and the new timed payloads (the vocabulary
;;; the Closure canon speaks).

;;; Reading an effect spec back out in player's words — the cards'
;;; text, derived so it cannot drift from the mechanics.  Dice first:
;;; a span, and a bare number where the spec can only land on one.
(check "dice-range spans the roll" '(4 16)
       (multiple-value-list (dice-range "4d4")))
(check "a bonus rides both ends" '(3 10)
       (multiple-value-list (dice-range "1d8+2")))
(check "a constant is its own span" '(8 8)
       (multiple-value-list (dice-range 8)))
(check "dice-range-text writes the span" "4-16" (dice-range-text "4d4"))
(check "a constant writes as one number" "8" (dice-range-text 8))
(check "and so does a one-faced die" "3" (dice-range-text "3d1"))

;; Every phrase in the vocabulary, against its exact words.  A key
;; that grows a phrase must grow a row here — the table below is the
;; specification of what a player reads.
(dolist (case '(;; instant
                ((:damage "2d6")            "Damage 2-12")
                ((:damage-per-level "1d4")  "Damage 1-4 a level")
                ((:damage-group "5d4")      "Group damage 5-20")
                ((:damage-all "10d4")       "Damage 10-40 to all")
                ((:slay 5)                  "Fells a foe (5%)")
                ((:push-foes t)             "Hurls the foes back")
                ((:halt-foes t)             "Freezes the foes")
                ((:calm t)                  "Soothes the foes")
                ((:heal "4d4")              "Heals 4-16")
                ((:heal :full)              "Heals fully")
                ((:heal-party "10d4")       "Heals all 10-40")
                ((:heal-party :full)        "Heals all fully")
                ((:resurrect t)             "Raises the fallen")
                ((:cure (:poison))          "Cures poison")
                ((:cure (:poison :insanity)) "Cures poison, insanity")
                ((:scry t)                  "Tells where you are")
                ((:disarm-traps 3)          "Disarms traps (3 sq)")
                ((:teleport 9)              "Folds space (9 sq)")
                ((:teleport t)              "Takes you to a known place")
                ((:summon "wolf")           "Summons wolf")
                ;; timed — each needs a duration to be a legal spec,
                ;; so the run is checked separately below
                ((:buff-ac 2 :duration 10)       "AC 2 better")
                ((:light t :duration 10)         "Light")
                ((:night-vision t :duration 10)  "Sight in the dark")
                ((:reveal t :duration 10)        "Magical sight")
                ((:compass t :duration 10)       "Shows your facing")
                ((:levitate t :duration 10)      "Floats over traps")
                ((:buff-damage 2 :duration 10)   "Damage +2")
                ((:save-bonus 2 :duration 10)    "Saving rolls +2")
                ((:regen-sp 2 :duration 10)      "SP return x2")
                ((:extra-attacks 1 :duration 10) "+1 strike a round")
                ((:combat-heal "1d4" :duration 10) "Heals 1-4 a round")
                ((:foes-ac 2 :duration 10)       "Foes easier to hit")
                ((:foes-attack 2 :duration 10)   "Foes hit less often")))
  (destructuring-bind (spec expected) case
    (check (format nil "~S reads as its phrase" (first spec))
           expected
           (first (effect-summary-lines spec)))))
;; every key the vocabulary knows has a phrase — a new key must not
;; slip onto a card as silence
(check "no effect key is left speechless" '()
       (remove-if (lambda (key) (assoc key *effect-phrases*))
                  (mapcar #'first (append *instant-effect-keys*
                                          *timed-effect-keys*))))
;; the timed run closes the reading
(check "minutes read as minutes" "for 60 minutes"
       (effect-duration-text '(:light t :duration 60)))
(check "a single minute is not plural" "for a minute"
       (effect-duration-text '(:light t :duration 1)))
(check "an endless effect says so" "until dispelled"
       (effect-duration-text '(:light t :duration :indefinite)))
(check "an instant spec has no run" nil
       (effect-duration-text '(:damage "1d4")))
(check "the run closes the summary"
       '("AC 2 better" "Light" "for 30 minutes")
       (effect-summary-lines '(:buff-ac 2 :light t :duration 30)))
(check "combined keys each get their line, instants first"
       '("Heals 4-16" "Saving rolls +2" "for 20 minutes")
       (effect-summary-lines '(:heal "4d4" :save-bonus 2 :duration 20)))
(check "a spec naming nothing says nothing" '()
       (effect-summary-lines '()))

;; Spell metadata rides along untouched by the mechanics.
(define-spell 'test-canon :code "TSTC" :range "1 foe (10')" :reach 10
  :duration-text "short" :cost 2 :level 1 :classes '(:t-mage)
  :notes "canon: a needle of flame (to come)"
  :damage "1d4")
(check "spell-code stored" "TSTC" (spell-code 'test-canon))
(check "spell-range stored" "1 foe (10')" (spell-range 'test-canon))
;; :RANGE is the spellbook's words, :REACH the number behind them —
;; the one the engine measures against a group's distance
(check "spell-reach stored" 10 (spell-reach 'test-canon))
(check "an unmeasured spell says NIL" nil (spell-reach 'test-bolt))
(check-error "a reach that is not feet is rejected"
  (define-spell 'test-bogus :damage "1d4" :reach "far away"))
(check-error "a reach of nothing is rejected"
  (define-spell 'test-bogus :damage "1d4" :reach 0))
(check "spell-duration-text stored" "short"
       (spell-duration-text 'test-canon))
(check "spell :notes rides along as data" "canon: a needle of flame (to come)"
       (spell-type-notes (find-spell-type 'test-canon)))
(check "metadata-free spells say NIL" nil (spell-code 'test-bolt))
(check "no spell :notes reads NIL" nil
       (spell-type-notes (find-spell-type 'test-bolt)))
(check-error "define-spell rejects non-string :notes"
  (define-spell 'test-bogus :damage "1d4" :notes '(:very :magic)))

;; Validation: value shapes, duration rules, malformed plists.
(check-error "a bad dice value is rejected"
  (define-spell 'test-bogus :damage "banana"))
(check-error "a flag key wants T"
  (define-spell 'test-bogus :light 5 :duration 10))
(check-error "slay wants a percent"
  (define-spell 'test-bogus :slay 200))
(check-error "heal takes dice or :full"
  (define-spell 'test-bogus :heal :lots))
(check-error "a duration without a timed key is rejected"
  (define-spell 'test-bogus :damage "1d4" :duration 10))
(check-error "a malformed plist is rejected"
  (define-spell 'test-bogus :damage))
(check-error "cure wants a list of ailment keywords"
  (define-spell 'test-bogus :cure :poison))
(check-error "summon wants a name string"
  (define-spell 'test-bogus :summon t))

;; What a reach buys in a fight: a spell that falls short of the
;; nearest group is refused before it is paid for, the card saying so
;; in its own narrow words.
(define-spell 'test-dart :cost 1 :level 1 :classes '(:t-mage)
  :range "1 foe (20')" :reach 20 :damage "1d4")

(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1 30)))
  (check "the card says the foes are too far" "The foes are too far."
         (spell-refusal g mage 'test-dart))
  (check "the cast menu will not offer it" nil
         (spell-castable-p g mage 'test-dart))
  (check "and the cast itself refuses" nil (cast-spell g mage 'test-dart))
  (check-true "saying how far short it fell"
              (find-if (lambda (s) (search "test dart falls short at 30 feet" s))
                       (funcall msgs)))
  (check "the points stay in the purse" 6 (hero-sp mage))
  (check "the rat stands untouched" 3
         (monster-hp (first (combat-monsters (game-combat g)))))
  ;; one round of walking brings it inside the dart's twenty feet
  (combat-round g '(:defend))
  (check "closed to twenty, the dart carries" nil
         (spell-refusal g mage 'test-dart))
  (check-true "and lands" (with-rng (3) (cast-spell g mage 'test-dart)))
  (check "an unmeasured spell never falls short" nil
         (spell-refusal g mage 'test-bolt)))

;; What a reach is measured for: everything aimed at the enemy, which
;; is the battle instants AND the two timed keys that work on the foes
;; rather than on the party.  A mending or warding word aims at no
;; distance and is never held to one.
(check-true "a battle instant reaches for the enemy"
            (effect-spec-reaches-foes-p '(:damage "1d4")))
(check-true "so does a word that blunts the foes' aim"
            (effect-spec-reaches-foes-p '(:foes-attack 2 :duration 10)))
(check-true "and one that opens their guard"
            (effect-spec-reaches-foes-p '(:foes-ac 2 :duration 10)))
(check "a mending word reaches for no one" nil
       (effect-spec-reaches-foes-p '(:heal "4d4")))
(check "nor does a shield over the party" nil
       (effect-spec-reaches-foes-p '(:buff-ac 2 :duration 10)))

;; So a slowing word is held to its feet too, even though it installs
;; a party-wide effect record like any other timed key.
(define-spell 'test-slowing :cost 1 :level 1 :classes '(:t-mage)
  :range "group (20')" :reach 20 :foes-ac 2 :duration 10)

(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (start-combat g '(("test rat" 1 40)))
  (check "a slowing word cannot slow what it cannot reach"
         "The foes are too far." (spell-refusal g mage 'test-slowing))
  (check "and the cast refuses" nil (cast-spell g mage 'test-slowing))
  (check "no effect was installed" '() (game-effects g))
  ;; out of a fight there is no distance to fall short of — the timed
  ;; foe words have always been castable ahead of the trouble
  (setf (game-combat g) nil)
  (check "out of a fight it measures nothing" nil
         (spell-refusal g mage 'test-slowing)))

;; The wide kinds cover only the ground the reach takes in: a group
;; spell breaks the group it lands among — the NEAREST, not every
;; monster of that kind — and an all-foes word leaves the lines
;; standing beyond it alone.
(define-spell 'test-burst :cost 1 :level 1 :classes '(:t-mage)
  :range "group (60')" :reach 60 :damage-group "4d4")
(define-spell 'test-gale :cost 1 :level 1 :classes '(:t-mage)
  :range "all foes (30')" :reach 30 :damage-all "4d4")

(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  ;; two groups of ONE kind, ten feet and fifty feet out
  (start-combat g '(("test rat" 2 10) ("test rat" 2 50)))
  (with-rng (3 3 3 3) (cast-spell g mage 'test-burst))
  (check "the group spell breaks the group it lands among"
         '(nil nil t t)
         (mapcar #'monster-alive-p (combat-monsters (game-combat g))))
  ;; the gale carries thirty feet and the survivors stand at fifty
  (check "an all-foes word that cannot carry is refused" nil
         (cast-spell g mage 'test-gale))
  (check "so the far line keeps its feet" '(3 3)
         (mapcar #'monster-hp
                 (alive-monsters (game-combat g)))))

(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (start-combat g '(("test rat" 1 10) ("test ogre" 1 50)))
  (with-rng (3) (cast-spell g mage 'test-gale))
  (check "an all-foes word strikes only what stands inside it"
         '(nil t)
         (mapcar #'monster-alive-p (combat-monsters (game-combat g)))))

;; Combined timed keys merge into ONE effect record (the batchspell).
(define-spell 'test-batch :cost 3 :classes '(:t-mage)
  :buff-ac 2 :light t :compass t :levitate t :duration 60)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (check-true "the batch casts" (cast-spell g mage 'test-batch))
  (check "the combo installs one effect" 1 (length (game-effects g)))
  (check "the combo shields" 2 (effects-ac-bonus g))
  (check-true "the combo lights" (light-active-p g))
  (check-true "the combo orients" (compass-active-p g))
  (check-true "the combo carries the levitate marker"
              (getf (effect-payload (find-effect g "test batch"))
                    :levitate)))

;; :INDEFINITE effects burn until removed.
(define-spell 'test-ward :cost 1 :classes '(:t-mage)
  :buff-ac 1 :duration :indefinite)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (cast-spell g mage 'test-ward)
  (check "an :indefinite effect has no expiry" nil
         (effect-expires-at (find-effect g "test ward")))
  (advance-time g 600)
  (check-true "it outlasts the day" (find-effect g "test ward")))

;; :NIGHT-VISION and :REVEAL defeat darkness like :LIGHT.
(define-spell 'test-eyes :cost 1 :classes '(:t-mage)
  :night-vision t :duration :indefinite)
(define-spell 'test-sight :cost 1 :classes '(:t-mage)
  :reveal t :duration 30)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (setf (game-time g) (* 22 60))        ; 22:00 — night outdoors
  (check-true "night is dark" (game-dark-p g))
  (cast-spell g mage 'test-eyes)
  (check-true "cat eyes defeat the dark" (not (game-dark-p g)))
  (remove-effect g "test eyes")
  (check-true "without them the night returns" (game-dark-p g))
  (cast-spell g mage 'test-sight)
  (check-true "revelation lights the night" (not (game-dark-p g))))

;; The mending family: :HEAL-PARTY, :RESURRECT, :CURE, :FULL — and
;; their targeting.
(define-spell 'test-mend-all :cost 2 :classes '(:t-mage)
  :heal-party "1d4")
(define-spell 'test-raise :cost 2 :classes '(:t-mage) :resurrect t)
(define-spell 'test-renew :cost 4 :classes '(:t-mage)
  :resurrect t :heal-party :full :cure '(:poison :insanity))
(check "a single-target mend aims at a hero" :hero
       (spell-target-kind 'test-raise))
(check "a party-wide mend aims at nobody" :none
       (spell-target-kind 'test-renew))
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt mage)))
       (msgs (watch-messages g)))
  (damage-hero g grunt 4)
  (damage-hero g mage 2)
  (check-true "the party mend casts"
              (with-rng (1 1) (cast-spell g mage 'test-mend-all)))
  (check "it healed the grunt" 6 (hero-hp grunt))
  (check "it healed the caster too" 7 (hero-hp mage))
  ;; raising the fallen
  (damage-hero g grunt 99)
  (check-true "the grunt fell" (not (hero-alive-p grunt)))
  (check-true "the raise casts" (cast-spell g mage 'test-raise grunt))
  (check "the fallen stand at one hp" 1 (hero-hp grunt))
  (check-true "the raise is announced"
              (find-if (lambda (s) (search "rises again" s))
                       (funcall msgs)))
  (check-true "raising the standing says so"
              (progn (cast-spell g mage 'test-raise grunt)
                     (find-if (lambda (s) (search "has not fallen" s))
                              (funcall msgs))))
  ;; the full renewal: raise everyone, heal to the brim, cleanse
  (damage-hero g grunt 99)
  (setf (hero-sp mage) 9)
  (check-true "the renewal casts" (cast-spell g mage 'test-renew))
  (check "the fallen grunt stands at full hp" (hero-max-hp grunt)
         (hero-hp grunt))
  (check "the caster is whole again" (hero-max-hp mage) (hero-hp mage))
  (check-true "the cleansing is announced"
              (find-if (lambda (s) (search "party is cleansed" s))
                       (funcall msgs))))

;; :CURE lifts the ailments it names and no others — a word against
;; poison leaves a madness where it found it — and a party-wide mend
;; cleanses the whole roster in one line.
(define-spell 'test-antidote :cost 1 :classes '(:t-mage) :cure '(:poison))
(check-error ":cure refuses anything that is not an ailment"
  (define-spell 'test-bad-cure :cost 1 :classes '(:t-mage)
    :cure '(:poison :sniffles)))
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt mage)))
       (msgs (watch-messages g)))
  (afflict-hero g grunt :poison)
  (afflict-hero g grunt :insanity)
  (afflict-hero g mage :poison)
  (check-true "the antidote casts" (cast-spell g mage 'test-antidote grunt))
  (check "it lifted the poison it named, and left the madness"
         '(:insanity) (hero-ailments grunt))
  (check-true "naming what it lifted"
              (find "Alva is cleansed of poison." (funcall msgs)
                    :test #'equal))
  (check "and it reached only its target" '(:poison) (hero-ailments mage))
  (check-true "cast on a hero with nothing it can lift, it says so"
              (progn (cast-spell g mage 'test-antidote grunt)
                     (find "Alva carries nothing that would lift."
                           (funcall msgs) :test #'equal)))
  (check "the madness is still there" '(:insanity) (hero-ailments grunt))
  ;; the renewal reaches everyone, fallen included
  (setf (hero-sp mage) 9)
  (damage-hero g grunt 99)
  (afflict-hero g grunt :paralysis)
  (check-true "the renewal casts" (cast-spell g mage 'test-renew))
  (check "it cleansed what it names across the party" '(:paralysis)
         (hero-ailments grunt))
  (check "the caster's own poison too" nil (hero-ailments mage)))

;; :SCRY speaks the party's position.
(define-spell 'test-eye :cost 1 :classes '(:t-mage) :scry t)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (check-true "the scry casts" (cast-spell g mage 'test-eye))
  (check-true "it speaks the position"
              (find-if (lambda (s) (search "You stand at (0,0)" s))
                       (funcall msgs))))

;; The damage family: per-level, group, all, slay.
(define-monster "test bat"
  :level 1 :hp-dice 4 :ac 10 :damage "1d2" :xp 5 :gold 0)
(define-spell 'test-jab :cost 2 :classes '(:t-mage)
  :damage-per-level "1d4")
(define-spell 'test-wave :cost 2 :classes '(:t-mage)
  :damage-group "2d4+2")
(define-spell 'test-storm :cost 3 :classes '(:t-mage)
  :damage-all "1d4+9")
(define-spell 'test-doom :cost 2 :classes '(:t-mage) :slay 50)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (setf (hero-level mage) 2
        (hero-sp mage) 20)
  (start-combat g '(("test rat" 1)))    ; 3 hp
  (check-true "the jab casts"
              (with-rng (0) (cast-spell g mage 'test-jab)))
  (check "its roll is multiplied by the caster level" 1
         (monster-hp (first (alive-monsters (game-combat g))))))
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (setf (hero-sp mage) 20)
  (start-combat g '(("test rat" 2) ("test bat" 1)))
  (check-true "the wave casts"
              (with-rng (3 3 3 3) (cast-spell g mage 'test-wave)))
  (check "the wave felled the front group" 1
         (length (alive-monsters (game-combat g))))
  (check "the second group stood clear" "test bat"
         (monster-type-name
          (monster-kind (first (alive-monsters (game-combat g))))))
  (check-true "the storm casts"
              (with-rng () (cast-spell g mage 'test-storm)))
  (check "the storm felled everything" nil
         (alive-monsters (game-combat g))))
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (setf (hero-sp mage) 20)
  (start-combat g '(("test rat" 1)))
  (check-true "a lucky doom casts"
              (with-rng (10) (cast-spell g mage 'test-doom)))
  (check-true "it slays outright"
              (find-if (lambda (s) (search "SLAYS the test rat" s))
                       (funcall msgs)))
  (check "nothing is left" nil (alive-monsters (game-combat g))))
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (setf (hero-sp mage) 20)
  (start-combat g '(("test rat" 1)))
  (check-true "an unlucky doom still casts"
              (with-rng (60) (cast-spell g mage 'test-doom)))
  (check-true "the foe resists"
              (find-if (lambda (s) (search "resists the spell" s))
                       (funcall msgs)))
  (check "the rat still stands" 1
         (length (alive-monsters (game-combat g)))))

;; Foe handling and the marvels speak their line (their subsystem is
;; still to come); the battle kinds refuse to cast outside combat.
(define-spell 'test-shove :cost 1 :classes '(:t-mage) :push-foes t)
(define-spell 'test-brave :cost 1 :classes '(:t-mage)
  :summon "test wolf")
(define-spell 'test-blink :cost 1 :classes '(:t-mage) :teleport t)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (check "a foe-handling spell is combat-only" nil
         (cast-spell g mage 'test-shove))
  (check-true "the refusal says why"
              (find-if (lambda (s) (search "nothing to strike" s))
                       (funcall msgs)))
  (check-true "the summon casts anywhere"
              (cast-spell g mage 'test-brave))
  (check-true "the ally answers -- for now in words"
              (find-if (lambda (s) (search "test wolf answers the call" s))
                       (funcall msgs)))
  ;; :teleport t flies to a named destination — with none picked (a
  ;; scripted cast has no menu behind it) the way stays shut; the
  ;; destination section below flies it for real.
  (check-true "the homing teleport casts" (cast-spell g mage 'test-blink))
  (check-true "and with nowhere named, the way stays shut"
              (find-if (lambda (s) (search "the way stays shut" s))
                       (funcall msgs)))
  (start-combat g '(("test rat" 1)))
  (check-true "the shove casts in combat"
              (cast-spell g mage 'test-shove))
  (check-true "the foes are hurled -- in words"
              (find-if (lambda (s) (search "hurled away" s))
                       (funcall msgs))))

;;; ---------------------------------------------------------------------
;;; Saving throws and floor traps

;; SAVING-THROW: d20 + level + LCK bonus + :save-bonus effects + :bonus
;; against the difficulty.  %combat-hero has LCK 3 (bonus -4), level 1:
;; the total is roll - 2, so difficulty 14 needs a 16.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (check-true "a high roll clears the bar"
              (with-rng (16) (saving-throw g h 14)))
  (check "a point short fails" nil
         (with-rng (15) (saving-throw g h 14)))
  (add-effect g "test blessing" :duration 10 :payload '(:save-bonus 4))
  (check-true ":save-bonus effects weigh in"
              (with-rng (12) (saving-throw g h 14)))
  (remove-effect g "test blessing")
  (check "without the blessing the same roll fails" nil
         (with-rng (12) (saving-throw g h 14)))
  (check-true "the :bonus argument counts"
              (with-rng (14) (saving-throw g h 14 :bonus 2)))
  (check "without it the same roll fails" nil
         (with-rng (14) (saving-throw g h 14))))

;; :TRAP-SKILL, the rogue's art: a percent, grown one point per level.
(check-error "trap-skill must be a percent"
  (define-hero-class :t-bogus-rogue :trap-skill 0))
(check-error "trap-skill tops out at 100"
  (define-hero-class :t-bogus-rogue :trap-skill 101))
(define-hero-class :t-rogue :hp-dice "1d8" :damage "1d4" :ac 9
                            :trap-skill 50)
(let ((rogue (with-rng (5) (make-hero "Sly" :t-rogue))))
  (check "trap skill grows one point per level" 51
         (hero-trap-skill rogue))
  (setf (hero-level rogue) 10)
  (check "a tenth-level rogue's hand is surer" 60
         (hero-trap-skill rogue))
  (setf (hero-level rogue) 99)
  (check "even the surest hand slips (the 99 cap)" 99
         (hero-trap-skill rogue))
  (check "the untrained have no trap sense" 0
         (hero-trap-skill (%combat-hero))))

;; The TRAP op springs on the unwary: text, then per living hero a
;; saving throw (the d20 first) and the damage dice (second) -- the
;; fixed roll order scripted here and in the game suites.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "1d4" "Spikes lance up!")))
  (turn-right g)
  (with-rng (0 3)                       ; save fails, die rolls 4
    (move-party g :forward))
  (check "the spring speaks its text" "Spikes lance up!"
         (first (funcall msgs)))
  (check "the unsaved take the full dice" '("Alva TAKES 4 damage.")
         (rest (funcall msgs)))
  (check "the wound is real" 4 (hero-hp h)))

;; A lucky save halves the blow.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "1d4" "Spikes!")))
  (turn-right g)
  (with-rng (19 3)                      ; save clears, die rolls 4
    (move-party g :forward))
  (check-true "the saved twist aside"
              (find-if (lambda (s) (search "twists aside" s))
                       (funcall msgs)))
  (check "the blow is halved" 6 (hero-hp h)))

;; A save on a low roll can cost nothing at all.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (setf (cell-special m 1 0) '((trap "1d4")))
  (turn-right g)
  (with-rng (19 0)                      ; save clears, die rolls 1 -> 0
    (move-party g :forward))
  (check "a halved scratch costs nothing" 8 (hero-hp h)))

;; A levitating party floats over -- no rolls at all.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "4d8" "Doom!")))
  (add-effect g "test lift" :duration 60 :payload '(:levitate t))
  (turn-right g)
  (move-party g :forward)
  (check "the float is announced" '("The party floats over a trap.")
         (funcall msgs))
  (check "nobody is hurt" 8 (hero-hp h)))

;; The rogue's hand beats the trap -- and must beat it again next time:
;; the trap re-arms behind the party.
(let* ((m (parse-map *art* :name "test"))
       (rogue (with-rng (5) (make-hero "Sly" :t-rogue)))
       (g (new-game m :party (list rogue)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "4d8" "Doom!")))
  (turn-right g)
  (with-rng (10)                        ; 10 < 51: spotted
    (move-party g :forward))
  (check "the rogue spots and disarms"
         '("Sly spots a trap and disarms it!") (funcall msgs))
  (move-party g :back)
  (with-rng (10)                        ; the trap re-armed: rolled anew
    (move-party g :forward))
  (check "the trap re-arms behind the party" 2
         (count-if (lambda (s) (search "spots a trap" s))
                   (funcall msgs)))
  (check "the rogue is unhurt" (hero-max-hp rogue) (hero-hp rogue)))

;; A fumbled detection lets it spring on the rogue too.
(let* ((m (parse-map *art* :name "test"))
       (rogue (with-rng (5) (make-hero "Sly" :t-rogue)))
       (g (new-game m :party (list rogue)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "1d4" "Snap!")))
  (turn-right g)
  (with-rng (60 0 3)                    ; 60 >= 51: fumbled; no save; 4
    (move-party g :forward))
  (check-true "the fumble springs the trap"
              (find-if (lambda (s) (search "Snap!" s)) (funcall msgs)))
  (check "the rogue pays for it" 4 (- (hero-max-hp rogue)
                                      (hero-hp rogue))))

;; A trap Trap Zap has destroyed stays quiet for good.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "4d8" "Doom!")))
  (set-flag g (trap-disarmed-flag m 1 0))
  (turn-right g)
  (move-party g :forward)
  (check "a destroyed trap is silent" '() (funcall msgs))
  (check "and harmless" 8 (hero-hp h)))

;; TEXT and DIFFICULTY are optional, in either order.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "1d4" 10 "Ouch!")))
  (turn-right g)
  (with-rng (12 0)                      ; roll - 2 = 10 clears bar 10
    (move-party g :forward))
  (check "the tail takes difficulty before text" "Ouch!"
         (first (funcall msgs)))
  (check-true "the eased bar saves on a lesser roll"
              (find-if (lambda (s) (search "twists aside" s))
                       (funcall msgs))))
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "1d4")))
  (turn-right g)
  (with-rng (0 0)
    (move-party g :forward))
  (check "a bare trap has a stock cry" "A trap springs!"
         (first (funcall msgs))))

;; A diceless trap is loud -- map data errors surface, not fizzle.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (setf (cell-special m 0 0) '((trap)))
  (check-error "a diceless trap is an error" (trigger-special g)))

;; Trap Zap: :DISARM-TRAPS N destroys the traps up to N squares ahead,
;; for good.
(check-error ":disarm-traps t is no longer a flag"
  (define-spell 'test-bogus :disarm-traps t))
(define-spell 'test-zap :cost 1 :classes '(:t-mage) :disarm-traps 2)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((trap "4d8" "Doom!")))
  (setf (cell-special m 2 0) '((message "a chest") (trap "4d8")))
  (turn-right g)                        ; facing east: (1,0), (2,0) ahead
  (check-true "the zap casts" (cast-spell g mage 'test-zap))
  (check "both traps ahead are destroyed" 2
         (count-if (lambda (s) (search "hidden trap is destroyed" s))
                   (funcall msgs)))
  (check-true "the way is made safe"
              (find-if (lambda (s) (search "made safe" s))
                       (funcall msgs)))
  (check-true "the kill is flagged for good"
              (and (flag g (trap-disarmed-flag m 1 0))
                   (flag g (trap-disarmed-flag m 2 0))))
  (move-party g :forward)
  (check "walking the zapped corridor is safe"
         (hero-max-hp mage) (hero-hp mage)))

;; The zap reaches only its range, and stops at the map's edge.
(define-spell 'test-zap-short :cost 1 :classes '(:t-mage)
  :disarm-traps 1)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (setf (cell-special m 2 0) '((trap "4d8")))
  (turn-right g)
  (cast-spell g mage 'test-zap-short)
  (check "the short zap spares the far trap" nil
         (flag g (trap-disarmed-flag m 2 0))))
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  ;; facing north from (0,0): nothing ahead but the map's edge
  (check-true "the zap casts into the void" (cast-spell g mage 'test-zap))
  (check-true "a clear corridor is still made safe"
              (find-if (lambda (s) (search "made safe" s))
                       (funcall msgs)))
  (check "no phantom kills" nil
         (find-if (lambda (s) (search "destroyed" s)) (funcall msgs))))

;;; ---------------------------------------------------------------------
;;; What a special's text may name: {leader}, so a parting scene can
;;; say a hero's name instead of "one of you".

(let* ((m (parse-map *art* :name "test"))
       (a (make-hero "Percival" :tester))
       (b (make-hero "Elaine" :tester))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g)))
  (check "a text with no brace comes back whole"
         "The stair goes down." (special-text g "The stair goes down."))
  (check "{leader} is the hero in front"
         "Percival steps through."
         (special-text g "{leader} steps through."))
  (check "a token may sit mid-sentence and twice over"
         "Percival looks at Percival."
         (special-text g "{leader} looks at {leader}."))
  (check "the token is read without regard to case"
         "Percival." (special-text g "{Leader}."))
  (check "a brace with no closing twin is plain text"
         "A {leader who never closes"
         (special-text g "A {leader who never closes"))
  (check-error "an unknown token is as loud as an unknown op"
    (special-text g "{leadre} steps through."))
  ;; the marching order decides, and death moves it on
  (setf (hero-hp a) 0)
  (check "with the first rank down the next one leads"
         "Elaine steps through."
         (special-text g "{leader} steps through."))
  (setf (hero-hp b) 0)
  (check "with all of them down the order still answers"
         "Percival steps through."
         (special-text g "{leader} steps through."))
  (setf (hero-hp a) 8 (hero-hp b) 8)
  ;; and the ops read their text through it
  (setf (cell-special m 1 0) '((message "{leader} stays behind.")))
  (turn-right g)
  (move-party g :forward)
  (check "the message op names the leader"
         '("Percival stays behind.") (funcall msgs)))

;; The damage and trap ops read their TEXT the same way.
(let* ((m (parse-map *art* :name "test"))
       (a (make-hero "Percival" :tester))
       (g (new-game m :party (list a)))
       (msgs (watch-messages g)))
  (setf (cell-special m 1 0) '((damage "1d1" "{leader} takes the brunt.")))
  (turn-right g)
  (move-party g :forward)
  (check-true "the damage op names the leader"
              (find "Percival takes the brunt." (funcall msgs)
                    :test #'string=)))

;;; ---------------------------------------------------------------------
;;; Teleport: an integer :TELEPORT folds space for real

(check-error ":teleport wants t or a positive range"
  (define-spell 'test-bogus :teleport 0))
(check-error ":teleport rejects a dice string"
  (define-spell 'test-bogus :teleport "1d4"))
(define-spell 'test-fold :cost 1 :classes '(:t-mage) :teleport 3)
(check "an offset teleport asks for a heading" :offset
       (spell-target-kind 'test-fold))
(check "the homing form asks for a destination" :destination
       (spell-target-kind 'test-blink))

;; The full key-drive: pick the caster, scroll to the spell, answer the
;; heading and the count -- the platform-free CAST-VIEW both front-ends
;; drive verbatim.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g))
       (view (make-cast-view)))
  (setf (cell-special m 2 0) '((message "a cold draught")))
  (check "the caster digit picks the mage" nil (cast-act g view #\1))
  (check "the mage holds the wand" mage (cast-view-hero view))
  ;; scroll the window to test-fold and pick it (MENU-WINDOW clamps the
  ;; offset near the end of the list, so aim the digit at the row)
  (let* ((spells (spells-for-hero mage))
         (pos (position 'test-fold spells))
         (start (max 0 (min pos (- (length spells) +menu-page-size+))))
         (digit (digit-char (1+ (- pos start)))))
    (setf (cast-view-top view) start)
    (check "the spell pick asks on instead of casting" nil
           (cast-act g view digit)))
  (check "the fold is chosen" 'test-fold (cast-view-spell view))
  (check "the heading page waits" nil (cast-view-dir view))
  (cast-act g view #\e)
  (check "e picks east" :east (cast-view-dir view))
  (cast-act g view #\2)
  (check "the count echoes" "2" (cast-view-distance view))
  (check "Return casts the fold" :done (cast-act g view #\Return))
  (check "the fold moved the party two squares" '(2 0)
         (list (game-x g) (game-y g)))
  (check "the facing is kept" +north+ (game-facing g))
  (check "the fold pays its sp" (1- (hero-max-sp mage)) (hero-sp mage))
  (check-true "the world lurches"
              (find-if (lambda (s) (search "world lurches" s))
                       (funcall msgs)))
  (check-true "the destination special fires"
              (find-if (lambda (s) (search "cold draught" s))
                       (funcall msgs))))

;; The count entry keeps trade-view manners: Backspace edits, Esc steps
;; back a page, Return on nothing goes nowhere.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (view (make-cast-view :hero mage)))
  (setf (cast-view-spell view) 'test-fold)
  (cast-act g view #\e)
  (cast-act g view #\1)
  (cast-act g view #\2)
  (check "two digits cap the count" "12"
         (progn (cast-act g view #\3) (cast-view-distance view)))
  (cast-act g view #\Backspace)
  (check "Backspace edits the count" "1" (cast-view-distance view))
  (cast-act g view #\Backspace)
  (check "Return on nothing goes nowhere" nil
         (cast-act g view #\Return))
  (check "no sp was paid" (hero-max-sp mage) (hero-sp mage))
  (cast-act g view #\Escape)
  (check "Esc steps back to the heading" nil (cast-view-dir view))
  (cast-act g view #\Escape)
  (check "Esc again returns to the spell list" nil
         (cast-view-spell view)))

;; Beyond the spell's range the cast refuses and keeps the sp.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g))
       (view (make-cast-view :hero mage)))
  (setf (cast-view-spell view) 'test-fold)
  (cast-act g view #\e)
  (cast-act g view #\9)
  (check "over the range the fold refuses" nil
         (cast-act g view #\Return))
  (check-true "the refusal says why"
              (find-if (lambda (s) (search "cannot fold space that far" s))
                       (funcall msgs)))
  (check "the sp is kept" (hero-max-sp mage) (hero-sp mage))
  (check "the entry resets for another try" ""
         (cast-view-distance view)))

;; Off the plain map's edge the way stays shut -- the spell is spent.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (check-true "the doomed fold casts"
              (cast-spell g mage 'test-fold '(:north 1)))
  (check "the party stands where it stood" '(0 0)
         (list (game-x g) (game-y g)))
  (check "the failed fold is paid for" (1- (hero-max-sp mage))
         (hero-sp mage))
  (check-true "the edge refuses"
              (find-if (lambda (s) (search "the way stays shut" s))
                       (funcall msgs))))

;; A wrapping zone folds around the seam.
(let* ((m (parse-map *wrap-art* :name "test" :wrap t))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (check-true "the seam fold casts"
              (cast-spell g mage 'test-fold '(:west 1)))
  (check "west of (0,0) wraps to the far column" '(1 0)
         (list (game-x g) (game-y g))))

;; Neither teleport casts in combat — there is no walking away from a
;; fight, through folded space or over a map.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (check-true "the fold is castable in the open"
              (spell-castable-p g mage 'test-fold))
  (start-combat g '(("test rat" 1)))
  (check "mid-fight there is no walking away" nil
         (spell-castable-p g mage 'test-fold))
  (check "and none by the homing road either" nil
         (spell-castable-p g mage 'test-blink)))

;; A direct CAST-SPELL with a real heading still refuses mid-fight --
;; the guard inside %APPLY-INSTANT-EFFECTS catches what never saw the
;; cast menu (a scripted or item-driven CAST-SPELL, the same door the
;; homing form's t-ring-home item covers further below).
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1)))
  (check-true "the fold still casts and spends its sp"
              (cast-spell g mage 'test-fold '(:north 1)))
  (check "the party stays put mid-fight" '(0 0)
         (list (game-x g) (game-y g)))
  (check "the sp is spent anyway" (1- (hero-max-sp mage)) (hero-sp mage))
  (check-true "and the way stays shut"
              (find-if (lambda (s) (search "the way stays shut" s))
                       (funcall msgs))))

;; An item-style cast (no prompt, no target) speaks the flavor line.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (check-true "the promptless fold casts" (cast-spell g mage 'test-fold))
  (check-true "and speaks the flavor line"
              (find-if (lambda (s) (search "the way stays shut" s))
                       (funcall msgs))))

;; :BUFF-DAMAGE strengthens every blow.
(define-spell 'test-arms :cost 1 :classes '(:t-mage)
  :buff-damage 2 :duration 30)
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage))))
  (cast-spell g mage 'test-arms)
  (start-combat g '(("test rat" 1)))    ; 3 hp
  ;; d20=10 hits; 1d6=1 base would leave the rat alive — the +2 slays
  (check "the armed blow wins the round" :victory
         (with-rng (10 0) (combat-round g '(:attack :defend)))))

;; :FOES-AC bares the foes to the party's blows.
(define-spell 'test-frost :cost 1 :classes '(:t-mage)
  :foes-ac 5 :duration 10)
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage))))
  (cast-spell g mage 'test-frost)
  (start-combat g '(("test rat" 1)))
  ;; d20=3 misses ac 10 (needs 8+) but hits the frozen rat (needs 3+);
  ;; 1d6=5 slays
  (check "the frozen foe is easy prey" :victory
         (with-rng (3 5) (combat-round g '(:attack :defend)))))

;; :FOES-ATTACK blunts the foes' swings.
(define-spell 'test-dread :cost 1 :classes '(:t-mage)
  :foes-attack 3 :duration 10)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (cast-spell g mage 'test-dread)
  (start-combat g '(("test rat" 1)))
  ;; the mage defends (ac 10-4=6, threshold 14); the rat's d20=12 with
  ;; +1 level would land 14 — the -3 dread makes it 10, a miss
  (with-rng (0 12) (combat-round g '(:defend)))
  (check-true "the frightened rat misses"
              (find-if (lambda (s) (search "misses" s)) (funcall msgs)))
  (check "the mage stands unhurt" (hero-max-hp mage) (hero-hp mage)))

;; :EXTRA-ATTACKS grants more strikes, each re-aimed at the survivor.
(define-spell 'test-fury :cost 1 :classes '(:t-mage)
  :extra-attacks 1 :duration 10)
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage))))
  (cast-spell g mage 'test-fury)
  (start-combat g '(("test rat" 2)))    ; 3 hp each
  ;; two strikes: d20=10 hits, 1d6=2 -> 3 damage slays; twice over
  (check "two strikes fell two rats" :victory
         (with-rng (10 2 10 2) (combat-round g '(:attack :defend)))))

;; :COMBAT-HEAL mends the party at the end of every round.
(define-spell 'test-balm :cost 1 :classes '(:t-mage)
  :combat-heal "1d2" :duration 20)
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (damage-hero g mage 4)                ; 3/7 hp
  (cast-spell g mage 'test-balm)
  (start-combat g '(("test rat" 1)))
  ;; the mage defends; the rat's d20=0 misses; the balm rolls 1d2=2
  (with-rng (0 0 1) (combat-round g '(:defend)))
  (check "the balm mended two points" 5 (hero-hp mage)))

;; :REGEN-SP quickens the road's magic (the Rhyme of Duotime).
(define-song 'test-road :regen-sp 2 :extra-attacks 1 :duration 60)
(let* ((m (parse-map *art* :name "test"))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (mage (%combat-mage))
       (g (new-game m :party (list bard mage))))
  (check-true "a song combines timed keys too"
              (sing-song g bard 'test-road))
  (check "the song arms the party" 1 (effects-extra-attacks g))
  (setf (hero-sp mage) 0)
  (advance-time g 8)                    ; two regen ticks, doubled
  (check "the road's magic came back twice as fast" 4 (hero-sp mage)))

;; Class arts: the warrior's extra attacks, the hunter's killing eye.
(check-error "extra-attack-levels must be a positive integer"
  (define-hero-class :t-bogus :extra-attack-levels 0))
(check-error "crit-chance must be a percent"
  (define-hero-class :t-bogus :crit-chance 150))
(define-hero-class :t-war :hp-dice "1d10+4" :damage "1d8" :ac 8
                          :extra-attack-levels 4
                          :description "A test warrior.")
(define-hero-class :t-hunter :hp-dice "1d8+2" :damage "1d6" :ac 8
                             :crit-chance 10)
(check "the class keeps its lore line" "A test warrior."
       (hero-class-property :t-war :description))
(let ((war (with-rng (5) (make-hero "Grim" :t-war))))
  (setf (hero-str war) 10)
  (check "a fresh warrior strikes once" 0 (hero-extra-attacks war))
  (setf (hero-level war) 5)
  (check "a fifth-level warrior strikes twice" 1 (hero-extra-attacks war))
  (setf (hero-level war) 9)
  (check "a ninth-level warrior strikes thrice" 2 (hero-extra-attacks war))
  (check "other classes never do" 0
         (hero-extra-attacks (%combat-hero))))
(let* ((m (parse-map *art* :name "test"))
       (war (with-rng (5) (make-hero "Grim" :t-war)))
       (g (progn (setf (hero-str war) 10)
                 (setf (hero-level war) 5)
                 (new-game m :party (list war)))))
  (start-combat g '(("test rat" 2)))
  ;; the trained warrior strikes twice in one round: d20=10 hits (level
  ;; 5 adds too), 1d8=2 -> 3 damage slays; twice over
  (check "the warrior's training fells two rats" :victory
         (with-rng (10 2 10 2) (combat-round g '(:attack)))))
(let* ((m (parse-map *art* :name "test"))
       (hunter (with-rng (5) (make-hero "Fang" :t-hunter)))
       (g (progn (setf (hero-str hunter) 10)
                 (new-game m :party (list hunter))))
       (msgs (watch-messages g)))
  (start-combat g '(("test bat" 1)))    ; 4 hp
  ;; d20=10 hits; the crit roll 5 beats chance 10+level 1 -> outright
  (with-rng (10 5) (combat-round g '(:attack)))
  (check-true "the hunter strikes a vital spot"
              (find-if (lambda (s) (search "vital spot" s))
                       (funcall msgs)))
  (check-true "the bat fell to one blow" (not (game-combat g))))
(let* ((m (parse-map *art* :name "test"))
       (hunter (with-rng (5) (make-hero "Fang" :t-hunter)))
       (g (progn (setf (hero-str hunter) 10)
                 (new-game m :party (list hunter))))
       (msgs (watch-messages g)))
  (start-combat g '(("test bat" 1)))    ; 4 hp
  ;; d20=10 hits; the crit roll 50 fails; 1d6=2 -> 3 damage, no kill
  (with-rng (10 50 2 0 0) (combat-round g '(:attack)))
  ;; a hit that leaves the monster standing names its remaining hp
  (check-true "an ordinary blow lands without the killing eye"
              (find-if (lambda (s)
                         (search "HITS the test bat for 3 damage (1 hp left)."
                                 s))
                       (funcall msgs)))
  (check-true "the bat fights on" (game-combat g)))

;; Items speak the same vocabulary: :FULL heals, new timed keys.
(define-item 'test-elixir :use '(:heal :full) :consumed t)
(define-item 'test-warstone :use '(:buff-damage 2 :duration 10))
(check-error "an item cannot carry a battle instant"
  (define-item 'test-dud :use '(:damage "1d4")))
(check-error "a healing :use stands alone"
  (define-item 'test-dud :use '(:heal "1d4" :light t :duration 5)))
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt))))
  (damage-hero g grunt 5)
  (give-item g grunt 'test-elixir)
  (check-true "the elixir goes down" (use-item g grunt 'test-elixir))
  (check "it heals to the brim" (hero-max-hp grunt) (hero-hp grunt))
  (give-item g grunt 'test-warstone)
  (check-true "the warstone rubs on" (use-item g grunt 'test-warstone))
  (check "its effect carries the damage bonus" 2
         (effects-damage-bonus g)))

;; Spell-trigger items: :use (:cast SPELL) casts the registered spell
;; for free -- no spell points, no spellbook, the item is the magic
;; (Bard's Tale's Wizhelm).  Non-battle instants (:summon ...) may
;; ride on an item directly.
(define-item 'test-bomb :use '(:cast test-bolt) :consumed t)
(define-item 'test-scroll :use '(:cast test-mend))
(define-item 'test-idol :use '(:summon "stone guardian"))
(check-error "a casting :use names a registered spell"
  (define-item 'test-dud :use '(:cast test-nonesuch)))
(check-error "a casting :use stands alone"
  (define-item 'test-dud :use '(:cast test-bolt :light t)))
(check "a battle trigger aims at nobody" :none
       (item-target-kind 'test-bomb))
(check "a mending trigger aims like its spell" :hero
       (item-target-kind 'test-scroll))
(check "a summon item aims at nobody" :none
       (item-target-kind 'test-idol))
(check "a healing item picks a hero" :hero
       (item-target-kind 'test-elixir))
(check "a timed-use item aims at nobody" :none
       (item-target-kind 'test-warstone))

;; A battle trigger out of combat is refused, and the item is kept; a
;; summon item is no battle item -- it fires anywhere.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt)))
       (msgs (watch-messages g)))
  (give-item g grunt 'test-bomb)
  (check "the bomb waits for a fight" nil (use-item g grunt 'test-bomb))
  (check-true "the refusal says why"
              (find-if (lambda (s) (search "nothing to strike" s))
                       (funcall msgs)))
  (check-true "the refused bomb is kept"
              (hero-carrying-p grunt 'test-bomb))
  (give-item g grunt 'test-idol)
  (check-true "the idol summons in the open"
              (use-item g grunt 'test-idol))
  (check-true "the summons answers"
              (find-if (lambda (s) (search "stone guardian" s))
                       (funcall msgs))))

;; In combat the trigger fires: a plain fighter casts the mage's bolt,
;; pays no spell points, and the spent bomb leaves the pack.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt)))
       (msgs (watch-messages g))
       (used '()))
  (on-event g :item-used (lambda (game h name) (declare (ignore game))
                           (push (list (hero-name h) name) used)))
  (give-item g grunt 'test-bomb)
  (start-combat g '(("test rat" 1)))    ; 3 hp
  (check-true "the bomb fires the bolt"
              (with-rng (2) (use-item g grunt 'test-bomb)))  ; 1d4 -> 3
  (check "no spell points change hands" 0 (hero-sp grunt))
  (check-true "the item speaks the cast"
              (find-if (lambda (s) (search "Test Bomb casts test bolt" s))
                       (funcall msgs)))
  (check ":item-used emitted" '(("Alva" test-bomb)) used)
  (check "the spent bomb is gone" nil (hero-carrying-p grunt 'test-bomb))
  (check "the bolt slew the rat" nil
         (alive-monsters (game-combat g))))

;; A mending trigger heals its chosen target through the spell path.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage))))
  (damage-hero g grunt 5)
  (give-item g mage 'test-scroll)
  (check-true "the scroll mends the chosen hero"
              (with-rng (3) (use-item g mage 'test-scroll grunt)))
  (check "the mending landed on the target" 7 (hero-hp grunt))
  (check "the scroll cost the user no sp" 6 (hero-sp mage))
  (check-true "the unconsumed scroll is kept"
              (hero-carrying-p mage 'test-scroll)))

;; combat-round accepts (:cast SPELL [TARGET]) beside :attack/:defend.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage))))
  (start-combat g '(("test rat" 2)))  ; two rats, 3 hp each
  ;; grunt attacks (d20=11 hits ac 10, 1d6=3 slays rat #1); the mage's
  ;; bolt (1d4=3) slays rat #2; nobody is left to strike back.
  (check "mixed action round wins" :victory
         (with-rng (10 2 2)
           (combat-round g (list :attack '(:cast test-bolt)))))
  (check "the cast in the round paid sp" 4 (hero-sp mage)))

;; Round structure: every round opens with its number.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g)))
  (start-combat g '(("test rat" 1)))
  (with-rng (0 13) (combat-round g '(:defend)))
  (check-true "round 1 header"
              (find "-- Round 1 --" (funcall msgs) :test #'equal))
  (check "the combat counts its rounds" 1
         (combat-round-no (game-combat g)))
  (with-rng (0 13) (combat-round g '(:defend)))
  (check-true "round 2 header"
              (find "-- Round 2 --" (funcall msgs) :test #'equal)))

;; The round-orders model: the fight opens on the party-level engage
;; page — Fight or Run, the one choice that is the whole
;; party's — then the page asks ONE hero at a time, and the last pick
;; raises the review page — the round waits there for Y.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage)))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 2)))
  (check "a fresh round's orders are not engaged" nil
         (combat-orders-engaged view))
  (check "the engage page carries the party's choice"
         '("Fight" "Run")
         (last (menu-texts (combat-orders-lines g view)) 2))
  (check-true "the engage page shows the coming round"
              (find-if (lambda (s) (search "Round 1" s))
                       (menu-texts (combat-orders-lines g view))))
  (check "no hero is asked before the party engages" nil
         (find "What will Alva do?"
               (menu-texts (combat-orders-lines g view))
               :test #'equal))
  (check "a hero key does nothing on the engage page" nil
         (combat-orders-act g view #\d))
  (check "the engage page took no hero pick" nil
         (combat-orders-chosen view))
  (check "f engages the party" nil (combat-orders-act g view #\f))
  (check-true "this round's orders are engaged"
              (combat-orders-engaged view))
  (check "orders ask the first hero" grunt (combat-orders-hero g view))
  (check-true "orders page shows the coming round"
              (find-if (lambda (s) (search "Round 1" s))
                       (menu-texts (combat-orders-lines g view))))
  (check-true "orders page lists the enemy group"
              (find-if (lambda (s) (search "2 test rats" s))
                       (menu-texts (combat-orders-lines g view))))
  (check-true "orders page asks the hero at hand"
              (find "What will Alva do?"
                    (menu-texts (combat-orders-lines g view))
                    :test #'equal))
  ;; the page names its actions bracket-free, one per row, first
  ;; letter as the key, and only the actions the hero can take — the
  ;; grunt neither casts, plays nor carries a thing; the navigation
  ;; keys (Esc undo, +/- speed) live on the help screen
  (check "orders page ends with the grunt's own actions"
         '("Attack" "Defend")
         (last (menu-texts (combat-orders-lines g view)) 2))
  (check "the page asks one hero, not the roster" nil
         (find-if (lambda (s) (search "Zzgo" s))
                  (menu-texts (combat-orders-lines g view))))
  (check "the first pick advances" nil (combat-orders-act g view #\a))
  (check "orders ask the second hero" mage (combat-orders-hero g view))
  (check-true "the page moved on to that hero"
              (find "What will Zzgo do?"
                    (menu-texts (combat-orders-lines g view))
                    :test #'equal))
  ;; the mage casts, so its page grows the Cast row the grunt's lacked
  (check "the caster's page offers Cast"
         '("Attack" "Defend" "Cast")
         (last (menu-texts (combat-orders-lines g view)) 3))
  (check "esc undoes the previous pick" nil
         (combat-orders-act g view #\Escape))
  (check "back to the first hero" grunt (combat-orders-hero g view))
  (combat-orders-act g view #\a)
  (check "the last pick opens the review, not the round" nil
         (combat-orders-act g view #\d))
  (check-true "the orders are under review"
              (combat-orders-review view))
  (let ((lines (menu-texts (combat-orders-lines g view))))
    (check-true "the review lists every hero's order"
                (and (find-if (lambda (s) (and (search "Alva" s)
                                               (search "attack" s)))
                              lines)
                     (find-if (lambda (s) (and (search "Zzgo" s)
                                               (search "defend" s)))
                              lines)))
    (check-true "the review asks the question"
                (find "Is this OK?" lines :test #'equal))
    (check "the review names the two answers, bracket-free"
           "Yes fight  No redo" (first (last lines))))
  (check "no round ran while picking" 0
         (combat-round-no (game-combat g)))
  (check "y fights the reviewed round"
         '(:fight (:attack :defend))
         (combat-orders-act g view #\y)))

;; N on the review throws the orders away and asks again from the
;; first hero; Esc there says the same thing.
(dolist (key (list #\n #\Escape))
  (let* ((m (parse-map *art* :name "test"))
         (grunt (%combat-hero))
         (mage (%combat-mage))
         (g (new-game m :party (list grunt mage)))
         (view (make-combat-orders)))
    (start-combat g '(("test rat" 1)))
    (combat-orders-act g view #\f)      ; the party engages
    (combat-orders-act g view #\a)
    (combat-orders-act g view #\d)
    (check-true "the review is up" (combat-orders-review view))
    (check (format nil "~:[n~;esc~] on the review keeps the fight waiting"
                   (eql key #\Escape))
           nil (combat-orders-act g view key))
    (check "the review is gone" nil (combat-orders-review view))
    (check "the orders are thrown away" nil (combat-orders-chosen view))
    (check "and the first hero is asked again" grunt
           (combat-orders-hero g view))))

;; Running is the engage page's alone — the whole party runs or
;; nobody, and only at the top of a round.  Once the party engages,
;; R answers nowhere for the rest of the round: not on a hero's page,
;; not on the review.  The pace keys keep answering on every page.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (view (make-combat-orders))
       (*combat-speed* 3))
  (start-combat g '(("test rat" 1)))
  (check "+ sets the pace on the engage page" nil
         (combat-orders-act g view #\+))
  (check "and it took" 4 *combat-speed*)
  (check "r runs from the engage page" :flee
         (combat-orders-act g view #\r))
  (combat-orders-act g view #\f)        ; the party engages instead
  (check "r does nothing on a hero's page" nil
         (combat-orders-act g view #\r))
  (check "no pick was recorded by it" nil (combat-orders-chosen view))
  (combat-orders-act g view #\a)
  (check-true "the review is up" (combat-orders-review view))
  (check "+ still sets the pace on the review" nil
         (combat-orders-act g view #\+))
  (check "and it took again" 5 *combat-speed*)
  (check "the review is still up" t (combat-orders-review view))
  (check "r does nothing on the review" nil
         (combat-orders-act g view #\r))
  (check-true "the review still stands" (combat-orders-review view)))

;; The choice returns at the top of EVERY round: after a failed run
;; (the monsters took their free round) and after a fought round
;; alike, the next round's fresh orders view opens on it again.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 2)))    ; 3 hp each
  (check "the engage page offers the run" :flee
         (combat-orders-act g view #\r))
  (check "the run fails and the fight goes on" :ongoing
         (with-rng (60 0 11 1) (attempt-flee g)))
  (let ((fresh (make-combat-orders)))
    (check "after the failed run the choice stands again"
           '("Fight" "Run")
           (last (menu-texts (combat-orders-lines g fresh)) 2))
    ;; engage, fight a round that leaves a rat standing (the hero
    ;; misses, the rat misses back) — the fight goes on
    (combat-orders-act g fresh #\f)
    (check "the fought round goes on" :ongoing
           (with-rng (0 0 0) (combat-round g)))
    (let ((next (make-combat-orders)))
      (check "the next round opens on the choice again"
             '("Fight" "Run")
             (last (menu-texts (combat-orders-lines g next)) 2))
      (check "and r still runs there" :flee
             (combat-orders-act g next #\r)))))

;; A hero's page costs a fixed handful of rows: a seven-strong party
;; asks seven pages, none of them longer than a two-strong party's.
(let* ((m (parse-map *art* :name "test"))
       (two (new-game (parse-map *art* :name "test")
                      :party (list (%combat-hero "A") (%combat-hero "B"))))
       (seven (new-game m :party (loop for i from 1 to 7
                                       collect (%combat-hero
                                                (format nil "H~D" i)))))
       (v2 (make-combat-orders))
       (v7 (make-combat-orders)))
  (start-combat two '(("test rat" 1)))
  (start-combat seven '(("test rat" 1)))
  (combat-orders-act two v2 #\f)        ; both parties engage
  (combat-orders-act seven v7 #\f)
  (check "the hero page does not grow with the party"
         (length (combat-orders-lines two v2))
         (length (combat-orders-lines seven v7))))

;; Both orders pages draw in the MESSAGE column on the Amiga (the
;; takeover, like a shop menu), not on the roomier view column the
;; page used to overlay: at the lores profile that column holds 27
;; characters across and 12 microfont rows.  Every row must fit that
;; width whole — a wrapped row costs a line — and a FULL party's
;; pages, hero pages and review alike, must fit the height.
;;
;; A hero who is both a caster and a singer (a game may define such a
;; class) draws the widest hero page there is: Attack, Defend, Cast,
;; Play and Use all at once.  A class and spell of its own keep
;; this fixture off the :T-MAGE roster the spell tests above depend
;; on.
(define-hero-class :t-adept :hp-dice "1d6+2" :damage "1d4" :ac 9
                            :caster t :singer t)
(define-spell 'test-adept-bolt :cost 1 :level 1 :classes '(:t-adept)
  :damage "1d4")

(defun %combat-adept (&optional (name "Ori"))
  "A deterministic level-1 :t-adept: caster and singer both."
  (with-rng (5) (make-hero name :t-adept)))

(let ((cols 27)
      (rows 12))
  (let* ((m (parse-map *art* :name "test"))
         (party (cons (%combat-adept "Hero1")
                      (loop for i from 2 to +party-limit+
                           collect (%combat-hero (format nil "Hero~D" i)))))
         (g (new-game m :party party))
         (view (make-combat-orders)))
    (give-item g (first party) 'test-elixir)
    (start-combat g '(("test rat" 2) ("test ogre" 1)))
    (flet ((fits (label)
             (let ((lines (menu-texts (combat-orders-lines g view))))
               ;; the page's own rows — a picked action's label carries
               ;; campaign text (a long spell title) and may still wrap
               (check (format nil "no ~A row overruns the lores column"
                              label)
                      nil
                      (remove-if (lambda (s) (<= (length s) cols)) lines))
               (check-true (format nil "the ~A fits the lores page height"
                                   label)
                           (<= (length (remove "" lines :test #'equal))
                               rows)))))
      (fits "engage page")
      (combat-orders-act g view #\f)    ; the party engages
      (fits "hero page")
      ;; attack is a front-rank pick; the back ranks defend (reach)
      (dotimes (i +party-limit+)
        (combat-orders-act g view (if (< i 3) #\a #\d)))
      (check-true "a full party's picks reach the review"
                  (combat-orders-review view))
      (fits "review page"))))

;; The spells/songs page for singers: the songbook under its own head
;; (a singer is a HERO-MAGIC-P hero too), its card offering the tune
;; instead of a cast.  A hero who both casts and sings runs the two
;; sections off ONE numbered list, so a single run of pick digits
;; addresses the whole book; the heads follow the window rather than
;; the book, so a scrolled page still says what it is looking at.  The
;; adept gets two more spells to grow past the window on (class-gated
;; to :t-adept, invisible to the :t-mage checks).
(define-spell 'test-adept-glow :cost 1 :level 1 :classes '(:t-adept)
  :light t :duration 5)
(define-spell 'test-adept-mend :cost 1 :level 1 :classes '(:t-adept)
  :heal "1d4")
(define-spell 'test-adept-ward :cost 2 :level 3 :classes '(:t-adept)
  :buff-ac 1 :duration 5)
(let* ((m (parse-map *art* :name "test"))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (adept (%combat-adept))
       (g (new-game m :party (list bard adept)))
       (bv (make-magic-view bard))
       (av (make-magic-view adept)))
  (check-true "the singer has a spells/songs page" (hero-magic-p bard))
  (check "the singer's page is the songbook under its head"
         '("Songs:" "1) test march" "2) test gleam" "3) test road")
         (menu-texts (butlast (magic-lines g bv) 2)))
  (check "the adept's page stacks both sections on one numbering"
         '("Spells:" "1) test adept bolt" "2) test adept glow"
           "3) test adept mend" ""
           "Songs:" "4) test march" "5) test gleam" "6) test road")
         (menu-texts (butlast (magic-lines g av) 2)))
  (check "the adept's short book does not scroll" nil
         (progn (magic-lines g av) *menu-scroll*))
  ;; the song card offers the tune, not a cast
  (magic-act g bv #\1)
  (let ((texts (menu-texts (magic-lines g bv))))
    (check "the song card heads with the title" "*** test march ***"
           (first texts))
    (check-true "it names the level and the tunes in hand"
                (member "Level 1   Tunes 1/1" texts :test #'equal))
    (check-true "it reads the effect out of the spec"
                (member "AC 2 better" texts :test #'equal)))
  (check-true "the song card offers the tune"
              (member (menu-option #\p "Play it") (magic-lines g bv)
                      :test #'equal))
  ;; playing it resolves on the spot: the front-end leaves the sheet so
  ;; the log can be read
  (check "p plays the tune" :done (magic-act g bv #\p))
  (check-true "and the song is up" (current-song g))
  ;; level 3 grows the adept's book to eight entries — ward and dirge
  ;; arrive — and eight is exactly the book's window
  ;; (+BOOK-PAGE-SIZE+): head + entries + spacer + NEXT are the lores
  ;; takeover page's eleven rows whole, so nothing scrolls yet
  (setf (hero-level adept) 3)
  (check "an eight-entry book fills its page whole" nil
         (progn (magic-lines g av) *menu-scroll*))
  (check "and d moves nothing on it" nil (magic-act g av #\d))
  ;; the ninth entry is the one that makes the book scroll
  (define-spell 'test-adept-seal :cost 3 :level 5 :classes '(:t-adept)
    :buff-ac 2 :duration 5)
  (setf (hero-level adept) 5)
  (check "the grown book windows at the page size" '(0 8 9)
         (progn (magic-lines g av) *menu-scroll*))
  (check "d scrolls the spells/songs page" 1
         (progn (magic-act g av #\d) (magic-view-top av)))
  ;; the head is re-emitted at the top of a scrolled window — the
  ;; section began above it, and a page with no head at all would not
  ;; say what it is looking at — and the digits number the WINDOW
  (check "a scrolled window still names its section"
         '("Spells:" "1) test adept glow" "2) test adept mend"
           "3) test adept ward" "4) test adept seal" ""
           "Songs:" "5) test march" "6) test gleam" "7) test dirge"
           "8) test road")
         (menu-texts (butlast (magic-lines g av) 2)))
  (check "and the window's first row is its digit 1" '(:spell . test-adept-glow)
         (progn (magic-act g av #\1)
                (prog1 (magic-view-pending av)
                  (magic-act g av #\Escape))))
  (check "n turns the carousel from the list" :next
         (magic-act g av #\n))
  (check "Esc gives way to the stat block" :cancelled
         (magic-act g av #\Escape)))

;; C during orders opens the spell pick for the hero at hand; the pick
;; lands as that hero's round action instead of fighting a round.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage)))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 2)))    ; 3 hp each
  (combat-orders-act g view #\f)        ; the party engages
  (combat-orders-act g view #\a)        ; the grunt attacks
  (check "c opens the mage's spell pick" nil (combat-orders-act g view #\c))
  (check-true "the pick page is the mage's cast menu"
              (find-if (lambda (s) (search "Zzgo casts" s))
                       (menu-texts (combat-orders-lines g view))))
  ;; Esc backs out of the pick to the action keys, hero unchanged
  (check "esc leaves the spell pick" nil
         (combat-orders-act g view #\Escape))
  (check "still asking the mage" mage (combat-orders-hero g view))
  (combat-orders-act g view #\c)
  (check "the spell pick completes the orders" nil
         (combat-orders-act g view #\1))        ; test-bolt, no target
  (let ((r (combat-orders-act g view #\y)))     ; ... and the review says go
    (check "the reviewed orders carry the cast"
           '(:fight (:attack (:cast test-bolt))) r)
    (check "picking paid no sp yet" 6 (hero-sp mage))
    (check "picking ran no round" 0 (combat-round-no (game-combat g)))
    ;; the returned actions fight the round the mixed-action way
    (check "the ordered round wins" :victory
           (with-rng (10 2 2) (combat-round g (second r))))
    (check "the ordered cast paid sp" 4 (hero-sp mage))))

;; A heal pick during orders carries its chosen target along.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage)))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 1)))
  (combat-orders-act g view #\f)        ; the party engages
  (combat-orders-act g view #\a)
  (combat-orders-act g view #\c)
  (combat-orders-act g view #\2)        ; test-mend: heal, pick a target
  (check "the heal pick completes the orders" nil
         (combat-orders-act g view #\1))        ; on the grunt
  (check-true "the review shows the heal with its target"
              (find-if (lambda (s) (and (search "Zzgo" s)
                                        (search "cast test mend" s)
                                        (search "on Alva" s)))
                       (menu-texts (combat-orders-lines g view))))
  (check "the reviewed orders carry the target"
         (list :fight (list :attack (list :cast 'test-mend grunt)))
         (combat-orders-act g view #\y)))

;; P during orders opens the song pick the same way.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (bard (with-rng () (make-hero "Mel" :t-bard)))
       (g (new-game m :party (list grunt bard)))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 1)))
  (combat-orders-act g view #\f)        ; the party engages
  (combat-orders-act g view #\a)
  (check "p opens the bard's song pick" nil (combat-orders-act g view #\p))
  (check "the song pick completes the orders" nil
         (combat-orders-act g view #\1))
  (check "the reviewed orders carry the song"
         '(:fight (:attack (:sing test-march)))
         (combat-orders-act g view #\y))
  (check "picking spent no tune" 1 (hero-tunes bard)))

;; U during orders opens the use pick: the Wizhelm's moment -- the
;; pick lands as (:use ITEM) and the round fires the item's spell.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt)))
       (view (make-combat-orders)))
  (give-item g grunt 'test-bomb)
  (start-combat g '(("test rat" 1)))    ; 3 hp
  (combat-orders-act g view #\f)        ; the party engages
  (check "u opens the use pick" nil (combat-orders-act g view #\u))
  (check-true "the pick page lists the bomb"
              (find-if (lambda (s) (search "Test Bomb" s))
                       (menu-texts (combat-orders-lines g view))))
  (check "the use pick completes the orders" nil
         (combat-orders-act g view #\1))
  (let ((r (combat-orders-act g view #\y)))
    (check "the reviewed orders carry the item"
           '(:fight ((:use test-bomb))) r)
    (check-true "picking spent nothing"
                (hero-carrying-p grunt 'test-bomb))
    ;; d20=10: the rat's strike back never comes -- the bolt (1d4=3)
    ;; slays it first
    (check "the ordered round wins by item" :victory
           (with-rng (2) (combat-round g (second r))))
    (check "the round spent the bomb" nil
           (hero-carrying-p grunt 'test-bomb))))

;; Refusals stay put.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (g (new-game m :party (list grunt)))
       (msgs (watch-messages g))
       (view (make-combat-orders)))
  (start-combat g '(("test rat" 1)))
  (combat-orders-act g view #\f)        ; the party engages
  (check "c on a non-caster stays put" nil (combat-orders-act g view #\c))
  (check-true "and says who cannot cast"
              (find "Alva cannot cast." (funcall msgs) :test #'equal))
  (check "p on a non-singer stays put" nil (combat-orders-act g view #\p))
  (check-true "and says who cannot play"
              (find "Alva cannot play." (funcall msgs) :test #'equal))
  (check "u with an empty pack stays put" nil
         (combat-orders-act g view #\u))
  (check-true "and says who has nothing"
              (find "Alva has nothing to use." (funcall msgs)
                    :test #'equal))
  (check "still asking the same hero" grunt (combat-orders-hero g view)))

;; Combat transcript speed: +/- during orders, clamped both ways; the
;; front-ends linger COMBAT-MESSAGE-DELAY seconds on each message.
;; Combat starts slow — the default is the floor, and +/- speed it up.
(check "combat speed starts slow" 1 *combat-speed*)
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g))
       (view (make-combat-orders))
       (*combat-speed* 3))
  (start-combat g '(("test rat" 1)))
  (check "speed 3 lingers half a second" 0.5 (combat-message-delay))
  (check "+ is no round action" nil (combat-orders-act g view #\+))
  (check "+ raised the speed" 4 *combat-speed*)
  (check-true "and said so"
              (find "Combat speed 4 of 5." (funcall msgs) :test #'equal))
  (combat-orders-act g view #\+)
  (combat-orders-act g view #\+)
  (check "speed caps at the maximum" 5 *combat-speed*)
  (check "the cap is instant" 0.0 (combat-message-delay))
  (dotimes (i 6) (combat-orders-act g view #\-))
  (check "speed floors at 1" 1 *combat-speed*)
  (check "the floor lingers a second" 1.0 (combat-message-delay)))

;; SP regen: daylight, outdoors, out of combat — 1 sp per 4 minutes.
(let* ((m (parse-map *corridor-art* :name "regen" :start-facing :east))
       (mage (%combat-mage))
       (g (new-game m :party (list mage))))
  (setf (hero-sp mage) 0)
  (setf (game-time g) 480)              ; day 1, 08:00 — daylight
  (dotimes (i 8) (turn-right g))        ; 8 minutes pass outdoors
  (check "eight daylight minutes regain two sp" 2 (hero-sp mage))
  (setf (game-time g) 1340)             ; night
  (dotimes (i 8) (turn-right g))
  (check "night regains nothing" 2 (hero-sp mage))
  (setf (hero-sp mage) (hero-max-sp mage))
  (setf (game-time g) 480)
  (dotimes (i 8) (turn-right g))
  (check "regen caps at max sp" (hero-max-sp mage) (hero-sp mage)))

;; No regen underground: a :dark zone shuts the sky out.
(let ((path "tests/tmp-dark-regen.map"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string "+-+-+
|@  |
+-+-+
(zone :dark t)
" s))
  (let* ((m (load-map-file path))
         (mage (%combat-mage))
         (g (new-game m :party (list mage))))
    (setf (hero-sp mage) 0)
    (setf (game-time g) 480)
    (dotimes (i 8) (turn-right g))
    (check "no regen in a :dark zone" 0 (hero-sp mage)))
  (delete-file path))

;; The cast menu model: the full key walk both frontends drive.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage)))
       (view (make-cast-view)))
  (check-true "caster page lists only casters"
              (let ((lines (menu-texts (cast-lines g view))))
                (and (find-if (lambda (s) (search "2) Zzgo" s)) lines)
                     (not (find-if (lambda (s) (search "Alva" s)) lines)))))
  (check "the caster row's key is its party slot" #\2
         (menu-line-key
          (find-if (lambda (line)
                     (search "Zzgo" (menu-line-text line)))
                   (cast-lines g view))))
  (check "a non-caster digit is ignored" nil
         (progn (cast-act g view #\1) (cast-view-hero view)))
  (cast-act g view #\2)
  (check "digit picks the caster by party slot" "Zzgo"
         (hero-name (cast-view-hero view)))
  (check-true "spell page shows the book"
              (find-if (lambda (s) (search "test bolt" s))
                       (menu-texts (cast-lines g view))))
  ;; a damage spell out of combat: refused in place, menu stays open
  (check "uncastable pick refuses and stays" nil (cast-act g view #\1))
  (check-true "menu still open on the spell page"
              (and (cast-view-hero view) (null (cast-view-spell view))))
  ;; a heal walks on to the target page and commits
  (damage-hero g grunt 3)
  (cast-act g view #\2)                 ; test-mend -> target page
  (check-true "heal pick opens the target page"
              (find-if (lambda (s) (search "on whom?" s))
                       (menu-texts (cast-lines g view))))
  (check "target digit commits the cast" :done
         (with-rng (7) (cast-act g view #\1)))
  (check "menu cast healed the grunt" 8 (hero-hp grunt))
  (check "menu cast paid the sp" 4 (hero-sp mage)))

;; Esc unwinds the menu one page at a time, then cancels.
(let* ((m (parse-map *art* :name "test"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (view (make-cast-view)))
  (cast-act g view #\1)
  (cast-act g view #\2)                 ; test-mend -> target page
  (check "esc leaves the target page" nil (cast-act g view #\Escape))
  (check "back on the spell page" nil (cast-view-spell view))
  (check "esc leaves the spell page" nil (cast-act g view #\Escape))
  (check "back on the caster page" nil (cast-view-hero view))
  (check "esc at the top cancels" :cancelled (cast-act g view #\Escape)))

;; In combat the menu composes a full round: the caster casts, every
;; other living hero attacks.
(let* ((m (parse-map *art* :name "test"))
       (grunt (%combat-hero))
       (mage (%combat-mage))
       (g (new-game m :party (list grunt mage)))
       (view (make-cast-view :in-combat t)))
  (start-combat g '(("test rat" 2)))
  (cast-act g view #\2)                 ; Zzgo
  ;; grunt attack: d20=11 hits, 1d6=3 slays; bolt 1d4=3 slays the other.
  (check "combat commit fights the round" :done
         (with-rng (10 2 2) (cast-act g view #\1)))
  (check "the round is won" nil (game-combat g)))

;;; ---------------------------------------------------------------------
;;; Items, inventory and equipment (M4)

(define-item 't-torch   :price 2)
(define-item 't-sword   :kind :weapon :price 10 :damage "1d6+2")
(define-item 't-axe     :kind :weapon :price 14 :damage "1d8")
(define-item 't-mail    :kind :armor  :price 20 :ac 4 :classes '(:tester))
(define-item 't-buckler :kind :shield :price 6  :ac 1)

(check "item-title capitalizes the name" "T Sword" (item-title 't-sword))
(check "item-title override" "Fancy Lamp"
       (item-title (define-item 't-lamp :title "Fancy Lamp" :price 1)))
(check-error "unknown item rejected" (find-item-type 't-nonesuch))
(check-error "define-item rejects a bad kind"
  (define-item 't-bogus :kind :hat))
(check ":notes rides along as data" "glows near orcs (to come)"
       (item-type-notes
        (find-item-type (define-item 't-relic :price 9
                          :notes "glows near orcs (to come)"))))
(check "no :notes reads NIL" nil (item-type-notes (find-item-type 't-sword)))
(check-error "define-item rejects non-string :notes"
  (define-item 't-scribble :price 1 :notes '(:very :magic)))
(check ":description rides along as data" "A plain blade, honest work."
       (item-type-description
        (find-item-type (define-item 't-plain-blade :price 3
                          :description "A plain blade, honest work."))))
(check "no :description reads NIL" nil
       (item-type-description (find-item-type 't-sword)))
(check-error "define-item rejects non-string :description"
  (define-item 't-mumble :price 1 :description '(:very :plain)))

(check "inventory limit is eight" 8 +inventory-limit+)

;; Pack: give up to the limit, refuse the ninth, drop.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (dotimes (i 8)
    (check-true (format nil "give-item accepts item ~D" (1+ i))
                (give-item g h 't-torch)))
  (check "pack holds eight" 8 (length (hero-items h)))
  (check "give-item refuses the ninth" nil (give-item g h 't-sword))
  (check-true "pack-full message"
              (find-if (lambda (s) (search "pack is full" s))
                       (funcall msgs)))
  (check-true "drop-item removes one" (drop-item g h 't-torch))
  (check "pack down to seven" 7 (length (hero-items h)))
  (check "drop-item without the item" nil (drop-item g h 't-sword))
  (check-error "give-item checks the item exists" (give-item g h 't-nada)))

;;; ---------------------------------------------------------------------
;;; Quest pieces (:QUEST): carried outside the eight-slot limit, read on
;;; the pack's own quest page, and neither sold nor thrown away.

(define-item 't-key :kind :misc :title "T Key"
  :quest t :description "A key of black iron.")
(define-item 't-token :kind :misc :title "T Token" :quest t)
(check-error "a quest piece may not carry a price"
  (define-item 't-priced :kind :misc :quest t :price 5))
(check-true "quest-item-p knows a plot piece" (quest-item-p 't-key))
(check "quest-item-p says no to gear" nil (quest-item-p 't-sword))

;; The limit counts the gear alone: eight swords and any number of
;; keys fit the same pack.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (dotimes (i 8) (give-item g h 't-torch))
  (check "the burden is the eight" 8 (pack-burden h))
  (check "and the ninth torch is refused" nil (give-item g h 't-sword))
  (check-true "a quest piece still fits a full pack"
              (give-item g h 't-key))
  (check-true "and a second one after it" (give-item g h 't-token))
  (check "the pack now holds ten things" 10 (length (hero-items h)))
  (check "but the burden is unchanged" 8 (pack-burden h))
  (check "the gear rows are still the eight" 8 (length (pack-gear h)))
  (check "the quest pieces list in pack order" '(t-key t-token)
         (hero-quest-items h))
  (check "the party's pieces are its heroes'" '(t-key t-token)
         (party-quest-items g))
  ;; no shop buys the way forward, no hand throws it away
  (check "a quest piece is not sold" nil (sell-item g h 't-key))
  (check-true "the shop says so"
              (find-if (lambda (s) (search "No shop will buy" s))
                       (funcall msgs)))
  (check "a quest piece is not thrown away" nil (discard-item g h 't-key))
  (check-true "the hero says so"
              (find-if (lambda (s) (search "will not part with" s))
                       (funcall msgs)))
  (check-true "it is still carried" (hero-carrying-p h 't-key))
  ;; the gate ops read it like any other item
  (check-true "a WHEN-ITEM gate sees it" (party-carrying-p g 't-key))
  (check-true "and TAKE-ITEM spends it" (run-take-item-op g 't-key))
  (check "so the piece is gone" nil (hero-carrying-p h 't-key)))

;; The pack page: gear numbered, quest pieces on their own page, and
;; a digit means the gear row it prints even with a piece in the pack.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (view (make-equip-view h)))
  (give-item g h 't-key)                ; the piece comes first in the pack
  (give-item g h 't-sword)
  (let ((texts (menu-texts (equip-lines g view))))
    (check-true "the sword is row 1, the key not a row at all"
                (find-if (lambda (s) (search "1) T Sword" s)) texts))
    (check "no quest piece among the gear rows" nil
           (find-if (lambda (s) (search "T Key" s)) texts))
    (check-true "the quest key is offered, and counts"
                (find-if (lambda (s) (search "Read the quest pieces (1)" s))
                         texts)))
  ;; row 1 is the sword, though the sword sits second in the pack
  (equip-act g view #\1)
  (check-true "the digit equips the gear row it printed"
              (member 't-sword (hero-equipped h)))
  (equip-act g view #\r)
  (let ((texts (menu-texts (equip-lines g view))))
    (check-true "the quest page names the piece"
                (find-if (lambda (s) (search "T Key" s)) texts))
    (check-true "and reads its description"
                (find-if (lambda (s) (search "key of black iron" s)) texts)))
  (check "Esc turns back to the pack" nil (equip-act g view #\Escape))
  (check-true "the gear rows are back"
              (find-if (lambda (s) (search "1) T Sword" s))
                       (menu-texts (equip-lines g view)))))

;; A hero carrying nothing but the story has no gear pages and says so.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (view (make-equip-view h)))
  (give-item g h 't-key)
  (equip-act g view #\t)
  (check-true "nothing to throw away"
              (find-if (lambda (s) (search "nothing to throw away" s))
                       (funcall msgs)))
  (check-true "the empty-pack row still shows"
              (find-if (lambda (s) (search "The pack is empty" s))
                       (menu-texts (equip-lines g view)))))

;; The shop's sell page lists the gear alone.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (give-item g h 't-key)
  (give-item g h 't-sword)
  (check "the sell page counts the gear" 1 (length (pack-gear h))))

;; Equipment: one per kind, class restrictions, misc not equippable.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (check "bare attack dice are the class dice" "1d6" (hero-attack-dice h))
  (check "bare effective ac is the class ac" 8 (hero-effective-ac h))
  (check "equip-item needs the item in the pack" nil
         (equip-item g h 't-sword))
  (give-item g h 't-sword)
  (give-item g h 't-axe)
  (give-item g h 't-mail)
  (give-item g h 't-buckler)
  (give-item g h 't-torch)
  (check-true "equip a weapon" (equip-item g h 't-sword))
  (check "equipped weapon found" 't-sword (equipped-of-kind h :weapon))
  (check "weapon changes the attack dice" "1d6+2" (hero-attack-dice h))
  (check-true "equip message"
              (find-if (lambda (s) (search "equips T Sword" s))
                       (funcall msgs)))
  (check-true "equipping a second weapon swaps it" (equip-item g h 't-axe))
  (check "swapped weapon" 't-axe (equipped-of-kind h :weapon))
  (check "only one weapon equipped" 1 (length (hero-equipped h)))
  (check "the swapped-out sword stays in the pack" 5
         (length (hero-items h)))
  (check-true "equip armor (class allowed)" (equip-item g h 't-mail))
  (check-true "equip a shield" (equip-item g h 't-buckler))
  (check "armor and shield lower the descending ac" 3
         (hero-effective-ac h))
  ;; the sheet agrees with the roster on the equipped hero: both read
  ;; HERO-EFFECTIVE-AC — a loaded save's sheet must not fall back to
  ;; the bare base slot beside a roster showing the equipped value
  (check "the sheet's AC line carries the equipped ac"
         (format nil "HP ~D/~D  AC 3" (hero-hp h) (hero-max-hp h))
         (third (hero-summary-lines h g)))
  (check "misc items cannot be equipped" nil (equip-item g h 't-torch))
  (check-true "unequip returns t" (unequip-item g h 't-mail))
  (check "unequip keeps the item in the pack" t
         (not (not (hero-carrying-p h 't-mail))))
  (check "ac back without the armor" 7 (hero-effective-ac h))
  (check "unequip when not equipped" nil (unequip-item g h 't-mail))
  ;; the equipped stars live on the pack page, not the sheet — the
  ;; sheet's stat block stays pack-free
  (check "the sheet stays pack-free with a full pack" nil
         (find-if (lambda (s) (search "Pack" s))
                  (hero-summary-lines h))))

;; Two-handed weapons: both hands or a shield hand, never both (the
;; D&D rule).  The flag is a weapon trait — other kinds refuse it.
(check-error "define-item rejects :two-handed off a weapon"
  (define-item 't-big-shield :kind :shield :two-handed t))

;; :REACH is a missile trait — only a bow, its arrows or a thrown
;; weapon ever consult it (%MISSILE-PAIR); any other kind refuses it,
;; same as :two-handed above.
(check-error "define-item rejects :reach off a missile kind"
  (define-item 't-far-potion :kind :misc :reach 10))
(define-item 't-greatsword :kind :weapon :price 30 :damage "2d6"
             :two-handed t)
(check "two-handed items carry the (2H) marker" " (2H)"
       (item-hand-marker 't-greatsword))
(check "one-handed items carry no marker" ""
       (item-hand-marker 't-sword))
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (give-item g h 't-greatsword)
  (give-item g h 't-buckler)
  (give-item g h 't-sword)
  (check-true "a two-handed weapon equips bare-handed"
              (equip-item g h 't-greatsword))
  (check "no shield beside a two-handed weapon" nil
         (equip-item g h 't-buckler))
  (check-true "the refusal says the hands are full"
              (find-if (lambda (s) (search "hands are full" s))
                       (funcall msgs)))
  (check-true "a one-handed weapon swaps in" (equip-item g h 't-sword))
  (check-true "now the shield goes on" (equip-item g h 't-buckler))
  (check "no two-handed weapon beside a shield" nil
         (equip-item g h 't-greatsword))
  (check-true "the refusal says both hands are needed"
              (find-if (lambda (s) (search "needs both hands" s))
                       (funcall msgs)))
  (check-true "shield off, the greatsword returns"
              (and (unequip-item g h 't-buckler)
                   (equip-item g h 't-greatsword)))
  (let ((view (make-equip-view h)))
    (check-true "the gear page marks the worn greatsword (2H)"
                (find-if (lambda (s) (search "T Greatsword* (2H)" s))
                         (menu-texts (equip-lines g view))))))

;; The Bard's Tale categories are all equipment: helmet, gloves, bow,
;; arrow, instrument, ring, wand, figurine -- one of each kind at a
;; time, and every worn piece's :AC counts at once.
(check-error "define-item still rejects a made-up kind"
  (define-item 't-bogus :kind :cloak))
(define-item 't-helm   :kind :helmet :price 50  :ac 1)
(define-item 't-cap    :kind :helmet :price 20  :ac 1)
(define-item 't-mitts  :kind :gloves :price 40  :ac 1)
(define-item 't-band   :kind :ring   :price 700 :ac 2)
(define-item 't-bow    :kind :bow    :price 60)
(define-item 't-quiver :kind :arrow  :price 130 :damage "2d4")
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (dolist (name '(t-helm t-cap t-mitts t-band t-bow t-quiver t-sword))
    (give-item g h name))
  (let ((base (hero-effective-ac h)))
    (check-true "a helmet goes on" (equip-item g h 't-helm))
    (check-true "gloves go on beside it" (equip-item g h 't-mitts))
    (check-true "a ring goes on beside both" (equip-item g h 't-band))
    (check-true "bow and arrows ride along"
                (and (equip-item g h 't-bow) (equip-item g h 't-quiver)))
    (check-true "the weapon still fits" (equip-item g h 't-sword))
    (check "six pieces worn at once" 6 (length (hero-equipped h)))
    (check "helm, gloves and ring all ward together" (- base 4)
           (hero-effective-ac h))
    (check "the sword, not the arrows, feeds the attack" "1d6+2"
           (hero-attack-dice h))
    (check-true "a second helmet swaps the first, kind for kind"
                (equip-item g h 't-cap))
    (check "still six pieces" 6 (length (hero-equipped h)))
    (check "the first helmet came off" 't-cap
           (equipped-of-kind h :helmet))
    (check "the swap kept the warding" (- base 4)
           (hero-effective-ac h))))

;; Class restrictions: a hero whose class the item excludes.
(define-hero-class :t-wizard :hp-dice "1d4" :damage "1d3" :ac 10)
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (give-item g h 't-mail)
  (check "class-restricted item refused" nil (equip-item g h 't-mail))
  (check-true "cannot-use message"
              (find-if (lambda (s) (search "cannot use" s))
                       (funcall msgs)))
  (check "item-usable-p for the wrong class" nil (item-usable-p h 't-mail))
  (check-true "item-usable-p unrestricted" (item-usable-p h 't-torch))
  ;; the (u) marker: the pack, gear and shop rows show a class
  ;; mismatch before the player tries — terse so the row never wraps
  (check "item-fit-marker for the wrong class" " (u)"
         (item-fit-marker h 't-mail))
  (check "item-fit-marker for a fitting item" ""
         (item-fit-marker h 't-torch))
  (check-true "the pack page marks the unfit item"
              (find-if (lambda (s) (search "T Mail (u)" s))
                       (menu-texts (equip-lines g (make-equip-view h))))))

;; The class registry lists the campaign's classes.
(let ((classes (hero-classes)))
  (check-true "hero-classes lists registered classes"
              (and (member :tester classes)
                   (member :t-wizard classes)))
  (check "hero-classes is sorted"
         (sort (copy-list classes) #'string< :key #'symbol-name)
         classes))

;; TOGGLE-EQUIP: the pack page's one-key toggle — on, off, and the
;; engine's refusals pass through.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (give-item g h 't-sword)
  (give-item g h 't-torch)
  (check-true "toggle equips an unworn item" (toggle-equip g h 't-sword))
  (check "toggled on" 't-sword (equipped-of-kind h :weapon))
  (check-true "toggle removes a worn item" (toggle-equip g h 't-sword))
  (check "toggled off" nil (equipped-of-kind h :weapon))
  (check-true "removal message"
              (find-if (lambda (s) (search "removes T Sword" s))
                       (funcall msgs)))
  (check "toggle refuses misc items" nil (toggle-equip g h 't-torch)))

;; Duplicates: two copies of one name are indistinguishable as items,
;; so the FIRST copy stands for the worn one — the pack page stars
;; exactly that row, its digit is the one that takes the item off, and
;; a copy leaving the pack takes the equipment along only when it was
;; the last one.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (mule (with-rng () (make-hero "Mule" :tester)))
       (g (new-game m :party (list h mule)))
       (view (make-equip-view h)))
  (give-item g h 't-sword)
  (give-item g h 't-sword)
  (equip-item g h 't-sword)
  (check-true "the worn copy's row is the starred instance"
              (equipped-instance-p h 't-sword 0))
  (check "the spare copy's row is not" nil
         (equipped-instance-p h 't-sword 1))
  (check "the pack page stars one copy, not both"
         '("1) T Sword*" "2) T Sword")
         (remove-if-not (lambda (s) (search "T Sword" s))
                        (menu-texts (equip-lines g view))))
  ;; a digit on the spare copy puts THAT one on — the same name, so
  ;; the hands hold what they held — and never silently strips the star
  (equip-act g view #\2)
  (check "picking the spare keeps the item worn" 't-sword
         (equipped-of-kind h :weapon))
  ;; the starred row's digit is the one that takes it off
  (equip-act g view #\1)
  (check "picking the starred copy takes it off" nil
         (equipped-of-kind h :weapon))
  (check "and no row is starred then"
         '("1) T Sword" "2) T Sword")
         (remove-if-not (lambda (s) (search "T Sword" s))
                        (menu-texts (equip-lines g view))))
  ;; a worn name with a spare in the pack gives the SPARE away: the
  ;; hands keep what they hold until the last copy leaves
  (equip-item g h 't-sword)
  (check-true "passing a spare copy leaves the worn one on"
              (pass-item g h mule 't-sword))
  (check "the sword stays in hand" 't-sword (equipped-of-kind h :weapon))
  (check-true "the last copy leaving takes the equipment along"
              (pass-item g h mule 't-sword))
  (check "empty-handed once the last copy went" nil
         (equipped-of-kind h :weapon)))

;; The pack page model (EQUIP-VIEW): both front-ends feed keys into
;; EQUIP-ACT and draw EQUIP-LINES — 'e' on the character sheet.
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game m :party (list h)))
       (view (make-equip-view h)))
  (check "a fresh view opens on the pack" :pack (equip-view-mode view))
  (check-true "empty pack says so"
              (find-if (lambda (s) (search "The pack is empty" s))
                       (menu-texts (equip-lines g view))))
  (give-item g h 't-sword)
  (give-item g h 't-mail)                ; :classes (:tester) — unfit
  (check-true "pack page names the hero"
              (search "*** Wiz's Pack ***"
                      (first (equip-lines g view))))
  (check-true "pack page shows ac and attack"
              (find-if (lambda (s) (search "AC 10   Attack 1d3" s))
                       (menu-texts (equip-lines g view))))
  (check-true "pack page lists the pack numbered"
              (find-if (lambda (s) (search "1) T Sword" s))
                       (menu-texts (equip-lines g view))))
  (check "the item row carries its pick key" #\1
         (menu-line-key
          (find-if (lambda (line)
                     (search "T Sword" (menu-line-text line)))
                   (equip-lines g view))))
  (check-true "pack page marks the unfit item"
              (find-if (lambda (s) (search "2) T Mail (u)" s))
                       (menu-texts (equip-lines g view))))
  ;; the pack page names only its own keys, bracket-free, each row a
  ;; clickable option (digit picks and Esc live on the help screen);
  ;; the carousel's NEXT row closes the page — 'n' turns to the
  ;; spells/songs page or back to the stat block (EQUIP-ACT's :NEXT)
  (check "pack page ends with its own keys, one option per row"
         (list (menu-option #\p "Pass an item")
               (menu-option #\i "Inspect an item")
               (menu-option #\t "Throw away an item")
               ""
               (menu-next-option))
         (last (equip-lines g view) 5))
  (check "n on the pack page turns the carousel" :next
         (equip-act g view #\n))
  (check "a digit equips the item" nil (equip-act g view #\1))
  (check "equipped through the page" 't-sword (equipped-of-kind h :weapon))
  (check-true "the worn item is starred"
              (find-if (lambda (s) (search "1) T Sword*" s))
                       (menu-texts (equip-lines g view))))
  (check-true "the attack line follows the weapon"
              (find-if (lambda (s) (search "Attack 1d6+2" s))
                       (menu-texts (equip-lines g view))))
  (check "the same digit removes it again" nil (equip-act g view #\1))
  (check "removed through the page" nil (equipped-of-kind h :weapon))
  (check "the unfit item stays refused" nil
         (progn (equip-act g view #\2)
                (equipped-of-kind h :armor)))
  (check "a digit past the pack does nothing" nil (equip-act g view #\9))
  (check "escape closes the pack page" :cancelled
         (equip-act g view #\Escape)))

;; The inspect flow ('i' on the pack page): pick an item, read its
;; card — the registered facts plus the campaign's player-facing
;; :description — Esc back one page at a time.
(define-item 't-heirloom :kind :armor :price 20 :ac 4 :classes '(:tester)
  :description "Mail of the old kings; it hums in the dark.")
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (view (make-equip-view h)))
  (check "i with an empty pack stays on the pack" nil
         (equip-act g view #\i))
  (check "the view stays on the pack" :pack (equip-view-mode view))
  (check-true "and says why"
              (find-if (lambda (s) (search "nothing to inspect" s))
                       (funcall msgs)))
  (give-item g h 't-sword)
  (give-item g h 't-heirloom)
  (check "i opens the inspect page" nil (equip-act g view #\i))
  (check "the view is on the inspect page" :inspect (equip-view-mode view))
  (check-true "the inspect page keeps the pack header"
              (search "*** Wiz's Pack ***" (first (equip-lines g view))))
  (check-true "the inspect page prompts its pick"
              (member "Inspect what?" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check "a digit shows the item's card" nil (equip-act g view #\1))
  (check "the card item is remembered" 't-sword (equip-view-pending view))
  (check-true "the card is titled with the item"
              (search "*** T Sword ***" (first (equip-lines g view))))
  (check-true "the card names the kind"
              (member "Kind: Weapon" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check-true "the card shows the damage"
              (member "Damage: 1d6+2" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check-true "the card shows the price"
              (member "Price: 10 gold" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check "a plain item's card shows no description"
         nil (find-if (lambda (s) (search "hums in the dark" s))
                      (menu-texts (equip-lines g view))))
  (check "esc leaves the card" nil (equip-act g view #\Escape))
  (check "back on the inspect page" :inspect (equip-view-mode view))
  (check "the card item is forgotten" nil (equip-view-pending view))
  (check "the next digit shows the next card" nil (equip-act g view #\2))
  (check-true "the described item's card carries the description"
              (member "Mail of the old kings; it hums in the dark."
                      (menu-texts (equip-lines g view)) :test #'equal))
  (check-true "the card shows the AC bonus"
              (member "AC bonus: 4" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check-true "the class restriction carries the unfit marker"
              (member "Classes: Tester (unfit)"
                      (menu-texts (equip-lines g view)) :test #'equal))
  (check "the card ends with the description, no key footer"
         "Mail of the old kings; it hums in the dark."
         (first (last (equip-lines g view))))
  (equip-act g view #\Escape)
  (check "esc then leaves the inspect page" nil (equip-act g view #\Escape))
  (check "back on the pack page" :pack (equip-view-mode view))
  (check "esc then closes the page" :cancelled (equip-act g view #\Escape)))

;; A full pack turns the pack page whole: the sheet windows its one
;; document — title, header, items, letter keys and NEXT — over
;; +TAKEOVER-ROWS+ rows, so the first window is the sheet's head and
;; the second is its foot, the item numbers never move, and a digit
;; picks its printed number even while the row is off the window.
(dolist (name '(teq-1 teq-2 teq-3 teq-4 teq-5 teq-6 teq-7))
  (define-item name :price 1))
(let* ((h (%combat-hero))
       (g (new-game (parse-map *art* :name "test") :party (list h)))
       (view (make-equip-view h)))
  (give-item g h 't-sword)
  (dolist (name '(teq-1 teq-2 teq-3 teq-4 teq-5 teq-6 teq-7))
    (give-item g h name))
  ;; the document: title, AC row, a blank, 8 items, then the footer
  ;; (blank, three letter keys, blank, NEXT) — 17 rows windowed at 11
  (check "full pack: the page geometry reaches the scrollbar"
         '(0 11 17) (progn (equip-lines g view) *menu-scroll*))
  (check-true "the first window opens on the title"
              (search "'s Pack ***" (first (equip-lines g view))))
  (check-true "and still ends with the whole pack"
              (find-if (lambda (s) (search "8) Teq 7" s))
                       (menu-texts (equip-lines g view))))
  (check "the letter keys wait below the fold" nil
         (member (menu-option #\i "Inspect an item")
                 (equip-lines g view) :test #'equal))
  (check "d turns the page" nil (equip-act g view #\d))
  (check "the view holds the clamped offset" 6 (equip-view-top view))
  (check "the turned page ends on NEXT" (menu-next-option)
         (first (last (equip-lines g view))))
  (check-true "the turned page carries the letter keys"
              (member (menu-option #\i "Inspect an item")
                      (equip-lines g view) :test #'equal))
  (check-true "the scrolled rows keep their absolute numbers"
              (find-if (lambda (s) (search "4) Teq 3" s))
                       (menu-texts (equip-lines g view))))
  (check "the title scrolled away with its window" nil
         (find-if (lambda (s) (search "'s Pack ***" s))
                  (menu-texts (equip-lines g view))))
  ;; the digits pick by the printed number, not the visible row
  (check "an off-window digit still picks its item" nil
         (equip-act g view #\1))
  (check "the sword went on by its absolute number" 't-sword
         (equipped-of-kind h :weapon))
  (check "u turns back" nil (equip-act g view #\u))
  (check "the view is back at the top" 0 (equip-view-top view))
  ;; every page change opens at its top: scroll down, open a picker
  (equip-act g view #\d)
  (check "t on the scrolled page opens the toss page" nil
         (equip-act g view #\t))
  (check "the picker opens at its top" 0 (equip-view-top view))
  (check "the picker fits its one window" nil
         (progn (equip-lines g view) *menu-scroll*)))

;; PASS-ITEM: handing an item to another party member.  The item is
;; unequipped on the way and is never destroyed — a full receiving pack
;; leaves it whole with the giver.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))
       (b (%combat-hero "Bo"))
       (g (new-game m :party (list a b)))
       (msgs (watch-messages g))
       (passed '()))
  (on-event g :item-passed
            (lambda (game from to name) (declare (ignore game))
              (push (list (hero-name from) (hero-name to) name) passed)))
  (check "pass-item needs the item in the giver's pack" nil
         (pass-item g a b 't-sword))
  (check-true "the refusal says who does not carry it"
              (find-if (lambda (s) (search "Ava does not carry T Sword" s))
                       (funcall msgs)))
  (give-item g a 't-sword)
  (check "a self-pass is refused" nil (pass-item g a a 't-sword))
  (check "a self-pass does not duplicate the item" 1 (length (hero-items a)))
  (check-true "the self-pass says so"
              (find-if (lambda (s) (search "Ava already carries T Sword" s))
                       (funcall msgs)))
  (check-error "pass-item checks the item exists" (pass-item g a b 't-nada))
  ;; the ordinary hand-over: out of one pack, into the other's end
  (equip-item g a 't-sword)
  (give-item g a 't-torch)
  (check-true "pass-item hands the item over" (pass-item g a b 't-sword))
  (check "the giver lost it" '(t-torch) (hero-items a))
  (check "the receiver gained it" '(t-sword) (hero-items b))
  (check "the item came off the giver's hands" nil (hero-equipped a))
  (check "and arrives unequipped" nil (hero-equipped b))
  (check-true "the hand-over is announced"
              (find-if (lambda (s) (search "Ava hands T Sword to Bo" s))
                       (funcall msgs)))
  (check ":item-passed carries giver, receiver and item"
         '(("Ava" "Bo" t-sword)) passed)
  ;; a full receiving pack refuses, and the item stays whole with the giver
  (dotimes (i 7) (give-item g b 't-torch))
  (check "the receiving pack is full" 8 (length (hero-items b)))
  (check "pass-item refuses a full pack" nil (pass-item g a b 't-torch))
  (check "the refused item stays with the giver" '(t-torch) (hero-items a))
  (check "and is not duplicated into the full pack" 8 (length (hero-items b)))
  (check-true "the full-pack refusal says so"
              (find-if (lambda (s) (search "Bo's pack is full" s))
                       (funcall msgs)))
  (check "the refusal emits nothing" 1 (length passed)))

;; Passing crosses the barriers CARRYING does not care about: a class
;; that cannot USE an item may still haul it, and the fallen both give
;; and receive (POOL-GOLD's rule).
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))
       (b (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game m :party (list a b))))
  (give-item g a 't-mail)                ; :classes (:tester) — unfit for Wiz
  (check "an unfit item passes anyway" t (pass-item g a b 't-mail))
  (check "the wizard hauls it" '(t-mail) (hero-items b))
  (check "but still cannot wear it" nil (equip-item g b 't-mail))
  (setf (hero-hp b) 0)
  (check "a fallen hero still gives" t (pass-item g b a 't-mail))
  (check "and still receives" t (pass-item g a b 't-mail)))

;; The give flow through the view: [p] opens it, a digit picks the item,
;; the party page picks who receives it, and Esc steps back one page at
;; a time (SHOP-ACT's pattern).
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))
       (b (%combat-hero "Bo"))
       (g (new-game m :party (list a b)))
       (view (make-equip-view a)))
  (give-item g a 't-sword)
  (give-item g a 't-torch)
  (check "p opens the give page" nil (equip-act g view #\p))
  (check "the view is on the give page" :give (equip-view-mode view))
  (check-true "the give page keeps the pack header"
              (search "*** Ava's Pack ***" (first (equip-lines g view))))
  (check-true "the give page prompts at giving"
              (member "Give what?" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check "a digit picks the item to hand over" nil (equip-act g view #\1))
  (check "the view moves to the recipient page" :to (equip-view-mode view))
  (check "the pending item is remembered" 't-sword (equip-view-pending view))
  (check-true "the recipient page asks whom"
              (find-if (lambda (s) (search "Give T Sword to whom?" s))
                       (menu-texts (equip-lines g view))))
  (check-true "the recipient rows show the room in each pack"
              (find-if (lambda (s) (search "2) Bo  (pack 0/8)" s))
                       (menu-texts (equip-lines g view))))
  (check-true "the giver's own row is marked"
              (find-if (lambda (s) (search "1) Ava  (pack 2/8) (giver)" s))
                       (menu-texts (equip-lines g view))))
  (check "the recipient row carries its pick key" #\2
         (menu-line-key
          (find-if (lambda (line) (search "Bo" (menu-line-text line)))
                   (equip-lines g view))))
  (check "a digit hands the item over" nil (equip-act g view #\2))
  (check "the item moved" '(t-sword) (hero-items b))
  (check "the page returns to giving for the next item" :give
         (equip-view-mode view))
  (check "the pending item is cleared" nil (equip-view-pending view))
  (check "the window starts over on the shrunken pack" 0
         (equip-view-top view))
  ;; Esc walks back out one page at a time, never straight to the sheet
  (check "esc leaves the give page for the pack" nil
         (equip-act g view #\Escape))
  (check "back on the pack page" :pack (equip-view-mode view))
  (check "esc then closes the page" :cancelled (equip-act g view #\Escape))
  ;; and from the recipient page Esc returns to the item list
  (equip-act g view #\p)
  (equip-act g view #\1)
  (check "esc leaves the recipient page" nil (equip-act g view #\Escape))
  (check "back on the give page" :give (equip-view-mode view))
  (check "the pending item is forgotten" nil (equip-view-pending view)))

;; [p] on an empty pack says so instead of opening a give page with
;; nothing on it.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (view (make-equip-view h)))
  (check "p on an empty pack does not open the give page" nil
         (equip-act g view #\p))
  (check "the view stays on the pack" :pack (equip-view-mode view))
  (check-true "and says why"
              (find-if (lambda (s) (search "Alva has nothing to give" s))
                       (funcall msgs))))

;; The throw-away flow ('t' on the pack page): a digit picks the
;; item, y then destroys it (DISCARD-ITEM), n keeps it — the one pack
;; action that destroys, hence the only one with an are-you-sure.
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero "Ava"))
       (g (new-game m :party (list a)))
       (msgs (watch-messages g))
       (view (make-equip-view a)))
  (give-item g a 't-sword)
  (give-item g a 't-torch)
  (equip-item g a 't-sword)
  (check "t opens the throw-away page" nil (equip-act g view #\t))
  (check "the view is on the toss page" :toss (equip-view-mode view))
  (check-true "the toss page prompts at tossing"
              (member "Throw away what?" (menu-texts (equip-lines g view))
                      :test #'equal))
  (check "a digit picks the item" nil (equip-act g view #\1))
  (check "the pick waits behind the are-you-sure" 't-sword
         (equip-view-pending view))
  (check-true "the page asks twice"
              (member "Throw away T Sword?"
                      (menu-texts (equip-lines g view)) :test #'equal))
  (check-true "the yes/no rows are clickable options"
              (and (member (menu-option #\y "Yes, be rid of it")
                           (equip-lines g view) :test #'equal)
                   (member (menu-option #\n "No, keep it")
                           (equip-lines g view) :test #'equal)))
  (check "n keeps the item" nil (equip-act g view #\n))
  (check "the pack is untouched" '(t-sword t-torch) (hero-items a))
  (check "the pick is forgotten" nil (equip-view-pending view))
  (equip-act g view #\1)
  (check "y throws the item away" nil (equip-act g view #\y))
  (check "the item is gone for good" '(t-torch) (hero-items a))
  (check "it came off the hands on the way out" nil
         (equipped-of-kind a :weapon))
  (check-true "the toss is announced"
              (find-if (lambda (s) (search "Ava throws T Sword away" s))
                       (funcall msgs)))
  (check "the page stays open for the next item" :toss
         (equip-view-mode view))
  ;; Esc walks back out one page at a time, never straight to the sheet
  (check "esc leaves the toss page for the pack" nil
         (equip-act g view #\Escape))
  (check "back on the pack page" :pack (equip-view-mode view))
  (check "esc then closes the page" :cancelled
         (equip-act g view #\Escape)))

;; 't' on an empty pack says so, and DISCARD-ITEM refuses what the
;; hero does not carry (the item stays a game situation, not a crash).
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (view (make-equip-view h)))
  (check "t on an empty pack does not open the toss page" nil
         (equip-act g view #\t))
  (check "the view stays on the pack" :pack (equip-view-mode view))
  (check-true "and says why"
              (find-if (lambda (s)
                         (search "Alva has nothing to throw away" s))
                       (funcall msgs)))
  (check "discard-item refuses what is not carried" nil
         (discard-item g h 't-sword))
  (check-true "and says so"
              (find-if (lambda (s) (search "does not carry" s))
                       (funcall msgs))))

;; Usable items: DEFINE-ITEM :use validation.
(define-item 't-potion   :price 10 :use '(:heal "1d4+1") :consumed t)
(define-item 't-lantern  :price 12 :use '(:light t :duration 15))
(define-item 't-fx-torch :price 2  :use '(:light t :duration 30)
             :consumed t :image "fx-torch.iff")
(define-item 't-elixir   :price 5  :use '(:heal "1d4") :consumed t
             :classes '(:tester))
(check-error "define-item rejects a malformed :use"
  (define-item 't-bogus :use '(:frobnicate t)))
(check-error "a timed :use needs a duration"
  (define-item 't-bogus :use '(:light t)))
(check-error ":consumed without a :use is rejected"
  (define-item 't-bogus :consumed t))

;; USE-ITEM mechanics: refusals say why; a timed :use installs the
;; effect (title + image), a heal heals, :consumed spends the item.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (used '()))
  (on-event g :item-used
            (lambda (game hero name) (declare (ignore game hero))
              (push name used)))
  (check "use-item needs the item in the pack" nil (use-item g h 't-potion))
  (check-true "does-not-carry message"
              (find-if (lambda (s) (search "does not carry" s))
                       (funcall msgs)))
  (give-item g h 't-torch)
  (check "an item without a :use does nothing" nil (use-item g h 't-torch))
  (check-true "nothing-happens message"
              (find-if (lambda (s) (search "Nothing happens" s))
                       (funcall msgs)))
  (check "refusals emit nothing" '() used)
  (give-item g h 't-fx-torch)
  (check "usable-items sees only the :use item" '(t-fx-torch)
         (usable-items h))
  (check-true "a light item lights the party" (use-item g h 't-fx-torch))
  (check-true "the torch effect burns" (light-active-p g))
  (check "the effect keeps the item's title" "t fx torch"
         (effect-label (find-effect g "T Fx Torch")))
  (check "the effect carries the item's image" "fx-torch.iff"
         (effect-image (find-effect g "T Fx Torch")))
  (check "the effect is timed" (+ (game-time g) 30)
         (effect-expires-at (find-effect g "T Fx Torch")))
  (check "the consumed torch left the pack" nil
         (hero-carrying-p h 't-fx-torch))
  (check ":item-used emitted" '(t-fx-torch) used)
  ;; a potion heals its user by default
  (give-item g h 't-potion)
  (damage-hero g h 5)
  (let ((before (hero-hp h)))
    (check-true "a potion heals"
                (with-rng (2) (use-item g h 't-potion)))  ; 1d4+1 -> 4
    (check "the heal landed on the user" (+ before 4) (hero-hp h)))
  (check "the potion is spent" nil (hero-carrying-p h 't-potion))
  ;; a reusable item stays
  (give-item g h 't-lantern)
  (check-true "the lantern lights" (use-item g h 't-lantern))
  (check-true "a reusable item stays in the pack"
              (hero-carrying-p h 't-lantern)))

;; Class-gated use: carrying is not using.
(let* ((m (parse-map *art* :name "test"))
       (wiz (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game m :party (list wiz)))
       (msgs (watch-messages g)))
  (give-item g wiz 't-elixir)
  (check "usable-items respects the class gate" '() (usable-items wiz))
  (check "use-item refuses the wrong class" nil (use-item g wiz 't-elixir))
  (check-true "cannot-use message on use"
              (find-if (lambda (s) (search "cannot use" s))
                       (funcall msgs))))

;; The use menu: pick the user, the item, and — for a heal — the
;; target (the CAST-VIEW pattern).
(let* ((m (parse-map *art* :name "test"))
       (a (%combat-hero))
       (b (%combat-mage))
       (g (new-game m :party (list a b))))
  (give-item g a 't-fx-torch)
  (give-item g a 't-potion)
  (let ((v (make-use-view)))
    (check "Esc at the top cancels" :cancelled (use-act g v #\Escape)))
  (let ((v (make-use-view)))
    (check-true "the menu opens on the user pick"
                (member "Who uses?" (use-lines g v) :test #'equal))
    (use-act g v #\2)
    (check "a hero with nothing usable is not picked" nil
           (use-view-hero v))
    (use-act g v #\1)
    (check "hero 1 picked" a (use-view-hero v))
    (check-true "the item page lists the torch"
                (find-if (lambda (s) (search "T Fx Torch" s))
                         (menu-texts (use-lines g v))))
    (use-act g v #\Escape)
    (check "Esc backs out to the user pick" nil (use-view-hero v))
    (use-act g v #\1)
    ;; a timed item commits at once
    (check "using the torch resolves the menu" :done (use-act g v #\1))
    (check-true "the torch burns" (light-active-p g)))
  ;; a healing item asks for its target
  (let ((v (make-use-view)))
    (use-act g v #\1)
    (check "the potion wants a target first" nil (use-act g v #\1))
    (check-true "the target page asks on whom"
                (find-if (lambda (s) (search "on whom?" s))
                         (menu-texts (use-lines g v))))
    (damage-hero g b 4)
    (let ((before (hero-hp b)))
      (check "picking the target commits" :done
             (with-rng (0) (use-act g v #\2)))  ; 1d4+1 -> 2
      (check "the heal landed on hero 2" (+ before 2) (hero-hp b))))
  ;; a mending (:cast SPELL) trigger asks for its target the same way
  (let ((v (make-use-view)))
    (give-item g a 'test-scroll)        ; the spent torch and potion
    (use-act g v #\1)                   ; left the pack; the scroll is 1
    (check "the scroll wants a target first" nil (use-act g v #\1))
    (damage-hero g b 4)
    (let ((before (hero-hp b)))
      (check "picking the scroll's target commits" :done
             (with-rng (2) (use-act g v #\2)))  ; 1d8 -> 3
      (check "the spell's mending landed on hero 2" (+ before 3)
             (hero-hp b)))))

;; Combat uses the equipment: weapon dice on the attack, effective AC
;; against the monsters.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (give-item g h 't-sword)
  (equip-item g h 't-sword)
  (start-combat g '(("test rat" 1)))
  ;; hero d20=11 hits ac 10; weapon 1d6+2 rolls 2 -> 5 damage, str 10
  ;; adds nothing: the 3 hp rat dies in one blow.
  (check "weapon dice carry the round" :victory
         (with-rng (10 1) (combat-round g))))
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (give-item g h 't-mail)
  (equip-item g h 't-mail)
  (give-item g h 't-buckler)
  (equip-item g h 't-buckler)
  (start-combat g '(("test rat" 1)))
  ;; effective ac 3: the rat needs d20 >= 16 (1 + roll + level 1).
  ;; hero misses (d20=1); rat d20=15 -> 17 < 20-3: a miss.
  (check "armor turns the blow" :ongoing
         (with-rng (0 0 14) (combat-round g)))
  (check "no damage through the armor" 8 (hero-hp h))
  ;; same roll against the bare hero would have hit (17 >= 12): take
  ;; the armor off and let it land.
  (unequip-item g h 't-mail)
  (unequip-item g h 't-buckler)
  (check "bare round" :ongoing (with-rng (0 0 14 0) (combat-round g)))
  (check "the same swing hits without armor" 7 (hero-hp h)))

;;; ---------------------------------------------------------------------
;;; Map files with a story layer

(with-open-file (s "tests/tmp.map" :direction :output :if-exists :supersede)
  (write-string "+-+-+
|@  |
+-+-+

;; the story layer
(special (1 0) (message \"hello\") (set-flag :was-here))
" s))
(let ((m (load-map-file "tests/tmp.map")))
  (check "map file art still parses" 2 (dungeon-map-width m))
  (check "special read from map file"
         '((message "hello") (set-flag :was-here))
         (cell-special m 1 0))
  (check "cells without specials" nil (cell-special m 0 0)))

(with-open-file (s "tests/tmp.map" :direction :output :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(special (5 5) (message \"nope\"))
" s))
(check-error "out-of-range special coordinates rejected"
  (load-map-file "tests/tmp.map"))

(with-open-file (s "tests/tmp.map" :direction :output :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(frobnicate 1)
" s))
(check-error "unknown map form rejected" (load-map-file "tests/tmp.map"))
(delete-file "tests/tmp.map")

;; The committed fixture world carries its story layer: the keep is a
;; city whose shoppe and stairs are data, the crypt a dark dungeon
;; whose ladder travels back up — after asking (the ASK op, see its
;; own section): the two stairs between them keep both manners alive,
;; the keep's carrying the party down on the step that finds them,
;; the crypt's waiting for a yes.
(let ((m (load-map-file "tests/world/keep.map")))
  (check "keep is a city zone" :city (dungeon-map-kind m))
  (check "keep title" "Testhold" (map-title m))
  (check-true "keep shoppe location"
              (find-if (lambda (op) (string-equal (first op) "LOCATION"))
                       (cell-special m 1 0)))
  (check-true "keep tavern location"
              (find-if (lambda (op) (string-equal (first op) "LOCATION"))
                       (cell-special m 3 0)))
  (check-true "keep stairs lead to the crypt"
              (find-if (lambda (op) (string-equal (first op) "TRAVEL"))
                       (cell-special m 4 0))))
(let ((m (load-map-file "tests/world/crypt.map")))
  (check "crypt is a dungeon zone" :dungeon (dungeon-map-kind m))
  (check-true "crypt is dark" (dungeon-map-dark m))
  (check "crypt ladder asks before it is climbed" "ASK"
         (symbol-name (first (first (cell-special m 2 0)))))
  (check-true "crypt ladder leads back to the keep on a yes"
              (find-if (lambda (op) (and (consp op)
                                         (string-equal (first op) "TRAVEL")))
                       (rest (first (cell-special m 2 0))))))

;; Zone tile packs: (zone :gfx DIR) names the zone's pack, and
;; ZONE-GFX-DIR resolves it in two steps — relative to the map file's
;; directory when the pack lives there (self-contained world), else
;; relative to the game directory (a shipped pack).
(check-error "zone :gfx must be a directory string"
  (%apply-map-form (parse-map *art*) '(zone :gfx 42) "x.map"))
(let* ((m (parse-map *art* :name "data/x.map"))
       (g (new-game m)))
  (%apply-map-form m '(zone :gfx "gfx/") "data/x.map")
  (check "zone :gfx parses" "gfx/" (dungeon-map-gfx m))
  (check "zone pack resolves map-relative when it lives there"
         "data/gfx/" (zone-gfx-dir g)))
(let* ((m (parse-map *art* :name "worlds/w/x.map"))
       (g (new-game m)))
  (%apply-map-form m '(zone :gfx "city-pack/") "x.map")
  (check "zone pack falls back to the game directory"
         "city-pack/" (zone-gfx-dir g)))
(check "a zone without :gfx has no pack" nil
       (zone-gfx-dir (new-game (parse-map *art*))))

;;; The tile-pack cache (*GFX-CACHE-PACKS*): swapping packs on zone
;;; travel parks the old pack instead of freeing it, so walking back is
;;; free.  The policy is pure list work — WALLS stands in for the piece
;;; bitmaps and FREED records what the front end would have released.
(let ((freed '()))
  (flet ((free-fn (walls) (push walls freed) nil)
         (reset () (setf freed '())))
    ;; put/take: a round trip returns the very same pack and palette
    (let ((cache (%pack-cache-put '() "town/" :town-walls :town-pal)))
      (multiple-value-bind (walls pal rest) (%pack-cache-take cache "town/")
        (check "cached pack comes back" :town-walls walls)
        (check "cached palette comes back" :town-pal pal)
        (check "taking it empties the cache" '() rest))
      (multiple-value-bind (walls pal rest) (%pack-cache-take cache "cellar/")
        (check "a miss yields no pack" nil walls)
        (check "a miss yields no palette" nil pal)
        (check "a miss leaves the cache alone" cache rest)))
    ;; a failed load (wireframe fallback) is not worth caching
    (check "an unloaded pack is not cached" '()
           (%pack-cache-put '() "broken/" nil nil))
    ;; re-putting a directory replaces its entry — two would leak the
    ;; older entry's bitmaps, which nothing would ever free
    (let ((cache (%pack-cache-put (%pack-cache-put '() "town/" :old :pal)
                                  "town/" :new :pal)))
      (check "re-putting a pack does not double it" 1 (length cache))
      (check "re-putting a pack keeps the newest" :new
             (second (first cache))))
    ;; the budget: N inactive packs, least-recently-used evicted
    (let ((*gfx-cache-packs* 1))
      (reset)
      (let* ((cache (%pack-cache-put '() "old/" :old-walls nil))
             (cache (%pack-cache-put cache "new/" :new-walls nil))
             (kept (%pack-cache-trim cache #'free-fn)))
        (check "the budget keeps the most recent" 1 (length kept))
        (check "... and it is the newest" "new/" (first (first kept)))
        (check "the evicted pack is freed" '(:old-walls) freed)))
    (let ((*gfx-cache-packs* 0))
      (reset)
      (let ((kept (%pack-cache-trim
                   (%pack-cache-put '() "town/" :town-walls nil)
                   #'free-fn)))
        (check "a 0 budget caches nothing" '() kept)
        (check "... and frees what it drops" '(:town-walls) freed)))
    ;; :auto keeps one pack, but not when the machine is tight
    (let ((*gfx-cache-packs* :auto)
          (*gfx-cache-min-free* 1000000))
      (check ":auto keeps one pack" 1 (%pack-cache-limit))
      (reset)
      (check ":auto caches when memory is free" 1
             (length (%pack-cache-trim
                      (%pack-cache-put '() "town/" :town-walls nil)
                      #'free-fn 4000000)))
      (check "... freeing nothing" '() freed)
      (reset)
      (check ":auto drops the cache when memory is tight" '()
             (%pack-cache-trim
              (%pack-cache-put '() "town/" :town-walls nil)
              #'free-fn 500000))
      (check "... and frees the dropped pack" '(:town-walls) freed)
      (reset)
      (check ":auto caches when free memory is unknown" 1
             (length (%pack-cache-trim
                      (%pack-cache-put '() "town/" :town-walls nil)
                      #'free-fn nil))))
    ;; a nonsense setting is a clear error, not a silent no-cache
    (let ((*gfx-cache-packs* :sometimes))
      (check-error "*gfx-cache-packs* rejects a bad value"
        (%pack-cache-limit)))
    (let ((*gfx-cache-packs* -1))
      (check-error "*gfx-cache-packs* rejects a negative budget"
        (%pack-cache-limit)))
    ;; freeing the whole cache at session end releases every pack
    (reset)
    (check "dropping the cache frees everything" nil
           (%pack-cache-drop (%pack-cache-put
                              (%pack-cache-put '() "a/" :a nil) "b/" :b nil)
                             #'free-fn))
    ;; freed in cache order, most recently used first
    (check "... both packs" '(:b :a) (reverse freed))))

;; A world is a directory: the campaign.lisp NEXT TO the map file is
;; the one that loads — a designer's own world brings its own classes,
;; monsters and items, never the demo's.
(with-open-file (s "tests/campaign.lisp" :direction :output
                   :if-exists :supersede)
  (write-string "(in-package :tale)
(define-item 't-camp-ration :price 7)
" s))
(check "load-campaign finds the campaign next to the map"
       "tests/campaign.lisp" (load-campaign "tests/anything.map"))
(check "its definitions are live" 7 (item-price 't-camp-ration))
(delete-file "tests/campaign.lisp")
(check "no campaign next to the map" nil
       (load-campaign "data/gfx/anything.map"))

;; Walk the fixture world end-to-end: shoppe -> stairs -> crypt and
;; back up — a committed world's whole loop on real files.  The
;; shoppe's stock names campaign items, so the campaign loads first
;; (exactly what PLAY/PLAY-AMIGA do).  The Closure game's own suite
;; walks its shipped world the same way.
(load-campaign "tests/world/keep.map")
(let* ((m (load-map-file "tests/world/keep.map"))
       (g (new-game m :party (default-party))))
  (trigger-special g)
  (check "the keep has no zone pack (profile default)" nil
         (zone-gfx-dir g))
  ;; through the shoppe door to the east
  (turn-right g)
  (check "shoppe door opens" :door (move-party g))
  (check "the shoppe is a shop location" :shop
         (location-kind (game-location g)))
  (leave-location g)
  ;; the Bard's Tale exit put the party back before the shoppe door,
  ;; about-faced — hop over the shoppe cell and on to the tavern
  (check "leaving the shoppe steps back onto the street" '(0 0)
         (list (game-x g) (game-y g)))
  (teleport-party g 2 0)
  (turn-around g)                         ; face east again
  (move-party g)                          ; (3,0) — the tavern
  (check "the tavern is a tavern location" :tavern
         (location-kind (game-location g)))
  (check "the keep's drinks cost two gold" 2
         (tavern-price (game-location g)))
  (check "Esc leaves the tavern" :left
         (location-act g nil #\Escape))
  (check "the tavern exit lands back on the street" '(2 0)
         (list (game-x g) (game-y g)))
  ;; on to the stairs
  (teleport-party g 4 0)
  (check "stairs travel landed in the crypt" "the crypt"
         (map-title (game-map g)))
  (check-true "the crypt is dark" (game-dark-p g))
  (check "crypt arrival at its start" '(0 0)
         (list (game-x g) (game-y g)))
  ;; the ladder back up: teleport to the crypt's < cell — which asks
  ;; first (the ASK op), so the party is still in the crypt until it
  ;; says yes
  (teleport-party g 2 0)
  (check "the ladder asks before it is climbed" "the crypt"
         (map-title (game-map g)))
  (check-true "the question stands" (game-question g))
  (check "yes climbs it" :yes (question-act g #\y))
  (check "ladder returns to the keep" "Testhold"
         (map-title (game-map g)))
  (check "ladder lands between shoppe and tavern" '(2 0)
         (list (game-x g) (game-y g))))

;;; ---------------------------------------------------------------------
;;; Save games

(with-open-file (s "tests/tmp.map" :direction :output :if-exists :supersede)
  (write-string "+-+-+-+
|@  | |
+ +D+ +
| |  <|
+-+-+-+

(special (1 0) (message \"dusty\"))
" s))

(define-hero-class :t-caster :hp-dice "1d4" :damage "1d3" :ac 10 :caster t)
(let* ((m (load-map-file "tests/tmp.map"))
       (a (with-rng (5) (make-hero "Alva" :tester)))
       (b (with-rng (5) (make-hero "Berk" :tester)))
       (c (with-rng (5) (make-hero "Cael" :t-caster)))
       (g (new-game m :party (list a b c))))
  (turn-right g)
  (move-party g :forward)               ; to (1,0)
  (set-flag g :quest 42)
  (set-flag g '(:seen "door") t)
  (damage-hero g a 3)
  (setf (hero-xp b) 60)
  (incf (hero-gold b) 17)
  (setf (hero-tunes b) 3)
  (decf (hero-sp c))
  ;; an art left behind rides along too: without it a reloaded
  ;; class-changer would forget every spell the old art had opened
  (setf (hero-class-levels b) '((:t-first . 4) (:t-second . 3)))
  ;; conditions ride along in the save: they are cured or carried, and a
  ;; party that saves poisoned loads poisoned
  (afflict-hero g a :poison)
  (afflict-hero g a :paralysis)
  (setf (game-time g) 700)
  (add-effect g "mage flame" :duration 60 :payload '(:light t)
                             :image "fx-flame.iff")
  (add-effect g "blessing" :payload '(:ac 1))
  (save-game g "tests/tmp-save.lisp")
  (let ((g2 (load-game "tests/tmp-save.lisp")))
    (check "loaded position" '(1 0) (list (game-x g2) (game-y g2)))
    (check "loaded facing" +east+ (game-facing g2))
    (check "loaded clock" 700 (game-time g2))
    (check "loaded effects in order" '("mage flame" "blessing")
           (mapcar #'effect-name (game-effects g2)))
    (check "loaded effect keeps its expiry" 760
           (effect-expires-at (find-effect g2 "mage flame")))
    (check "loaded effect keeps its payload" '(:light t)
           (effect-payload (find-effect g2 "mage flame")))
    (check "loaded effect keeps its image" "fx-flame.iff"
           (effect-image (find-effect g2 "mage flame")))
    (check "loaded undated effect stays undated" nil
           (effect-expires-at (find-effect g2 "blessing")))
    (check "loaded imageless effect stays imageless" nil
           (effect-image (find-effect g2 "blessing")))
    (check "loaded :ac payload feeds the party bonus" 1
           (effects-ac-bonus g2))
    (check "loaded flag value" 42 (flag g2 :quest))
    (check "loaded equal-key flag" t (flag g2 '(:seen "door")))
    (check "loaded party size" 3 (length (game-party g2)))
    (let ((a2 (first (game-party g2)))
          (b2 (second (game-party g2)))
          (c2 (third (game-party g2))))
      (check "loaded hero name" "Alva" (hero-name a2))
      (check "loaded hero class" :tester (hero-class a2))
      (check "loaded hero damage taken" 5 (hero-hp a2))
      (check "loaded hero max hp" 8 (hero-max-hp a2))
      (check "loaded hero xp" 60 (hero-xp b2))
      (check "loaded hero gold" 17 (hero-gold b2))
      (check "loaded hero tunes" 3 (hero-tunes b2))
      (check "loaded hero ailments, in vocabulary order"
             '(:poison :paralysis) (hero-ailments a2))
      (check "a hale hero loads hale" nil (hero-ailments b2))
      (check "loaded hero's arts, frozen where they were left"
             '((:t-first . 4) (:t-second . 3)) (hero-class-levels b2))
      (check "and they still answer for their spells"
             4 (hero-class-level b2 :t-first))
      (check "a hero who never changed class loads with none"
             '() (hero-class-levels a2))
      (check-true "and the loaded condition still speaks for itself"
                  (and (hero-helpless-p a2)
                       (equal "poisoned, paralysed"
                              (hero-condition-titles a2))))
      (check-true "loaded caster hero is still a caster" (hero-caster-p c2))
      (check "loaded caster max-sp" (hero-max-sp c) (hero-max-sp c2))
      (check "loaded caster sp" (hero-sp c) (hero-sp c2))
      ;; the sheet a front-end draws for the loaded hero agrees with
      ;; the roster: one AC, HERO-EFFECTIVE-AC (the loaded blessing's
      ;; :ac 1 included) — the sheet falling back to the bare base
      ;; slot beside the roster's equipped value was a live bug
      (check "loaded hero's sheet shows the roster's ac"
             (format nil "HP ~D/~D  AC ~D" (hero-hp a2) (hero-max-hp a2)
                     (hero-effective-ac a2 g2))
             (third (hero-summary-lines a2 g2))))
    (check-true "loaded knowledge: visited cell explored"
                (cell-explored-p (game-knowledge g2) 1 0))
    (check "loaded knowledge: unseen cell unexplored" nil
           (cell-explored-p (game-knowledge g2) 1 1))
    (check-true "loaded knowledge: seen wall known"
                (wall-known-p (game-knowledge g2) 1 0 :north))
    (check "loaded map keeps its story layer"
           '((message "dusty")) (cell-special (game-map g2) 1 0))))

;; Saving mid-combat is refused; junk files are rejected on load.
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (list (%combat-hero)))))
  (start-combat g '(("test rat" 1)))
  (check-error "no saving during combat" (save-game g "tests/tmp-save.lisp")))
(check-error "load-game rejects non-save files"
  (load-game "tests/tmp.map"))
(delete-file "tests/tmp.map")
(delete-file "tests/tmp-save.lisp")

;;; ---------------------------------------------------------------------
;;; The world: zones and travel (M4).  Cities and dungeons are both
;;; first-class: ordinary maps that self-describe through their ZONE
;;; form, linked by TRAVEL specials.

;; Relative map paths resolve against the current map's directory.
(check "resolve sibling path" "worlds/w/town.map"
       (%resolve-map-path "worlds/w/cellar.map" "town.map"))
(check "resolve flat path" "town.map"
       (%resolve-map-path "cellar.map" "town.map"))
(check "resolve amiga volume base" "dh0:games/b.map"
       (%resolve-map-path "dh0:games/a.map" "b.map"))
(check "resolve volume-only base" "dh0:b.map"
       (%resolve-map-path "dh0:a.map" "b.map"))
(check "absolute posix target stays" "/maps/b.map"
       (%resolve-map-path "data/a.map" "/maps/b.map"))
(check "absolute amiga target stays" "vol:b.map"
       (%resolve-map-path "data/a.map" "vol:b.map"))

;; The two test zones: a city with a shop and a stairs-down cell, and a
;; dungeon whose (1,0) leads back up.
(with-open-file (s "tests/tmp-town.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+-+
|@D  <|
+-+-+-+

(zone :kind :city :title \"Testville\")
(special (1 0)
  (location \"The Test Shoppe\" :shop :stock (t-sword t-mail t-torch)))
(special (2 0) (message \"Down you go.\") (travel \"tmp-dung.map\"))
" s))
(with-open-file (s "tests/tmp-dung.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@  |
+-+-+

(zone :kind :dungeon :title \"Testpit\" :start-facing :east)
(special (1 0)
  (message \"A ladder leads up.\")
  (travel \"tmp-town.map\" 0 0 :north)
  (set-flag :after-travel))
" s))

;; Zone metadata comes from the file; plain maps stay dungeons.
(let ((m (load-map-file "tests/tmp-town.map")))
  (check "zone kind read" :city (dungeon-map-kind m))
  (check "zone title read" "Testville" (dungeon-map-title m))
  (check "map-title prefers the zone title" "Testville" (map-title m)))
(let ((m (parse-map *art* :name "plain")))
  (check "default zone kind" :dungeon (dungeon-map-kind m))
  (check "map-title falls back to the name" "plain" (map-title m)))
(let ((m (load-map-file "tests/tmp-dung.map")))
  (check "zone start-facing applies" :east (dungeon-map-start-facing m)))
(with-open-file (s "tests/tmp.map" :direction :output :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(zone :wrap t :start-facing :south)
" s))
(let ((m (load-map-file "tests/tmp.map")))
  (check-true "zone :wrap applies" (dungeon-map-wrap m))
  (check "zone :start-facing keyword form" :south
         (dungeon-map-start-facing m)))
(with-open-file (s "tests/tmp.map" :direction :output :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(zone :kind \"city\")
" s))
(check-error "zone kind must be a keyword" (load-map-file "tests/tmp.map"))
(delete-file "tests/tmp.map")

;; Travel: switch zones, keep each zone's map and knowledge alive.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (g (new-game m))
       (msgs (watch-messages g))
       (zones '()))
  (on-event g :enter-zone
            (lambda (game map) (declare (ignore game))
              (push (map-title map) zones)))
  (check-true "town start explored"
              (cell-explored-p (game-knowledge g) 0 0))
  (travel-party g "tmp-dung.map")
  (check "travel lands in the dungeon" "Testpit" (map-title (game-map g)))
  (check "travel resolved the sibling path" "tests/tmp-dung.map"
         (dungeon-map-name (game-map g)))
  (check ":enter-zone emitted" '("Testpit") zones)
  (check-true "enter-zone message"
              (find-if (lambda (s) (search "You enter Testpit" s))
                       (funcall msgs)))
  (check "arrival at the target start" '(0 0)
         (list (game-x g) (game-y g)))
  (check "arrival facing the zone's start-facing" +east+ (game-facing g))
  (check-true "dungeon knowledge is fresh"
              (not (cell-explored-p (game-knowledge g) 1 0)))
  ;; step east onto the ladder cell: its special travels back up, and
  ;; the op AFTER the travel must not run (it belongs to the old cell)
  (let ((dung-map (game-map g))
        (dung-knowledge (game-knowledge g)))
    (move-party g :forward)
    (check "ladder special travels back to town" "Testville"
           (map-title (game-map g)))
    (check "explicit travel target position" '(0 0)
           (list (game-x g) (game-y g)))
    (check "explicit travel facing" +north+ (game-facing g))
    (check "ops after travel are skipped" nil (flag g :after-travel))
    (check-true "ladder message ran before the travel"
                (find-if (lambda (s) (search "ladder" s)) (funcall msgs)))
    ;; back down: the dungeon zone is reused, not reloaded
    (travel-party g "tmp-dung.map")
    (check-true "revisited zone keeps its map object"
                (eq dung-map (game-map g)))
    (check-true "revisited zone keeps its knowledge"
                (eq dung-knowledge (game-knowledge g)))
    (check-true "knowledge remembers the ladder cell"
                (cell-explored-p (game-knowledge g) 1 0))))

;; Travel guards: bad targets fail loudly.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (g (new-game m)))
  (check-error "travel to a missing map file"
    (travel-party g "tmp-nonesuch.map"))
  (check-error "travel target outside the map"
    (travel-party g "tmp-dung.map" 9 9))
  (start-combat g '(("test rat" 1)))
  (check-error "no traveling during combat"
    (travel-party g "tmp-dung.map")))

;;; ---------------------------------------------------------------------
;;; Named destinations: the places a homing spell can carry the party to.

(check "the registry starts empty" '() (destinations))
(check-error "a destination needs a symbol for a name"
  (define-destination "town" :map "tmp-town.map"))
(check-error "a destination needs a map file"
  (define-destination 'test-nowhere))
(check-error "a cell is both coordinates or neither"
  (define-destination 'test-half :map "tmp-town.map" :x 1))
(check-error "an unregistered destination is loud"
  (find-destination 'test-nonesuch))
(check "the soft lookup keeps quiet" nil
       (find-destination 'test-nonesuch nil))

(define-destination 'test-town :title "Testville Guild"
  :map "tmp-town.map" :x 0 :y 0 :facing :north)
(define-destination 'test-pit :map "tmp-dung.map")
(check "registration order is menu order" '(test-town test-pit)
       (mapcar #'destination-name (destinations)))
(check "a title defaults to the name" "Test Pit"
       (destination-title (find-destination 'test-pit)))
(check "an omitted cell is the map's own start" nil
       (destination-x (find-destination 'test-pit)))
;; registering a name twice replaces it and keeps its place
(define-destination 'test-town :title "Testville" :map "tmp-town.map")
(check "the redefinition keeps its place" '(test-town test-pit)
       (mapcar #'destination-name (destinations)))
(check "and carries the new title" "Testville"
       (destination-title (find-destination 'test-town)))
(check "the menu numbers what it lists" 'test-pit (destination-by-digit 2))
(check "and nothing past its end" nil (destination-by-digit 3))
(check-true "the rows print the titles"
            (equal '("Where to?" "" "1) Testville" "2) Test Pit")
                   (menu-texts (destination-rows "Where to?"))))

;; The flight itself lands where the destination says, arrival special
;; and all — TRAVEL-PARTY's own manners.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (g (new-game m)))
  (travel-to-destination g 'test-pit)
  (check "the flight lands in the named zone" "Testpit"
         (map-title (game-map g)))
  (check "at the map's own start" '(0 0) (list (game-x g) (game-y g)))
  (check-error "an unregistered flight is loud"
    (travel-to-destination g 'test-nonesuch)))

;; The full key-drive: pick the caster, the spell, then the place.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (view (make-cast-view)))
  (check "a homing spell asks for a destination" :destination
         (spell-target-kind 'test-blink))
  (check-true "and is castable with somewhere to go"
              (spell-castable-p g mage 'test-blink))
  (check "the caster digit picks the mage" nil (cast-act g view #\1))
  (let* ((spells (spells-for-hero mage))
         (pos (position 'test-blink spells))
         (start (max 0 (min pos (- (length spells) +menu-page-size+))))
         (digit (digit-char (1+ (- pos start)))))
    (setf (cast-view-top view) start)
    (check "the spell pick asks on instead of casting" nil
           (cast-act g view digit)))
  (check "the flight is chosen" 'test-blink (cast-view-spell view))
  (check-true "the page lists the destinations"
              (find-if (lambda (s) (search "2) Test Pit" s))
                       (menu-texts (cast-lines g view))))
  (check "a digit past the list does nothing" nil (cast-act g view #\9))
  (check "the second row casts" :done (cast-act g view #\2))
  (check "the party stands in the named zone" "Testpit"
         (map-title (game-map g)))
  (check "the flight pays its sp" (1- (hero-max-sp mage)) (hero-sp mage)))

;; Esc backs out of the destination page to the spell list.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (view (make-cast-view)))
  (setf (cast-view-hero view) mage
        (cast-view-spell view) 'test-blink)
  (check "Esc steps back one page" nil (cast-act g view #\Escape))
  (check "the spell is unchosen" nil (cast-view-spell view))
  (check "and the party has not moved" "Testville" (map-title (game-map g))))

;; An item that casts a homing spell asks the same question.
(define-item 't-ring-home :kind :ring :price 100
  :title "T Ring Home" :use '(:cast test-blink))
(check "the item asks for a destination" :destination
       (item-target-kind 't-ring-home))
(let* ((m (load-map-file "tests/tmp-town.map"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (view (make-use-view)))
  (give-item g mage 't-ring-home)
  (check "the user digit picks the mage" nil (use-act g view #\1))
  (check "the item pick asks on instead of using" nil (use-act g view #\1))
  (check-true "the page lists the destinations"
              (find-if (lambda (s) (search "2) Test Pit" s))
                       (menu-texts (use-lines g view))))
  (check "the second row uses the ring" :done (use-act g view #\2))
  (check "the ring carried the party" "Testpit" (map-title (game-map g)))
  (check "and cost no spell points" (hero-max-sp mage) (hero-sp mage)))

;; No flight out of a fight, whichever door it comes through.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (mage (%combat-mage))
       (g (new-game m :party (list mage)))
       (msgs (watch-messages g)))
  (give-item g mage 't-ring-home)
  (start-combat g '(("test rat" 1)))
  (check "the spell is refused mid-fight" nil
         (spell-castable-p g mage 'test-blink))
  (check-true "the ring still uses -- and goes nowhere"
              (use-item g mage 't-ring-home 'test-pit))
  (check "the party is where the fight is" "Testville"
         (map-title (game-map g)))
  (check-true "and the way stays shut"
              (find-if (lambda (s) (search "the way stays shut" s))
                       (funcall msgs))))

;; With nothing registered the spell has nowhere to go, and says so --
;; *DESTINATIONS* is bound locally so the empty registry lasts only for
;; this check, not for whatever the file appends after it.
(let ((*destinations* '()))
  (check "clearing empties the registry" '() (destinations))
  (let* ((m (load-map-file "tests/tmp-town.map"))
         (mage (%combat-mage))
         (g (new-game m :party (list mage))))
    (check "a homing spell with nowhere to go is refused" nil
           (spell-castable-p g mage 'test-blink))
    (check "and the card says why" "Nowhere to go."
           (spell-refusal g mage 'test-blink))))

;; The wandering roll respects the party's whereabouts: a step into a
;; location draws no roll, however certain-fire the street's table.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (g (new-game m :party (list (%combat-hero)))))
  (%apply-map-form m '(zone :encounters (("test rat" 1))
                            :encounter-chance 100)
                   "tmp-town")
  (setf (game-facing g) +east+)
  (let ((*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (move-party g)                          ; into the shoppe
    (check-true "a location cell skips the wandering roll"
                (game-location g))))

;; ... and a TRAVEL step rolls neither on the zone it left nor with
;; the destination's own certain-fire table (the roll belongs to the
;; map the step was taken on).  The zone form in the file also proves
;; the new keys ride the story layer end-to-end.
(with-open-file (s "tests/tmp-wander.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@ >|
+-+-+

(zone :kind :city :title \"Wanderville\"
      :encounters ((\"test rat\" 1)) :encounter-chance 100)
(special (1 0) (travel \"tmp-dung.map\"))
" s))
(let* ((m (load-map-file "tests/tmp-wander.map"))
       (g (new-game m :party (list (%combat-hero)))))
  (check "the file's zone form armed the table" 100
         (dungeon-map-encounter-chance m))
  (travel-party g "tmp-dung.map")           ; load + cache the dungeon zone
  (%apply-map-form (game-map g)
                   '(zone :encounters (("test rat" 1))
                          :encounter-chance 100)
                   "tmp-dung")
  (travel-party g "tmp-wander.map" 0 0 :east)
  (let ((*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (move-party g)                          ; onto the stairs: travel
    (check "the travel step draws no wandering roll" "Testpit"
           (map-title (game-map g)))
    (check "and starts no fight" nil (game-combat g))))
(delete-file "tests/tmp-wander.map")
(when (probe-file "tests/tmp-wander.mapc")
  (delete-file "tests/tmp-wander.mapc"))

;; A travel loop in map data hits the recursion cap instead of hanging.
(with-open-file (s "tests/tmp-loop-a.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(special (0 0) (travel \"tmp-loop-b.map\"))
" s))
(with-open-file (s "tests/tmp-loop-b.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(special (0 0) (travel \"tmp-loop-a.map\"))
" s))
(check-error "travel loop capped"
  (let ((g (new-game (load-map-file "tests/tmp-loop-a.map"))))
    (trigger-special g)))
(delete-file "tests/tmp-loop-a.map")
(delete-file "tests/tmp-loop-b.map")

;;; ---------------------------------------------------------------------
;;; The question a cell may put: (ask TEXT... OP...).  Stairs that
;;; carry the party down on the step that finds them are a footfall; a
;;; stair that asks first is a decision.  The op holds the question open
;;; (GAME-QUESTION) as a page both front-ends draw over the play page,
;;; and the ops run on a yes, drop on a no.

;; The op's shape is checked at the asking, not on the first yes.
(let ((g (new-game (parse-map *art*))))
  (check-error "ask needs its question first"
    (run-special g '((ask (travel "x.map")))))
  (check-error "ask needs ops to run on a yes"
    (run-special g '((ask "Take them?"))))
  (check-error "ask's ops must have an op's shape"
    (run-special g '((ask "Take them?" "travel"))))
  (check "a malformed ask leaves no question standing" nil
         (game-question g)))

;; The stair that asks: a three-cell corridor whose east end is the
;; stair down, the test dungeon below it.  The zone table is certain
;; fire so the wandering roll can be listened for on the one step that
;; matters — every other step runs with the dial off.
(with-open-file (s "tests/tmp-ask.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+-+
|@   >|
+-+-+-+

(zone :kind :city :title \"Askton\" :start-facing :east
      :encounters ((\"test rat\" 1)) :encounter-chance 100)
(special (2 0)
  (message \"Worn steps lead down.\")
  (ask \"Steps lead down into the dark, {leader}.\" \"Take them?\"
       (message \"Down you go.\")
       (travel \"tmp-dung.map\"))
  (set-flag :after-ask))
" s))

(let* ((m (load-map-file "tests/tmp-ask.map"))
       (hero (%combat-hero))
       (g (new-game m :party (list hero)))
       (msgs (watch-messages g))
       (asked '())
       (*encounter-rate* nil))
  (on-event g :question (lambda (game q) (declare (ignore game))
                          (push (question-text q) asked)))
  (setf (hero-name hero) "Ulf")
  (check "nothing asked on the start cell" nil (game-question g))
  (check "no page with no question" nil (question-lines g))
  (check "answering with no question is harmless" nil
         (answer-question g t))
  (move-party g)
  ;; the step onto the stair: the cell speaks, asks, and carries on —
  ;; the party is still on it, and certain-fire rats rolled nothing
  (let ((*encounter-rate* 1)
        (*rng* (lambda (n) (declare (ignore n)) (error "rolled ~D" n))))
    (check "the step lands on the stair" :moved (move-party g)))
  (check "the party stands on the stair" '(2 0) (list (game-x g) (game-y g)))
  (check "and has not gone down" "Askton" (map-title (game-map g)))
  (check-true "the question stands" (game-question g))
  (check ":question was emitted with the text, {leader} named"
         '(("Steps lead down into the dark, Ulf." "Take them?"))
         asked)
  (check "the ops after the ask ran at once" t (flag g :after-ask))
  (check-true "the message before the ask ran"
              (find "Worn steps lead down." (funcall msgs) :test #'equal))
  (check-true "the message inside the ask did not"
              (not (find "Down you go." (funcall msgs) :test #'equal)))
  (check "a step ending on a question drew no wandering roll" nil
         (game-combat g))
  ;; the page: the question's text — each string wrapped to the
  ;; narrowest column — a blank, then two option rows that click
  (let ((lines (question-lines g)))
    (check "the page opens on the question, wrapped to the column"
           '("Steps lead down into the" "dark, Ulf." "Take them?" "")
           (menu-texts (subseq lines 0 4)))
    (check-true "every text line fits the narrowest column"
                (every (lambda (line)
                         (<= (length (menu-line-text line))
                             +takeover-columns+))
                       lines))
    (check "yes is picked by Y" #\y
           (menu-line-key (find "Yes" lines :key #'menu-line-text
                                            :test #'equal)))
    (check "no is picked by N" #\n
           (menu-line-key (find "No" lines :key #'menu-line-text
                                           :test #'equal))))
  ;; every other key is eaten — not even a step gets through
  (check "a step key is swallowed" nil (question-act g #\w))
  (check "a digit is swallowed" nil (question-act g #\1))
  (check "Q is not the page's to answer" nil (question-act g #\q))
  (check-true "and the question still stands" (game-question g))
  ;; no: the question drops, the party stays where it stood
  (check "N declines" :no (question-act g #\n))
  (check "the question is gone" nil (game-question g))
  (check "the party still stands on the stair" '(2 0)
         (list (game-x g) (game-y g)))
  (check "in the zone it was in" "Askton" (map-title (game-map g)))
  ;; the cell asks again on the next visit — off and back on
  (move-party g :back)
  (move-party g)
  (check-true "re-entering the stair asks again" (game-question g))
  (check "Esc declines as the character" :no (question-act g #\Escape))
  (move-party g :back)
  (move-party g)
  (check "Esc declines as the front-end's keyword" :no
         (question-act g :esc))
  (move-party g :back)
  (move-party g)
  (check "shift makes no difference" :yes (question-act g #\Y))
  ;; yes: the ops run in order, the travel lands the party below, and
  ;; the question is gone before it does
  (check "the question is answered" nil (game-question g))
  (check-true "the message inside the ask spoke"
              (find "Down you go." (funcall msgs) :test #'equal))
  (check "and the stair was taken" "Testpit" (map-title (game-map g)))
  (check "landing at the dungeon's start" '(0 0)
         (list (game-x g) (game-y g))))

;; A question stands until it is answered — the engine keeps it through
;; anything short of that — but a fight on the cell cannot take the
;; answer: the ops would be skipped, and a yes that did nothing would
;; pass quietly.
(let* ((m (load-map-file "tests/tmp-ask.map"))
       (g (new-game m :party (list (%combat-hero))))
       (*encounter-rate* nil))
  (move-party g) (move-party g)
  (check-true "asked" (game-question g))
  (start-combat g '(("test rat" 1)))
  (check-error "no answering mid-combat" (answer-question g t))
  (check-true "the question outlasts the refusal" (game-question g))
  (setf (game-combat g) nil)
  (check "and takes its answer after the fight" :no (question-act g #\n)))

;; A once belongs inside the ask: there, a no leaves the one time
;; unspent, and the yes that follows spends it.
(with-open-file (s "tests/tmp-ask-once.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@  |
+-+-+

(zone :start-facing :east)
(special (1 0)
  (ask \"A lever.  Pull it?\"
       (once (set-flag :pulled) (message \"Clunk.\"))))
" s))
(let* ((g (new-game (load-map-file "tests/tmp-ask-once.map")))
       (msgs (watch-messages g)))
  (move-party g)
  (check "no spends nothing" :no (question-act g #\n))
  (check "the lever is unpulled" nil (flag g :pulled))
  (move-party g :back) (move-party g)
  (check "yes runs the once" :yes (question-act g #\y))
  (check "the lever is pulled" t (flag g :pulled))
  (check "once" 1 (count "Clunk." (funcall msgs) :test #'equal))
  (move-party g :back) (move-party g)
  (check-true "the cell asks again -- the ask is not the once"
              (game-question g))
  (check "a second yes" :yes (question-act g #\y))
  (check "finds the once spent" 1
         (count "Clunk." (funcall msgs) :test #'equal)))
(delete-file "tests/tmp-ask-once.map")

;; The step that asked is the step the ops run under: a LOCATION behind
;; an ASK records the door it was entered by, so leaving still steps
;; back out through it (the Bard's Tale exit), however long the player
;; took to say yes.
(with-open-file (s "tests/tmp-ask-loc.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@D |
+-+-+

(zone :start-facing :east)
(special (1 0)
  (ask \"The door stands ajar.  Go in?\"
       (location \"The Quiet House\" :shrine)))
" s))
(let* ((g (new-game (load-map-file "tests/tmp-ask-loc.map"))))
  (check "the door step lands on the cell" :door (move-party g))
  (check "asked, not yet inside" nil (game-location g))
  (check "a second thought: back on the street" :no (question-act g #\n))
  (move-party g :back)
  (move-party g)
  (check "yes walks in" :yes (question-act g #\y))
  (check "inside now" "The Quiet House"
         (location-title (game-location g)))
  (check "the location knows the door it was entered by" +east+
         (location-entry-dir (game-location g)))
  (leave-location g)
  (check "and leaving steps back out through it" '(0 0)
         (list (game-x g) (game-y g)))
  (check "facing away from the door" +west+ (game-facing g)))
(delete-file "tests/tmp-ask-loc.map")

;; A scripted caller may put a question of its own — the op's
;; mechanism is open — and a newer question replaces an older one.
(let* ((g (new-game (load-map-file "tests/tmp-ask.map"))))
  (ask-question g '("First?") '((set-flag :first)))
  (ask-question g '("Second?") '((set-flag :second)))
  (check "the newer question has the floor" '("Second?")
         (question-text (game-question g)))
  (check "a yes" :yes (question-act g #\y))
  (check "runs the newer question's ops" t (flag g :second))
  (check "and never the replaced one's" nil (flag g :first)))
(delete-file "tests/tmp-ask.map")
(when (probe-file "tests/tmp-ask.mapc")
  (delete-file "tests/tmp-ask.mapc"))

;; A step that lands on a TRAVEL cell, whose destination cell is itself
;; a LOCATION, must not carry the step's direction into the location:
;; TRAVEL-PARTY's own arrival is never a step (see LEAVE-LOCATION's
;; docstring), even when TRAVEL-PARTY was reached via MOVE-PARTY's step.
(with-open-file (s "tests/tmp-travel-loc-a.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@  |
+-+-+
(zone :start-facing :east)
(special (1 0) (travel \"tmp-travel-loc-b.map\" 0 0))
" s))
(with-open-file (s "tests/tmp-travel-loc-b.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+
|@|
+-+
(special (0 0) (location \"Way Station\" :shrine))
" s))
(let* ((m (load-map-file "tests/tmp-travel-loc-a.map"))
       (g (new-game m)))
  (check "step lands on the travel cell, arriving in the other map"
         "tests/tmp-travel-loc-b.map"
         (progn (move-party g) (dungeon-map-name (game-map g))))
  (check-true "the arrival cell's location was entered" (game-location g))
  (check "a location entered via TRAVEL (not a step) has no entry-dir" nil
         (location-entry-dir (game-location g)))
  (leave-location g)
  (check "leaving it leaves the party where it stands (no Bard's-Tale exit)"
         '(0 0) (list (game-x g) (game-y g))))
(delete-file "tests/tmp-travel-loc-a.map")
(delete-file "tests/tmp-travel-loc-b.map")

;; The lone-exit case is the same mechanic, not a guild special case:
;; a step-less arrival (TRAVEL here) on a cell with exactly one
;; passable side leaves through that door regardless of location kind.
(with-open-file (s "tests/tmp-lone-exit-a.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+
|@|
+-+
" s))
(with-open-file (s "tests/tmp-lone-exit-b.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@D |
+-+-+

(special (0 0)
  (location \"Lone Door Shrine\" :shrine))
" s))
(let* ((m (load-map-file "tests/tmp-lone-exit-a.map"))
       (g (new-game m)))
  (travel-party g "tmp-lone-exit-b.map")
  (check-true "the arrival cell's location was entered" (game-location g))
  (check "a location entered via TRAVEL has no entry-dir" nil
         (location-entry-dir (game-location g)))
  (leave-location g)
  (check "a non-guild kind still takes its cell's lone door out" '(1 0)
         (list (game-x g) (game-y g)))
  (check "facing away from the door" :east
         (dir-keyword (game-facing g))))
(delete-file "tests/tmp-lone-exit-a.map")
(delete-file "tests/tmp-lone-exit-b.map")

;;; ---------------------------------------------------------------------
;;; Locations and shops (M4)

;; Stepping onto the shop cell enters the location; the game becomes
;; modal like combat.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g))
       (entered '())
       (left '()))
  (on-event g :enter-location
            (lambda (game loc) (declare (ignore game))
              (push (location-title loc) entered)))
  (on-event g :leave-location
            (lambda (game loc) (declare (ignore game))
              (push (location-title loc) left)))
  (turn-right g)
  (check "stepping into the shop passes the door" :door
         (move-party g :forward))
  (check-true "location is set" (game-location g))
  (check "location title" "The Test Shoppe"
         (location-title (game-location g)))
  (check "location kind" :shop (location-kind (game-location g)))
  (check "shop stock from map data" '(t-sword t-mail t-torch)
         (shop-stock (game-location g)))
  (check ":enter-location emitted" '("The Test Shoppe") entered)
  ;; entering is quiet — the location's own page names it; routine
  ;; comings and goings must not silt up the log
  (check "entering says nothing" nil (funcall msgs))
  (check-error "no walking inside a location" (move-party g :forward))
  (check-error "no nested locations"
    (enter-location g '("Another" :shop)))
  (check-true "leave-location returns the location" (leave-location g))
  (check "location cleared" nil (game-location g))
  (check "leaving says nothing either" nil (funcall msgs))
  (check ":leave-location emitted" '("The Test Shoppe") left)
  ;; the Bard's Tale exit: leaving stepped the party back out the
  ;; door onto the cell it came from, facing away from the shoppe
  (check "the exit lands before the door" '(0 0)
         (list (game-x g) (game-y g)))
  (check "the exit faces away from the door" :west
         (dir-keyword (game-facing g)))
  (check "leave-location when outside" nil (leave-location g))
  (check "a back-step re-enters the shoppe" :door (move-party g :back))
  (check "re-entry is modal again" "The Test Shoppe"
         (location-title (game-location g))))

;; A :house location: the generic location menu is the Bard's Tale
;; interior — the title over a lone clickable EXIT — and exiting steps
;; the party back out the front door, about-faced to the street.
(with-open-file (s "tests/tmp-house.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@D |
+-+-+

(zone :kind :city :title \"Hausen\")
(special (1 0)
  (location \"A Stone Cottage\" :house))
" s))
(let* ((m (load-map-file "tests/tmp-house.map"))
       (g (new-game m :party (list (%combat-hero)))))
  (turn-right g)
  (check "the cottage door opens" :door (move-party g))
  (check "the cottage is a house location" :house
         (location-kind (game-location g)))
  (check "the house menu shows EXIT" "EXIT"
         (menu-line-text (find "EXIT" (location-lines g nil)
                               :key #'menu-line-text :test #'equal)))
  (check "EXIT clicks as the leave key" #\Escape
         (menu-line-key (find "EXIT" (location-lines g nil)
                              :key #'menu-line-text :test #'equal)))
  (check "e exits the house" :left (location-act g nil #\e))
  (check "the house is left behind" nil (game-location g))
  (check "the exit lands on the street" '(0 0)
         (list (game-x g) (game-y g)))
  (check "the exit faces away from the house" :west
         (dir-keyword (game-facing g)))
  ;; a back-step entry records the true entry direction — the exit
  ;; still lands on the street, facing away from the door
  (check "a back-step enters the cottage again" :door
         (move-party g :back))
  (check "Esc exits after a back-step entry" :left
         (location-act g nil #\Escape))
  (check "back on the street again" '(0 0)
         (list (game-x g) (game-y g)))
  (check "still facing away from the door" :west
         (dir-keyword (game-facing g))))

;; Opening hours: (location ... :closed BAND-OR-BANDS) keeps the door
;; shut through the named day-band(s) — the party is told, and an
;; entering step is bounced back onto the street, left facing the shut
;; door at no clock cost beyond the step already taken.  The location
;; op stays top-level map data, so the street facade still shows.
(with-open-file (s "tests/tmp-hours.map" :direction :output
                   :if-exists :supersede)
  (write-string "+-+-+
|@D |
+-+-+

(zone :kind :city :title \"Hours\")
(special (1 0)
  (location \"The Day Shoppe\" :shop :closed :night))
" s))
(let* ((m (load-map-file "tests/tmp-hours.map"))
       (g (new-game m :party (list (%combat-hero))))
       (msgs (watch-messages g))
       (closed '()))
  (on-event g :location-closed
            (lambda (game loc) (declare (ignore game))
              (push (location-title loc) closed)))
  (turn-right g)                        ; a new game starts at 08:00
  (check "the shoppe opens by day" :door (move-party g))
  (check-true "and admits the party" (game-location g))
  (leave-location g)                    ; back to (0,0), facing west
  (funcall msgs)
  (setf (game-time g) 1330)             ; 22:10 — night
  (check "the door still swings at night" :door (move-party g :back))
  (check "but the shoppe is not entered" nil (game-location g))
  (check "the party is bounced back onto the street" '(0 0)
         (list (game-x g) (game-y g)))
  (check "left facing the shut door" :east (dir-keyword (game-facing g)))
  (check "and told" '("The Day Shoppe is closed.") (funcall msgs))
  (check ":location-closed emitted" '("The Day Shoppe") closed)
  (check "the bounce costs no time beyond the step taken" 1331
         (game-time g))
  ;; a scripted entry (no step) refuses in place; the :closed arg
  ;; also takes a list of bands
  (check "a scripted entry refuses in place" nil
         (enter-location g '("The Night Cave" :hut
                             :closed (:evening :night))))
  (check "the party stays put" '(0 0) (list (game-x g) (game-y g)))
  (funcall msgs)
  (setf (game-time g) (+ 1440 480))     ; day 2, 08:00
  (check "morning opens the door again" :door (move-party g))
  (check-true "and the shoppe admits the party once more"
              (game-location g))
  (leave-location g))
(delete-file "tests/tmp-hours.map")

;; Location specs are validated loudly.
(let ((g (new-game (parse-map *art* :name "test"))))
  (check-error "location title must be a string"
    (enter-location g '(nope :shop)))
  (check-error "location kind must be a keyword"
    (enter-location g '("X" shop)))
  (check-error "shop stock items must exist"
    (enter-location g '("X" :shop :stock (t-nada))))
  (check-error ":closed bands must be day-bands"
    (enter-location g '("X" :shop :closed :midnight))))

;; Buying and selling.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (msgs (watch-messages g)))
  (setf (hero-gold h) 30)
  (check "item price" 10 (item-price 't-sword))
  (check "sell price is half" 5 (item-sell-price 't-sword))
  (check-true "buy a sword" (buy-item g h 't-sword))
  (check "gold deducted" 20 (hero-gold h))
  (check-true "bought item in the pack" (hero-carrying-p h 't-sword))
  (check "fresh equipment auto-equips" 't-sword
         (equipped-of-kind h :weapon))
  (check-true "buy message"
              (find-if (lambda (s) (search "buys T Sword for 10 gold" s))
                       (funcall msgs)))
  (check-true "buy a second sword" (buy-item g h 't-sword))
  (check "no re-equip with a weapon in hand" 1 (length (hero-equipped h)))
  (setf (hero-gold h) 1)
  (check "cannot afford" nil (buy-item g h 't-torch))
  (check "gold untouched on refusal" 1 (hero-gold h))
  (check-true "afford message"
              (find-if (lambda (s) (search "cannot afford" s))
                       (funcall msgs)))
  ;; fill the pack: the shop refuses when there is no room
  (loop while (< (length (hero-items h)) +inventory-limit+)
        do (give-item g h 't-torch))
  (setf (hero-gold h) 50)
  (check "full pack refuses the purchase" nil (buy-item g h 't-torch))
  (check "gold untouched on a full pack" 50 (hero-gold h))
  ;; selling: half price back; the hands keep the sword while a spare
  ;; copy covers it (DROP-ITEM), and only the last copy unequips
  (check-true "sell the equipped sword" (sell-item g h 't-sword))
  (check "sell pays half price" 55 (hero-gold h))
  (check "a spare copy keeps the sword in hand" 't-sword
         (equipped-of-kind h :weapon))
  (check-true "the second sword is still packed"
              (hero-carrying-p h 't-sword))
  (check-true "sell the spare too" (sell-item g h 't-sword))
  (check "selling the last copy unequips" nil (equipped-of-kind h :weapon))
  (check "sell without the item" nil (sell-item g h 't-mail)))

;; A class the armor excludes buys it without auto-equip.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (h (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game m :party (list h))))
  (setf (hero-gold h) 30)
  (check-true "wizard buys the mail anyway" (buy-item g h 't-mail))
  (check "but does not auto-equip it" nil (equipped-of-kind h :armor)))

;; The shared shop interaction model: both front-ends feed keys into
;; SHOP-ACT and draw SHOP-LINES, so the whole flow tests here.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (view (make-shop-view)))
  (setf (hero-gold h) 30)
  (turn-right g)
  (move-party g :forward)               ; into the shop
  (check-true "pick page shows the shop name"
              (search "The Test Shoppe" (first (shop-lines g view))))
  ;; the pick page is a bare prompt naming the digit range — the
  ;; roster pane already lists the party (the temple-lines pattern)
  (check-true "pick page asks who shops, naming the range"
              (find-if (lambda (s) (search "Who is shopping?  (1)" s))
                       (menu-texts (shop-lines g view))))
  (check "the pick page repeats no roster names" nil
         (find-if (lambda (s) (search (hero-name h) s))
                  (menu-texts (shop-lines g view))))
  (check "digit picks the hero" nil (shop-act g view #\1))
  (check-true "hero selected" (eq h (shop-view-hero view)))
  (check-true "buy page lists the stock priced"
              (find-if (lambda (s) (search "1) T Sword  10 gp" s))
                       (menu-texts (shop-lines g view))))
  (check "the stock row carries its pick key" #\1
         (menu-line-key
          (find-if (lambda (line)
                     (search "T Sword" (menu-line-text line)))
                   (shop-lines g view))))
  (shop-act g view #\1)                 ; buy the sword
  (check "shop-act buys" 20 (hero-gold h))
  (check "s flips to the sell page" nil (shop-act g view #\s))
  (check "sell mode" :sell (shop-view-mode view))
  (check-true "sell page lists the pack with sell prices"
              (find-if (lambda (s) (search "1) T Sword*  5 gp" s))
                       (menu-texts (shop-lines g view))))
  (shop-act g view #\1)                 ; sell it again
  (check "shop-act sells" 25 (hero-gold h))
  (check "b flips back to the buy page" nil (shop-act g view #\b))
  ;; the inspect flow ('i' on the buy page): read the card, spend no gold
  (check-true "the buy page offers inspect as a clickable option"
              (member (menu-option #\i "Inspect")
                      (shop-lines g view) :test #'equal))
  (check "i opens the inspect page" nil (shop-act g view #\i))
  (check "inspect mode" :inspect (shop-view-mode view))
  (check-true "the inspect page browses"
              (find-if (lambda (s) (search "browses.  Gold: 25 gp" s))
                       (menu-texts (shop-lines g view))))
  (check-true "the stock is still listed priced"
              (find-if (lambda (s) (search "1) T Sword  10 gp" s))
                       (menu-texts (shop-lines g view))))
  (check "the inspect page carries no key footer" nil
         (find-if (lambda (s) (find #\[ s))
                  (menu-texts (shop-lines g view))))
  (check "a digit shows the item's card" nil (shop-act g view #\1))
  (check "the card item is remembered" 't-sword (shop-view-pending view))
  (check-true "the card is titled with the item"
              (search "*** T Sword ***" (first (shop-lines g view))))
  (check-true "the card shows the damage"
              (member "Damage: 1d6+2" (menu-texts (shop-lines g view))
                      :test #'equal))
  (check "inspecting spends no gold" 25 (hero-gold h))
  (check "esc leaves the card" nil (shop-act g view #\Escape))
  (check "the card item is forgotten" nil (shop-view-pending view))
  ;; inspecting is a one-card detour: the card's Esc lands straight
  ;; back on the buy page, so the next digit buys instead of opening
  ;; another card
  (check "the card's esc lands back on the buy page" :buy
         (shop-view-mode view))
  (shop-act g view #\1)
  (check "the next digit buys again, not another card" 15 (hero-gold h))
  (check "i then esc with no card picked also returns to buying" :buy
         (progn (shop-act g view #\i) (shop-act g view #\Escape)
                (shop-view-mode view)))
  (check "the hero stays selected" h (shop-view-hero view))
  (check "escape backs out to the pick page" nil
         (shop-act g view #\Escape))
  (check "hero deselected" nil (shop-view-hero view))
  (check "escape from the pick page leaves" :left
         (shop-act g view #\Escape))
  (check "location closed by the model" nil (game-location g)))

;; Duplicate copies on the sell page: the star marks the worn COPY —
;; the pack page's rule (EQUIPPED-INSTANCE-P) — not every row of the
;; worn name.
(let* ((m (load-map-file "tests/tmp-town.map"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (view (make-shop-view)))
  (setf (hero-gold h) 40)
  (turn-right g)
  (move-party g :forward)               ; into the shop
  (shop-act g view #\1)                 ; the hero shops
  (shop-act g view #\1)                 ; buys a sword (auto-equips)
  (shop-act g view #\1)                 ; and a spare copy
  (shop-act g view #\s)                 ; to the sell page
  (let ((texts (menu-texts (shop-lines g view))))
    (check-true "the worn copy wears the star"
                (find-if (lambda (s) (search "1) T Sword*" s)) texts))
    (check "the spare copy does not" nil
           (find-if (lambda (s) (search "2) T Sword*" s)) texts))
    (check-true "but the spare is listed"
                (find-if (lambda (s) (search "2) T Sword " s)) texts)))
  (leave-location g))

;; A quest piece mixed into the pack: the sell page numbers gear only
;; (PACK-GEAR), so row 1 is the sword even though the key sits first
;; in HERO-ITEMS — the same pack-index-vs-row-number rule the pack
;; page's EQUIP-ACT already gets a dedicated test for.
(let* ((h (%combat-hero))
       (g (new-game (parse-map *art* :name "test") :party (list h)))
       (view (make-shop-view)))
  (enter-location g '("The Reliquary" :shop :stock (t-sword)))
  (give-item g h 't-key)                ; the piece comes first in the pack
  (give-item g h 't-sword)
  (shop-act g view #\1)                 ; the hero shops
  (shop-act g view #\s)                 ; to the sell page
  (let ((texts (menu-texts (shop-lines g view))))
    (check-true "the sword is row 1, the key not a row at all"
                (find-if (lambda (s) (search "1) T Sword" s)) texts))
    (check "no quest piece among the sell rows" nil
           (find-if (lambda (s) (search "T Key" s)) texts)))
  ;; row 1 is the sword, though the key sits first in the pack
  (shop-act g view #\1)
  (check "the digit sells the gear row it printed" 5 (hero-gold h))
  (check "the sword is gone from the pack" nil
         (hero-carrying-p h 't-sword))
  (check-true "the quest piece survives the sell"
              (hero-carrying-p h 't-key))
  (leave-location g))

;; More than one hero: the bare prompt names the whole digit range.
(let* ((g (new-game (parse-map *art* :name "test")
                    :party (with-rng () (list (make-hero "A" :tester)
                                              (make-hero "B" :tester)))))
       (view (make-shop-view)))
  (enter-location g '("The Range" :shop :stock (t-sword)))
  (check-true "the prompt spans the roster"
              (find-if (lambda (s) (search "Who is shopping?  (1-2)" s))
                       (menu-texts (shop-lines g view))))
  (leave-location g))

;; The shop marks stock (and pack) items the shopper's class cannot
;; use — buying stays allowed (another hero may carry it), the marker
;; just warns before the gold is gone.
(let* ((h (with-rng () (make-hero "Wiz" :t-wizard)))
       (g (new-game (parse-map *art* :name "test") :party (list h)))
       (view (make-shop-view)))
  (setf (hero-gold h) 100)
  (enter-location g '("The Fitting Room" :shop :stock (t-sword t-mail)))
  (shop-act g view #\1)                 ; the wizard shops
  (let ((texts (menu-texts (shop-lines g view))))
    (check-true "buy page marks unfit stock"
                (find-if (lambda (s)
                           (search "2) T Mail (u)  20 gp" s))
                         texts))
    (check-true "buy page leaves fitting stock unmarked"
                (find-if (lambda (s) (search "1) T Sword  10 gp" s))
                         texts)))
  (shop-act g view #\2)                 ; buy the unfit mail anyway
  (check "an unfit purchase is still allowed" 80 (hero-gold h))
  (check "but it does not auto-equip" nil (equipped-of-kind h :armor))
  (shop-act g view #\s)
  (check-true "sell page marks the unfit item"
              (find-if (lambda (s)
                         (search "1) T Mail (u)  10 gp" s))
                       (menu-texts (shop-lines g view))))
  (leave-location g))

;; 'p' pools the party's gold onto the shopper — offered on both shop
;; pages, so a hero short of a purchase can draw on the party purse.
(let* ((a (with-rng () (make-hero "A" :tester)))
       (b (with-rng () (make-hero "B" :tester)))
       (g (new-game (parse-map *art* :name "test") :party (list a b)))
       (view (make-shop-view)))
  (setf (hero-gold a) 5 (hero-gold b) 40)
  (enter-location g '("The Vault" :shop :stock (t-sword)))
  (shop-act g view #\1)                 ; A shops
  (check-true "the buy footer offers pooling"
              (member (menu-option #\p "Pool gold")
                      (shop-lines g view) :test #'equal))
  (check "p pools onto the shopper" nil (shop-act g view #\p))
  (check "the shopper holds the party's gold" 45 (hero-gold a))
  (check "the partner's purse is empty" 0 (hero-gold b))
  (shop-act g view #\s)
  (check-true "the sell footer offers pooling too"
              (member (menu-option #\p "Pool gold")
                      (shop-lines g view) :test #'equal))
  (setf (hero-gold b) 7)
  (check "P pools from the sell page too" nil (shop-act g view #\P))
  (check "the sell-page pool lands" 52 (hero-gold a))
  (leave-location g))

;; Kinds the engine has no mechanics for still enter and leave cleanly.
(let* ((g (new-game (parse-map *art* :name "test")))
       (view (make-shop-view)))
  (enter-location g '("Empty Hut" :hut))
  (check-true "unknown kind gets the plain notice"
              (find-if (lambda (s) (search "nothing to do" s))
                       (location-lines g view)))
  (check "escape leaves the unknown kind" :left
         (location-act g view #\Escape))
  (check "unknown kind left" nil (game-location g)))

;; Location pictures: the location op's :IMAGE resolves map-relative
;; (the effect-icon rule) — the Amiga front-end shows it in the view
;; column while the location's menu takes over the message area.
(let ((g (new-game (parse-map *art* :name "world/town"))))
  (enter-location g '("The Pictured Inn" :tavern :image "gfx/inn.iff"))
  (check "location-image reads the :IMAGE arg" "gfx/inn.iff"
         (location-image (game-location g)))
  (check "the picture resolves beside the map" "world/gfx/inn.iff"
         (location-image-path g))
  (leave-location g)
  (check "no location, no picture" nil (location-image-path g))
  (enter-location g '("Bare Hut" :hut))
  (check "a location without :IMAGE has no picture" nil
         (location-image-path g))
  (leave-location g))

;; Location music: the location op's :MUSIC resolves map-relative like
;; :IMAGE — the Amiga front-end loops the 8SVX while the party stands
;; inside and silences it at the door.
(let ((g (new-game (parse-map *art* :name "world/town"))))
  (enter-location g '("The Singing Inn" :tavern
                      :music "sfx/inn-theme.8svx"))
  (check "location-music reads the :MUSIC arg" "sfx/inn-theme.8svx"
         (location-music (game-location g)))
  (check "the tune resolves beside the map" "world/sfx/inn-theme.8svx"
         (location-music-path g))
  (leave-location g)
  (check "no location, no tune" nil (location-music-path g))
  (enter-location g '("Quiet Hut" :hut))
  (check "a location without :MUSIC has no tune" nil
         (location-music-path g))
  (leave-location g))

;;; ---------------------------------------------------------------------
;;; Menu scrolling on the interaction models: a stock/pack/spell list
;;; deeper than a page windows with u/d and digits pick within the
;;; visible window — the front-ends inherit all of it from the models.

;; a nine-item stock: deeper than the page (7), windows to a full page
(dolist (spec '((tscr-1 1) (tscr-2 2) (tscr-3 3) (tscr-4 4) (tscr-5 5)
                (tscr-6 6) (tscr-7 7) (tscr-8 8) (tscr-9 9)))
  (define-item (first spec) :price (second spec)))
(let* ((h (%combat-hero))
       (g (new-game (parse-map *art* :name "test") :party (list h)))
       (view (make-shop-view)))
  (setf (hero-gold h) 100)
  (enter-location g '("The Deep Shoppe" :shop
                      :stock (tscr-1 tscr-2 tscr-3 tscr-4 tscr-5
                              tscr-6 tscr-7 tscr-8 tscr-9)))
  (shop-act g view #\1)                 ; the hero shops
  (let ((texts (menu-texts (shop-lines g view))))
    (check-true "deep stock: the first window starts at the head"
                (find-if (lambda (s) (search "1) Tscr 1" s)) texts))
    (check-true "deep stock: the seventh item still shows"
                (find-if (lambda (s) (search "7) Tscr 7" s)) texts))
    (check "deep stock: the eighth is over the window's edge" nil
           (find-if (lambda (s) (search "Tscr 8" s)) texts))
    (check "deep stock: the geometry reaches the scrollbar"
           '(0 7 9) *menu-scroll*))
  (check-true "the head window carries the footer keys"
              (member (menu-option #\p "Pool gold")
                      (shop-lines g view) :test #'equal))
  (check "d scrolls the stock" nil (shop-act g view #\d))
  (check "the view holds the clamped offset" 2 (shop-view-top view))
  (let ((texts (menu-texts (shop-lines g view))))
    (check-true "scrolled stock: row 1 is the third item"
                (find-if (lambda (s) (search "1) Tscr 3" s)) texts))
    (check "scrolled stock: the geometry follows"
           '(2 9 9) *menu-scroll*))
  ;; the footer keys ride the first window only — a scrolled window
  ;; gives its rows to the stock, and the keys themselves keep working
  (check "the scrolled window sheds the footer keys" nil
         (member (menu-option #\p "Pool gold")
                 (shop-lines g view) :test #'equal))
  (shop-act g view #\2)                 ; buys the fourth item, tscr-4
  (check "a digit buys within the window" '(tscr-4) (hero-items h))
  (check "the windowed buy paid the right price" 96 (hero-gold h))
  (check "a digit past the window buys nothing" nil
         (progn (shop-act g view #\8) (rest (hero-items h))))
  (check "u scrolls back to the head" nil (shop-act g view #\u))
  (check "the offset is back at the head" 0 (shop-view-top view))
  (check-true "the head window brings the footer keys back"
              (member (menu-option #\p "Pool gold")
                      (shop-lines g view) :test #'equal))
  ;; the sell page scrolls the pack the same way
  (dotimes (i 7) (give-item g h 'tscr-1))
  (shop-act g view #\s)
  (check "the page flip resets the offset" 0 (shop-view-top view))
  (check "a full pack scrolls on the sell page"
         '(0 7 8) (progn (shop-lines g view) *menu-scroll*))
  (shop-act g view #\d)
  (check "the pack window clamps to its tail" 1 (shop-view-top view))
  (check "the scrolled sell window sheds its footer too" nil
         (member (menu-option #\b "Buy")
                 (shop-lines g view) :test #'equal))
  (shop-act g view #\1)                 ; sells pack item 2 (tscr-1)
  (check "a digit sells within the window" 7 (length (hero-items h)))
  (check "escape resets the scroll offset" 0
         (progn (shop-act g view #\Escape) (shop-view-top view)))
  (leave-location g))

;; the use menu windows a full pack of usable items
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h)))
       (v (make-use-view)))
  (dotimes (i 8) (give-item g h 't-lantern))
  (use-act g v #\1)                     ; the hero uses
  (check "a full pack scrolls on the use menu"
         '(0 7 8) (progn (use-lines g v) *menu-scroll*))
  (check "d scrolls the use list" 1
         (progn (use-act g v #\d) (use-view-top v)))
  (check "a windowed digit resolves the use" :done (use-act g v #\1))
  (check-true "the scrolled use landed" (light-active-p g)))

;; the cast menu windows a deep spell book
(dolist (name '(tscr-spell-1 tscr-spell-2 tscr-spell-3 tscr-spell-4))
  (define-spell name :cost 1 :level 1 :classes '(:t-mage) :heal "1d4"))
(let* ((m (parse-map *art* :name "test"))
       (h (with-rng () (make-hero "Mage" :t-mage)))
       (g (new-game m :party (list h)))
       (v (make-cast-view))
       (book (spells-for-hero h)))
  (check-true "the test book is deeper than a page"
              (> (length book) +menu-page-size+))
  (cast-act g v #\1)                    ; the mage casts
  (check-true "a deep book scrolls on the cast menu"
              (progn (cast-lines g v) *menu-scroll*))
  (loop for top = (cast-view-top v)    ; page down to the very bottom
        do (cast-act g v #\d)
        until (= top (cast-view-top v)))
  (check "the cast window scrolled to the end"
         (- (length book) +menu-page-size+)
         (cast-view-top v))
  ;; the last row is tscr-spell-4, a heal — castable out of combat
  (cast-act g v (digit-char +menu-page-size+))
  (check "a windowed digit picks the right spell"
         (first (last book)) (cast-view-spell v)))

;; the save picker windows a slot list past the page
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (v (%make-save-menu
           :mode :load
           :slots '("s1" "s2" "s3" "s4" "s5" "s6" "s7" "s8" "s9"))))
  (check "nine slots scroll in the picker"
         '(0 7 9) (progn (save-menu-lines g v) *menu-scroll*))
  (check "d scrolls the slots" nil (save-menu-act g v #\d))
  (check "the slot window scrolled" 2 (save-menu-top v))
  (check "a windowed digit loads the right slot"
         (list :load (slot-path "s4"))
         (save-menu-act g v #\2)))

;; the character sheet windows a long stat block (a full pack lists
;; one row per item) and scrolls through HERO-SHEET-SCROLL
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  ;; the pack left the sheet for its own page ('i' — EQUIP-LINES),
  ;; so even a full pack no longer grows or scrolls the stat block
  (check "the sheet carries no pack row" nil
         (find-if (lambda (s) (search "Pack" s))
                  (menu-texts (hero-sheet-lines g 0))))
  (check "the sheet keeps its own keys, aired one per row"
         (list (menu-option #\p "Pool gold")
               ""
               (menu-option #\t "Trade gold")
               ""
               (menu-option #\o "Order party")
               ""
               (menu-next-option))
         (last (hero-sheet-lines g 0) 7))
  (check "the sheet does not scroll" nil
         (hero-sheet-scroll g 0 0 #\d))
  (give-item g h 't-sword)
  (equip-item g h 't-sword)
  (dotimes (i 7) (give-item g h 't-torch))
  (let ((texts (menu-texts (hero-sheet-lines g 0))))
    (check "a full pack no longer scrolls the sheet" nil *menu-scroll*)
    (check "a full pack stays off the sheet" nil
           (find-if (lambda (s) (search "T Torch" s)) texts)))
  (check "a full pack leaves the sheet unscrollable" nil
         (hero-sheet-scroll g 0 0 #\d))
  (check "an empty roster slot does not scroll" nil
         (hero-sheet-scroll g 4 0 #\d)))

;; The one stat block that still overflows +SHEET-PAGE-SIZE+: a raced
;; hero who both casts and sings pays a row for the race line, the SP
;; line and the Tunes line at once — nine rows against the eight-row
;; page, so the block windows and u/d scroll it.  (A raceless one, or
;; either half of the pair alone, fits whole.)
(let* ((m (parse-map *art* :name "test"))
       (adept (%combat-adept))
       (raced (%combat-adept "Ora"))
       (g (new-game m :party (list adept raced))))
  (setf (hero-race raced) :r-human)
  (check "the raceless caster-singer's block fits whole" 8
         (length (hero-summary-lines adept)))
  (check "the raced caster-singer's block overflows" 9
         (length (hero-summary-lines raced)))
  (check "so it windows at the page size" '(0 8 9)
         (progn (hero-sheet-lines g 1) *menu-scroll*))
  (check "and d scrolls it" 1 (hero-sheet-scroll g 1 0 #\d))
  (check "while the raceless one does not scroll" nil
         (hero-sheet-scroll g 0 0 #\d)))

;;; ---------------------------------------------------------------------
;;; Save games: the whole world round-trips — every visited zone's
;;; knowledge, the party's packs and equipment.

(let* ((m (load-map-file "tests/tmp-town.map"))
       (h (%combat-hero))
       (g (new-game m :party (list h))))
  (give-item g h 't-sword)
  (give-item g h 't-torch)
  (equip-item g h 't-sword)
  (travel-party g "tmp-dung.map")       ; explore the dungeon start
  (travel-party g "tmp-town.map" 0 0 :north)
  (save-game g "tests/tmp-save.lisp")
  (let ((g2 (load-game "tests/tmp-save.lisp")))
    (check "world load restores the current zone" "Testville"
           (map-title (game-map g2)))
    (check-true "world town knowledge restored"
                (cell-explored-p (game-knowledge g2) 0 0))
    (let ((h2 (first (game-party g2))))
      (check "world pack restored" '(t-sword t-torch) (hero-items h2))
      (check "world equipment restored" '(t-sword) (hero-equipped h2))
      (check "world attack dice from restored gear" "1d6+2"
             (hero-attack-dice h2)))
    (check-true "unvisited zone knowledge kept pending"
                (assoc "tests/tmp-dung.map" (game-zone-knowledge g2)
                       :test #'equal))
    ;; traveling back restores the pending knowledge
    (travel-party g2 "tmp-dung.map")
    (check-true "world dungeon knowledge restored on travel"
                (cell-explored-p (game-knowledge g2) 0 0))
    (check "pending knowledge consumed" nil
           (assoc "tests/tmp-dung.map" (game-zone-knowledge g2)
                  :test #'equal))))
(delete-file "tests/tmp-save.lisp")

;; v8: the portrait stamped at creation rides the save — a woman's
;; face comes back as hers, not as the class default's.
(let ((g (new-game (load-map-file "tests/tmp-town.map")
                   :party (with-rng ()
                            (list (make-hero "Mab" :t-two-faced
                                             :woman t))))))
  (save-game g "tests/tmp-save.lisp")
  (check "the stamped portrait round-trips" "gfx/woman.iff"
         (hero-image
          (first (game-party (load-game "tests/tmp-save.lisp"))))))
(delete-file "tests/tmp-save.lisp")

;; Save versions: an older save is attempted, not refused — every
;; format bump only added plist keys, so the keys an old save lacks
;; fall back to the defaults.  Only a save from a newer build, or one
;; that actually fails to restore, is an error.
(with-open-file (s "tests/tmp-save.lisp" :direction :output
                   :if-exists :supersede)
  (write-string "(:lambda-tale-save 1 :map-file \"tests/tmp-town.map\")" s))
(let ((g (load-game "tests/tmp-save.lisp")))
  (check "a v1 save loads its map" "Testville" (map-title (game-map g)))
  (check "missing position defaults to the map start" '(0 0)
         (list (game-x g) (game-y g)))
  (check "missing clock defaults to a fresh game's" *new-game-minutes*
         (game-time g))
  (check "missing party defaults to none" '() (game-party g)))
;; a v4-era save: hero plists knew nothing of :race, :ailments or
;; :class-levels yet — the loaded hero defaults them and keeps the rest
(with-open-file (s "tests/tmp-save.lisp" :direction :output
                   :if-exists :supersede)
  (write-string "(:lambda-tale-save 4 :map-file \"tests/tmp-town.map\" :x 1 :y 0 :facing 1 :time 300 :party ((:name \"Rip\" :class :tester :level 2 :xp 50 :max-hp 12 :hp 9)))" s))
(let* ((g (load-game "tests/tmp-save.lisp"))
       (h (first (game-party g))))
  (check "an old hero keeps what its day wrote" '("Rip" 2 9)
         (list (hero-name h) (hero-level h) (hero-hp h)))
  (check "and defaults what it never knew" '(nil () ())
         (list (hero-race h) (hero-ailments h) (hero-class-levels h)))
  (check "the clock an old save did write is kept" 300 (game-time g))
  (check "position from the old save" '(1 0 1)
         (list (game-x g) (game-y g) (game-facing g))))
;; a newer build's save is still refused — its keys and their meaning
;; are unknowable here
(with-open-file (s "tests/tmp-save.lisp" :direction :output
                   :if-exists :supersede)
  (write-string "(:lambda-tale-save 99 :map-file \"tests/tmp-town.map\")" s))
(check-error "a save from a newer build is refused"
             (load-game "tests/tmp-save.lisp"))
;; and an old save that cannot restore (its map is gone) is an error,
;; not a crash into the debugger with half a game built
(with-open-file (s "tests/tmp-save.lisp" :direction :output
                   :if-exists :supersede)
  (write-string "(:lambda-tale-save 3 :map-file \"tests/no-such.map\")" s))
(check-error "an old save that fails to restore is an error"
             (load-game "tests/tmp-save.lisp"))
(delete-file "tests/tmp-save.lisp")
;; saving where no file can be written signals (the front-ends catch
;; this and put it in the log — see SAVES-ACT in both UIs)
(let* ((m (load-map-file "tests/tmp-town.map"))
       (g (new-game m)))
  (check-error "saving into a missing directory is an error"
               (save-game g "tests/no-such-dir/tmp-save.lisp")))

;;; ---------------------------------------------------------------------
;;; Named saves: the save/load slot menu (src/save-menu.lisp)

(check "slot-path names saves/NAME.sav" "saves/alpha.sav"
       (slot-path "alpha"))
(check-true "slot names allow letters, digits, - and _"
            (every #'slot-name-char-p "Alpha-2_b"))
(check-true "slot names refuse path characters"
            (notany #'slot-name-char-p "/:. "))

(let ((*save-dir* "tests/tmp-saves/"))
  (check "no save dir means no slots" '() (save-slots))
  (let* ((m (load-map-file "tests/tmp-town.map"))
         (g (new-game m :party (list (%combat-hero))))
         (view (make-save-menu :save)))
    ;; an empty :save menu offers only the new-name entry
    (check-true "empty save menu says so"
                (find-if (lambda (s) (search "No saved games yet" s))
                         (save-menu-lines g view)))
    (check "digits without slots do nothing" nil (save-menu-act g view #\1))
    ;; 'n' starts name entry; name chars accumulate, junk is ignored,
    ;; backspace deletes, the live echo shows the name
    (check "n opens the name entry" nil (save-menu-act g view #\n))
    (dolist (c '(#\a #\l #\p #\h #\/ #\a))   ; the / must be ignored
      (save-menu-act g view c))
    (check "name entry keeps only name characters" "alpha"
           (save-menu-entry view))
    (save-menu-act g view #\Backspace)
    (check "backspace deletes" "alph" (save-menu-entry view))
    (save-menu-act g view #\a)
    (check-true "the entry line echoes the name"
                (find-if (lambda (s) (search "New name: alpha_" s))
                         (save-menu-lines g view)))
    ;; Return commits: the model returns the decision, the front-end
    ;; executes it — exactly what both UIs do
    (let ((r (save-menu-act g view #\Return)))
      (check "return commits the new name" '(:save "tests/tmp-saves/alpha.sav")
             r)
      (ensure-save-dir)
      (save-game g (second r)))
    (check "the slot now lists" '("alpha") (save-slots))
    ;; an empty name does not commit
    (let ((view (make-save-menu :save)))
      (save-menu-act g view #\n)
      (check "return on an empty name stays" nil
             (save-menu-act g view #\Return))
      (check "esc leaves the name entry" nil
             (save-menu-act g view #\Escape))
      (check "back on the slot list" nil (save-menu-entry view))
      ;; the fresh menu lists the existing slot; a digit overwrites it
      (check-true "existing slot listed"
                  (find-if (lambda (s) (search "1) alpha" s))
                           (menu-texts (save-menu-lines g view))))
      (check "the slot row carries its pick key" #\1
             (menu-line-key
              (find-if (lambda (line)
                         (search "1) alpha" (menu-line-text line)))
                       (save-menu-lines g view))))
      (check "digit picks the overwrite slot"
             '(:save "tests/tmp-saves/alpha.sav")
             (save-menu-act g view #\1))
      (check "esc cancels the menu" :closed
             (save-menu-act g view #\Escape)))
    ;; the name cap holds
    (let ((view (make-save-menu :save)))
      (save-menu-act g view #\n)
      (dotimes (i 20) (save-menu-act g view #\x))
      (check "slot names cap at the limit" +slot-name-limit+
             (length (save-menu-entry view))))
    ;; load mode: pick the slot, execute the decision, world restored
    (let ((view (make-save-menu :load)))
      (check-true "load menu lists the slot"
                  (find-if (lambda (s) (search "1) alpha" s))
                           (menu-texts (save-menu-lines g view))))
      (let ((r (save-menu-act g view #\1)))
        (check "digit picks the load slot"
               '(:load "tests/tmp-saves/alpha.sav") r)
        (check "the picked save loads" "Testville"
               (map-title (game-map (load-game (second r)))))))
    ;; the Amiga vanillakey Return (code 13) commits too
    (let ((view (make-save-menu :save)))
      (save-menu-act g view #\n)
      (save-menu-act g view #\b)
      (check "code-char 13 commits like Return"
             '(:save "tests/tmp-saves/b.sav")
             (save-menu-act g view (code-char 13))))
    ;; combat refuses politely: the page says so, digits do nothing,
    ;; only Esc reacts — the shared rule both front-ends inherit
    (start-combat g '(("test rat" 1)))
    (let ((view (make-save-menu :save)))
      (check-true "combat save page refuses"
                  (find-if (lambda (s) (search "No saving during combat" s))
                           (save-menu-lines g view)))
      (check "combat ignores slot digits" nil (save-menu-act g view #\1))
      (check "combat ignores the name key" nil (save-menu-act g view #\n))
      (check "esc still closes in combat" :closed
             (save-menu-act g view #\Escape)))
    ;; loading is not blocked by combat at the menu level (the fight is
    ;; abandoned with the old game object, like quitting to a save)
    (let ((view (make-save-menu :load)))
      (check "combat load still picks"
             '(:load "tests/tmp-saves/alpha.sav")
             (save-menu-act g view #\1))))
  (delete-file "tests/tmp-saves/alpha.sav")
  ;; the slot cap: with +MAX-SAVE-SLOTS+ slots already on disk, every
  ;; one stays reachable by its single digit only if 'n' refuses to
  ;; open a 10th — otherwise a name typed past the cap would be listed
  ;; but never pickable by number (a real bug: no cap plus a
  ;; single-digit-only picker leaves the extra slots orphaned)
  (let* ((g (new-game (load-map-file "tests/tmp-town.map")
                       :party (list (%combat-hero))))
         (view (%make-save-menu
                :mode :save
                :slots (loop for i from 1 to +max-save-slots+
                             collect (format nil "s~D" i)))))
    (check "n is refused once the slot cap is reached" nil
           (save-menu-act g view #\n))
    (check "no name entry opens at the cap" nil (save-menu-entry view))
    ;; exact, the cap read off the constant but the wording spelled out:
    ;; this line is drawn by the Amiga's font, which has no glyphs past
    ;; ASCII, so a UTF-8 dash in it would arrive as three of garbage
    (check-true "the cap message is shown"
                (member (format nil "Slot limit reached (~D) - delete a save first."
                                +max-save-slots+)
                        (menu-texts (save-menu-lines g view))
                        :test #'string=))
    ;; the cap message clears once a slot is picked
    (check "picking a slot still works at the cap"
           '(:save "tests/tmp-saves/s1.sav")
           (save-menu-act g view #\1))
    (check-true "the cap message is gone after a pick"
                (notany (lambda (s) (search "Slot limit reached" s))
                        (menu-texts (save-menu-lines g view))))))

(delete-file "tests/tmp-town.map")
(delete-file "tests/tmp-dung.map")

;;; ---------------------------------------------------------------------
;;; ILBM images (M3): the image model, ByteRun1, reader/writer round trips.

(check-error "make-image rejects zero width" (make-image 0 5 2))
(check-error "make-image rejects depth 9" (make-image 4 4 9))

(let ((img (make-image 7 5 3)))
  (check "fresh image is pen 0" 0 (pixel-ref img 6 4))
  (setf (pixel-ref img 6 4) 5)
  (check "pixel-ref reads back" 5 (pixel-ref img 6 4))
  (check "row-major neighbors untouched" 0 (pixel-ref img 5 4)))

;; ByteRun1 pack/unpack, straight on byte rows: repeats, literals,
;; run-length caps at 128, runs of exactly 2 (stay literal) and 3.
(labels ((rt (bytes label)
           (let* ((row (coerce bytes '(vector (unsigned-byte 8))))
                  (packed (coerce (%pack-byte-run1 row)
                                  '(vector (unsigned-byte 8))))
                  (out (make-array (length row)
                                   :element-type '(unsigned-byte 8))))
             (%unpack-byte-run1 packed 0 (length packed) out (length row)
                                "test")
             (check label (coerce row 'list) (coerce out 'list))
             packed)))
  (let ((packed (rt (make-list 300 :initial-element 7)
                    "ByteRun1 round-trips a 300-byte repeat")))
    (check-true "long repeat splits into 128-byte runs" (<= (length packed) 6)))
  (rt (loop for i below 200 collect (mod i 251))
      "ByteRun1 round-trips a 200-byte literal row")
  (rt (loop for i below 40 collect (if (evenp i) 1 2))
      "ByteRun1 round-trips alternating bytes")
  (rt '(9 9 3 3 3 4 4 5 5 5 5) "runs of 2 stay literal, 3+ compress")
  (rt '(1) "single-byte row")
  (check-true "truncated ByteRun1 input signals"
              (handler-case
                  (let ((out (make-array 8 :element-type '(unsigned-byte 8))))
                    (%unpack-byte-run1
                     (coerce '(200) '(vector (unsigned-byte 8)))
                     0 1 out 8 "test")
                    nil)
                (error () t))))

;; Reader/writer round trips: both compressions, pad-boundary widths,
;; depth 1 and depth 8, palette preserved.
(labels ((checker (w h depth)
           (let ((img (make-image w h depth)))
             (dotimes (y h img)
               (dotimes (x w)
                 (setf (pixel-ref img x y)
                       (mod (+ x (* 3 y)) (ash 1 depth)))))))
         (same-image (label a b)
           (check (format nil "~A: dimensions" label)
                  (list (image-width a) (image-height a) (image-depth a))
                  (list (image-width b) (image-height b) (image-depth b)))
           (check-true (format nil "~A: pixels" label)
                       (equalp (image-pixels a) (image-pixels b)))))
  (dolist (compression '(1 0))
    (dolist (dims '((7 5 2) (16 4 3) (17 3 4) (24 2 1) (33 2 8)))
      (destructuring-bind (w h depth) dims
        (let ((img (checker w h depth))
              (path "tests/tmp-img.iff"))
          (write-ilbm img path :compression compression)
          (same-image (format nil "round trip ~Dx~Dx~D cmp ~D"
                              w h depth compression)
                      img (read-ilbm path))))))
  ;; palette round trip (partial CMAP: only set entries are written)
  (let ((img (checker 8 4 2)))
    (setf (aref (image-palette img) 0) '(0 0 0)
          (aref (image-palette img) 1) '(255 255 255)
          (aref (image-palette img) 2) '(136 136 136)
          (aref (image-palette img) 3) '(255 170 51))
    (write-ilbm img "tests/tmp-img.iff")
    (check "palette survives the round trip"
           '(255 170 51)
           (aref (image-palette (read-ilbm "tests/tmp-img.iff")) 3))))

;; Unknown chunks are skipped (with odd-length padding): splice an ANNO
;; chunk between BMHD and BODY by byte surgery and re-read.
(let ((img (make-image 8 3 2)))
  (setf (pixel-ref img 2 1) 3)
  (write-ilbm img "tests/tmp-img.iff")
  (let* ((bytes (with-open-file (s "tests/tmp-img.iff"
                                   :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s)
                                       :element-type '(unsigned-byte 8))))
                    (read-sequence v s)
                    v)))
         ;; ANNO chunk, 5 data bytes -> padded to 6 on disk
         (anno (coerce (append (map 'list #'char-code "ANNO")
                               '(0 0 0 5)
                               (map 'list #'char-code "prop!")
                               '(0))
                       '(vector (unsigned-byte 8))))
         (cut (+ 12 8 20))                 ; after FORM hdr + BMHD chunk
         (spliced (concatenate '(vector (unsigned-byte 8))
                               (subseq bytes 0 cut) anno
                               (subseq bytes cut))))
    ;; fix the FORM length
    (let ((len (- (length spliced) 8)))
      (setf (aref spliced 4) (ldb (byte 8 24) len)
            (aref spliced 5) (ldb (byte 8 16) len)
            (aref spliced 6) (ldb (byte 8 8) len)
            (aref spliced 7) (ldb (byte 8 0) len)))
    (with-open-file (s "tests/tmp-img.iff" :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
      (write-sequence spliced s))
    (let ((back (read-ilbm "tests/tmp-img.iff")))
      (check "reader skips unknown ANNO chunk" 3 (pixel-ref back 2 1)))))

;; Not-an-ILBM and truncated BODY both signal clear errors.
(with-open-file (s "tests/tmp-img.iff" :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede)
  (map nil (lambda (c) (write-byte (char-code c) s)) "just some text"))
(check-error "read-ilbm rejects a non-IFF file"
  (read-ilbm "tests/tmp-img.iff"))
(let ((img (make-image 32 8 4)))
  (write-ilbm img "tests/tmp-img.iff" :compression 0)
  (let* ((bytes (with-open-file (s "tests/tmp-img.iff"
                                   :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s)
                                       :element-type '(unsigned-byte 8))))
                    (read-sequence v s)
                    v))))
    (with-open-file (s "tests/tmp-img.iff" :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
      (write-sequence (subseq bytes 0 (- (length bytes) 10)) s))
    (check-error "read-ilbm rejects a truncated BODY"
      (read-ilbm "tests/tmp-img.iff"))))

;;; The plane fold decodes a scanline's planes together, writes each pen
;;; once, and skips groups of eight pixels whose plane bytes are all
;;; zero.  These pin the paths that skip touches: a pen carried only by
;;; a high plane (the low planes' bytes for that group are zero, so the
;;; group must still be folded), a pen in the final partial group of a
;;; width that is not a multiple of eight, and an image that is almost
;;; entirely pen 0.
(dolist (compression '(1 0))
  (let ((img (make-image 20 3 3))          ; 20 wide: 3 groups, last is 4 px
        (path "tests/tmp-img.iff"))
    (setf (pixel-ref img 0 0) 4            ; plane 2 only, planes 0/1 zero
          (pixel-ref img 8 1) 1            ; plane 0 only, second group
          (pixel-ref img 19 2) 7           ; last pixel of a partial group
          (pixel-ref img 15 2) 6)          ; group boundary, planes 1+2
    (write-ilbm img path :compression compression)
    (let ((back (read-ilbm path)))
      (check-true (format nil "sparse image round trips exactly (cmp ~D)"
                          compression)
                  (equalp (image-pixels img) (image-pixels back)))
      (check (format nil "high-plane-only pen survives the zero-group skip ~
(cmp ~D)" compression)
             4 (pixel-ref back 0 0))
      (check (format nil "last pixel of a partial group decodes (cmp ~D)"
                     compression)
             7 (pixel-ref back 19 2))
      (check (format nil "untouched pens stay 0 (cmp ~D)" compression)
             0 (pixel-ref back 7 0)))))

;;; The planar reader is the same BODY decoded without folding to
;;; pens: READ-ILBM-PLANAR keeps the bitplane rows so the Amiga can
;;; poke them straight into a BitMap.  It has to agree with READ-ILBM
;;; exactly — that equivalence is the only thing making the fast path
;;; safe — so every pen is cross-checked here, on real pack art and on
;;; synthetic images whose widths straddle the row padding.
(flet ((planar-pen (img x y)
         ;; the pen at (X,Y), reassembled from the plane bits
         (let ((row-bytes (planar-image-row-bytes img))
               (pen 0))
           (dotimes (p (planar-image-depth img) pen)
             (let ((byte (aref (planar-image-plane img p)
                               (+ (* y row-bytes) (ash x -3)))))
               (when (logbitp (- 7 (logand x 7)) byte)
                 (setf pen (logior pen (ash 1 p)))))))))
  (dolist (compression '(1 0))
    (dolist (dims '((7 5 2) (16 4 3) (17 3 4) (20 3 3) (24 2 1) (33 2 8)))
      (destructuring-bind (w h depth) dims
        (let ((img (make-image w h depth))
              (path "tests/tmp-img.iff"))
          ;; a pattern with both dense and empty regions
          (dotimes (y h)
            (dotimes (x w)
              (setf (pixel-ref img x y)
                    (if (zerop (mod (+ x y) 3))
                        0
                        (mod (+ x (* 3 y)) (ash 1 depth))))))
          (write-ilbm img path :compression compression)
          (let ((chunky (read-ilbm path))
                (planar (read-ilbm-planar path))
                (bad '()))
            (check (format nil "planar geometry ~Dx~Dx~D cmp ~D"
                           w h depth compression)
                   (list w h depth)
                   (list (planar-image-width planar)
                         (planar-image-height planar)
                         (planar-image-depth planar)))
            (dotimes (y h)
              (dotimes (x w)
                (unless (= (pixel-ref chunky x y) (planar-pen planar x y))
                  (push (list x y) bad))))
            (check (format nil "planar pens match chunky ~Dx~Dx~D cmp ~D"
                           w h depth compression)
                   nil bad))))))
  ;; and on the shipped pack art, where the real widths and run
  ;; patterns live
  (dolist (file '("front-0.iff" "side-0-l.iff" "flank-3-r.iff"
                  "ceiling.iff" "floor.iff"))
    (let* ((path (engine-path (concatenate 'string "data/gfx/" file)))
           (chunky (read-ilbm path))
           (planar (read-ilbm-planar path))
           (bad 0))
      (dotimes (y (image-height chunky))
        (dotimes (x (image-width chunky))
          (unless (= (pixel-ref chunky x y) (planar-pen planar x y))
            (incf bad))))
      (check (format nil "planar ~A matches chunky pen for pen" file)
             0 bad)
      (check (format nil "planar ~A carries the palette" file)
             (aref (image-palette chunky) 1)
             (aref (planar-image-palette planar) 1)))))

;; The mask shortcut: for the usual pen-0 key the cookie-cut mask is
;; just the OR of the planes, which must equal what MASK-BYTES derives
;; from chunky pens.
(dolist (file '("front-0.iff" "side-0-l.iff" "ceiling.iff"))
  (let* ((path (engine-path (concatenate 'string "data/gfx/" file)))
         (chunky (read-ilbm path))
         (planar (read-ilbm-planar path)))
    (multiple-value-bind (want want-bpr)
        (mask-bytes (image-width chunky) (image-height chunky)
                    (image-pixels chunky))
      (multiple-value-bind (got got-bpr) (planar-mask-bytes planar)
        (check (format nil "planar mask row width matches for ~A" file)
               want-bpr got-bpr)
        (check-true (format nil "planar mask matches MASK-BYTES for ~A" file)
                    (equalp want got))))
    (check (format nil "planar transparency agrees for ~A" file)
           (not (null (image-transparent-p chunky)))
           (not (null (planar-image-transparent-p planar))))))

;; A non-zero transparent key has no OR shortcut — say so rather than
;; return a wrong mask.
(let ((planar (read-ilbm-planar (engine-path "data/gfx/front-0.iff"))))
  (check "planar mask declines a non-zero key" nil
         (planar-mask-bytes planar 3)))

;; Interleaved mask planes (masking = mskHasMask): the BODY carries one
;; extra plane row per scanline that the chunky reader must decode past
;; and the planar reader must never gather.  WRITE-ILBM never emits
;; masks, so build one by surgery: rebuild the BODY with an #xAA mask
;; row after each scanline's plane rows, patch the BMHD masking and
;; compression bytes and both lengths, then cross-check both readers
;; against the original pens — for cmpNone and cmpByteRun1.
(dolist (compression '(0 1))
  (let* ((w 20) (h 3) (depth 3)
         (img (make-image w h depth))
         (row-bytes (%row-bytes w))
         (path "tests/tmp-img.iff"))
    (dotimes (y h)
      (dotimes (x w)
        (setf (pixel-ref img x y) (mod (+ x (* 3 y)) (ash 1 depth)))))
    (write-ilbm img path :compression 0)
    (let* ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                    (let ((v (make-array (file-length s)
                                         :element-type '(unsigned-byte 8))))
                      (read-sequence v s)
                      v)))
           (body-id (search (map 'vector #'char-code "BODY") bytes))
           ;; the new BODY: per scanline DEPTH plane rows + one mask row
           (body '()))
      (dotimes (y h)
        (dolist (row (append (%chunky-row-to-planes img y row-bytes)
                             (list (make-array row-bytes
                                               :element-type '(unsigned-byte 8)
                                               :initial-element #xAA))))
          (setf body (append body
                             (if (= compression 1)
                                 (%pack-byte-run1 row)
                                 (coerce row 'list))))))
      (let* ((blen (length body))
             (head (subseq bytes 0 (+ body-id 4)))
             (masked (concatenate '(vector (unsigned-byte 8))
                                  head
                                  (list (ldb (byte 8 24) blen)
                                        (ldb (byte 8 16) blen)
                                        (ldb (byte 8 8) blen)
                                        (ldb (byte 8 0) blen))
                                  body
                                  (if (oddp blen) '(0) '()))))
        ;; BMHD data starts at 20 (first chunk after the FORM header):
        ;; masking at +9, compression at +10; FORM length at 4.
        (setf (aref masked 29) 1
              (aref masked 30) compression)
        (let ((flen (- (length masked) 8)))
          (setf (aref masked 4) (ldb (byte 8 24) flen)
                (aref masked 5) (ldb (byte 8 16) flen)
                (aref masked 6) (ldb (byte 8 8) flen)
                (aref masked 7) (ldb (byte 8 0) flen)))
        (with-open-file (s path :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
          (write-sequence masked s))
        (let ((chunky (read-ilbm path))
              (planar (read-ilbm-planar path))
              (bad 0))
          (dotimes (y h)
            (dotimes (x w)
              (let ((want (pixel-ref img x y)))
                (unless (= want (pixel-ref chunky x y))
                  (incf bad))
                (let ((pen 0))
                  (dotimes (p depth)
                    (when (logbitp (- 7 (logand x 7))
                                   (aref (planar-image-plane planar p)
                                         (+ (* y (planar-image-row-bytes planar))
                                            (ash x -3))))
                      (setf pen (logior pen (ash 1 p)))))
                  (unless (= want pen)
                    (incf bad))))))
          (check (format nil "interleaved mask plane is skipped (cmp ~D)"
                         compression)
                 0 bad))))))

;; A short BODY signals cleanly on the planar path too (the whole chunk
;; is decoded in one piece now — the length checks must still fire).
;; Shrink the BODY chunk itself (patching its length and the FORM's) so
;; the failure lands in the decode, not the outer runs-past-EOF check.
(dolist (compression '(1 0))
  (let ((img (make-image 32 8 4))
        (path "tests/tmp-img.iff"))
    (dotimes (x 32) (setf (pixel-ref img x 3) (mod x 16)))
    (write-ilbm img path :compression compression)
    (let* ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                    (let ((v (make-array (file-length s)
                                         :element-type '(unsigned-byte 8))))
                      (read-sequence v s)
                      v)))
           (body-id (search (map 'vector #'char-code "BODY") bytes))
           (new-len (- (%u32be bytes (+ body-id 4)) 10))
           (trimmed (subseq bytes 0 (+ body-id 8 new-len)))
           (flen (- (length trimmed) 8)))
      (setf (aref trimmed (+ body-id 4)) (ldb (byte 8 24) new-len)
            (aref trimmed (+ body-id 5)) (ldb (byte 8 16) new-len)
            (aref trimmed (+ body-id 6)) (ldb (byte 8 8) new-len)
            (aref trimmed (+ body-id 7)) (ldb (byte 8 0) new-len)
            (aref trimmed 4) (ldb (byte 8 24) flen)
            (aref trimmed 5) (ldb (byte 8 16) flen)
            (aref trimmed 6) (ldb (byte 8 8) flen)
            (aref trimmed 7) (ldb (byte 8 0) flen))
      (with-open-file (s path :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
        (write-sequence trimmed s))
      (check-error (format nil "read-ilbm-planar rejects a short BODY ~
(cmp ~D)" compression)
        (read-ilbm-planar path)))))

;; A second BODY would decode against already-written pens, so it is
;; rejected rather than blended: duplicate the BODY chunk by surgery.
(let ((img (make-image 8 2 2)))
  (setf (pixel-ref img 1 1) 3)
  (write-ilbm img "tests/tmp-img.iff")
  (let* ((bytes (with-open-file (s "tests/tmp-img.iff"
                                   :element-type '(unsigned-byte 8))
                  (let ((v (make-array (file-length s)
                                       :element-type '(unsigned-byte 8))))
                    (read-sequence v s)
                    v)))
         (body (search (map 'vector #'char-code "BODY") bytes))
         (doubled (concatenate '(vector (unsigned-byte 8))
                               bytes (subseq bytes body))))
    (let ((len (- (length doubled) 8)))
      (setf (aref doubled 4) (ldb (byte 8 24) len)
            (aref doubled 5) (ldb (byte 8 16) len)
            (aref doubled 6) (ldb (byte 8 8) len)
            (aref doubled 7) (ldb (byte 8 0) len)))
    (with-open-file (s "tests/tmp-img.iff" :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
      (write-sequence doubled s))
    (check-error "read-ilbm rejects a second BODY chunk"
      (read-ilbm "tests/tmp-img.iff"))))
(delete-file "tests/tmp-img.iff")

;;; ---------------------------------------------------------------------
;;; Animation frames: an image may ship -f1/-f2/... files beside it —
;;; the wall pieces' -vN convention applied to time.  The naming, the
;;; probe (stops at the first gap) and PLANAR-DIFF-RECT, the dirty
;;; rectangle the Amiga stepper re-blits per step.

(check "frame file naming" "gfx/mon-kobold-f1.iff"
       (image-frame-file "gfx/mon-kobold.iff" 1))
(check "frame file naming counts past 9" "gfx/mon-kobold-f12.iff"
       (image-frame-file "gfx/mon-kobold.iff" 12))
(check "frame file naming keys on the LAST dot" "a.b/img-f1.iff"
       (image-frame-file "a.b/img.iff" 1))
(check "frame file naming without a suffix" "portrait-f1"
       (image-frame-file "portrait" 1))

;; the probe stops at the first missing frame: an orphaned -f3 with no
;; -f2 before it is dead data, not frame 3
(let ((img (make-image 16 8 2)))
  (write-ilbm img "tests/tmp-anim.iff")
  (check "a still image has no frames" '()
         (image-frame-files "tests/tmp-anim.iff"))
  (write-ilbm img "tests/tmp-anim-f1.iff")
  (write-ilbm img "tests/tmp-anim-f3.iff")
  (check "the probe stops at the gap" '("tests/tmp-anim-f1.iff")
         (image-frame-files "tests/tmp-anim.iff"))
  (write-ilbm img "tests/tmp-anim-f2.iff")
  (check "the full frame set, in frame order"
         '("tests/tmp-anim-f1.iff" "tests/tmp-anim-f2.iff"
           "tests/tmp-anim-f3.iff")
         (image-frame-files "tests/tmp-anim.iff"))
  (delete-file "tests/tmp-anim.iff")
  (delete-file "tests/tmp-anim-f1.iff")
  (delete-file "tests/tmp-anim-f2.iff")
  (delete-file "tests/tmp-anim-f3.iff"))

;; PLANAR-DIFF-RECT bounds the differing pixels, byte-aligned in x (it
;; compares the packed plane bytes) and clamped to the width.  Frames
;; go through a WRITE-ILBM round trip like the real files.
(flet ((planar (img)
         (write-ilbm img "tests/tmp-anim.iff")
         (prog1 (read-ilbm-planar "tests/tmp-anim.iff")
           (delete-file "tests/tmp-anim.iff"))))
  (let ((base (make-image 24 8 3)))
    (setf (pixel-ref base 2 2) 5)       ; some base ink
    (let ((a (planar base))
          (same (planar base)))
      (check "identical frames have no dirty rect" nil
             (planar-diff-rect a same)))
    ;; one pixel at (9,3), pen 6 — differs on planes 1 and 2, byte
    ;; column 1, so the box is the second byte of row 3
    (let ((b (make-image 24 8 3)))
      (setf (pixel-ref b 2 2) 5
            (pixel-ref b 9 3) 6)
      (check "a single pixel: its byte column, its row" '(8 3 8 1)
             (planar-diff-rect (planar base) (planar b))))
    ;; opposite corners span the whole image
    (let ((b (make-image 24 8 3)))
      (setf (pixel-ref b 2 2) 5
            (pixel-ref b 0 0) 1
            (pixel-ref b 23 7) 1)
      (check "corner-to-corner spans the image" '(0 0 24 8)
             (planar-diff-rect (planar base) (planar b)))))
  ;; x+w clamps to the width, not the padded row (20px pads to 32)
  (let ((a (make-image 20 4 2))
        (b (make-image 20 4 2)))
    (setf (pixel-ref b 19 1) 3)
    (check "the box clamps to the width, not the row padding"
           '(16 1 4 1)
           (planar-diff-rect (planar a) (planar b))))
  ;; mismatched geometry is a broken pack, not frame data
  (check-error "diff rejects mismatched geometry"
    (planar-diff-rect (planar (make-image 16 8 2))
                      (planar (make-image 16 4 2))))
  ;; and says so in ASCII: a pack this broken is as likely to be found
  ;; on the Amiga as on the host, and that font draws nothing else
  (check-true "and says both geometries, in ASCII"
              (let ((said (handler-case
                              (planar-diff-rect (planar (make-image 16 8 2))
                                                (planar (make-image 16 4 2)))
                            (error (e) (princ-to-string e)))))
                (and (search "16x8x2 vs 16x4x2" said)
                     (every (lambda (c) (< (char-code c) 128)) said)))))

;; %RECT-UNION grows the box over a whole frame set
(check "rect union bounds both boxes" '(0 0 16 16)
       (%rect-union '(0 0 4 4) '(8 8 8 8)))
(check "rect union with nothing yet" '(3 4 5 6)
       (%rect-union nil '(3 4 5 6)))
(check "rect union of nothing" nil (%rect-union nil nil))

;; The placeholder art tools draw the frames a pack generator writes
;; under the -fN names: frame 1 must differ from frame 0 (or nothing
;; animates) at unchanged geometry (or the loader rejects the file).
(load "tools/gen-walls.lisp")
(let ((f0 (draw-effect-icon :flame))
      (f1 (draw-effect-icon :flame 1)))
  (check "flame frames share geometry" (list (image-width f0)
                                             (image-height f0)
                                             (image-depth f0))
         (list (image-width f1) (image-height f1) (image-depth f1)))
  (check-true "flame frame 1 flickers"
              (not (equalp (image-pixels f0) (image-pixels f1)))))
(check "the compass icon holds still whatever the frame says" t
       (equalp (image-pixels (draw-effect-icon :compass))
               (image-pixels (draw-effect-icon :compass 1))))
(dolist (style '(:beast :goblin :undead :brigand :plain))
  (let ((f0 (draw-monster-portrait style 64 64))
        (f1 (draw-monster-portrait style 64 64 1)))
    (check (format nil "monster ~A frames share geometry" style)
           (list (image-width f0) (image-height f0))
           (list (image-width f1) (image-height f1)))
    (check-true (format nil "monster ~A frame 1 pulses" style)
                (not (equalp (image-pixels f0) (image-pixels f1))))))

;;; ---------------------------------------------------------------------
;;; Pointer sprites: an image becomes hardware-sprite plane words, the
;;; hot spot is the topmost-leftmost inked pixel, and the built-in
;;; hand pointer honors both.  (The SetPointer glue is Amiga-only —
;;; see the amiga-ui pointer tests below.)

(let ((img (make-image 8 3 2)))
  (setf (pixel-ref img 1 0) 1           ; low plane
        (pixel-ref img 2 0) 2           ; high plane
        (pixel-ref img 3 0) 3)          ; both planes
  (check "pointer rows: pens split onto the two sprite planes"
         '((#x5000 #x3000) (0 0) (0 0))
         (pointer-sprite-rows img))
  (check "pointer hotspot: leftmost inked pixel of the topmost row"
         '(1 0)
         (multiple-value-bind (x y) (pointer-hotspot img) (list x y))))
(check "pointer hotspot of an empty image is the corner" '(0 0)
       (multiple-value-bind (x y) (pointer-hotspot (make-image 4 4 2))
         (list x y)))
(check-error "pointer rows reject an image wider than a sprite"
  (pointer-sprite-rows (make-image 17 2 2)))
(let ((img (make-image 8 2 3)))
  (setf (pixel-ref img 0 0) 4)
  (check-error "pointer rows reject pens above 3"
    (pointer-sprite-rows img)))

(let ((hand (hand-pointer-image)))
  (check "hand pointer is sprite-wide" 16 (image-width hand))
  (check "hand pointer converts row for row"
         (length *hand-pointer-art*)
         (length (pointer-sprite-rows hand)))
  (check "hand hotspot sits on the finger tip" '(7 0)
         (multiple-value-bind (x y) (pointer-hotspot hand) (list x y)))
  ;; The fingers must stand well clear of the palm — the first version
  ;; drew four four-row stubs and read as a mitten, not a hand.  The
  ;; palm is the first row of unbroken skin; everything above it is
  ;; fingers.
  (check "hand fingers are at least nine rows long" t
         (>= (or (position-if (lambda (row) (search "11111111" row))
                              *hand-pointer-art*)
                 0)
             9))
  (check "hand palette holds the three sprite colors"
         *hand-pointer-colors*
         (list (aref (image-palette hand) 1)
               (aref (image-palette hand) 2)
               (aref (image-palette hand) 3))))

(let ((point (point-pointer-image)))
  (check "point pointer is sprite-wide" 16 (image-width point))
  (check "point pointer converts row for row"
         (length *point-pointer-art*)
         (length (pointer-sprite-rows point)))
  (check "point hotspot sits on the finger tip" '(4 0)
         (multiple-value-bind (x y) (pointer-hotspot point) (list x y)))
  (check "point pointer shares the sprite colors"
         *hand-pointer-colors*
         (list (aref (image-palette point) 1)
               (aref (image-palette point) 2)
               (aref (image-palette point) 3))))

;; The four move-zone arrows honor the same sprite contract: 16 wide,
;; one plane-word pair per art row, the shared sprite palette, and the
;; hot spot on the topmost row of the arrow.
(dolist (entry (list (list "forward" (forward-pointer-image)
                           *forward-pointer-art* '(7 0))
                     (list "back" (back-pointer-image)
                           *back-pointer-art* '(5 0))
                     (list "turn-left" (turn-left-pointer-image)
                           *turn-left-pointer-art* '(5 0))
                     (list "turn-right" (turn-right-pointer-image)
                           *turn-right-pointer-art* '(9 0))))
  (destructuring-bind (name img art spot) entry
    (check (format nil "~A arrow is sprite-wide" name) 16
           (image-width img))
    (check (format nil "~A arrow converts row for row" name)
           (length art) (length (pointer-sprite-rows img)))
    (check (format nil "~A arrow shares the sprite colors" name)
           *hand-pointer-colors*
           (list (aref (image-palette img) 1)
                 (aref (image-palette img) 2)
                 (aref (image-palette img) 3)))
    (check (format nil "~A arrow hotspot sits on the art" name) spot
           (multiple-value-bind (x y) (pointer-hotspot img)
             (list x y)))))

;; The busy hourglass draws glass and sand in separate pens — frame on
;; the high sprite plane, sand on the low one — and leaves part of the
;; glass empty, rather than a solid one-color silhouette.
(let* ((busy (busy-pointer-image))
       (rows (pointer-sprite-rows busy)))
  (check "busy pointer is sprite-wide" 16 (image-width busy))
  (check "busy pointer converts row for row"
         (length *busy-pointer-art*) (length rows))
  (check "busy pointer shares the sprite colors"
         *hand-pointer-colors*
         (list (aref (image-palette busy) 1)
               (aref (image-palette busy) 2)
               (aref (image-palette busy) 3)))
  (check-true "busy pointer inks sand in the sand pen"
              (some (lambda (row) (plusp (first row))) rows))
  (check-true "busy pointer inks the frame in the frame pen"
              (some (lambda (row) (plusp (second row))) rows))
  (check-true "busy pointer leaves empty glass between frame and sand"
              (some (lambda (row-art) (search ".1" row-art))
                    *busy-pointer-art*)))

;; The hover state machine behind the pointer swap: over a move zone
;; its directional arrow, over any other hotspot the finger, elsewhere
;; the hand.  *BUSY-POINTER-ACTIVE* suppresses the SetPointer call, so
;; the pure state transitions run without a display; the busy
;; bracket's unwind applies the pending state.  (amiga-ui.lisp only
;; loads on the Amiga — see also the on-screen hover checks in the
;; game-window test below.)
#+amigaos
(let ((*hotspots* '((30 30 40 40 #\w :forward)
                    (10 10 20 20 #\w)))
      (*pointer-hot* nil)
      (*busy-pointer-active* t))
  (%track-pointer-hot nil 15 15)
  (check "hover onto a click target arms the pointing finger" :point
         *pointer-hot*)
  (%track-pointer-hot nil 15 15)
  (check "resting on the target keeps the state" :point *pointer-hot*)
  (%track-pointer-hot nil 35 35)
  (check "hover onto a move zone arms its directional arrow" :forward
         *pointer-hot*)
  (%track-pointer-hot nil 5 5)
  (check "hover off the target goes back to the hand" nil
         *pointer-hot*)
  (let ((*hotspots* '()))
    (%track-pointer-hot nil 15 15)
    (check "a redraw that dropped the targets keeps the hand" nil
           *pointer-hot*)))

;; The background-music readiness gate.  A campaign that starts at its
;; guild is inside a :MUSIC location from NEW-GAME's specials — before
;; the display opens, and before the UNWIND-PROTECT that gives channel
;; and chip RAM back is standing.  A tune begun there would play to a
;; blank screen through the tile-pack load and leak its channel if the
;; display then failed to open, so nothing sounds until the session
;; raises the flag.  (No display and no audio.device are needed for
;; these: the gate turns the calls into no-ops well before either.)
#+amigaos
(let ((*amiga-music-ready* nil)
      (*amiga-music* nil))
  (check "an ungated tune does not play" nil
         (amiga-music-play "worlds/nonesuch/theme.8svx"))
  (check "and loads nothing to leak" nil *amiga-music*)
  ;; a missing file is silence, not an error — the cue layer's rule —
  ;; and the state it leaves keeps the session from re-reading it
  (setf *amiga-music-ready* t)
  (check "a tune the disk does not hold stays silent" nil
         (amiga-music-play "worlds/nonesuch/theme.8svx"))
  (check-true "but the failed read is remembered" *amiga-music*)
  (check "the file it failed on names the state"
         "worlds/nonesuch/theme.8svx" (amiga-music-file *amiga-music*))
  (check "with no channel open there is nothing to stop" nil
         (amiga-music-stop))
  ;; closing ends the session's music: the flag drops with it, so a
  ;; later tune waits for a fresh session to raise it again
  (check "closing lowers the readiness" nil (amiga-music-close))
  (check "and drops the loaded tune" nil *amiga-music*)
  (check "a tune after the close stays silent" nil
         (amiga-music-play "worlds/nonesuch/theme.8svx")))

;;; ---------------------------------------------------------------------
;;; Wall-art assets (M3): the checked-in tile packs — one per display
;;; profile — must match what the generator draws today, pixel for
;;; pixel, so art and code can never drift apart.  (Regenerate with
;;; `make assets` after changing tools/gen-walls.lisp.)

(load "tools/gen-walls.lisp")

(check "wall-piece-file name" "side-door-2-l.iff"
       (wall-piece-file '(:side-door 2 :l)))

;; one checked-in pack per display profile, each pinned to the
;; generator at that profile's viewport
(dolist (profile *display-profiles*)
  (with-display-profile (profile)
    (let ((planes (view-planes *fp-view-width* *fp-view-height*))
          (dir *gfx-dir*)
          (stale '()))
      (dolist (piece (wall-piece-names))
        (let ((file (concatenate 'string dir (wall-piece-file piece))))
          (if (not (probe-file file))
              (push (list piece :missing) stale)
              (let ((disk (read-ilbm file))
                    (drawn (draw-wall-piece piece planes)))
                (destructuring-bind (x y w h) (wall-piece-rect planes piece)
                  (declare (ignore x y))
                  (unless (and (= (image-width disk) w)
                               (= (image-height disk) h))
                    (push (list piece :wrong-size) stale)))
                (unless (equalp (image-pixels disk) (image-pixels drawn))
                  (push (list piece :differs) stale))))))
      (check (format nil "all 40 ~A assets exist and match the generator"
                     dir)
             nil stale)
      ;; the demo ceiling/floor backdrops are pinned the same way
      (let ((stale '()))
        (loop for key in '(:ceiling :floor)
              for name in '("ceiling.iff" "floor.iff")
              for rect in (backdrop-rects planes)
              do (let ((file (concatenate 'string dir name)))
                   (if (not (probe-file file))
                       (push (list key :missing) stale)
                       (let ((disk (read-ilbm file))
                             (drawn (draw-backdrop-piece key planes)))
                         (unless (and (= (image-width disk) (third rect))
                                      (= (image-height disk)
                                         (fourth rect)))
                           (push (list key :wrong-size) stale))
                         (unless (equalp (image-pixels disk)
                                         (image-pixels drawn))
                           (push (list key :differs) stale))))))
        (check (format nil "the ~A backdrop assets match the generator"
                       dir)
               nil stale))
      ;; the reader restores the dungeon palette from the CMAP
      (check (format nil "~A palette carries the dungeon colors" dir)
             (coerce *wall-palette* 'list)
             (coerce (image-palette
                      (read-ilbm (concatenate 'string dir
                                              (wall-piece-file
                                               '(:front 0)))))
                     'list)))))

;; Flank pieces are the same flat wall as the front piece at their
;; depth, continued through the open side — their mortar joints must
;; land on the FRONT slot's brick grid (bond offsets 0 / brick/2),
;; carried across the seam via the pattern window, not on a grid
;; scaled to the flank's own narrow slot (which showed a flat wall
;; with three-times-smaller bricks on its adjacent segments).
(let ((planes (view-planes *fp-view-width* *fp-view-height*)))
  (dotimes (d 2)
    (destructuring-bind (fx fy fw fh)
        (wall-piece-rect planes (list :front d))
      (declare (ignore fx fy fh))
      (let ((brick (max 6 (round fw 5))))
        (dolist (side '(:l :r))
          (let* ((img (draw-wall-piece (list :flank d side) planes))
                 (w (image-width img))
                 (x0 (if (eq side :l) (- w) fw))  ; pattern window origin
                 (joints '()))
            ;; probe a row inside the first brick course (row 0 is the
            ;; white edge highlight)
            (dotimes (x w)
              (when (= 4 (pixel-ref img x 2)) (push x joints)))
            (check-true
             (format nil "depth-~D ~A flank joints sit on the front ~
brick grid" d side)
             (and joints
                  (every (lambda (x)
                           (member (mod (+ x0 x) brick)
                                   (list 0 (floor brick 2))))
                         joints)))))))))

;; Effects-band icons: the generator draws 16x16 pen-0-keyed art, and
;; the fixture world's checked-in fx-needle.iff is pinned to it the
;; same way as the packs (regenerate with `make assets`).
(dolist (kind '(:compass :flame :shield))
  (let ((img (draw-effect-icon kind)))
    (check (format nil "the ~A icon is 16x16" kind)
           (list *effect-icon-size* *effect-icon-size*)
           (list (image-width img) (image-height img)))
    (check-true (format nil "the ~A icon keeps the transparent key" kind)
                (image-transparent-p img))))
(let ((disk (read-ilbm "tests/world/fx-needle.iff"))
      (drawn (draw-effect-icon :compass)))
  (check-true "the fixture icon matches its generator"
              (equalp (image-pixels disk) (image-pixels drawn))))

;; Effect icons resolve map-relative, like zone tile packs.
(let* ((m (load-map-file "tests/world/keep.map"))
       (wanda (make-hero "W" :w-wizard))
       (g (new-game m :party (list wanda))))
  (check-true "the wizard casts the fixture compass"
              (cast-spell g wanda 'w-compass))
  (let ((e (find-effect g "w compass")))
    (check "the cast effect carries the campaign's image"
           "fx-needle.iff" (effect-image e))
    (check "the image resolves next to the map file"
           "tests/world/fx-needle.iff" (effect-image-path g e))
    (check-true "the resolved icon file exists"
                (probe-file (effect-image-path g e))))
  (add-effect g "plain" :payload '(:light t))
  (check "an imageless effect has no path" nil
         (effect-image-path g (find-effect g "plain")))
  (add-effect g "abs" :payload '(:light t) :image "/elsewhere/x.iff")
  (check "an absolute image path passes through" "/elsewhere/x.iff"
         (effect-image-path g (find-effect g "abs"))))

;; Takeover art: location scenes and portraits draw to the ordered
;; size and stay within the fixed UI pens 0-3 (black, white, grey,
;; amber) — a pack may only recolor pens 4+, so pictures painted with
;; higher pens would change color under foreign packs.
(dolist (kind '(:shop :tavern :temple :energy :hut))
  (let ((img (draw-location-scene kind 60 44))
        (maxpen 0))
    (check (format nil "the ~A scene sizes to order" kind) '(60 44)
           (list (image-width img) (image-height img)))
    (dotimes (y 44)
      (dotimes (x 60)
        (setf maxpen (max maxpen (pixel-ref img x y)))))
    (check-true (format nil "the ~A scene keeps to the UI pens" kind)
                (<= maxpen 3))))
(dolist (style '(:helm :crest :hood :cap :hat :plain))
  (let ((img (draw-portrait style)))
    (check (format nil "the ~A portrait is the standard size" style)
           (list *portrait-size* *portrait-size*)
           (list (image-width img) (image-height img)))))

;; Monster portraits (DEFINE-MONSTER :IMAGE): the same bust size and
;; the same four UI pens, at any size the caller asks for — a game
;; draws them at its viewport size so the enemy fills the view column.
(dolist (style '(:beast :goblin :undead :brigand :plain))
  (let ((img (draw-monster-portrait style)))
    (check (format nil "the ~A monster portrait is the standard size" style)
           (list *portrait-size* *portrait-size*)
           (list (image-width img) (image-height img)))
    (let ((maxpen 0))
      (dotimes (y (image-height img))
        (dotimes (x (image-width img))
          (setf maxpen (max maxpen (pixel-ref img x y)))))
      (check-true (format nil "the ~A monster portrait keeps to the UI pens"
                          style)
                  (<= maxpen 3)))))
(let ((img (draw-monster-portrait :beast 96 72)))
  (check "a monster portrait takes the size it is given" '(96 72)
         (list (image-width img) (image-height img))))
;; The eyes go on after the headgear, so no style buries them: the
;; brigand's hood shadow used to be painted over them and left a blank
;; face.  Amber (pen 3) burns in every hide, black (pen 0) in the
;; skull's bone.  Coordinates as DRAW-MONSTER-PORTRAIT computes them.
(let* ((w *portrait-size*)
       (h *portrait-size*)
       (cx (floor w 2))
       (cy (floor (* 2 h) 5))
       (rx (floor w 6))
       (ry (floor h 5))
       (ex (floor rx 2))
       (ey (floor ry 4)))
  (dolist (style '(:beast :goblin :undead :brigand :plain))
    (let ((img (draw-monster-portrait style))
          (pen (if (eq style :undead) 0 3)))
      (check (format nil "the ~A's left eye shows" style) pen
             (pixel-ref img (- cx ex) (- cy ey)))
      (check (format nil "the ~A's right eye shows" style) pen
             (pixel-ref img (+ cx ex) (- cy ey))))))

;; Transparency contract: receding side pieces keep pen-0 corners so the
;; backdrop shows through the cookie-cut blit; front/flank pieces fill
;; their whole rect (opaque), drawing black as the mortar pen, not pen 0.
(let ((planes (view-planes *fp-view-width* *fp-view-height*)))
  (check-true "wall pieces are depth 3"
              (= 3 (image-depth (draw-wall-piece '(:front 0) planes))))
  (check-true "a side piece leaves transparent corners"
              (image-transparent-p (draw-wall-piece '(:side 0 :l) planes)))
  (check "a side piece's far top corner is transparent" +pen-bg+
         (let ((img (draw-wall-piece '(:side 0 :l) planes)))
           (pixel-ref img (1- (image-width img)) 0)))
  (check-true "a front piece is fully opaque"
              (not (image-transparent-p (draw-wall-piece '(:front 0) planes))))
  (check-true "a flank piece is fully opaque"
              (not (image-transparent-p
                    (draw-wall-piece '(:flank 0 :l) planes))))
  (check-true "opaque black is the mortar pen (4), never pen 0"
              (and (find +pen-mortar+
                         (image-pixels (draw-wall-piece '(:front 0) planes)))
                   (not (image-transparent-p
                         (draw-wall-piece '(:front 0) planes))))))

;; Backdrops: the ceiling is solid distance bands split at the
;; perspective-plane rows (lores planes top at rows 0/22/37, horizon
;; between 55 and 56), darkening toward the horizon — the wall blits
;; go on top, so each band lines up with the corridor cell at its
;; depth.  The floor is one flat color, no distance shading.
(with-display-profile (:lores)
  (let* ((planes (view-planes *fp-view-width* *fp-view-height*))
         (ceiling (draw-backdrop-piece :ceiling planes))
         (floor (draw-backdrop-piece :floor planes)))
    (check "ceiling bands read near/mid/far down the slot"
           (list +pen-dim+ +pen-dark+ +pen-bg+)
           (list (pixel-ref ceiling 0 0)      ; band 0: rows 0-21
                 (pixel-ref ceiling 0 22)     ; band 1: rows 22-36
                 (pixel-ref ceiling 0 37)))   ; band 2: rows 37-horizon
    (check "the floor is one flat color edge to edge"
           (list +pen-mid+)
           (let ((pens '()))
             (dotimes (y (image-height floor))
               (dotimes (x (image-width floor))
                 (pushnew (pixel-ref floor x y) pens)))
             pens))
    (check "the far ceiling band reaches the horizon"
           +pen-bg+
           (pixel-ref ceiling 0 (1- (image-height ceiling))))))

;;; ---------------------------------------------------------------------
;;; Art-driven tile packs (tools/gen-pack-from-art.lisp): the same 40
;;; slots, derived from one hand-drawn facade instead of procedural
;;; brick.  These tests pin the whole chain — deep-ILBM reading,
;;; resampling, quantization, the pen contract, and the end-to-end
;;; pack — against a synthetic source, so nothing here depends on art
;;; that isn't checked in.

(load "tools/gen-pack-from-art.lisp")
(load "tools/preview-view.lisp")

;; A 16x16 source with four flat quadrants — enough distinct color for
;; the quantizer to have something to do, few enough that every mapping
;; is predictable.  *TEST-ART-2* is a second, disjoint look, so variant
;; packs can be told apart piece by piece.
(defun %test-quadrants (a b c d)
  (let ((img (%make-rgb 16 16)))
    (dotimes (y 16 img)
      (dotimes (x 16)
        (setf (rgb-ref img x y)
              (cond ((and (< x 8) (< y 8)) a)
                    ((< y 8) b)
                    ((< x 8) c)
                    (t d)))))))

(defparameter *test-art* (%test-quadrants '(200 0 0) '(0 200 0)
                                          '(0 0 200) '(0 0 0)))
(defparameter *test-art-2* (%test-quadrants '(200 200 0) '(0 200 200)
                                            '(200 0 200) '(0 0 0)))

;; Deep-ILBM reading, both compressions — the riskiest code in the tool
;; (an off-by-one in the plane fold silently shifts every color).
(dolist (compression '(0 1))
  (let ((path (format nil "tests/tmp-art-~D.iff" compression)))
    (write-deep-ilbm *test-art* path :compression compression)
    (check (format nil "deep ILBM depth is reported without decoding ~
(compression ~D)" compression)
           24 (%ilbm-depth path))
    (let ((back (read-art path)))
      (check (format nil "deep ILBM round-trips its geometry ~
(compression ~D)" compression)
             (list 16 16)
             (list (rgb-image-width back) (rgb-image-height back)))
      (check (format nil "deep ILBM round-trips every quadrant ~
(compression ~D)" compression)
             '((200 0 0) (0 200 0) (0 0 200) (0 0 0))
             (mapcar (lambda (p)
                       (multiple-value-list
                        (rgb-ref back (first p) (second p))))
                     '((0 0) (12 0) (0 12) (12 12)))))
    (delete-file path)))

;; An indexed source goes through the same door, expanded via its CMAP.
(let ((path "tests/tmp-art-indexed.iff")
      (img (make-image 4 4 2 :palette #((10 20 30) (40 50 60)
                                        (70 80 90) (0 0 0)))))
  (setf (pixel-ref img 0 0) 1
        (pixel-ref img 3 3) 2)
  (write-ilbm img path)
  (let ((back (read-art path)))
    (check "an indexed source expands through its CMAP"
           '((40 50 60) (70 80 90) (10 20 30))
           (list (multiple-value-list (rgb-ref back 0 0))
                 (multiple-value-list (rgb-ref back 3 3))
                 (multiple-value-list (rgb-ref back 1 1)))))
  (delete-file path))

;; PPM (P6) is the bridge for art that never was an IFF; READ-ART
;; sniffs the magic rather than trusting the file name.
(let ((path "tests/tmp-art.ppm"))
  (with-open-file (s path :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (flet ((str (text) (map nil (lambda (c) (write-byte (char-code c) s))
                            text)))
      (str "P6")
      (str (format nil "~%# written by the test suite~%2 2~%255~%"))
      (dolist (b '(255 0 0  0 255 0
                   0 0 255  9 9 9))
        (write-byte b s))))
  (let ((img (read-art path)))
    (check "a PPM reads its geometry past a comment"
           (list 2 2) (list (rgb-image-width img) (rgb-image-height img)))
    (check "a PPM reads its pixels in order"
           '((255 0 0) (0 255 0) (0 0 255) (9 9 9))
           (list (multiple-value-list (rgb-ref img 0 0))
                 (multiple-value-list (rgb-ref img 1 0))
                 (multiple-value-list (rgb-ref img 0 1))
                 (multiple-value-list (rgb-ref img 1 1)))))
  (delete-file path))

(check-error "read-ppm rejects a file that is not P6"
  (let ((path "tests/tmp-not.ppm"))
    (unwind-protect
         (progn (write-ilbm (make-image 2 2 2) path) (read-ppm path))
      (delete-file path))))

;; write-deep-ilbm is the inverse of read-deep-ilbm: art that arrived
;; in some other format becomes a source you can keep editing.
(let ((path "tests/tmp-art-deep.iff"))
  (write-deep-ilbm *test-art* path)
  (check-true "a written deep ILBM round-trips through read-art"
              (let ((back (read-art path)))
                (equalp (rgb-image-pixels back)
                        (rgb-image-pixels *test-art*))))
  (check "a written deep ILBM really is 24 planes" 24 (%ilbm-depth path))
  (delete-file path))

(check-error "read-deep-ilbm rejects an indexed file"
  (let ((path "tests/tmp-art-shallow.iff"))
    (unwind-protect
         (progn (write-ilbm (make-image 2 2 2) path)
                (read-deep-ilbm path))
      (delete-file path))))

;; Resampling: a box filter, so shrinking averages the covered source
;; pixels rather than dropping them (thin timber lines must survive).
(check "the box filter averages the whole source rect"
       '(50 50 50)
       (let ((img (%make-rgb 2 2)))
         (setf (rgb-ref img 0 0) '(200 200 200)
               (rgb-ref img 1 0) '(0 0 0)
               (rgb-ref img 0 1) '(0 0 0)
               (rgb-ref img 1 1) '(0 0 0))
         (multiple-value-list (rgb-ref (%rgb-scale img 1 1) 0 0))))
(check "scaling up keeps the source colors"
       '((200 0 0) (0 0 0))
       (let ((up (%rgb-scale *test-art* 32 32)))
         (list (multiple-value-list (rgb-ref up 0 0))
               (multiple-value-list (rgb-ref up 31 31)))))

;; The screen is 12-bit (SET-RGB4), so the quantizer works on the grid
;; the machine can actually show — otherwise pens are spent on colors
;; that display identically (the first art pack had three pens at $000).
(check "snapping rounds to the nearest nibble, stored so it floors back"
       ;; 4/17 rounds to nibble 0, 20/17 to nibble 1 — the grid is
       ;; coarse and snapping must be honest about it
       '((0 0 0) (255 255 255) (136 136 136) (17 0 17) (238 68 68))
       (mapcar #'snap-12-bit
               '((0 0 0) (255 255 255) (136 136 136) (11 4 20) (238 68 68))))
(check-true "a snapped color survives the engine's own FLOOR v 17"
            (let ((c (snap-12-bit '(200 130 99))))
              (equal c (mapcar (lambda (v) (* 17 (floor v 17))) c))))
(check "quantized colors are all realizable on screen"
       nil
       (remove-if (lambda (c) (equal c (snap-12-bit c)))
                  (median-cut (list *test-art*) 16)))
(check "no two art colors collapse to the same screen color"
       nil
       (let* ((colors (median-cut (list *test-art-2*) 16))
              (dupes (remove-if (lambda (c) (= 1 (count c colors
                                                        :test #'equal)))
                                colors)))
         dupes))
(check "a color the palette already carries is not given a pen of its own"
       nil
       (member '(255 255 255)
               (median-cut (list (%test-quadrants '(255 255 255) '(0 200 0)
                                                  '(0 0 200) '(17 17 17)))
                           8 :exclude (art-fixed-colors 5))
               :test #'equal))

;; A source with plenty of color but one hugely dominant background:
;; the heavy color used to swallow a whole split, leaving the tail of
;; the CMAP black — the pack budget must be spent in full.
(defparameter *test-art-busy*
  (let ((img (%make-rgb 32 32)))
    (dotimes (y 32 img)
      (dotimes (x 32)
        (setf (rgb-ref img x y) '(255 255 255))))     ; dominant field
    (dotimes (i 40)
      (setf (rgb-ref img (mod i 32) (floor i 32))
            (list (* 6 (mod i 40)) (* 5 (mod i 33)) (* 4 (mod i 51)))))
    img))

(check "the full pen budget is spent when the sources can fill it"
       22 (length (median-cut (list *test-art-busy*) 22)))
(check-true "a dominant background does not starve the split"
            (> (length (median-cut (list *test-art-busy*) 16)) 8))

;; Quantization
(check "median-cut stops at the number of distinct colors"
       4 (length (median-cut (list *test-art*) 16)))
(check "median-cut finds the four quadrant colors"
       ;; 200 is not on the 12-bit grid; 204 (nibble 12) is
       '((0 0 0) (0 0 204) (0 204 0) (204 0 0))
       ;; sorted on the packed value: three of the four share a red
       ;; channel, so ordering on one component is not a total order
       (sort (median-cut (list *test-art*) 4) #'<
             :key (lambda (c) (+ (ash (first c) 16) (ash (second c) 8)
                                 (third c)))))
(check-true "median-cut honours a smaller budget"
            (<= (length (median-cut (list *test-art*) 2)) 2))

(let ((palette (art-pack-palette '((200 0 0) (0 200 0)) 5)))
  (check "the pack palette is 2^depth entries" 32 (length palette))
  (check "pens 0-4 are the fixed contract"
         '((0 0 0) (255 255 255) (170 170 170) (255 170 51) (0 0 0))
         (coerce (subseq palette 0 5) 'list))
  (check "pen 5 is the sky, pen 6 the ground"
         (list *default-sky* *default-ground*)
         (list (aref palette +art-pen-sky+) (aref palette +art-pen-ground+)))
  (check "the art colors start at pen 7"
         '((200 0 0) (0 200 0))
         (list (aref palette 7) (aref palette 8)))
  ;; Pens 17-19 are the mouse pointer's sprite registers, re-latched
  ;; from the pointer art AFTER a pack's palette loads — art quantized
  ;; into them renders in the pointer's red instead of its own color.
  ;; Pens 24-31 are the shared figure core, whose whole point is that
  ;; a pack cannot move them (see src/palette.lisp).
  (check "the pointer's and the figures' pens are held back from the ~
art plan"
         '(7 8 9 10 11 12 13 14 15 16 20 21 22 23)
         (art-pen-plan 5))
  (check "a 16-color profile has neither to dodge — it is a wall-pack ~
target only"
         '(7 8 9 10 11 12 13 14 15)
         (art-pen-plan 4))
  (check "the figure core is 32-color only"
         '((24 25 26 27 28 29 30 31) ())
         (list (figure-pens 5) (figure-pens 4)))
  (check "the CMAP carries the figure core's own colors"
         (mapcar #'second *figure-pens*)
         (loop for pen in (figure-pens 5) collect (aref palette pen)))
  (check "the CMAP records the pointer's own colors at 17-19"
         '((238 68 68) (51 0 0) (238 238 204))
         (list (aref palette 17) (aref palette 18) (aref palette 19)))
  (let ((mapper (%make-pen-mapper palette)))
    (check "art black lands on the opaque black pen, never the ~
transparent key"
           4 (funcall mapper '(0 0 0)))
    (check "an art color maps to its own pen" 7 (funcall mapper '(200 0 0)))
    (check "a near color maps to the nearest pen" 8 (funcall mapper '(4 190 6)))
    (check-true "no pixel is ever mapped to pen 0"
                (let ((zero nil))
                  (dolist (rgb '((0 0 0) (1 1 1) (255 255 255) (0 0 137)
                                 (200 0 0) (128 128 128))
                               (not zero))
                    (when (zerop (funcall mapper rgb)) (setf zero t)))))))

;; ---------------------------------------------------------------------
;; The pen contract (src/palette.lisp).  A bitmap is pen indices and
;; %CACHED-IMAGE keys by path, so an image loaded in one zone is still
;; on screen in the next: the split between pens a pack owns and pens
;; the engine fixes is what keeps travelling art the color it was drawn.

(dolist (depth '(4 5))
  (let* ((all (loop for p from 0 below (ash 1 depth) collect p))
         (pack (pack-pens depth))
         (fixed (loop for p in all when (fixed-pen-color p) collect p)))
    ;; SORT is destructive and APPEND shares its last argument's
    ;; structure, so this must not be handed FIXED itself.
    (check (format nil "every pen is owned exactly once (depth ~D)" depth)
           all (sort (append pack (copy-list fixed)) #'<))
    (check-true (format nil "no pen is both a pack's and the engine's ~
(depth ~D)" depth)
                (null (intersection pack fixed)))
    (check-true (format nil "a pack owns no pen the engine fixes ~
(depth ~D)" depth)
                (every (lambda (p) (null (fixed-pen-color p))) pack))
    (check (format nil "the pack owns sky and ground, then the art pens ~
(depth ~D)" depth)
           (list* +art-pen-sky+ +art-pen-ground+ (art-pen-plan depth))
           pack)))

;; The figure set is what GENERATE-FIGURE may emit — the opaque UI pens
;; and the core.  Pen 0 is transparency rather than a color, and 5/6
;; are the pack's sky and ground, so none of them belong here.
(check "figures may ink the opaque UI pens and the core"
       '(1 2 3 4 24 25 26 27 28 29 30 31)
       (figure-palette-pens 5))
(check "on a 16-color screen a figure has only the UI pens"
       '(1 2 3 4) (figure-palette-pens 4))
(check-true "no figure pen is one a pack can recolor"
            (null (intersection (figure-palette-pens 5) (pack-pens 5))))
(check-true "the figure core never collides with the pointer's registers"
            (null (intersection (figure-pens 5) (pointer-pens 5))))

;; Every fixed color must survive SET-RGB4 unchanged, or the palette
;; the artist draws against is not the palette the screen shows.
(check-true "every engine-fixed color is on the Amiga's 12-bit grid"
            (loop for pen from 0 below 32
                  for rgb = (fixed-pen-color pen)
                  always (or (null rgb) (equal rgb (snap-12-bit rgb)))))
(check-true "no two engine-fixed pens share a screen color"
            (let ((seen (loop for pen from 0 below 32
                              for rgb = (fixed-pen-color pen)
                              when (and rgb (/= pen +pen-transparent+))
                                collect rgb)))
              ;; pen 0 excluded: it is a key, and legitimately black
              (= (length seen) (length (remove-duplicates seen
                                                          :test #'equal)))))

;; The shipped packs are procedural (gen-walls.lisp) and ink pens 0-9,
;; which is why the core could be reserved downward from 31 without
;; regenerating a single existing asset.  If that ever stops holding,
;; this is the test that says so.
(check-true "the figure core sits clear of the procedural packs' pens"
            (every (lambda (p) (> p +pen-roof+)) (figure-pens 5)))

;; The figure mapper: restricted to the fixed pens, whatever it is fed.
(let ((mapper (%make-figure-pen-mapper (figure-palette 5) 5)))
  (check-true "a figure pixel never lands on a pack pen"
              (let ((legal (figure-palette-pens 5)))
                (loop for rgb in '((0 0 0) (255 255 255) (1 2 3) (0 0 136)
                                   (204 153 102) (250 200 150) (170 20 20)
                                   (60 130 70) (100 120 150) (17 17 17))
                      always (member (funcall mapper rgb) legal))))
  (check "figure black lands on the opaque black pen, never the key"
         +pen-opaque-black+ (funcall mapper '(0 0 0)))
  (check "a skin tone lands on the flesh ramp" 24 (funcall mapper
                                                   '(250 200 150)))
  (check "the pack's own night-blue sky is NOT available to a figure"
         ;; (0 0 136) is pen 5 in Closure's street pack; a figure must
         ;; be pushed onto a fixed pen instead of following the pack
         t (and (member (funcall mapper '(0 0 136)) (figure-palette-pens 5))
                t)))

;; The build-time audit.  This is the check that makes the contract
;; real: it runs on the host, where a per-pixel pen scan is free.
(let ((legal (make-image 4 4 5 :palette (figure-palette 5)))
      (illegal (make-image 4 4 5 :palette (figure-palette 5))))
  (dotimes (y 4)
    (dotimes (x 4)
      (setf (pixel-ref legal x y) (if (evenp (+ x y)) +pen-transparent+ 26))
      (setf (pixel-ref illegal x y) 26)))
  (setf (pixel-ref illegal 2 1) +art-pen-sky+)   ; a pack pen sneaks in
  (check-true "a figure drawn in the core passes the audit"
              (%check-figure-pens legal "legal.iff" 5))
  (check-error "a figure using a pack pen is rejected at build time"
               (%check-figure-pens illegal "illegal.iff" 5)))

;; End to end: a picture in arbitrary colors becomes a figure whose
;; every pixel is a pen no pack can move.
(let ((src "tests/tmp-figure-src.iff")
      (out "tests/tmp-figure.iff"))
  (write-deep-ilbm (%test-quadrants '(250 200 150) '(170 20 20)
                                    '(60 130 70) '(255 0 255))
                   src)
  (with-display-profile (:lores)
    (generate-figure src out :transparent '(255 0 255)))
  (let* ((img (read-ilbm out))
         (legal (cons +pen-transparent+ (figure-palette-pens 5)))
         (pens '()))
    (dotimes (y (image-height img))
      (dotimes (x (image-width img))
        (pushnew (pixel-ref img x y) pens)))
    (check-true "GENERATE-FIGURE emits only pens the engine fixes"
                (every (lambda (p) (member p legal)) pens))
    (check-true "the :transparent source color becomes the cookie-cut key"
                (member +pen-transparent+ pens))
    (check-true "the figure keeps more than one color"
                (> (length pens) 2)))
  (delete-file src)
  (delete-file out))

;; The pieces: the manifest's geometry and the transparency contract,
;; exactly as the procedural pack must meet them.
(with-display-profile (:lores)
  (let* ((planes (view-planes *fp-view-width* *fp-view-height*))
         (palette (art-pack-palette (median-cut (list *test-art*) 25) 5))
         (mapper (%make-pen-mapper palette))
         (wrong '()))
    (dolist (piece (wall-piece-names))
      (let ((img (art-wall-piece piece planes *test-art* palette 5 mapper)))
        (destructuring-bind (x y w h) (wall-piece-rect planes piece)
          (declare (ignore x y))
          (unless (and (= (image-width img) w) (= (image-height img) h))
            (push (list piece :size) wrong)))
        (unless (= (image-depth img) 5)
          (push (list piece :depth) wrong))
        ;; side pieces keep pen-0 corners (cookie-cut); every other
        ;; piece fills its rect, so a mask would be wasted chip RAM
        (if (member (first piece) '(:side :side-door))
            (unless (image-transparent-p img)
              (push (list piece :corners-not-transparent) wrong))
            (when (image-transparent-p img)
              (push (list piece :unexpected-transparency) wrong)))))
    (check "all 40 art pieces match their slots and the pen contract"
           nil wrong)
    ;; no piece may land on the pointer's registers
    (check "no art piece uses a pointer pen"
           nil
           (let ((hits '()))
             (dolist (piece (wall-piece-names) (remove-duplicates hits))
               (let ((px (image-pixels
                          (art-wall-piece piece planes *test-art*
                                          palette 5 mapper))))
                 (dolist (pen '(17 18 19))
                   (when (find pen px) (push pen hits)))))))
    (check "a side piece's far top corner is the transparent key"
           0 (let ((img (art-wall-piece '(:side 0 :l) planes *test-art*
                                        palette 5 mapper)))
               (pixel-ref img (1- (image-width img)) 0)))
    ;; a right piece is the mirror of the left one
    (check-true "the right side piece mirrors the left"
                (let* ((l (art-wall-piece '(:side 0 :l) planes *test-art*
                                          palette 5 mapper))
                       (r (art-wall-piece '(:side 0 :r) planes *test-art*
                                          palette 5 mapper))
                       (w (image-width l)))
                  (loop for y below (image-height l)
                        always (loop for x below w
                                     always (= (pixel-ref l x y)
                                               (pixel-ref r (- w 1 x) y))))))
    ;; a left flank shows the wall's RIGHT edge: it stands left of the
    ;; front slot, so the viewport cuts its left side off
    (check-true "a flank is cropped from the front slot's scale, not ~
squeezed into its own"
                (let* ((front (art-wall-piece '(:front 1) planes *test-art*
                                              palette 5 mapper))
                       (flank (art-wall-piece '(:flank 1 :l) planes *test-art*
                                              palette 5 mapper))
                       (fw (image-width front))
                       (kw (image-width flank)))
                  (and (< kw fw)
                       (loop for y below (image-height flank)
                             always (loop for x below kw
                                          always (= (pixel-ref flank x y)
                                                    (pixel-ref front
                                                               (+ (- fw kw) x)
                                                               y)))))))))

;; End to end: a whole pack on disk, then loaded back through the same
;; checks the Amiga front end applies, then composited into a view.
(with-display-profile (:lores)
  (let ((src "tests/tmp-art-src.iff")
        (dir "tests/tmp-art-pack/")
        (pic "picture.iff"))
    (write-deep-ilbm *test-art* src)
    (let ((n (generate-pack-from-art src :out dir
                                         :pictures (list (cons src pic)))))
      (check "a pack is 40 pieces + 2 backdrops + palette + pictures"
             45 n))
    (let ((planes (view-planes *fp-view-width* *fp-view-height*)))
      ;; %preview-load-pack applies the loader's size contract, so this
      ;; failing means the Amiga would reject the pack too
      (let ((walls (%preview-load-pack dir planes)))
        (check "every piece plus both backdrops loaded" 42
               (hash-table-count walls))
        (check-true "a pack without -vN files has exactly one variant ~
per piece"
                    (let ((ok t))
                      (maphash (lambda (k v)
                                 (declare (ignore k))
                                 (unless (= 1 (length v)) (setf ok nil)))
                               walls)
                      ok)))
      (check "the backdrops are flat sky and flat ground"
             (list (list +art-pen-sky+) (list +art-pen-ground+))
             (mapcar (lambda (name)
                       (remove-duplicates
                        (coerce (image-pixels
                                 (read-ilbm (concatenate 'string dir name)))
                                'list)))
                     '("ceiling.iff" "floor.iff")))
      (check "palette.iff carries one pixel per pen"
             (list 32 1)
             (let ((img (read-ilbm (concatenate 'string dir "palette.iff"))))
               (list (image-width img) (image-height img))))
      ;; palette.gpl is the same colors for a paint program, so the
      ;; next house can be DRAWN against the pack instead of requantized
      (let ((lines '()))
        (with-open-file (s (concatenate 'string dir "palette.gpl"))
          (loop for line = (read-line s nil nil)
                while line do (push line lines)))
        (setf lines (nreverse lines))
        (check "palette.gpl is a GIMP palette" "GIMP Palette" (first lines))
        (check "palette.gpl has one entry per pen"
               32 (count-if (lambda (l) (find #\Tab l)) lines))
        (check-true "palette.gpl names the reserved pens"
                    (and (find-if (lambda (l) (search "transparent key" l))
                                  lines)
                         (find-if (lambda (l) (search "mouse pointer" l))
                                  lines))))
      ;; and it closes the loop: feeding a pack's own palette back in
      ;; as :PALETTE-SOURCE reproduces that palette exactly
      (let ((dir2 "tests/tmp-art-fixedpal/"))
        (generate-pack-from-art src :out dir2
                                    :palette-source (concatenate
                                                     'string dir
                                                     "palette.iff"))
        (check "a pack built on a fixed palette keeps it exactly"
               (coerce (image-palette
                        (read-ilbm (concatenate 'string dir
                                                "palette.iff")))
                       'list)
               (coerce (image-palette
                        (read-ilbm (concatenate 'string dir2
                                                "palette.iff")))
                       'list))
        (dolist (piece (wall-piece-names))
          (delete-file (concatenate 'string dir2 (wall-piece-file piece))))
        (dolist (name '("ceiling.iff" "floor.iff" "palette.iff"
                        "palette.gpl"))
          (delete-file (concatenate 'string dir2 name))))
      (check-true "a picture is quantized into the pack's own CMAP, ~
never its own"
                  (let ((img (read-ilbm (concatenate 'string dir pic)))
                        (pal (image-palette
                              (read-ilbm (concatenate 'string dir
                                                      "palette.iff")))))
                    (and (= (image-depth img) 5)
                         (equal (coerce (image-palette img) 'list)
                                (coerce pal 'list))
                         ;; the source's four quadrant colors survive
                         (= 4 (length (remove-duplicates
                                       (coerce (image-pixels img) 'list)))))))
      ;; and it composites: the preview is the blit path in pure Lisp
      (let* ((m (parse-map *art* :name "preview"))
             (view (preview-view m 0 0 :east :dir dir)))
        (check "the preview is the profile's viewport"
               (list *fp-view-width* *fp-view-height*)
               (list (image-width view) (image-height view)))
        (check-true "the preview drew walls over the backdrop"
                    (> (length (remove-duplicates
                                (coerce (image-pixels view) 'list)))
                       2)))
      ;; Which backdrop pair a zone takes.  ground.iff is the open
      ;; zone's floor and floor.iff the dark one's, and neither may
      ;; stand in for the other: a dungeon floor is drawn in pen 5,
      ;; which outdoors is the sky, so a zone reaching for the wrong
      ;; pair paints the street with the sky colour.  Compared as whole
      ;; images rather than by probing a pixel, so the checks do not
      ;; depend on where the walls happen to leave the floor showing.
      (let* ((m (parse-map *art* :name "preview"))
             (ground (concatenate 'string dir "ground.iff"))
             (bare-open (image-pixels (preview-view m 0 0 :east :dir dir))))
        (setf (dungeon-map-dark m) t)
        (let ((bare-dark (image-pixels (preview-view m 0 0 :east :dir dir))))
          ;; a floor that could not be mistaken for either flat fill
          (destructuring-bind (ceiling floor) (backdrop-rects
                                               (view-planes *fp-view-width*
                                                            *fp-view-height*))
            (declare (ignore ceiling))
            (let ((img (make-image (third floor) (fourth floor) 5)))
              (dotimes (y (fourth floor))
                (dotimes (x (third floor))
                  (setf (pixel-ref img x y) 15)))
              (write-ilbm img ground)))
          ;; CHECK compares with EQUAL, which on two arrays is identity —
          ;; pixel buffers have to be held up to EQUALP by hand.
          (check-true "a dark zone ignores ground.iff and keeps floor.iff"
                      (equalp bare-dark
                              (image-pixels
                               (preview-view m 0 0 :east :dir dir))))
          (setf (dungeon-map-dark m) nil)
          (check-true "an open zone paints its street from ground.iff"
                      (not (equalp bare-open
                                   (image-pixels
                                    (preview-view m 0 0 :east :dir dir)))))
          (delete-file ground)
          (check-true "removing ground.iff returns the open zone to the ~
flat fill"
                      (equalp bare-open
                              (image-pixels
                               (preview-view m 0 0 :east :dir dir)))))))
    ;; tidy up
    (dolist (piece (wall-piece-names))
      (delete-file (concatenate 'string dir (wall-piece-file piece))))
    (dolist (name (list "ceiling.iff" "floor.iff" "palette.iff"
                        "palette.gpl" pic))
      (delete-file (concatenate 'string dir name)))
    (delete-file src)))

;; Variants: each extra source is a whole extra look, written as the
;; -vN files the view deals out per building.  They share the pack's
;; single CMAP with the base look — that is the whole reason the
;; quantizer pools every source before choosing pens.
(with-display-profile (:lores)
  (let ((src "tests/tmp-art-a.iff")
        (var "tests/tmp-art-b.iff")
        (dir "tests/tmp-art-vpack/"))
    (write-deep-ilbm *test-art* src)
    (write-deep-ilbm *test-art-2* var)
    (check "two looks write both the base and the -v1 files"
           (+ (* 2 40) 4)
           (generate-pack-from-art src :out dir :variants (list var)))
    (let* ((planes (view-planes *fp-view-width* *fp-view-height*))
           (walls (%preview-load-pack dir planes)))
      (check-true "every piece carries two variants"
                  (let ((ok t))
                    (dolist (piece (wall-piece-names) ok)
                      (unless (= 2 (length (gethash piece walls)))
                        (setf ok nil)))))
      (check-true "the two looks actually differ"
                  (let ((entry (gethash '(:front 0) walls)))
                    (not (equalp (image-pixels (aref entry 0))
                                 (image-pixels (aref entry 1))))))
      (check-true "both looks share the pack's one palette"
                  (let ((entry (gethash '(:front 0) walls)))
                    (equalp (image-palette (aref entry 0))
                            (image-palette (aref entry 1)))))
      ;; the renderer indexes with (MOD STYLE COUNT), so a building's
      ;; style picks a look and styles wrap rather than fall over
      (check "style selection wraps over the looks a pack ships"
             '(0 1 0 1)
             (let ((count (length (gethash '(:front 0) walls))))
               (mapcar (lambda (style) (mod style count)) '(0 1 2 3)))))
    (dolist (piece (wall-piece-names))
      (delete-file (concatenate 'string dir (wall-piece-file piece)))
      (delete-file (concatenate 'string dir
                                (wall-piece-variant-file piece 1))))
    (dolist (name '("ceiling.iff" "floor.iff" "palette.iff" "palette.gpl"))
      (delete-file (concatenate 'string dir name)))
    (delete-file src)
    (delete-file var)))

;;; ---------------------------------------------------------------------
;;; The shipped packs, composited: the three pens the Amiga suite reads
;;; back off the screen, read here off PREVIEW-VIEW's image instead.
;;;
;;; The Amiga counterpart of this (search "read-back probes") is the
;;; check that matters — it proves the OS blits land where the geometry
;;; says.  But it is hand-run, and hand-run is how it went stale: its
;;; lores probe rows were written for the 112-row viewport and outlived
;;; its move to 100 (Engine 0.21.0's 200-line layout) unnoticed for a
;;; dozen releases, because nothing on the host was watching.  Now
;;; something is: PREVIEW-VIEW composites the same slices and backdrops
;;; in pure Lisp, so the pens are checkable here every `make test`, and
;;; both suites read the same rows out of %FP-PROBE-ROWS.

(defun %fp-probe-rows ()
  "The three viewport rows the view's read-back probes read, for the
active display profile: the front piece's top row (its white edge
highlight), the sky row just above it, and a ground row below the
piece's foot.  Derived from the plane geometry rather than written
down, so a viewport resize carries the probes with it."
  (let ((planes (view-planes *fp-view-width* *fp-view-height*)))
    (destructuring-bind (px0 py0 px1 py1) (aref planes 1)
      (declare (ignore px0 px1))
      (list py0 (1- py0) (floor (+ py1 *fp-view-height*) 2)))))

;; The columns, one triple per profile: an X the front piece covers and
;; the nearer side/flank pieces do not, so the probe reads the piece
;; (or, a row higher, the open sky) and not a neighbour blitted over it.
(defparameter *fp-probe-columns*
  '((:lores 80 43 70)
    (:hires 100 100 90))
  "(PROFILE FRONT-X SKY-X GROUND-X) — the columns the view's read-back
probes read; the rows come from %FP-PROBE-ROWS.")

(dolist (spec *fp-probe-columns*)
  (destructuring-bind (pname front-x sky-x ground-x) spec
    (with-display-profile (pname)
      (let* ((m (parse-map *art* :name "test"))
             (g (new-game m))
             (img (preview-view m (game-x g) (game-y g) (game-facing g)
                                :depth (render-view-depth g))))
        (destructuring-bind (front-y sky-y ground-y) (%fp-probe-rows)
          (check (format nil "~A: the front piece's top row is its white ~
edge highlight" pname)
                 1 (pixel-ref img front-x front-y))
          ;; the fixture map declares no zone, so it is an OUTDOOR one:
          ;; with no sky.iff/ground.iff in the shipped packs both
          ;; backdrops are the flat day-band fills
          (check (format nil "~A: the row above it is the flat sky-pen ~
fill" pname)
                 +art-pen-sky+ (pixel-ref img sky-x sky-y))
          (check (format nil "~A: below the piece's foot is the flat ~
ground-pen fill" pname)
                 +art-pen-ground+ (pixel-ref img ground-x ground-y)))))))

;;; ---------------------------------------------------------------------
;;; The help page: pure text both front-ends draw verbatim.

(let ((lines (help-lines)))
  (check-true "help-lines is a non-empty list of strings"
              (and (consp lines) (every #'stringp lines)))
  (check-true "help mentions movement, map and help keys"
              (and (find-if (lambda (s) (search "W forward" s)) lines)
                   (find-if (lambda (s) (search "M map" s)) lines)
                   (find-if (lambda (s) (search "this help" s)) lines)))
  (check-true "help mentions combat and save keys"
              (and (find-if (lambda (s) (search "Combat:" s)) lines)
                   (find-if (lambda (s) (search "save" s)) lines)))
  (check-true "help mentions pooling gold"
              (find-if (lambda (s) (search "pool gold" s)) lines))
  (check-true "help mentions the sheet carousel's next page"
              (find-if (lambda (s) (search "next page" s)) lines))
  (check-true "help mentions the marching order"
              (find-if (lambda (s) (search "marching order" s)) lines))
  (check-true "help mentions taking a level"
              (find-if (lambda (s) (search "take a level" s)) lines))
  (check-true "help says quitting asks first"
              (find-if (lambda (s) (search "asks first" s)) lines)))

;;; The map-view line must not overclaim U/D scroll for a front-end
;;; that does not wire it in (the host UI's MAP-ACT has no u/d case).
(check-true "help-lines with no arg (host) does not advertise map U/D scroll"
            (not (find-if (lambda (s) (search "Map view: U/D scroll" s))
                           (help-lines))))
(check-true "help-lines(t) (Amiga) advertises map U/D scroll"
            (find-if (lambda (s) (search "Map view: U/D scroll" s))
                     (help-lines t)))

;;; Same contract for the mouse: only a front-end that hangs a menu
;;; strip off the right button (the Amiga UI does; the host ASCII walk
;;; has no such thing) may name one — it is the only click that reaches
;;; the map, help, cast, play, use and save pages.
(check-true "help-lines with no menu strip does not name one"
            (not (find-if (lambda (s) (search "menu strip" s))
                          (help-lines t))))
(check-true "help-lines(t t) (Amiga) names the right button's menu strip"
            (find-if (lambda (s) (search "right button: menu strip" s))
                     (help-lines t t)))
;;; The reference is drawn in a fixed-width face on a narrow page: a
;;; line wider than the widest line already there would be cut off.
(check-true "help: the menu-strip line fits the page's width"
            (<= (reduce #'max (help-lines t t) :key #'length)
                (reduce #'max (help-lines t) :key #'length)))

;;; ---------------------------------------------------------------------
;;; The quit confirmation: Q, Esc and the menu strip's Quit all ask
;;; before the session ends — one page, two options, both front-ends.

(let ((lines (quit-confirm-lines)))
  (check-true "the confirmation asks whether to quit"
              (find-if (lambda (line)
                         (search "Really quit" (menu-line-text line)))
                       lines))
  (check-true "the confirmation warns about unsaved progress"
              (find-if (lambda (line)
                         (search "Unsaved" (menu-line-text line)))
                       lines))
  ;; both options are option rows, so a front-end can click them
  (check "yes is picked by Y" #\y
         (menu-line-key (find-if (lambda (line)
                                   (search "Yes" (menu-line-text line)))
                                 lines)))
  (check "no is picked by N" #\n
         (menu-line-key (find-if (lambda (line)
                                   (search "No," (menu-line-text line)))
                                 lines))))

(check "Y confirms the quit" :quit (quit-confirm-act #\y))
(check "shift makes no difference" :quit (quit-confirm-act #\Y))
(check "N backs out" :cancel (quit-confirm-act #\n))
(check "Esc backs out as the character" :cancel (quit-confirm-act #\Escape))
(check "Esc backs out as the front-end's keyword" :cancel
       (quit-confirm-act :esc))
;; every other key is eaten: nothing leaks through to the game while
;; the question stands — not even another Q
(check "Q neither confirms nor cancels" nil (quit-confirm-act #\q))
(check "a step key is swallowed" nil (quit-confirm-act #\w))
(check "a digit is swallowed" nil (quit-confirm-act #\1))

;;; ---------------------------------------------------------------------
;;; The roster's class codes and column plists.

;; Class codes are always two characters — the roster's CL column is
;; two cells wide, leaving the freed room to the name column.
(with-rng ()
  (check "single-word class abbreviates to two letters" "TE"
         (hero-class-abbrev (make-hero "A" :tester))))
(define-hero-class :war-mage :hp-dice "1d4" :caster t)
(with-rng ()
  (check "multi-word class abbreviates to two initials" "WM"
         (hero-class-abbrev (make-hero "A" :war-mage))))
(define-hero-class :knight-of-the-realm :hp-dice "1d8")
(with-rng ()
  (check "many-word class caps at two initials" "KO"
         (hero-class-abbrev (make-hero "A" :knight-of-the-realm))))

;; Both profiles carry the full Bard's Tale column set, in order.
(dolist (p (list *lores-profile* *hires-profile*))
  (let ((cols (display-profile-roster-cols p)))
    (check-true (format nil "~A roster columns are complete and ordered"
                        (display-profile-name p))
                (apply #'< (mapcar (lambda (k) (getf cols k))
                           '(:no :name :ac :hit :hpts :spl :spts :cl))))))

;;; ---------------------------------------------------------------------
;;; The microfont: the engine's own pixel faces — the 7x7 display
;;; face, and the condensed bold 5x7 small face the pages set.

(check "microfont advance is 8 pixels" 8 +microfont-advance+)
(check "microfont line height is 8 pixels" 8 +microfont-line-height+)
(check "microfont text width" 40 (microfont-text-width "hello"))

;; Glyph shapes: 'A' has its bold two-pixel strokes, crossbar and foot
;; serifs, space is blank; anything outside printable ASCII falls back
;; to the hollow box.
(check-true "glyph A"
            (equalp #(#b0011000 #b0111100 #b1100110 #b1100110
                      #b1111110 #b1100110 #b1110111)
                    (microfont-glyph #\A)))
(check-true "space is blank" (equalp #(0 0 0 0 0 0 0)
                                     (microfont-glyph #\Space)))
(check-true "non-ASCII falls back to the box"
            (eq *microfont-fallback* (microfont-glyph (code-char 200))))

;; The small face: condensed bold 5x7 glyphs on a 6px advance — the
;; engine's page face (log, takeover, overlay menus, the whole map
;; page), with the metrics of the actual Bard's Tale II text.
(check "small face advance is 6 pixels" 6 +microfont-small-advance+)
(check-true "small glyph T carries its serifs"
            (equalp #(#b11111 #b10101 #b00100 #b00100
                      #b00100 #b00100 #b01110)
                    (microfont-small-glyph #\T)))
(check-true "small face non-ASCII falls back to the 5-wide box"
            (eq tale::*microfont-small-fallback*
                (microfont-small-glyph (code-char 200))))

;; Rendering: row-major pens, FG where a glyph bit is set, BG
;; elsewhere; WIDTH pads or cuts.
(multiple-value-bind (pens w h) (microfont-line "A" 7 2)
  (check "rendered width of one glyph cell" 8 w)
  (check "rendered height is the line height" 8 h)
  (check "buffer covers the cell" 64 (length pens))
  ;; row 0 of 'A' is the 0011000 apex -> pens 2 2 7 7 2 2 2, then the
  ;; spacing column
  (check "top row pixels" '(2 2 7 7 2 2 2 2)
         (loop for x below 8 collect (aref pens x)))
  ;; row 4 is the 1111110 crossbar
  (check "crossbar row pixels" '(7 7 7 7 7 7 2 2)
         (loop for x below 8 collect (aref pens (+ (* 4 8) x))))
  ;; row 7 is the spacing row
  (check "spacing row is background" '(2 2 2 2 2 2 2 2)
         (loop for x below 8 collect (aref pens (+ (* 7 8) x)))))
(multiple-value-bind (pens w h) (microfont-line "AB" 1 0 :width 8)
  (declare (ignore pens))
  (check "explicit width cuts the text" 8 w)
  (check "height stays fixed" 8 h))
(multiple-value-bind (pens w h) (microfont-line "" 1 0 :width 24)
  (check "explicit width pads short text" 24 w)
  (check "padded buffer is all background" 0
         (loop for p across pens maximize p))
  (check "padded height" 8 h))

;; The small face renders through the same path at its 6px advance.
(check "small face text width" 30 (microfont-small-text-width "hello"))
(multiple-value-bind (pens w h) (microfont-small-line "T" 7 2)
  (check "small rendered width of one glyph cell" 6 w)
  (check "small rendered height is the line height" 8 h)
  ;; row 0 of small 'T' is the 11111 top bar, then the spacing column
  (check "small top row pixels" '(7 7 7 7 7 2)
         (loop for x below 6 collect (aref pens x)))
  ;; row 7 is the spacing row
  (check "small spacing row is background" '(2 2 2 2 2 2)
         (loop for x below 6 collect (aref pens (+ (* 7 6) x)))))

;;; ---------------------------------------------------------------------
;;; Amiga front-end smoke tests in a window on the Workbench screen.
;;; DISABLED for now (gated on :LAMBDA-TALE-WINDOW-TESTS — push it
;;; onto *FEATURES* before loading to re-enable): the Workbench screen
;;; is not under the suite's control (size, font, depth, RTG promotion
;;; vary per setup), and the custom screen is the game's presentation
;;; — see the :SCREEN tests further down, which carry this coverage.

#+lambda-tale-window-tests
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (log (attach-message-log g)))
  (say g "Smoke test line one.")
  (say g "Smoke test line two.")
  (add-effect g "shield")
  (add-effect g "lamp")
  (check "amiga-ui draws the play layout into a real window" t
         (amiga.intuition:with-window
             (win :title "Lambda's Tale Test"
                  :left 0 :top 0
                  :width (display-profile-win-width *display-profile*)
                  :height (display-profile-win-height *display-profile*)
                  :idcmp amiga.intuition:+idcmp-closewindow+)
           (let* ((rp (amiga.intuition:window-rastport win))
                  (l (%amiga-layout win rp)))
             ;; layout invariants: the taller plaque, the roster right
             ;; under it (no status line), the page/strip gap
             (check "plaque is two pixels taller than a text line"
                    (+ (ui-layout-plaque-y l) (ui-layout-lh l) 2)
                    (ui-layout-plaque-b l))
             (check "roster header sits right under the plaque"
                    (+ (ui-layout-plaque-b l) +roster-gap+)
                    (ui-layout-hdr-y l))
             (check-true "seven roster rows fit above the bottom edge"
                         (<= (+ (ui-layout-party-y l)
                                (* (ui-layout-row-h l) +party-limit+) -1)
                             (ui-layout-bottom l)))
             (check "the log page ends a gap above the effect strip"
                    (ui-layout-band-y l) (+ (ui-layout-page-b l) 4))
             (check "the effect strip's bottom row is the column's last"
                    (+ (ui-layout-by l) (ui-layout-col-h l) -1)
                    (+ (ui-layout-band-y l) (ui-layout-band-h l) -1))
             (%amiga-draw-fp rp g (ui-layout-bx l) (ui-layout-by l)
                             (ui-layout-fp-w l) (ui-layout-fp-h l))
             ;; the live session's path: the frame composes in the
             ;; offscreen back buffer and lands as one blit
             (let ((back (%alloc-fp-backbuffer rp l)))
               (check-true "fp back buffer allocates in the display format"
                           back)
               (when back
                 (unwind-protect
                     (%amiga-draw-fp rp g (ui-layout-bx l) (ui-layout-by l)
                                     (ui-layout-fp-w l) (ui-layout-fp-h l)
                                     nil back)
                   (amiga.gfx:free-bitmap back))))
             (%amiga-draw-band rp g l)
             (%amiga-draw-log rp log l)
             ;; the cached-bitmap log path (the live session's) and
             ;; the help page draw too
             (let ((cache (make-hash-table :test #'equal)))
               (%amiga-draw-log rp log l cache)
               (check-true "log lines were cached as bitmaps"
                           (plusp (hash-table-count cache)))
               (%free-log-lines cache))
             (%amiga-draw-help rp l)
             ;; the full map mode over the same window
             (%amiga-draw-map-page rp g l nil)
             (%amiga-draw-map-page rp g l t)
             t))))

;; A zone title wider than the plaque must lose trailing characters
;; rather than overrun the border (the bug %PLAQUE-NAME fixes — see
;; %CHROME-FRAMES).
#+lambda-tale-window-tests
(let* ((m (parse-map *art* :name "A Very Long Location Name That Overflows The Plaque"))
       (g (new-game m)))
  (check "amiga-ui truncates a plaque title wider than the view column" t
         (amiga.intuition:with-window
             (win :title "Lambda's Tale Test"
                  :left 0 :top 0
                  :width (display-profile-win-width *display-profile*)
                  :height (display-profile-win-height *display-profile*)
                  :idcmp amiga.intuition:+idcmp-closewindow+)
           (let* ((rp (amiga.intuition:window-rastport win))
                  (l (%amiga-layout win rp))
                  (w (ui-layout-fp-w l))
                  (full (title-case (map-title (game-map g))))
                  (name (%plaque-name rp full w)))
             (check-true "the untruncated title overruns the plaque"
                         (> (amiga.gfx:text-length rp full) (- w 2)))
             (check-true "the truncated title fits within the plaque"
                         (<= (amiga.gfx:text-length rp name) (- w 2)))
             (check-true "the title was actually shortened"
                         (< (length name) (length full)))
             t))))

;; The full map view must cope with a map bigger than the window —
;; the layout the spec is actually about.
#+lambda-tale-window-tests
(let* ((m (parse-map (%big-map-art 30 30) :name "big30"))
       (g (new-game m))
       (log (attach-message-log g)))
  ;; a place whose name is wider than the narrow legend column a 30x30
  ;; map leaves — the legend wraps it onto continuation lines instead
  ;; of cutting the tail ("Wolfgar'S A" was all that survived)
  (setf (cell-special m 2 1)
        '((location "Wolfgar's Arms & Armour" :shop)))
  (setf (game-x g) 15 (game-y g) 15)
  (observe g)
  (check "amiga-ui map page on a 30x30 map" t
         (amiga.intuition:with-window
             (win :title "Lambda's Tale Test"
                  :left 0 :top 0
                  :width (display-profile-win-width *display-profile*)
                  :height (display-profile-win-height *display-profile*)
                  :idcmp amiga.intuition:+idcmp-closewindow+)
           (let* ((rp (amiga.intuition:window-rastport win))
                  (l (%amiga-layout win rp)))
             (%amiga-draw-band rp g l)
             (%amiga-draw-log rp log l)
             (%amiga-draw-map-page rp g l nil)
             ;; :full lists the shoppe without walking there — draws
             ;; the wrapped legend beside the map
             (%amiga-draw-map-page rp g l t)
             t))))

;; GadTools menu strip (creation/layout via WITH-VISUAL-INFO/WITH-MENUS)
;; and the party roster pane — with a full seven-member roster.
#+lambda-tale-window-tests
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party
                    (with-rng ()
                      (loop for i from 1 to +party-limit+
                            collect (make-hero (format nil "Hero~D" i)
                                               :tester))))))
  (check "amiga-ui menu strip and 7-row party roster draw without error" t
         (amiga.intuition:with-pub-screen (scr)
           (amiga.gadtools:with-visual-info (vi scr)
             (amiga.intuition:with-window
                 (win :title "Lambda's Tale Test"
                      :left 0 :top 0
                      :width (display-profile-win-width *display-profile*)
                  :height (display-profile-win-height *display-profile*)
                      :idcmp amiga.intuition:+idcmp-closewindow+)
               (amiga.gadtools:with-menus (menu *menu-entries* vi win)
                 (let* ((rp (amiga.intuition:window-rastport win))
                        (l (%amiga-layout win rp)))
                   (%amiga-party rp g l)
                   ;; the numbered roster's character-sheet page draws too
                   (%amiga-draw-sheet rp g 0 l)
                   t)))))))

;; The location interaction: the overlay page variant, the message-area
;; takeover (the menu owns the whole white page) and the view-column
;; picture with its fall-back contract.
#+lambda-tale-window-tests
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (with-rng () (list (make-hero "A" :tester)))))
       (log (attach-message-log g))
       (view (make-shop-view)))
  (say g "Takeover smoke test line.")
  (enter-location g '("Smoke Shoppe" :shop :stock (t-sword t-torch)))
  (check "amiga-ui draws the location pages" t
         (amiga.intuition:with-window
             (win :title "Lambda's Tale Test"
                  :left 0 :top 0
                  :width (display-profile-win-width *display-profile*)
                  :height (display-profile-win-height *display-profile*)
                  :idcmp amiga.intuition:+idcmp-closewindow+)
           (let* ((rp (amiga.intuition:window-rastport win))
                  (l (%amiga-layout win rp)))
             (%amiga-draw-page rp (location-lines g view) l)   ; pick-hero
             (shop-act g view #\1)
             (%amiga-draw-page rp (location-lines g view) l)   ; buy page
             (shop-act g view #\s)
             (%amiga-draw-page rp (location-lines g view) l)   ; sell page
             (%amiga-draw-log rp log l)
             ;; a combat round's own transcript page (top-aligned,
             ;; messages since the mark only)
             (%amiga-draw-transcript rp log l 0)
             ;; the message-area takeover, uncached and cached: the
             ;; location menu and the character sheet
             (%amiga-draw-takeover rp (location-lines g view) log l)
             ;; roster rows click as their digits when enabled; the
             ;; empty row below the party registers nothing
             (let ((*hotspots* '()))
               (%amiga-party rp g l t)
               (check "roster: the hero row clicks as its digit" #\1
                      (%hotspot-at (+ (ui-layout-bx l) 3)
                                   (+ (ui-layout-party-y l) 2)))
               (check "roster: an empty row is not a target" nil
                      (%hotspot-at (+ (ui-layout-bx l) 3)
                                   (+ (ui-layout-party-y l)
                                      (ui-layout-row-h l) 2))))
             (let ((*hotspots* '()))
               (%amiga-party rp g l)
               (check "roster: not clickable unless asked" nil
                      (%hotspot-at (+ (ui-layout-bx l) 3)
                                   (+ (ui-layout-party-y l) 2))))
             ;; the click-to-walk zones on the first-person view
             (let ((*hotspots* '()))
               (%register-move-zones l)
               (let* ((bx (ui-layout-bx l)) (by (ui-layout-by l))
                      (w (ui-layout-fp-w l)) (h (ui-layout-fp-h l))
                      (cx (+ bx (floor w 2))))
                 ;; each zone carries the arrow cursor of its move
                 (check "view: the middle walks forward" '(#\w :forward)
                        (multiple-value-list
                         (%hotspot-at cx (+ by 4))))
                 (check "view: the bottom middle steps back" '(#\s :back)
                        (multiple-value-list
                         (%hotspot-at cx (+ by h -4))))
                 (check "view: the left quarter turns left"
                        '(#\a :turn-left)
                        (multiple-value-list
                         (%hotspot-at (+ bx 2) (+ by (floor h 2)))))
                 (check "view: the right quarter turns right"
                        '(#\d :turn-right)
                        (multiple-value-list
                         (%hotspot-at (+ bx w -3) (+ by (floor h 2)))))
                 (check "view: outside the view is no target" nil
                        (%hotspot-at (+ bx w 20) (+ by (floor h 2))))))
             ;; the busy pointer brackets a load and restores, and a
             ;; nested use keeps the outer pointer up
             (check "busy pointer wraps a body and restores" :ok
                    (%call-with-busy-pointer win
                     (lambda ()
                       (check "nested busy pointer runs the body" :inner
                              (%call-with-busy-pointer
                               win (lambda () :inner)))
                       :ok)))
             (let ((cache (make-hash-table :test #'equal)))
               (%amiga-draw-takeover rp (hero-sheet-lines g 0) log l cache)
               (check-true "takeover lines were cached as bitmaps"
                           (plusp (hash-table-count cache)))
               (%free-log-lines cache))
             ;; the view-column picture: a real ILBM draws and centers;
             ;; a missing file defers to the caller (falls back to the
             ;; first-person view) after logging once
             (let ((images (make-hash-table :test #'equal))
                   (path "tests/tmp-pic.iff"))
               (write-ilbm (draw-location-scene :shop 40 30) path)
               (check-true "a location picture draws in the view column"
                           (%amiga-draw-picture rp images path l log))
               (check "a missing picture defers to the caller" nil
                      (%amiga-draw-picture rp images "tests/no-such.iff"
                                           l log))
               (check-true "the missing picture said so in the log"
                           (find-if (lambda (s) (search "No image" s))
                                    (log-recent log 5)))
               (%free-images images)
               (delete-file path))
             t)))
  (leave-location g))

;; Mouse hotspots on the menu renderers, with the game font selected
;; the way PLAY-AMIGA always does — the page budgets are tuned for
;; topaz 8, and on an RTG Workbench the system font would shrink the
;; overlay page below the footer row.  Row positions are recomputed
;; exactly the way the renderers wrap.
#+lambda-tale-window-tests
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m :party (with-rng () (list (make-hero "A" :tester)))))
       (log (attach-message-log g))
       (view (make-shop-view)))
  (enter-location g '("Smoke Shoppe" :shop :stock (t-sword t-torch)))
  (check "amiga-ui menu hotspots sit on the drawn rows" t
         (%with-game-font
          (lambda (font)
            (amiga.intuition:with-window
                (win :title "Lambda's Tale Test"
                     :left 0 :top 0
                     :width (display-profile-win-width *display-profile*)
                     :height (display-profile-win-height *display-profile*)
                     :idcmp amiga.intuition:+idcmp-closewindow+)
              (let* ((rp (%game-rastport win font))
                     (l (%amiga-layout win rp)))
                ;; the overlay page (cast/use/sing/save look the same;
                ;; microfont geometry, like the takeover below)
                (let* ((*hotspots* '())
                       (lh +microfont-line-height+)
                       (cw +microfont-small-advance+)
                       (px (+ (ui-layout-bx l) 4))
                       (py (+ (ui-layout-by l) 4))
                       (max-chars (floor (- (ui-layout-fp-w l) 16) cw))
                       (rows (mapcan (lambda (line)
                                       (wrap-menu-line line max-chars))
                                     (location-lines g view)))
                       (hero-row (position-if #'menu-line-key rows))
                       (row-y (lambda (n) (+ py 4 (* n lh) 2))))
                  (%amiga-draw-page rp (location-lines g view) l)
                  (check "page: clicking the hero row picks it" #\1
                         (%hotspot-at (+ px 10)
                                      (funcall row-y hero-row)))
                  (check "page: the title row is not a target" nil
                         (%hotspot-at (+ px 10) (funcall row-y 0))))
                ;; the message-area takeover (microfont geometry)
                (let* ((*hotspots* '())
                       (fresh (make-shop-view))
                       (ox (ui-layout-log-x l))
                       (oy (ui-layout-by l))
                       (max-chars (max 4 (floor (- (ui-layout-log-w l)
                                                   4)
                                                +microfont-small-advance+)))
                       (page-rows (max 1 (floor (- (ui-layout-page-b l)
                                                   oy 2)
                                                +microfont-line-height+)))
                       (rows (let ((all
                                     (mapcan
                                      (lambda (line)
                                        (wrap-menu-line line max-chars))
                                      (location-lines g fresh))))
                               ;; the renderer's overflow rule
                               (if (> (length all) page-rows)
                                   (delete-if (lambda (r)
                                                (equal
                                                 (menu-line-text r)
                                                 ""))
                                              all)
                                   all)))
                       (hero-row (position-if #'menu-line-key rows))
                       (row-y (lambda (n)
                                (+ oy 1
                                   (* n +microfont-line-height+) 3))))
                  (%amiga-draw-takeover rp (location-lines g fresh)
                                        log l)
                  (check "takeover: clicking the hero row picks it" #\1
                         (%hotspot-at (+ ox 2)
                                      (funcall row-y hero-row)))
                  (check "takeover: the title row is not a target" nil
                         (%hotspot-at (+ ox 2) (funcall row-y 0)))
                  ;; the buy page's own keys are option rows — the
                  ;; whole row clicks as its key
                  (shop-act g fresh #\1)
                  (let* ((*hotspots* '())
                         (rows (let ((all
                                       (mapcan
                                        (lambda (line)
                                          (wrap-menu-line line
                                                          max-chars))
                                        (location-lines g fresh))))
                                 (if (> (length all) page-rows)
                                     (delete-if (lambda (r)
                                                  (equal
                                                   (menu-line-text r)
                                                   ""))
                                                all)
                                     all)))
                         (sell-row (position-if
                                    (lambda (r)
                                      (eql (menu-line-key r) #\s))
                                    rows)))
                    (%amiga-draw-takeover rp (location-lines g fresh)
                                          log l)
                    (check "takeover: the Sell option row clicks" #\s
                           (%hotspot-at (+ ox 2)
                                        (funcall row-y sell-row)))))
                t)))))
  (leave-location g))

;;; ---------------------------------------------------------------------
;;; Regression: OPEN-WINDOW/OPEN-SCREEN's WA_Title/SA_Title string must
;;; stay allocated until CLOSE-WINDOW/CLOSE-SCREEN — Intuition holds
;;; the pointer live for title-bar redraws, not just for the
;;; OpenWindowTagList/OpenScreenTagList call that installs it.  Force
;;; intervening foreign allocations (candidates to reuse a
;;; prematurely-freed title block) between open and close, confirm the
;;; title is tracked in AMIGA.INTUITION::*TITLE-STRINGS* while open,
;;; still readable through the live Window struct after the pressure,
;;; and untracked again once closed.
#+amigaos
(let ((title "Title Survival Test")
      (before (hash-table-count amiga.intuition::*title-strings*)))
  (amiga.intuition:with-window
      (win :title title :left 5 :top 5 :width 160 :height 60)
    (check "window title is tracked while the window is open"
           (1+ before)
           (hash-table-count amiga.intuition::*title-strings*))
    (dotimes (i 64)
      (ffi:free-foreign (ffi:alloc-foreign 64)))
    (check "window title string survives allocation pressure" title
           (ffi:foreign-to-string
            (ffi:make-foreign-pointer
             (amiga.intuition:window-title win)))))
  (check "window title is untracked after the window closes"
         before
         (hash-table-count amiga.intuition::*title-strings*)))

#+amigaos
(let ((before (hash-table-count amiga.intuition::*title-strings*)))
  (amiga.intuition:with-screen
      (scr :width (display-profile-screen-width *display-profile*)
           :height (display-profile-screen-height *display-profile*)
           :depth (display-profile-screen-depth *display-profile*)
           :title "Screen Title Survival Test")
    (dotimes (i 64)
      (ffi:free-foreign (ffi:alloc-foreign 64)))
    (check "screen title is tracked while the screen is open"
           (1+ before)
           (hash-table-count amiga.intuition::*title-strings*)))
  (check "screen title is untracked after the screen closes"
         before
         (hash-table-count amiga.intuition::*title-strings*)))

;; Custom screen support: open an own screen (RTG-aware mode pick),
;; set the palette, draw into a backdrop window, close it all again.
#+amigaos
(let* ((m (parse-map *art* :name "test"))
       (g (new-game m))
       (log (attach-message-log g)))
  (say g "Custom screen smoke test.")
  ;; The same path %CALL-WITH-GAME-WINDOW takes: a mode for the layout,
  ;; the screen at the height that mode's display can actually show,
  ;; and the backdrop window clamped back to the layout.  Reproducing
  ;; it here is the point — the layout invariants below are only worth
  ;; anything if they hold on the window the game really opens, which
  ;; on a PAL machine sits in the top 200 rows of a 256-row screen.
  ;;
  ;; The FONT is part of that path and not an afterthought: every row
  ;; %AMIGA-LAYOUT spends below the viewport is a multiple of the
  ;; rastport's glyph height, so the layout the game gets is topaz 8's
  ;; — which is why the session opens that font itself (%WITH-GAME-FONT)
  ;; and selects it (%GAME-RASTPORT) before laying anything out.  Measured
  ;; with the bare window rastport instead, this test would be measuring
  ;; whatever font the Workbench happens to default to: on a machine
  ;; whose default is 13 pixels tall the roster alone eats 45 rows more
  ;; and the viewport clamps to 55, which is a true reading of that font
  ;; and tells us nothing about the game.
  (let* ((mode (amiga.gfx:best-mode-id
                :width (display-profile-screen-width *display-profile*)
                :height (screen-ask-height *display-profile*)
                :depth (display-profile-screen-depth *display-profile*)))
         (display-h (and mode (amiga.intuition:display-mode-height mode)))
         (screen-h (screen-height-for *display-profile* display-h)))
   (check "amiga-ui draws on an own custom screen" t
    (%with-game-font
     (lambda (font)
       (amiga.intuition:with-screen
           (scr :width (display-profile-screen-width *display-profile*)
                :height screen-h
                :depth (display-profile-screen-depth *display-profile*)
                :title "Lambda's Tale Test"
                :mode-id mode)
         (%game-screen-palette scr)
         (check "custom screen reports its width"
                (display-profile-screen-width *display-profile*)
                (amiga.intuition:screen-width scr))
         (check "the screen is at least the layout tall"
                t (>= (amiga.intuition:screen-height scr)
                      (display-profile-screen-height *display-profile*)))
         (amiga.intuition:with-window
             (win :title nil          ; the game's backdrop is untitled
                  :left 0 :top 0
                  :width (amiga.intuition:screen-width scr)
                  :height (min (display-profile-screen-height
                                *display-profile*)
                               (amiga.intuition:screen-height scr))
                  :screen scr
                  :flags (logior amiga.intuition:+wflg-borderless+
                                 amiga.intuition:+wflg-backdrop+
                                 amiga.intuition:+wflg-activate+)
                  :idcmp amiga.intuition:+idcmp-closewindow+)
           (let* ((rp (%game-rastport win font))
                  (l (%amiga-layout win rp)))
             ;; the font the layout is designed around, selected the way
             ;; the session selects it — see the note above
             (check "the game font gives the designed line height"
                    10 (ui-layout-lh l))
             ;; Layout invariants on the game's own presentation: the
             ;; taller plaque, the roster set solid right under it
             ;; (no status line, no leading between rows).
             (check "plaque is two pixels taller than a text line"
                    (+ (ui-layout-plaque-y l) (ui-layout-lh l) 2)
                    (ui-layout-plaque-b l))
             (check "roster header sits right under the plaque"
                    (+ (ui-layout-plaque-b l) +roster-gap+)
                    (ui-layout-hdr-y l))
             (check "roster rows carry no leading"
                    (- (ui-layout-lh l) 2) (ui-layout-row-h l))
             (check "the header row is one roster row tall"
                    (+ (ui-layout-hdr-y l) (ui-layout-row-h l))
                    (ui-layout-party-y l))
             ;; The viewport gets its full asset height and the column
             ;; ends exactly on the last usable row — the live
             ;; counterpart of the ":lores fills its 200 lines" test
             ;; above, measured here with the real font.  Miss either
             ;; and a US NTSC machine loses its wall graphics:
             ;; %AMIGA-COMPOSE-FP only blits pieces at their full
             ;; size and falls back to the wireframe below it (see
             ;; *LORES-PROFILE*).
             (check "the viewport keeps its full asset height"
                    *fp-view-height* (ui-layout-fp-h l))
             (let ((last-row (+ (ui-layout-party-y l)
                                (* (ui-layout-row-h l) +party-limit+)
                                -1)))
               (check-true "seven roster rows fit above the bottom edge"
                           (<= last-row (ui-layout-bottom l)))
               (check "roster row 7 ends on the last usable row"
                      (ui-layout-bottom l) last-row)
               ;; the ornate ring is drawn 6 pixels in from the window
               ;; edge (%CHROME-BG), so the roster must clear it — the
               ;; bottom pad is what stands between the two
               (check-true "the roster clears the chrome ring"
                           (< last-row
                              (- (amiga.intuition:window-height win)
                                 1 6))))
             (%amiga-draw-fp rp g (ui-layout-bx l) (ui-layout-by l)
                             (ui-layout-fp-w l) (ui-layout-fp-h l))
             (%amiga-draw-band rp g l)
             (%amiga-draw-log rp log l)
             ;; G is a bare walkabout, so this draws a roster of
             ;; nothing but slot numbers — the empty-slot path
             (%amiga-party rp g l)
             ;; A part-filled roster: the claimed rows carry heroes
             ;; and click as their digits, the slots below them show
             ;; their number and nothing else, and clicking one of
             ;; those does nothing (there is no sheet to open).  The
             ;; numbered empty rows are the point — they are what
             ;; shows on screen that the seventh row really fits.
             (let ((party-g (new-game (parse-map *art* :name "roster")
                                      :party (list (%combat-hero "A")
                                                   (%combat-hero "B"))))
                   (row-h (ui-layout-row-h l))
                   (py (ui-layout-party-y l))
                   (px (+ (ui-layout-bx l) 3)))
               (let ((*hotspots* '()))
                 (%amiga-party rp party-g l t)
                 (check "roster: a filled row clicks as its digit" #\1
                        (%hotspot-at px (+ py 2)))
                 (check "roster: the second filled row too" #\2
                        (%hotspot-at px (+ py row-h 2)))
                 (check "roster: an empty slot is not a click target" nil
                        (%hotspot-at px (+ py (* 2 row-h) 2)))
                 (check "roster: nor is the last slot" nil
                        (%hotspot-at px (+ py (* (1- +party-limit+) row-h)
                                           2))))
               (let ((*hotspots* '()))
                 (%amiga-party rp party-g l)
                 (check "roster: not clickable unless asked" nil
                        (%hotspot-at px (+ py 2)))))
             ;; the map page (all small-face type) and the
             ;; save/load page draw on the custom screen too
             (%amiga-draw-map-page rp g l nil)
             (%amiga-draw-page rp (save-menu-lines
                                   g (make-save-menu :save))
                               l nil)
             ;; the dialog box every picker draws in: four fifths of
             ;; the content width, the leftover split evenly so it
             ;; sits centered, and wide enough that a full slot name
             ;; never truncates in the lores view column
             (multiple-value-bind (px py pw ph) (%menu-page-box l)
               (declare (ignore py ph))
               (let ((span (- (ui-layout-right l) (ui-layout-bx l))))
                 (check "the dialog page is four fifths of the content"
                        (floor (* 4 span) 5) pw)
                 (check-true "the dialog page is narrower than the content"
                             (< pw span))
                 (check-true "the dialog page is wider than the view column"
                             (> pw (ui-layout-fp-w l)))
                 (check-true "the dialog page is horizontally centered"
                             (<= (abs (- (- px (ui-layout-bx l))
                                         (- (ui-layout-right l) (+ px pw))))
                                 1))
                 (check-true "a full slot name fits the dialog page"
                             (>= (floor (- pw 16 6) +microfont-small-advance+)
                                 (+ 3 +slot-name-limit+)))))
             ;; the view-column picture contract, on the game's own
             ;; screen: a real ILBM draws and centers; a missing
             ;; file defers to the caller (falls back to the
             ;; first-person view) after logging once
             (let ((images (make-hash-table :test #'equal))
                   (path "tests/tmp-pic.iff"))
               (write-ilbm (draw-location-scene :shop 40 30) path)
               (check-true "a location picture draws in the view column"
                           (%amiga-draw-picture rp images path l log))
               (check "a missing picture defers to the caller" nil
                      (%amiga-draw-picture rp images "tests/no-such.iff"
                                           l log))
               (check-true "the missing picture said so in the log"
                           (find-if (lambda (s) (search "No image" s))
                                    (log-recent log 5)))
               (%free-images images)
               (delete-file path))
             ;; The game hides the OS screen bar: ShowTitle NIL plus
             ;; the full-height backdrop window (%CALL-WITH-GAME-WINDOW
             ;; does the same).  Probe the screen's own rastport
             ;; (offset 84 in struct Screen): were the bar layer
             ;; still in front, the screen bitmap's top rows would
             ;; hold the bar's rendering, not our pixels.
             (amiga.intuition:show-title scr nil)
             (amiga.gfx:set-a-pen rp 3)
             (amiga.gfx:rect-fill rp 0 0 50 3)
             (amiga.gfx:set-a-pen rp 1)
             (check "the screen bar stays hidden behind the backdrop"
                    3
                    (amiga.gfx:read-pixel
                     (ffi:make-foreign-pointer
                      (+ (ffi:foreign-pointer-address scr) 84))
                     25 1))
             t)))))))

;; The same probe through the production path: %CALL-WITH-GAME-WINDOW
;; (RTG-aware mode-id promotion + backdrop window + ShowTitle).  The
;; game once called ShowTitle before opening the window; on a
;; Picasso96-promoted screen the bar layer then stayed in front of the
;; later-opened backdrop and the title bar was visible in play.
;; ShowTitle must run after the window opens, and this probe holds it
;; to that.
#+amigaos
(check "the screen bar stays hidden on the game window path" 3
       (%call-with-game-window
        :screen
        (lambda (scr win)
          ;; the backdrop carries no WA_Title — even OPEN-WINDOW's
          ;; default title would cost a title bar (border-top) that
          ;; sits in front of the hidden screen bar
          (check "the game backdrop window has no top border" 0
                 (amiga.intuition:window-border-top win))
          ;; The standard pointer: the hand sprite loads, its palette
          ;; drives the sprite colors (screen colors 17-19 — read back
          ;; through the viewport's ColorMap, the diagnostic for RTG
          ;; setups where the pointer rendered black), a busy bracket
          ;; restores it, and dropping it clears the state.
          (let ((cm (amiga.gfx:viewport-color-map
                     (amiga.intuition:screen-viewport scr)))
                (*game-pointer* nil)
                (*point-pointer* nil)
                (*move-pointers* '())
                (*pointer-hot* nil))
            (%ensure-standard-pointer scr win :screen)
            (check-true "the hand pointer is loaded" *game-pointer*)
            (check-true "the pointing finger is loaded" *point-pointer*)
            (check "the four move arrows are loaded"
                   '(:forward :back :turn-left :turn-right)
                   (loop for entry on *move-pointers* by #'cddr
                         collect (first entry)))
            (check "sprite color 17 took the hand's skin tone" #x0EDB
                   (amiga.gfx:get-rgb4 cm 17))
            (check "sprite color 18 took the hand's outline" #x0111
                   (amiga.gfx:get-rgb4 cm 18))
            ;; hover feedback: crossing onto a click target shows the
            ;; finger sprite, a move zone its arrow, leaving them goes
            ;; back to the hand
            (let ((*hotspots* '((30 30 40 40 #\a :turn-left)
                                (10 10 20 20 #\w))))
              (%track-pointer-hot win 15 15)
              (check "over a click target the finger is up" :point
                     *pointer-hot*)
              (%track-pointer-hot win 35 35)
              (check "over a move zone its arrow is up" :turn-left
                     *pointer-hot*)
              (%track-pointer-hot win 5 5)
              (check "off the target the hand is back" nil
                     *pointer-hot*))
            (let ((outer *game-pointer*))
              (check "busy pointer wraps a body and restores" :ok
                     (%call-with-busy-pointer win
                      (lambda ()
                        (check "nested busy pointer runs the body"
                               :inner
                               (%call-with-busy-pointer
                                win (lambda () :inner)))
                        :ok)))
              (check "the busy bracket restores the hand pointer"
                     outer *game-pointer*))
            ;; a campaign overrides art and colors with a pointer.iff
            ;; in its tile pack: 8x2, one pen-1 pixel at (2,0) — the
            ;; hot spot — one pen-2 pixel below, a green/blue CMAP.
            ;; Every palette entry is set: WRITE-ILBM compacts NIL
            ;; entries out of the CMAP, which would shift the colors.
            (let ((img (make-image 8 2 2)))
              (setf (pixel-ref img 2 0) 1
                    (pixel-ref img 2 1) 2)
              (setf (aref (image-palette img) 0) '(0 0 0)
                    (aref (image-palette img) 1) '(0 255 0)
                    (aref (image-palette img) 2) '(0 0 255)
                    (aref (image-palette img) 3) '(255 255 255))
              (write-ilbm img "tests/pointer.iff"))
            (let ((*gfx-dir* "tests/"))
              (%ensure-standard-pointer scr win :screen))
            (destructuring-bind (chip height xoff yoff) *game-pointer*
              (check "pointer.iff: sprite height follows the art" 2
                     height)
              (check "pointer.iff: hot spot on the inked pixel" '(-2 0)
                     (list xoff yoff))
              (check "pointer.iff: plane words in chip RAM"
                     '(#x2000 #x0000)
                     (list (ffi:peek-u16 chip 4) (ffi:peek-u16 chip 6))))
            (check "pointer.iff: its CMAP drives sprite color 17" #x00F0
                   (amiga.gfx:get-rgb4 cm 17))
            (delete-file "tests/pointer.iff")
            (%free-standard-pointer win)
            (check "dropping the pointer clears the session state" nil
                   (or *game-pointer* *point-pointer* *move-pointers*
                       *pointer-hot*)))
          (let ((rp (amiga.intuition:window-rastport win)))
            (amiga.gfx:set-a-pen rp 3)
            (amiga.gfx:rect-fill rp 0 0 50 3)
            (amiga.gfx:set-a-pen rp 1)
            (amiga.gfx:read-pixel
             (ffi:make-foreign-pointer
              (+ (ffi:foreign-pointer-address scr) 84))
             25 1))))))

;; Wall-piece assets (M3): each profile's pack ILBMs load into
;; offscreen bitmaps and the first-person view composits them with
;; real OS blits.  Runs on the game's own custom screen at the
;; profile's LAYOUT height, which is what guarantees the full
;; asset-size viewport (a Workbench window can be shorter, where the
;; view correctly falls back to the wireframe).  The play path opens
;; the screen taller where the display allows — see SCREEN-HEIGHT-FOR
;; — but that only adds background below the layout, so the viewport
;; question is settled at the layout height either way.
;; Read-back probes, dead end at (0,0) facing north: the front wall
;; piece's top row is the white edge highlight.  The probe map has no
;; zone form, so it is an OUTDOOR zone: since the day-and-night sky the
;; ceiling and floor draw as flat fills in the sky and ground pens
;; (+ART-PEN-SKY+/+ART-PEN-GROUND+, re-tinted by the hour) — the packs'
;; banded backdrop tiles blit only in indoor (:DARK) zones.
;;
;; The rows come from %FP-PROBE-ROWS and the columns from
;; *FP-PROBE-COLUMNS*, the same pair the host counterpart reads (search
;; "The shipped packs, composited").  Written-down rows are what went
;; wrong here before: they were measured on the 112-row lores viewport
;; and stayed behind when Engine 0.21.0's 200-line layout cut it to
;; 100, so these four checks read the wall body where they meant to
;; read its highlight and the sky — for a dozen releases, because a
;; hand-run suite is only as current as the last hand that ran it.
#+amigaos
(dolist (spec *fp-probe-columns*)
 (destructuring-bind (pname front-x sky-x ground-x) spec
  (with-display-profile (pname)
   (let* ((m (parse-map *art* :name "test"))
          (g (new-game m))
          (rows (%fp-probe-rows))
          (front-y (first rows))
          (sky-y (second rows))
          (ground-y (third rows)))
    (%with-game-font
     (lambda (font)
      (amiga.intuition:with-screen
          (scr :width (display-profile-screen-width *display-profile*)
               :height (display-profile-screen-height *display-profile*)
               :depth (display-profile-screen-depth *display-profile*)
               :title "Walls Test"
               :mode-id (amiga.gfx:best-mode-id
                         :width (display-profile-screen-width
                                 *display-profile*)
                         :height (screen-ask-height *display-profile*)
                         :depth (display-profile-screen-depth
                                 *display-profile*)))
        (%game-screen-palette scr)
        ;; untitled: a backdrop window with a WA_Title still gets a
        ;; title bar, which would push the layout below the asset size
        (amiga.intuition:with-window
            (win :title nil :left 0 :top 0
                 :width (amiga.intuition:screen-width scr)
                 :height (amiga.intuition:screen-height scr)
                 :screen scr
                 :flags (logior amiga.intuition:+wflg-borderless+
                                amiga.intuition:+wflg-backdrop+
                                amiga.intuition:+wflg-activate+)
                 :idcmp amiga.intuition:+idcmp-closewindow+)
          (check (format nil "~A: backdrop window has no top border" pname)
                 0 (amiga.intuition:window-border-top win))
          (let* ((rp (%game-rastport win font))
                 (l (%amiga-layout win rp)))
           (multiple-value-bind (walls pack-palette)
               (%load-wall-assets rp nil)
            (check (format nil "~A: game font gives the designed ~
line height" pname)
                   10 (ui-layout-lh l))
            (check-true (format nil "~A: wall assets load into bitmaps"
                                pname)
                        walls)
            (when walls
              ;; Every piece the profile's draw distance can SHOW gets a
              ;; bitmap, plus the two backdrops.  The deeper levels are
              ;; not decoded — but %LOAD-WALL-ASSETS still required them
              ;; to exist, so reaching here at all proves the full pack
              ;; is on disk (a missing file errors out and returns NIL
              ;; walls, which the check above would have caught).
              (check (format nil "~A: every piece the draw depth shows ~
got a bitmap (walls + backdrops)" pname)
                     (+ 2 (length (wall-piece-names (%draw-depth))))
                     (hash-table-count walls))
              (check-true (format nil "~A: a piece past the draw depth ~
was not decoded" pname)
                          (or (= (%draw-depth) +view-depth+)
                              (null (gethash (list :front (%draw-depth))
                                             walls))))
              (check-true (format nil "~A: the pack palette is the ~
demo CMAP" pname)
                          (equalp pack-palette *wall-palette*))
              ;; transparency wiring: receding side pieces carry a
              ;; cookie-cut mask; opaque front pieces and backdrops don't
              (check-true (format nil "~A: a side piece got a ~
cookie-cut mask" pname)
                          (cdr (svref (gethash '(:side 0 :l) walls) 0)))
              (check-true (format nil "~A: a front piece is an opaque ~
blit (no mask)" pname)
                          (not (cdr (svref (gethash '(:front 0) walls)
                                           0))))
              (check-true (format nil "~A: the ceiling backdrop is ~
opaque (no mask)" pname)
                          (not (cdr (svref (gethash '(:ceiling) walls)
                                           0))))
              (check-true (format nil "~A: custom screen leaves the ~
full asset-size viewport" pname)
                          (= (ui-layout-fp-h l) *fp-view-height*))
              (%amiga-draw-fp rp g (ui-layout-bx l) (ui-layout-by l)
                              (ui-layout-fp-w l) (ui-layout-fp-h l)
                              walls)
              (check (format nil "~A: blitted front wall edge pixel"
                             pname)
                     1
                     (amiga.gfx:read-pixel rp (+ (ui-layout-bx l) front-x)
                                           (+ (ui-layout-by l) front-y)))
              ;; the fixture map has no :DARK, so it is an outdoor
              ;; zone: since day/night the ceiling and floor are flat
              ;; fills in the sky/ground pens (%APPLY-ZONE-PALETTE
              ;; tints them per hour) — the pack's backdrop bitmaps
              ;; only cover indoor/dark zones
              (check (format nil "~A: outdoor ceiling is the flat ~
sky-pen fill" pname)
                     +art-pen-sky+
                     (amiga.gfx:read-pixel rp (+ (ui-layout-bx l) sky-x)
                                           (+ (ui-layout-by l) sky-y)))
              (check (format nil "~A: outdoor floor is the flat ~
ground-pen fill" pname)
                     +art-pen-ground+
                     (amiga.gfx:read-pixel rp (+ (ui-layout-bx l) ground-x)
                                           (+ (ui-layout-by l) ground-y)))
              ;; The back-buffered path (the live session's) must land
              ;; the SAME pens on screen as the direct compose above:
              ;; spoil the viewport, redraw through the back buffer,
              ;; and read the same three witnesses back.
              (let ((back (%alloc-fp-backbuffer rp l)))
                (check-true (format nil "~A: fp back buffer allocates ~
on the custom screen" pname)
                            back)
                (when back
                  (unwind-protect
                      (progn
                        (amiga.gfx:set-a-pen rp 3)
                        (amiga.gfx:rect-fill
                         rp (ui-layout-bx l) (ui-layout-by l)
                         (+ (ui-layout-bx l) (ui-layout-fp-w l) -1)
                         (+ (ui-layout-by l) (ui-layout-fp-h l) -1))
                        (%amiga-draw-fp rp g (ui-layout-bx l)
                                        (ui-layout-by l)
                                        (ui-layout-fp-w l)
                                        (ui-layout-fp-h l)
                                        walls back)
                        (check (format nil "~A: back-buffered front ~
wall edge pixel" pname)
                               1
                               (amiga.gfx:read-pixel
                                rp (+ (ui-layout-bx l) front-x)
                                (+ (ui-layout-by l) front-y)))
                        (check (format nil "~A: back-buffered ceiling ~
is the sky-pen fill" pname)
                               +art-pen-sky+
                               (amiga.gfx:read-pixel
                                rp (+ (ui-layout-bx l) sky-x)
                                (+ (ui-layout-by l) sky-y)))
                        (check (format nil "~A: back-buffered floor ~
is the ground-pen fill" pname)
                               +art-pen-ground+
                               (amiga.gfx:read-pixel
                                rp (+ (ui-layout-bx l) ground-x)
                                (+ (ui-layout-by l) ground-y))))
                    (amiga.gfx:free-bitmap back))))
              ;; The planar fast path (*WALL-LOAD-PLANAR*, the default)
              ;; pokes ILBM plane rows into a scratch BitMap and lets
              ;; the blitter move them into the piece bitmap, never
              ;; going through chunky pens.  It has to land the SAME
              ;; pens on screen as WriteChunkyPixels would — so blit a
              ;; whole opaque piece to a clear patch and read it back
              ;; against the pens READ-ILBM declares for that file.
              ;; (Sampled every 7th pixel: READ-PIXEL is a library call
              ;; per pixel, and a stride catches plane-order, row-pad
              ;; and stride mistakes alike.)
              (let* ((key '(:front 0))
                     (file (concatenate 'string *gfx-dir*
                                        (wall-piece-file key)))
                     (want (read-ilbm file))
                     (bm (car (svref (gethash key walls) 0)))
                     (pw (image-width want))
                     (ph (image-height want))
                     (bad '()))
                (amiga.gfx:set-a-pen rp 0)
                (amiga.gfx:rect-fill rp 0 0 (1- pw) (1- ph))
                (amiga.gfx:blt-bitmap-rastport bm 0 0 rp 0 0 pw ph)
                (dotimes (y ph)
                  (dotimes (x pw)
                    (when (and (zerop (mod (+ x (* 3 y)) 7))
                               (< (length bad) 8)) ; keep the report short
                      (let ((got (amiga.gfx:read-pixel rp x y))
                            (expect (pixel-ref want x y)))
                        (unless (= got expect)
                          (push (list x y :got got :want expect) bad))))))
                (check (format nil "~A: planar-loaded piece blits the ~
pens its ILBM declares" pname)
                       nil bad)
                (amiga.gfx:set-a-pen rp 1))
              ;; ... and the two loaders agree piece for piece: same
              ;; bitmap contents, same mask presence.  A pack loaded
              ;; the slow way is the reference.
              (let ((chunky-walls (let ((*wall-load-planar* nil))
                                    (%load-wall-assets rp nil)))
                    (mismatched '()))
                (maphash
                 (lambda (key entries)
                   (let ((others (gethash key chunky-walls)))
                     (unless (and others
                                  (= (length entries) (length others))
                                  (every (lambda (entry other)
                                           (eq (not (cdr entry))
                                               (not (cdr other))))
                                         entries others))
                       (push (list key :mask) mismatched))))
                 walls)
                (check (format nil "~A: both loaders agree on which ~
pieces need a mask" pname)
                       nil mismatched)
                (%free-wall-assets chunky-walls)))
            (%free-wall-assets walls)))))))))))

;;; ---------------------------------------------------------------------
;;; Seams for tooling: *GAME*, *KEY-HOOK*, *TICK-HOOK* (src/game.lisp)
;;;
;;; The engine sets none of the hooks itself, so the first thing to
;;; check is that they are inert — a game that never installs anything
;;; must run exactly as it did before they existed.

(check-true "seam: the hooks are inert until something installs them"
            (and (null *key-hook*) (null *tick-hook*)))

;; PLAY without a TTY reads one key per line from *STANDARD-INPUT*.
;; That is what lets the suite drive the real host front-end headlessly:
;; feed it keys, swallow the screen it paints.
#-amigaos
(defun %play-scripted (map-file keys)
  "Run PLAY on MAP-FILE, feeding it the characters of KEYS one per
line.  Returns the text the session painted."
  (with-output-to-string (out)
    (let ((*standard-output* out)
          (*standard-input*
            (make-string-input-stream
             (with-output-to-string (s)
               (loop for c across keys do (write-char c s) (terpri s))))))
      (play map-file))))

#-amigaos
(progn
  (setf *game* nil)
  (%play-scripted "tests/world/keep.map" "qy")
  (check-true "seam: PLAY leaves the session's game in *GAME*"
              (game-p *game*))
  (check "seam: *GAME* is the game PLAY played"
         (map-title (load-map-file "tests/world/keep.map"))
         (map-title (game-map *game*))))

;; Every key, in the order struck, before any page sees it — including
;; the q that raises the quit confirmation and the y that answers it.
#-amigaos
(let ((seen '()))
  (let ((*key-hook* (lambda (game c)
                      (declare (ignore game))
                      (push c seen)
                      (eql c #\.))))
    (%play-scripted "tests/world/keep.map" ".wqy"))
  (check "seam: *KEY-HOOK* sees every key, in order"
         (list #\. #\w #\q #\y) (reverse seen)))

;; A true return consumes the key: the walkabout never acts on it.  The
;; companion run — same script, no hook — is what makes that meaningful,
;; by showing the turn lands when nothing intercepts it.  'd' rather
;; than a step: a turn cannot be refused by a wall, so the fixture's
;; geometry has no say in the result.
#-amigaos
(let ((start (dir-index (dungeon-map-start-facing
                         (load-map-file "tests/world/keep.map"))))
      (turned nil)
      (held nil))
  (setf *game* nil)
  (%play-scripted "tests/world/keep.map" "dqy")
  (setf turned (game-facing *game*))
  (setf *game* nil)
  (let ((*key-hook* (lambda (game c) (declare (ignore game)) (eql c #\d))))
    (%play-scripted "tests/world/keep.map" "dqy"))
  (setf held (game-facing *game*))
  (check "seam: an unhooked turn reaches the walkabout"
         (turn-dir start 1) turned)
  (check "seam: a consumed key never reaches the page" start held))

;; The question a cell puts (the ASK op) through the real host
;; front-end: the fixture crypt's ladder asks before it is climbed.
;; d turns east, w w walks onto the ladder and the page comes up; n
;; declines (the party stays), s steps back, w walks on again and y
;; climbs — into the keep, at the ladder's own landing — and q/y ends
;; the session there.  The keep is the zone that proves the yes: the
;; crypt has no other way out.
#-amigaos
(let ((text (let ((*encounter-rate* nil))
              (setf *game* nil)
              (%play-scripted "tests/world/crypt.map" "dwwnswyqy"))))
  (check-true "host ui: the question's page was drawn"
              (search "Climb it?" text))
  (check-true "host ui: with its option rows"
              (and (search "Yes" text) (search "No" text)))
  (check "host ui: declined, then climbed: the party is in the keep"
         "Testhold" (map-title (game-map *game*)))
  (check "host ui: at the ladder's landing" '(2 0)
         (list (game-x *game*) (game-y *game*)))
  (check "host ui: the question did not outlive its answer" nil
         (game-question *game*)))

;; *autoplay* drives a full unattended PLAY-AMIGA session: scripted keys
;; are fed one per INTUITICK (~10/s), ending in #\q #\y — q raises the
;; quit confirmation and y answers it — so the event loop exits on its
;; own.  (A script that stopped at q would hang the session: the box
;; waits for an answer.)  Verifies the whole real event path — window, menu
;; strip, redraws, key dispatch — with no user at the keyboard.  The
;; script also opens the help page (h) and leaves it (Esc), enters map
;; mode (m), toggles the debug full view (f) twice, opens help from
;; the map view too (? — the second h returns to the map) and leaves
;; map mode (m) before quitting.  The first q is answered with n: the
;; confirmation backs out, the session plays on (s steps) and only the
;; second q/y ends it.
;; The scripted keys also open a character sheet (1), switch to another
;; roster slot (2) and leave it (:esc) — exercising the whole :sheet
;; mode through the real event loop.  The fixture crypt is a :DARK
;; zone, so the whole session renders at the one-cell darkness view
;; depth.  This session keeps :DISPLAY :WINDOW — the Workbench-window
;; development view — alive as a smoke test (it only checks the
;; session reaches :DONE, so it isn't sensitive to the Workbench
;; geometry variability that gates the detailed layout checks behind
;; :LAMBDA-TALE-WINDOW-TESTS above); the rest of the suite runs
;; :DISPLAY :SCREEN, the game's presentation.
#+amigaos
(check "amiga-ui autoplay plays a scripted session and quits" :done
       (let ((*autoplay* (list #\w #\d #\1 #\2 :esc #\w #\a
                               #\h #\d #\u :esc   ; help, scrolled there
                                                  ; and back (u/d)
                               #\m #\f #\f #\? #\h #\m #\s
                               #\q #\n          ; asked, backed out
                               #\s #\q #\y)))   ; asked again, answered
         (play-amiga "tests/world/crypt.map" :display :window)
         :done))

;; The crypt ladder's question (the ASK op) through the real Amiga
;; event loop: d turns east, w w walks onto the ladder and the box
;; draws over the play page (%AMIGA-DRAW-CONFIRM with the question's
;; own lines — it is modal for the mouse too, so the click at the far
;; left lands on the page-wide "no" behind the box and declines), s
;; steps back, w walks on again and y climbs — a zone change under
;; the box, repainted whole by the answer's redraw.  Checked on where
;; the party lands: the keep's (2,0) is a cell only the climb reaches.
#+amigaos
(check "amiga-ui autoplay answers the crypt ladder's question"
       '("Testhold" 2 0)
       (let ((*autoplay* (list #\d #\w #\w
                               '(:click 15 136)     ; beside the box: no
                               #\s #\w #\y
                               #\q #\y)))
         (setf *game* nil)
         (play-amiga "tests/world/crypt.map" :display :screen)
         (list (map-title (game-map *game*))
               (game-x *game*) (game-y *game*))))

;;; The tooling seams through the real Amiga event loop: the key hook
;;; ahead of the front-end's own dispatch, and the heartbeat hook that a
;;; debug channel drains its posted work on.  *AUTOPLAY* keys reach ACT
;;; by the same path a struck key does, so the hook sees them too.
#+amigaos
(check-true "seam: PLAY-AMIGA leaves the session's game in *GAME*"
            (let ((*autoplay* (list #\w #\q #\y)))
              (setf *game* nil)
              (play-amiga "tests/world/crypt.map" :display :window)
              (game-p *game*)))

#+amigaos
(check "seam: *KEY-HOOK* consumes a key before the Amiga pages"
       '(#\. #\w #\q #\y)
       (let* ((seen '())
              (*key-hook* (lambda (game c)
                            (declare (ignore game))
                            (push c seen)
                            (eql c #\.)))
              (*autoplay* (list #\. #\w #\q #\y)))
         (play-amiga "tests/world/crypt.map" :display :window)
         (reverse seen)))

;; The heartbeat runs many times over a session; the hook returning NIL
;; must leave the frame alone (a redraw at tick rate is more than a
;; 68020 has to give — see *TICK-HOOK*).
#+amigaos
(check-true "seam: *TICK-HOOK* runs on the Amiga heartbeat"
            (let* ((ticks 0)
                   (*tick-hook* (lambda (g) (declare (ignore g))
                                  (incf ticks)
                                  nil))
                   (*autoplay* (list #\w #\d #\q #\y)))
              (play-amiga "tests/world/crypt.map" :display :window)
              (plusp ticks)))

;; ... and a hook that reports a change drives the redraw branch.
#+amigaos
(check "seam: a *TICK-HOOK* reporting a change redraws the frame" :done
       (let ((*tick-hook* (lambda (g) (declare (ignore g)) t))
             (*autoplay* (list #\w #\d #\q #\y)))
         (play-amiga "tests/world/crypt.map" :display :window)
         :done))

;; The keep: an unattended session first casts through the real event
;; loop — open the cast menu (c), pick Wanda the wizard (2), cast the
;; flame (1), then the compass (4: the band draws the rose and the
;; map footer shows the facing for the rest of the session) — and
;; Wilhelm strikes up the march through the sing menu (p, 1, 1 — his
;; one tune).  Then it saves and reloads through the slot picker (S,
;; n, type "t1", Return; L, 1 — the whole name-entry and re-wire path
;; through real vanillakeys), turns to the shoppe door (a LOCATION
;; special), shops for real — pick a hero (1), buy the sword (1) and
;; a torch (2), flip to the sell page (s), sell the sword again (1),
;; back out (Esc Esc) — walks east to the tavern, where Wilhelm's
;; drink (1) brings his tunes back (Esc leaves), drops down the stairs
;; into the crypt (a :DARK zone — Wanda's flame is what keeps the view
;; lit) and burns the bought torch through the use menu (u, hero 1,
;; item 1) before quitting.
#+amigaos
(check "amiga-ui autoplay casts, saves, shops, drops to the dark crypt"
       :done
       (let ((*autoplay* (list #\c #\2 #\1
                               #\c #\2 #\4
                               #\p #\1 #\1
                               #\S #\n #\t #\1 #\Return
                               #\L #\1
                               #\d #\w
                               #\1 #\1 #\2 #\s #\1 :esc :esc
                               #\w #\w #\1 :esc
                               #\w #\w
                               #\u #\1 #\1 #\q #\y))
             ;; scratch save, like every other test's tests/tmp-* state —
             ;; keeps the real saves/ dir untouched by the test suite
             (*save-dir* "tests/tmp-saves/"))
         (play-amiga "tests/world/keep.map" :display :screen)
         (when (probe-file "tests/tmp-saves/t1.sav")
           (delete-file "tests/tmp-saves/t1.sav"))
         :done))

;; The same unattended session on an own custom screen (:display :screen)
;; — the whole open-screen/backdrop-window/menus/event-loop path, with
;; the tile pack named explicitly (:gfx-dir): the lores depth-5 screen,
;; the pack palette and the ceiling/floor backdrop all draw for real.
#+amigaos
(check "amiga-ui autoplay on an own custom screen" :done
       (let ((*autoplay* (list #\w #\d #\m #\m #\q #\y)))
         (play-amiga "tests/world/crypt.map" :display :screen
                                             :gfx-dir (engine-path "data/gfx/"))
         :done))

;; The hires profile end to end: its 640x256 depth-4 screen, the
;; 240x130 viewport and the data/gfx-hires pack, through the same
;; scripted event loop.
#+amigaos
(check "amiga-ui autoplay on the hires profile" :done
       (let ((*autoplay* (list #\w #\d #\m #\m #\q #\y)))
         (play-amiga "tests/world/crypt.map" :display :screen
                                             :profile :hires)
         :done))

;; Mouse control through the same scripted loop: (:CLICK X Y) entries
;; resolve through the live hotspot map exactly like a left click —
;; the view's walk zones, a roster row (opens that character sheet)
;; and the sheet's click-anywhere-else Esc.  The lores custom screen
;; lays out deterministically (borderless backdrop, pads 10/10, the
;; 120x100 view at 10,10; the solid-set topaz-8 roster puts party row
;; 1 at y=133..140), so
;; the script clicks absolute pixels.  The quit confirmation is modal
;; for the mouse too: while it is up the only click targets are its own
;; rows and the page-wide "no" behind them, so the click at the far
;; left backs out of it.
#+amigaos
(check "amiga-ui autoplay drives the game by mouse clicks" :done
       (let ((*autoplay* (list '(:click 90 60)   ; view middle: forward
                               '(:click 20 60)   ; left quarter: turn
                               '(:click 15 136)  ; roster row 1: sheet
                               '(:click 90 60)   ; off-target: Esc back
                               #\q
                               '(:click 15 136)  ; beside the box: no
                               #\q #\y)))
         (play-amiga "tests/world/crypt.map" :display :screen
                                             :gfx-dir (engine-path "data/gfx/"))
         :done))

;;; ---------------------------------------------------------------------
;;; The debug log (src/debug-log.lisp): a timestamped trace file —
;;; image/map/campaign loads with durations, emitted events, key
;;; presses — switchable at runtime and free when off.

(defun %slurp-file (path)
  (with-open-file (s path)
    (let ((out (make-string-output-stream)))
      (loop for line = (read-line s nil nil)
            while line
            do (progn (write-string line out)
                      (write-char #\Newline out)))
      (get-output-stream-string out))))

(let ((path "tests/tmp-saves/debug.log"))
  (when (probe-file path) (delete-file path))
  (check "debug log is off by default" nil (debug-log-enabled-p))
  (dlog "never written")
  (check-true "dlog while disabled writes no file"
              (not (probe-file path)))
  (check "enable returns the path" path (debug-log-enable path))
  (check-true "enabled-p reports the open log" (debug-log-enabled-p))
  (dlog "hello ~A ~D" "world" 42)
  (check "dlog-timed returns its body's value" 3
         (dlog-timed ("timed block") (+ 1 2)))
  (check "dlog-timed passes multiple values through" '(:a :b)
         (multiple-value-list (dlog-timed ("mv block") (values :a :b))))
  ;; events trace with brief arguments and the handler count
  (let ((g (new-game (parse-map *art* :name "log fixture"))))
    (on-event g :ping (lambda (gm n) (declare (ignore gm n))))
    (emit g :ping 7)
    (emit g :enter-zone (game-map g)))
  ;; the loaders leave timed lines
  (load-map-file "tests/world/keep.map")
  (read-ilbm (engine-path "data/gfx/ceiling.iff"))
  (debug-log-disable)
  (check "disable closes the log" nil (debug-log-enabled-p))
  (let ((text (%slurp-file path)))
    (check-true "the log opens with the session banner"
                (search "debug log enabled" text))
    (check-true "lines carry a bracketed timestamp"
                (eql #\[ (char text 0)))
    (check-true "the timestamp has a millisecond fraction"
                (let ((dot (position #\. text)))
                  (and dot (< dot (position #\] text)))))
    (check-true "dlog formats its arguments"
                (search "hello world 42" text))
    (check-true "dlog-timed writes the begin line"
                (search "timed block ..." text))
    (check-true "dlog-timed writes the timed done line"
                (search "timed block done [" text))
    (check-true "done lines carry a millisecond duration"
                (search " ms]" text))
    (check-true "an event line names topic, args and handler count"
                (search "event :PING 7 handlers=1" text))
    (check-true "event arguments print briefly"
                (search "#<map log fixture>" text))
    (check-true "a map load leaves a timed line"
                (search "map tests/world/keep.map done [" text))
    (check-true "story forms leave a read/apply split line while enabled"
                (search "story forms tests/world/keep.map" text))
    (check-true "an image load leaves a timed line"
                (search "ceiling.iff done [" text))
    (check-true "the log closes with the banner"
                (search "debug log disabled" text)))
  ;; off means off: nothing is written; re-enabling appends
  (let ((len (length (%slurp-file path))))
    (dlog "dropped")
    (check "dlog after disable writes nothing"
           len (length (%slurp-file path)))
    (debug-log-enable path)
    (debug-log-disable)
    (check-true "re-enabling appends a second session"
                (> (length (%slurp-file path)) len)))
  (delete-file path))

;; Regression: the whole-second fields (from GET-UNIVERSAL-TIME) and the
;; millisecond field (from GET-INTERNAL-REAL-TIME) once came from two
;; clocks with unrelated epochs, so the printed ".mmm" did not actually
;; belong to the printed HH:MM:SS.  Pin the wall-clock anchor to a known
;; moment and confirm the printed date/time matches DECODE-UNIVERSAL-TIME
;; of that exact anchor — that only holds when both fields derive from
;; the same anchored clock.
(let ((path "tests/tmp-saves/debug-clock.log"))
  (when (probe-file path) (delete-file path))
  (debug-log-enable path)
  (setf *debug-log-anchor-universal* 1000000000
        *debug-log-anchor-real* (get-internal-real-time))
  (dlog "clock check")
  (debug-log-disable)
  (let ((text (%slurp-file path)))
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time 1000000000)
      (check-true "sec/min/hour and the ms fraction derive from the same anchored clock"
                  (search (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D."
                                  year month day hour min sec)
                          text))))
  (delete-file path))

;; The debug log traces a real session end to end: the session line,
;; every key through the real event loop, the wall-pack image loads
;; and the emitted events.
#+amigaos
(check-true "amiga-ui autoplay leaves a debug-log trace"
            (let ((path "tests/tmp-saves/debug-amiga.log")
                  (*autoplay* (list #\w #\q #\y)))
              (when (probe-file path) (delete-file path))
              (debug-log-enable path)
              (play-amiga "tests/world/crypt.map" :display :screen)
              (debug-log-disable)
              (let ((text (%slurp-file path)))
                (prog1 (and (search "play-amiga tests/world/crypt.map" text)
                            (search "key #\\w mode :PLAY" text)
                            (search "wall pack" text)
                            (search "image" text)
                            (search "event :" text)
                            t)
                  (delete-file path)))))

;;; ---------------------------------------------------------------------
;;; Automap wall runs (MAP-EDGE-RUNS — the map page's draw list)

(flet ((runs-equal (label expected actual)
         (check-true label (null (set-exclusive-or expected actual
                                                   :test #'equal)))))
  (let ((m (parse-map *art*)))
    ;; omniscient: every wall of the 3x2 fixture, merged into runs
    (multiple-value-bind (walls doors) (map-edge-runs m nil 0 0 3 2)
      (runs-equal "edge runs: omniscient walls"
                  '((t 0 0 3) (t 0 2 3)
                    (nil 0 0 2) (nil 1 1 1) (nil 2 0 1) (nil 3 0 2))
                  walls)
      (runs-equal "edge runs: omniscient doors" '((t 1 1 1)) doors))
    ;; a fresh knowledge shows nothing
    (let ((k (make-map-knowledge m)))
      (multiple-value-bind (walls doors) (map-edge-runs m k 0 0 3 2)
        (check "edge runs: unexplored map is empty" nil walls)
        (check "edge runs: unexplored map has no doors" nil doors))
      ;; standing in (0,0) reveals exactly its own walls
      (know-cell k 0 0)
      (multiple-value-bind (walls doors) (map-edge-runs m k 0 0 3 2)
        (runs-equal "edge runs: one explored cell -> its walls"
                    '((t 0 0 1) (nil 0 0 1)) walls)
        (check "edge runs: no door seen yet" nil doors)))
    ;; adjacent known walls merge into a single run
    (let ((k (make-map-knowledge m)))
      (know-wall k 0 0 :north)
      (know-wall k 1 0 :north)
      (know-wall k 2 0 :north)
      (multiple-value-bind (walls doors) (map-edge-runs m k 0 0 3 2)
        (declare (ignore doors))
        (runs-equal "edge runs: three known walls merge into one run"
                    '((t 0 0 3)) walls)))
    ;; a shared edge shows from either side's knowledge
    (let ((k (make-map-knowledge m)))
      (know-wall k 1 0 :south)          ; the far (northern) cell only
      (multiple-value-bind (walls doors) (map-edge-runs m k 0 0 3 2)
        (declare (ignore walls))
        (runs-equal "edge runs: door known from the far side"
                    '((t 1 1 1)) doors)))
    (let ((k (make-map-knowledge m)))
      (know-wall k 1 1 :north)          ; the near (southern) cell only
      (multiple-value-bind (walls doors) (map-edge-runs m k 0 0 3 2)
        (declare (ignore walls))
        (runs-equal "edge runs: door known from the near side"
                    '((t 1 1 1)) doors)))
    ;; a sub-region stays in region-relative coordinates
    (multiple-value-bind (walls doors) (map-edge-runs m nil 1 1 2 1)
      (runs-equal "edge runs: sub-region walls are region-relative"
                  '((t 0 1 2) (nil 0 0 1) (nil 2 0 1))
                  walls)
      (runs-equal "edge runs: sub-region door is region-relative"
                  '((t 0 0 1)) doors))))

;;; ---------------------------------------------------------------------
;;; Facades from the street (CELL-LOCATION-OP / FACING-LOCATION-IMAGE-PATH)

(let* ((m (parse-map *art*))
       (g (new-game m)))
  (setf (cell-special m 1 1)
        '((location "The Cooper's" :house :image "gfx/house-1.iff")))
  (check "cell-location-op finds the location tail"
         '("The Cooper's" :house :image "gfx/house-1.iff")
         (cell-location-op m 1 1))
  (check "cell-location-op is NIL without a location op"
         nil (cell-location-op m 0 1))
  ;; stand in the street before the door, facing it
  (setf (game-x g) 1 (game-y g) 0 (game-facing g) +south+)
  (check "facing a location's door shows its facade"
         "gfx/house-1.iff" (facing-location-image-path g))
  (setf (game-facing g) +north+)
  (check "facing a plain wall shows no facade"
         nil (facing-location-image-path g))
  (setf (game-facing g) +east+)
  (check "facing open street shows no facade"
         nil (facing-location-image-path g))
  ;; a location with both pictures: :facade is the street face,
  ;; :image the inside picture (the shop/tavern/house-interior split)
  (setf (cell-special m 1 1)
        '((location "The Cooper's" :house
           :image "gfx/interior-1.iff" :facade "gfx/house-1.iff")))
  (setf (game-facing g) +south+)
  (check ":facade wins over :image from the street"
         "gfx/house-1.iff" (facing-location-image-path g))
  (let ((loc (enter-location g '("The Cooper's" :house
                                 :image "gfx/interior-1.iff"
                                 :facade "gfx/house-1.iff"))))
    (check "inside, :image is the location's picture"
           "gfx/interior-1.iff" (location-image loc))
    (check "location-image-path resolves the inside picture"
           "gfx/interior-1.iff" (location-image-path g))
    (leave-location g))
  ;; a door with nothing behind it stays bare
  (setf (cell-special m 1 1) nil)
  (setf (game-facing g) +south+)
  (check "a door without a location shows no facade"
         nil (facing-location-image-path g)))

;;; ---------------------------------------------------------------------
;;; The map legend (MAP-LEGEND-ENTRIES)

(let* ((m (parse-map *art*))
       (k (make-map-knowledge m)))
  (setf (cell-special m 1 1)
        '((location "A Stone Cottage" :house :image "gfx/house-0.iff")))
  (setf (cell-special m 2 1) '((location "Wolfgar's Arms" :shop)))
  (setf (cell-special m 0 1) '((message "just a street sign")))
  (check "legend: nothing found yet" nil (map-legend-entries m k))
  (know-cell k 1 1)
  (check "legend: a plain house is scenery, not a legend entry"
         nil (map-legend-entries m k))
  (know-cell k 2 1)
  (check "legend: a special place is listed once found"
         '((#\1 2 1 "Wolfgar's Arms"))
         (map-legend-entries m k))
  (check "legend: full mode lists every special place, found or not"
         '((#\1 2 1 "Wolfgar's Arms"))
         (map-legend-entries m nil :full t))
  (check "legend: full mode leaves houses off too"
         nil (find 1 (map-legend-entries m nil :full t) :key #'second))
  (check "legend: message-only specials are not locations"
         nil (find 0 (map-legend-entries m nil :full t) :key #'second))
  ;; any kind but :house is a place, and places come in map order
  (setf (cell-special m 0 0) '((location "The Temple" :temple)))
  (check "legend: kinds other than :house are always places"
         '((#\1 0 0 "The Temple") (#\2 2 1 "Wolfgar's Arms"))
         (map-legend-entries m nil :full t)))

;; A place is found from the street too: standing right before it,
;; facing it, is enough (see OBSERVE) — entering is not required.
(let* ((m (parse-map *art*))
       (g (new-game m))
       (k (game-knowledge g)))
  (setf (cell-special m 1 1) '((location "Wolfgar's Arms" :shop)))
  ;; before the door but facing away: nothing found
  (setf (game-x g) 1 (game-y g) 0 (game-facing g) +north+)
  (observe g)
  (check "legend: a place beside the party stays unfound"
         nil (map-legend-entries m k))
  ;; turn to face the door: found, without a step inside
  (setf (game-facing g) +south+)
  (observe g)
  (check-true "facing the door marks the place found"
              (cell-found-p k 1 1))
  (check "found from the street is not explored"
         nil (cell-explored-p k 1 1))
  (check "legend: the place shows once faced from the street"
         '((#\1 1 1 "Wolfgar's Arms"))
         (map-legend-entries m k))
  ;; an open-fronted place one cell ahead is spotted the same way
  (setf (cell-special m 1 0) '((location "The Fountain" :temple)))
  (setf (game-x g) 0 (game-y g) 0 (game-facing g) +east+)
  (observe g)
  (check "legend: an open-fronted place ahead is found too"
         '((#\1 1 0 "The Fountain") (#\2 1 1 "Wolfgar's Arms"))
         (map-legend-entries m k)))

;;; ---------------------------------------------------------------------
;;; Keyboard input normalization (src/keys.lisp): VANILLA-KEY-CHAR maps
;;; an IDCMP_VANILLAKEY Code+Qualifier to the key ACT dispatches on.
;;; Letter case must come from the Shift qualifier alone — with Caps
;;; Lock active (or desynced under emulation) the keymap delivers 'S'
;;; for a plain 's', which used to open the save picker instead of
;;; stepping back.  Qualifier bits: LSHIFT #x0001, RSHIFT #x0002,
;;; CAPSLOCK #x0004 (devices/inputevent.h).

(check "keys: plain s stays lowercase (step back, not the save picker)"
       #\s (vanilla-key-char (char-code #\s) 0))
(check "keys: Shift-S is uppercase (opens the save picker)"
       #\S (vanilla-key-char (char-code #\S) #x0001))
(check "keys: right Shift counts too"
       #\S (vanilla-key-char (char-code #\S) #x0002))
(check "keys: Caps Lock without Shift is downcased — the bug this guards"
       #\s (vanilla-key-char (char-code #\S) #x0004))
(check "keys: Caps Lock with Shift held still yields uppercase"
       #\S (vanilla-key-char (char-code #\S) #x0005))
(check "keys: a lowercase char with Shift held is upcased"
       #\L (vanilla-key-char (char-code #\l) #x0001))
(check "keys: Escape maps to :ESC whatever the qualifier"
       :esc (vanilla-key-char 27 #x0004))
(check "keys: caseless chars pass through untouched (? needs Shift)"
       #\? (vanilla-key-char (char-code #\?) #x0001))
(check "keys: digits pass through untouched under Caps Lock"
       #\1 (vanilla-key-char (char-code #\1) #x0004))

;; The digit keys translate from their raw POSITION code when the
;; front-end can supply it (AMIGA.INTUITION:MSG-RAW-KEY): a shift
;; wedged down on the host — the emulator screenshot-chord classic —
;; rides the qualifier into the keymap, the digit row arrives as
;; punctuation, and hero selection silently dies.  The raw code never
;; moves: 0x01-0x0A is the digit row and the pad its own block on
;; every Amiga keymap.
(check "keys: the digit row answers by position, not by keymap"
       #\1 (vanilla-key-char (char-code #\!) #x0001 #x01))
(check "keys: ... the whole row, 0 at its end"
       #\0 (vanilla-key-char (char-code #\)) #x0001 #x0a))
(check "keys: the numeric pad answers by position too"
       #\7 (vanilla-key-char (char-code #\7) 0 #x3d))
(check "keys: a digit key with no shift wedged is the same digit"
       #\4 (vanilla-key-char (char-code #\4) 0 #x04))
(check "keys: a letter key's raw code changes nothing"
       #\s (vanilla-key-char (char-code #\s) 0 #x21))
(check "keys: Shift-S still opens by case, raw code or none"
       #\S (vanilla-key-char (char-code #\S) #x0001 #x21))
(check "keys: Escape stays :ESC beside its raw code"
       :esc (vanilla-key-char 27 0 #x45))
(check "keys: raw-key-digit ignores every other key" nil
       (raw-key-digit #x40))
(check "keys: ... and a missing raw code" nil (raw-key-digit nil))

;;; ---------------------------------------------------------------------
;;; The Amiga menu strip (src/keys.lisp): the model AMIGA-UI turns into
;;; a GadTools NewMenu array, and MENU-PICK-ACTION, which reads an
;;; IDCMP_MENUPICK code back out of it.  The strip is the mouse's only
;;; way to the pages that open out of nothing — map, help, cast, play,
;;; use, save, load, quit — so its shape is a specification, not a
;;; detail: the decode indexes straight into *MENU-STRIP*, and a
;;; reordered item is a differently numbered item.

(flet ((pick (menu item)
         ;; a FULLMENUNUM as Intuition packs it: menu bits 0-4, item
         ;; bits 5-10 (sub-item bits 11-15 unused by this strip)
         (menu-pick-action (logior menu (ash item 5)))))
  (check "menu: Game holds Save, Load, a bar and Quit"
         '(:save :load nil :quit)
         (list (pick 0 0) (pick 0 1) (pick 0 2) (pick 0 3)))
  (check "menu: Screens holds Map, Help, a bar, Cast, Play and Use"
         '(:map :help nil :cast :play :use)
         (list (pick 1 0) (pick 1 1) (pick 1 2)
               (pick 1 3) (pick 1 4) (pick 1 5)))
  (check "menu: an item past the end of a drop-down names nothing" nil
         (pick 1 6))
  (check "menu: a drop-down past the end of the strip names nothing" nil
         (pick 2 0)))

;; The sub-item field is NOSUB (#x1F) on every pick from a strip with
;; no sub-items — decoding must mask it off, not carry it into the
;; item number.
(check "menu: the sub-item bits do not disturb the item" :cast
       (menu-pick-action (logior 1 (ash 3 5) (ash #x1F 11))))
(check "menu: MENUNULL names nothing" nil (menu-pick-action +menu-null+))
(check "menu: a missing code names nothing" nil (menu-pick-action nil))

(let ((actions '())
      (labels-seen '()))
  (dolist (menu *menu-strip*)
    (dolist (item (rest menu))
      (when (consp item)
        (push (first item) actions)
        (push (second item) labels-seen))))
  (setf actions (nreverse actions)
        labels-seen (nreverse labels-seen))
  ;; every action the MENUPICK handler in AMIGA-UI dispatches on — an
  ;; item added here without a case there would be a dead menu row
  (check "menu: the strip names exactly the actions the UI handles"
         '(:save :load :quit :map :help :cast :play :use)
         actions)
  (check "menu: the drop-down titles, in order"
         '("Game" "Screens")
         (mapcar #'first *menu-strip*))
  (check-true "menu: every label is a non-empty string"
              (every (lambda (s) (and (stringp s) (plusp (length s))))
                     labels-seen))
  ;; An item is (ACTION LABEL) and nothing more: Intuition can only
  ;; show right-Amiga+key as a commkey, while the game's own shortcuts
  ;; are Shift-S, Shift-L, Q, M, H, C, P and U — a shortcut column
  ;; here could only ever contradict the help page.
  (check-true "menu: no item carries a shortcut of its own"
              (every (lambda (menu)
                       (every (lambda (item)
                                (or (eq item :bar)
                                    (= (length item) 2)))
                              (rest menu)))
                     *menu-strip*)))

;;; The GadTools translation (AMIGA-UI, so AmigaOS only): the same
;;; strip spelled as a NewMenu entry list — a title row per drop-down
;;; and its items after it, separators keeping their slots so Intuition
;;; numbers the items the way MENU-PICK-ACTION reads them back.  No
;;; entry carries a :COMMKEY: Intuition would show right-Amiga+key,
;;; which is not what any of these pages answers to.
#+amigaos
(check "menu: the strip becomes the NewMenu list GadTools wants"
       (list (list amiga.gadtools:+nm-title+ "Game")
             (list amiga.gadtools:+nm-item+ "Save")
             (list amiga.gadtools:+nm-item+ "Load")
             :bar
             (list amiga.gadtools:+nm-item+ "Quit")
             (list amiga.gadtools:+nm-title+ "Screens")
             (list amiga.gadtools:+nm-item+ "Map")
             (list amiga.gadtools:+nm-item+ "Help")
             :bar
             (list amiga.gadtools:+nm-item+ "Cast")
             (list amiga.gadtools:+nm-item+ "Play")
             (list amiga.gadtools:+nm-item+ "Use"))
       *menu-entries*)

;;; ---------------------------------------------------------------------
;;; Version (src/version.lisp)

(check "version: ENGINE-VERSION-STRING is built from the constants"
       (format nil "~D.~D.~D" +engine-version-major+
               +engine-version-minor+ +engine-version-patch+)
       (engine-version-string))
(check "version: ENGINE-VERSION yields major, minor, patch"
       (list +engine-version-major+ +engine-version-minor+ +engine-version-patch+)
       (multiple-value-list (engine-version)))
(check-true "version: the components are non-negative integers"
            (every (lambda (n) (and (integerp n) (>= n 0)))
                   (multiple-value-list (engine-version))))
(check-true "version: the engine names itself"
            (and (stringp *engine-name*) (plusp (length *engine-name*))))
;; DD.MM.YYYY, the repo's date convention (CL_VERSION_DATE).
(check "version: the date is DD.MM.YYYY" 10 (length *engine-version-date*))
(check-true "version: the date's separators are dots"
            (and (char= #\. (char *engine-version-date* 2))
                 (char= #\. (char *engine-version-date* 5))))
(check-true "version: the date is otherwise all digits"
            (every #'digit-char-p
                   (remove #\. *engine-version-date*)))
;; The game slots stay empty in an engine-only session — this suite
;; loads no game.  A game's version.lisp fills them in (see the Closure
;; suite next door, which checks the other side of this contract).
(check "version: no game name until a game sets one" nil *game-name*)
(check "version: no game version until a game sets one" nil *game-version*)
(check "version: no game date until a game sets one" nil *game-version-date*)
;; The version symbols are engine API, not internals.
(dolist (name '("ENGINE-VERSION" "ENGINE-VERSION-STRING" "*ENGINE-NAME*"
                "*ENGINE-VERSION-DATE*" "+ENGINE-VERSION-MAJOR+"
                "*GAME-NAME*" "*GAME-VERSION*" "*GAME-VERSION-DATE*"))
  (multiple-value-bind (sym status) (find-symbol name "TALE")
    (declare (ignore sym))
    (check (format nil "version: TALE exports ~A" name) :external status)))

;;; ---------------------------------------------------------------------
;;; Sound: 8SVX samples, the cue layer and sound packs (M4)

;; WRITE-8SVX / READ-8SVX round-trip — an odd sample count pads the
;; BODY chunk on disk but comes back exactly.
(let ((bytes (make-array 101 :element-type '(unsigned-byte 8))))
  (dotimes (i 101) (setf (aref bytes i) (mod (* i 7) 256)))
  (write-8svx "tests/tmp-sound.8svx" (make-sound 8000 bytes))
  (let ((s (read-8svx "tests/tmp-sound.8svx")))
    (check "8svx round-trip: rate" 8000 (sound-rate s))
    (check "8svx round-trip: length survives the pad" 101
           (length (sound-bytes s)))
    (check-true "8svx round-trip: samples" (equalp bytes (sound-bytes s)))))
(delete-file "tests/tmp-sound.8svx")

;; Error paths: not IFF at all, missing file.
(with-open-file (s "tests/tmp-sound.8svx" :direction :output
                   :if-exists :supersede)
  (write-line "not an iff file at all" s))
(check-error "read-8svx rejects a non-IFF file"
  (read-8svx "tests/tmp-sound.8svx"))
(delete-file "tests/tmp-sound.8svx")
(check-error "read-8svx on a missing file"
  (read-8svx "tests/no-such.8svx"))

;; Fibonacci-delta BODY (sCompression 1): seed 0, then nibble codes
;; 9 10 15 8 = deltas +1 +2 +21 +0 -> samples 1 3 24 24.
(let ((bytes '(70 79 82 77  0 0 0 44        ; FORM, size 44
               56 83 86 88                  ; 8SVX
               86 72 68 82  0 0 0 20        ; VHDR, size 20
               0 0 0 4  0 0 0 0  0 0 0 32   ; oneShot, repeat, hiCycle
               31 64 1 1  0 1 0 0           ; 8000 Hz, 1 octave, fib, 1.0
               66 79 68 89  0 0 0 4         ; BODY, size 4
               0 0 154 248)))               ; pad, seed 0, 4 codes
  (with-open-file (s "tests/tmp-sound.8svx" :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
    (write-sequence (coerce bytes 'vector) s)))
(let ((s (read-8svx "tests/tmp-sound.8svx")))
  (check "fibonacci-delta: rate read" 8000 (sound-rate s))
  (check "fibonacci-delta: two samples per code byte" 4
         (length (sound-bytes s)))
  (check-true "fibonacci-delta: decoded samples"
              (equalp #(1 3 24 24) (sound-bytes s))))
(delete-file "tests/tmp-sound.8svx")

;; PLAY-SOUND: silent without a backend, funnels cues through one.
(check "play-sound without a backend returns the cue" 'hit
       (play-sound 'hit))
(let ((heard '()))
  (let ((*sound-backend* (lambda (name) (push name heard))))
    (play-sound 'door)
    (play-sound 'coin))
  (check "play-sound funnels every cue through the backend"
         '(door coin) (reverse heard)))

;; The cue recorder the event checks below share.
(defmacro with-cues ((cues) &body body)
  "Run BODY with a recording *SOUND-BACKEND* bound; CUES is a closure
yielding the cues played so far, oldest first."
  (let ((acc (gensym "CUES")))
    `(let* ((,acc '())
            (*sound-backend* (lambda (name) (push name ,acc)))
            (,cues (lambda () (reverse ,acc))))
       ,@body)))

;; ATTACH-SOUNDS: a wall bump and a door step become their cues.
(let* ((m (parse-map *art* :name "test"))
       (g (attach-sounds (new-game m :party (list (%combat-hero))))))
  (with-cues (cues)
    (move-party g :forward)             ; north into the border: blocked
    (turn-right g)
    (move-party g :forward)             ; east to (1,0): plain step
    (turn-right g)
    (move-party g :forward)             ; south through the door
    (check "movement cues: the bump and the door, steps silent"
           '(blocked door) (funcall cues))))

;; Combat cues: the sting, the killing blow, the fanfare — the same
;; scripted round the clean-kill check above plays.
(let* ((m (parse-map *art* :name "test"))
       (g (attach-sounds (new-game m :party (list (%combat-hero))))))
  (with-cues (cues)
    (start-combat g '(("test rat" 1)))
    (with-rng (10 2) (combat-round g))
    (check "combat cues: sting, slay, victory"
           '(combat slay victory) (funcall cues))))

;; A landed blow that does not kill cues HIT (via the shared
;; %STRIKE-MONSTER funnel), a miss cues MISS.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (attach-sounds (new-game m :party (list h)))))
  (start-combat g '(("test ogre" 1)))
  (let ((ogre (first (combat-monsters (game-combat g)))))
    (setf (monster-hp ogre) 50)
    (with-cues (cues)
      (%strike-monster g "Alva" ogre 3)
      (check "a landed blow cues hit" '(hit) (funcall cues)))
    (with-cues (cues)
      (with-rng (0) (%hero-attack g h ogre))   ; roll 1: a clean miss
      (check "a swing gone wide cues miss" '(miss) (funcall cues)))))

;; Hero damage: HURT while they stand, the death cry alone when they
;; fall (never both for one blow).
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (attach-sounds (new-game m :party (list h)))))
  (with-cues (cues)
    (damage-hero g h 3)
    (check "a standing hero cues hurt" '(hurt) (funcall cues)))
  (with-cues (cues)
    (damage-hero g h 100)
    (check "the killing blow cues the death cry alone" '(death)
           (funcall cues)))
  (with-cues (cues)
    (award-xp g h 200)                  ; past level 2's 100 xp
    (advance-level g h)
    (check "rising a level cues level" '(level) (funcall cues))))

;; Gold changing hands cues COIN — the sell side exercises the emit.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (attach-sounds (new-game m :party (list h)))))
  (give-item g h 't-torch)
  (with-cues (cues)
    (sell-item g h 't-torch)
    (check "a sale cues coin" '(coin) (funcall cues))))

;; The rest of the mapping table, driven by the events directly.
(let* ((m (parse-map *art* :name "test"))
       (h (%combat-hero))
       (g (attach-sounds (new-game m :party (list h)))))
  (with-cues (cues)
    (emit g :spell-cast h 'test-bolt)
    (emit g :song-sung h 'test-tune)
    (emit g :drink h)
    (emit g :combat-end :fled)          ; fleeing has no cue
    (emit g :combat-end :defeat)
    (check "cast, song, drink and defeat map; fleeing stays silent"
           '(cast song drink defeat) (funcall cues))))

;; The zone form carries the sound pack like the tile pack.
(let ((path "tests/tmp-sfx.map"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string *art* s)
    (terpri s)
    (write-line "(zone :kind :city :title \"Sfxville\" :sfx \"sfx/\")" s))
  (let ((m (load-map-file path)))
    (check "zone :sfx is map data" "sfx/" (dungeon-map-sfx m))
    (check "zone :gfx stays untouched" nil (dungeon-map-gfx m)))
  (delete-file path)
  (let ((cache (probe-file "tests/tmp-sfx.mapc")))
    (when cache (delete-file cache))))
(let ((path "tests/tmp-sfx.map"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string *art* s)
    (terpri s)
    (write-line "(zone :sfx sounds)" s))
  (check-error "zone :sfx must be a directory string"
    (load-map-file path))
  (delete-file path))

;; ZONE-SFX-DIR resolves beside the map when the pack lives there,
;; else game-relative; no :SFX zone means no pack.  LOAD-SOUND-PACK
;; reads the shipped subset of the vocabulary, in vocabulary order.
(ensure-directories-exist "tests/tmp-sfx/")
(write-8svx "tests/tmp-sfx/hit.8svx"
            (make-sound 8000 (make-array 64 :element-type '(unsigned-byte 8)
                                            :initial-element 100)))
(write-8svx "tests/tmp-sfx/door.8svx"
            (make-sound 4000 (make-array 32 :element-type '(unsigned-byte 8)
                                            :initial-element 156)))
(let* ((m (parse-map *art* :name "tests/tmp-here.map"))
       (g (new-game m :party (list (%combat-hero)))))
  (check "no :sfx zone, no pack dir" nil (zone-sfx-dir g))
  (setf (dungeon-map-sfx m) "tmp-sfx/")
  (check "the pack beside the map wins" "tests/tmp-sfx/"
         (zone-sfx-dir g))
  (setf (dungeon-map-sfx m) "elsewhere/sfx/")
  (check "no local probe file: the dir goes game-relative"
         "elsewhere/sfx/" (zone-sfx-dir g)))
(let ((pack (load-sound-pack "tests/tmp-sfx/")))
  (check "the pack ships its subset, vocabulary order" '(hit door)
         (mapcar #'car pack))
  (check "pack samples arrive decoded" 64
         (length (sound-bytes (cdr (assoc 'hit pack)))))
  (check "pack rates arrive per file" 4000
         (sound-rate (cdr (assoc 'door pack)))))
(check "an empty dir is an empty pack" '()
       (load-sound-pack "tests/no-such-dir/"))
(delete-file "tests/tmp-sfx/hit.8svx")
(delete-file "tests/tmp-sfx/door.8svx")

;; The vocabulary itself is engine API — a pack generator iterates it.
(check "the cue vocabulary" 15 (length *sound-names*))
(check-true "hit is the pack probe cue" (member 'hit *sound-names*))

;;; ---------------------------------------------------------------------
;;; Summary

(format t "~%Lambda's Tale engine tests: ~D checks, ~D failures.~%"
        *checks* *failures*)
(cl-user::quit (if (zerop *failures*) 0 1))
