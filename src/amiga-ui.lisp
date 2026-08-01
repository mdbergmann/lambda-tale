;;; Lambda's Tale — AmigaOS front-end (Intuition window or custom screen,
;;; wireframe view).  Layout per specs/ui-and-engine.md, the Bard's
;;; Tale two-column split, sized by the active display profile:
;;;
;;;   +--------------------+----------------------+
;;;   | first-person view  | message log          |
;;;   |                    | (microfont, newest   |
;;;   |                    |  at the bottom)      |
;;;   +--------------------+----------------------+
;;;   | location plaque    | effect icons [+rose] |
;;;   +--------------------+----------------------+
;;;   | party roster (header + 7 rows)            |
;;;   +-------------------------------------------+
;;;
;;; There is no status line: the party roster sits right under the
;;; view, the key reference lives on the help page ('h' or '?'), and
;;; the position/clock show in the map mode's footer.  The message log
;;; renders in the microfont's condensed bold small face
;;; (microfont.lisp) — 6px per character — so the narrow column holds
;;; more text.  Active effects show as icons
;;; only, laid out left to right in effect order on the 20px grey
;;; strip below the log page; an effect that grants a :COMPASS shows
;;; the live rose in its own slot (no fixed compass corner), and the
;;; facing also shows in the map footer while one burns
;;; (COMPASS-ACTIVE-P).
;;;
;;; The automap lives in a full-screen map mode under the 'm' key —
;;; black ink on the grey page, doors and the party amber, walls drawn
;;; as merged runs (MAP-EDGE-RUNS) so a city-sized map opens fast, and
;;; the legend of found locations (MAP-LEGEND-ENTRIES) beside the map.
;;;
;;; Locations (shop, tavern, ...) and the character sheet TAKE OVER
;;; the message area (%AMIGA-DRAW-TAKEOVER): their menu lines render
;;; at the top of the log page, the trailing log lines keep scrolling
;;; below a rule, and the view column shows the location's :image /
;;; the sheet hero's class portrait when the campaign provides one
;;; (%AMIGA-DRAW-PICTURE; the live first-person view otherwise).  A
;;; fight reads the same way: the round-orders page takes over the
;;; message area and the view column carries the enemy's portrait
;;; (DEFINE-MONSTER :IMAGE, COMBAT-IMAGE-PATH) — the party sees what it
;;; is fighting while it picks the round.  The cast/use/sing menus and
;;; the save picker keep their overlay page over the view column
;;; (%AMIGA-DRAW-PAGE) — the log stays readable beside them.  On the
;;; street, facing a
;;; location's door shows its facade in the view column
;;; (FACING-LOCATION-IMAGE-PATH) — houses have faces before the party
;;; ever steps in.
;;;
;;; Loaded only on AmigaOS (see src/load.lisp); the requires below must run
;;; before the rest of this file is read so AMIGA.INTUITION / AMIGA.GFX /
;;; AMIGA.GADTOOLS symbols resolve.
;;;
;;; Pens: 0 background, 1 wireframe/text, 2 the grey chrome and the
;;; map page, 3 doors and the party marker; on the custom screen pens
;;; 4 up to the profile's depth carry the tile pack's colors (see
;;; %APPLY-PACK-PALETTE).
;;;
;;; Save/Load/Quit live in an Intuition menu strip (right mouse button),
;;; built with gadtools.library — the Amiga-native place for them; both
;;; items (and the S/L keys) open the shared save-slot picker
;;; (src/save-menu.lisp), saves/NAME.sav.

(require "amiga/intuition")
(require "amiga/graphics")
(require "amiga/gadtools")
(require "amiga/exec")

(in-package :tale)

(defvar *autoplay* nil
  "Testing hook: a list of inputs fed to the game one per Intuition
tick (~10/s), driving a full unattended session — key characters,
:ESC, or (:CLICK X Y) entries resolved through the mouse hotspot map
exactly like a real left click.")

;;; Screen and window geometry, viewport size and tile pack all come
;;; from the active DISPLAY-PROFILE (src/profiles.lisp) — PLAY-AMIGA's
;;; :PROFILE argument selects one.  Window mode uses the profile's
;;; window size at 0,0 so both displays lay out identically;
;;; BEST-MODE-ID promotes the screen mode on Picasso96/CyberGraphX/
;;; MorphOS RTG systems.

(defconstant +game-idcmp+
  (logior amiga.intuition:+idcmp-closewindow+
          amiga.intuition:+idcmp-vanillakey+
          amiga.intuition:+idcmp-menupick+
          ;; left-button clicks: everything play-able by key is
          ;; clickable too (see the hotspot list below)
          amiga.intuition:+idcmp-mousebuttons+
          ;; the *autoplay* heartbeat and the pointer's hover tracking
          ;; (hand vs pointing finger — see %TRACK-POINTER-HOT)
          amiga.intuition:+idcmp-intuiticks+))

;;; ---------------------------------------------------------------------
;;; Mouse hotspots: the click-to-key map.  The game's whole input funnel
;;; is ACT (one key character per action), so mouse support synthesizes
;;; keys: every REDRAW rebuilds *HOTSPOTS* — the renderers register a
;;; rectangle per pickable thing (a menu option row, its footer's [s]
;;; hint, a roster row, a movement zone on the view) with the key it
;;; stands for — and a left click feeds the key under the pointer into
;;; ACT, which routes it exactly like the keyboard.  The list is
;;; rebuilt on every redraw, so it always matches what is on screen and
;;; which model currently eats the keys.

(defvar *hotspots* '()
  "Click targets of the current frame: entries (X0 Y0 X1 Y1 KEY)
or (X0 Y0 X1 Y1 KEY CURSOR), newest first — later registrations win,
so a mode's whole-page catch-all goes in first and the specific
targets after it.  CURSOR names the hover pointer the target wants
(the move zones' directional arrows); targets without one get the
pointing finger.")

(defun %hotspot (key x0 y0 x1 y1 &optional cursor)
  "Register the inclusive pixel rectangle as a click target for KEY (a
character or :ESC).  NIL keys register nothing.  CURSOR optionally
names the hover pointer shown over the target (:FORWARD, :BACK,
:TURN-LEFT, :TURN-RIGHT — see %APPLY-STANDARD-POINTER); without one
the pointing finger shows."
  (when key
    (push (list x0 y0 x1 y1 key cursor) *hotspots*)))

(defun %hotspot-at (x y)
  "The key registered under pixel (X,Y) and that target's hover
cursor, as (VALUES KEY CURSOR); NIL when nothing is registered there.
Most recently registered target first."
  (dolist (spot *hotspots*)
    (destructuring-bind (x0 y0 x1 y1 key &optional cursor) spot
      (when (and (<= x0 x x1) (<= y0 y y1))
        (return (values key cursor))))))

(defun %register-line-hotspots (line x y advance line-h row-x0 row-x1)
  "Click targets for one drawn menu line: an option line (a key from
MENU-LINE-KEY) gets the whole row ROW-X0..ROW-X1, while a row carrying
several options — a packed one (MENU-LINE-SPANS) or a plain line's
bracket hints ([S]ell, [Esc] back — see MENU-KEY-SPANS) — gives each
of them its own span.  (X,Y) is the drawn text's top-left, ADVANCE the
fixed character cell width, LINE-H the row height."
  (let ((key (menu-line-key line))
        (y1 (+ y line-h -1)))
    (if key
        (%hotspot key row-x0 y row-x1 y1)
        (dolist (span (or (menu-line-spans line)
                          (menu-key-spans (menu-line-text line))))
          (destructuring-bind (start end span-key) span
            ;; Esc goes through ACT as :ESC — the same shape a real
            ;; VANILLAKEY 27 arrives in, so every mode recognizes it
            (%hotspot (if (eql span-key #\Escape) :esc span-key)
                      (+ x (* start advance)) y
                      (+ x (* end advance) -1) y1))))))

(defun %register-move-zones (l)
  "Click-to-walk zones on the first-person view: the left and right
quarters turn (A/D), the middle walks forward (W) with a back-step
band (S) along its bottom third.  Each zone names its directional
hover cursor, so the pointer over the view is the arrow of the move a
click would make."
  (let* ((x0 (ui-layout-bx l))
         (y0 (ui-layout-by l))
         (w (ui-layout-fp-w l))
         (h (ui-layout-fp-h l))
         (x1 (+ x0 w -1))
         (y1 (+ y0 h -1))
         (q (floor w 4))
         (split (+ y0 (floor (* h 2) 3))))
    (%hotspot #\a x0 y0 (+ x0 q -1) y1 :turn-left)
    (%hotspot #\d (- x1 q -1) y0 x1 y1 :turn-right)
    (%hotspot #\w (+ x0 q) y0 (- x1 q) (1- split) :forward)
    (%hotspot #\s (+ x0 q) split (- x1 q) y1 :back)))

;;; ---------------------------------------------------------------------
;;; Layout: computed from the window's actual inner size, so the same
;;; code serves the Workbench window, a 640x256 custom screen and
;;; whatever an RTG driver promotes the mode to.

;;; The roster is the one table that sets its rows solid — ROW-H is the
;;; font's glyph box, with none of the one-pixel leading LH adds — and
;;; sits close under the plaque rather than a text line under it.  A
;;; roster is read column-down (whose points are low?), not line by
;;; line like the log, so the rows want to group; and the 16 pixels the
;;; tight rows save over LH are most of what buys the layout its NTSC
;;; fit (see *LORES-PROFILE*, and +ROSTER-GAP+ in profiles.lisp for the
;;; breathing room the saving paid for above the header).

(defstruct (ui-layout (:constructor %make-ui-layout))
  bx by            ; content top-left (inside the chrome ring)
  right bottom     ; content right/bottom edges
  lh base          ; text line height / baseline (rastport font metrics)
  row-h            ; roster row height — the glyph box, no leading
  cw               ; character cell width (rastport font metrics)
  fp-w fp-h        ; first-person view size
  log-x log-w      ; message log column
  col-h            ; height of the log/page column
  page-b           ; log page bottom edge (its outline row)
  band-y band-h    ; effect icons + compass strip below the log page
  plaque-y plaque-b ; location plaque top / bottom edges (under the view)
  hdr-y            ; party roster header row top
  party-y          ; party roster rows top
  ring-p)          ; draw the ornate border ring (full-screen backdrop)

(defun %amiga-layout (win rp)
  ;; The Bard's Tale chrome (border ring, framed view, plaque, roster
  ;; header) needs the profile's full backdrop; a bordered Workbench
  ;; window skips the ring so the framed view still fits.
  (let* ((p *display-profile*)
         (ring-p (zerop (amiga.intuition:window-border-top win)))
         (pad-x (if ring-p (display-profile-pad-x p) 4))
         (pad-y (if ring-p (display-profile-pad-y p) 4))
         (bx (+ (amiga.intuition:window-border-left win) pad-x))
         (by (+ (amiga.intuition:window-border-top win) pad-y))
         (right (- (amiga.intuition:window-width win)
                   (amiga.intuition:window-border-right win) pad-x))
         (bottom (- (amiga.intuition:window-height win)
                    (amiga.intuition:window-border-bottom win) pad-y))
         (lh (+ (amiga.gfx:rastport-tx-height rp) 2))
         (row-h (max 1 (amiga.gfx:rastport-tx-height rp)))
         (base (amiga.gfx:rastport-tx-baseline rp))
         (cw (max 1 (amiga.gfx:text-length rp "M")))
         ;; top-down: view, plaque, then the roster right below —
         ;; leftover space becomes bottom margin (no status line).
         ;; BOTTOM is the last usable row, not one past it, so the
         ;; column's budget counts it: (1+ BOTTOM) minus BY is how many
         ;; rows there are to spend.  The off-by-one is worth a pixel,
         ;; and on the 200-line screen the whole column is one pixel
         ;; from exact (see *LORES-PROFILE*).
         (fp-h (min *fp-view-height*
                    (- (1+ bottom) by
                       (+ lh 3)                    ; the plaque
                       (+ +roster-gap+ row-h)      ; roster gap + header
                       (* row-h +party-limit+))))  ; roster rows
         (plaque-y (+ by fp-h 1))
         (plaque-b (+ plaque-y lh 2))   ; 2px taller than a text line so
                                        ; the glyphs clear both borders
         (col-h (- (1+ plaque-b) by))
         (log-x (+ bx *fp-view-width* (display-profile-view-gap p)))
         ;; the effect strip sits at the foot of the log column, its
         ;; bottom flush with the plaque's; the white log page ends a
         ;; small gap above it
         (band-h (min (display-profile-band-height p) (- col-h 8)))
         (band-y (- (+ by col-h -1) band-h))
         (page-b (- band-y 4))
         (hdr-y (+ plaque-b +roster-gap+))
         (party-y (+ hdr-y row-h)))
    (%make-ui-layout :bx bx :by by :right right :bottom bottom
                     :lh lh :row-h row-h :base base :cw cw
                     :fp-w *fp-view-width* :fp-h fp-h
                     :log-x log-x :log-w (- right log-x)
                     :col-h col-h
                     :page-b page-b
                     :band-y band-y :band-h band-h
                     :plaque-y plaque-y :plaque-b plaque-b
                     :hdr-y hdr-y :party-y party-y
                     :ring-p ring-p)))

;;; ---------------------------------------------------------------------
;;; Chrome: the Bard's Tale presentation — grey screen, riveted border
;;; ring, the dungeon view as a framed picture with a drop shadow and a
;;; location plaque, the message log as a white page.  All procedural
;;; (pens 0/1/2), so it needs no art assets.

(defun %chrome-rect (rp x0 y0 x1 y1)
  "Rectangle outline with the current pen."
  (amiga.gfx:draw-line rp x0 y0 x1 y0)
  (amiga.gfx:draw-line rp x0 y1 x1 y1)
  (amiga.gfx:draw-line rp x0 y0 x0 y1)
  (amiga.gfx:draw-line rp x1 y0 x1 y1))

(defun %chrome-bg (rp win l)
  "Grey background over the whole inner window, plus the riveted
double-outline border ring on the full-screen backdrop."
  (let* ((el (amiga.intuition:window-border-left win))
         (et (amiga.intuition:window-border-top win))
         (er (- (amiga.intuition:window-width win)
                (amiga.intuition:window-border-right win) 1))
         (eb (- (amiga.intuition:window-height win)
                (amiga.intuition:window-border-bottom win) 1)))
    (amiga.gfx:set-a-pen rp 2)
    (amiga.gfx:rect-fill rp el et er eb)
    (when (ui-layout-ring-p l)
      (amiga.gfx:set-a-pen rp 0)
      (%chrome-rect rp (+ el 1) (+ et 1) (- er 1) (- eb 1))
      (%chrome-rect rp (+ el 6) (+ et 6) (- er 6) (- eb 6))
      ;; rivets along the band between the outlines
      (loop for x from (+ el 8) to (- er 10) by 16
            do (amiga.gfx:rect-fill rp x (+ et 3) (+ x 2) (+ et 4))
               (amiga.gfx:rect-fill rp x (- eb 4) (+ x 2) (- eb 3)))
      (loop for y from (+ et 8) to (- eb 10) by 16
            do (amiga.gfx:rect-fill rp (+ el 3) y (+ el 4) (+ y 2))
               (amiga.gfx:rect-fill rp (- er 4) y (- er 3) (+ y 2))))
    (amiga.gfx:set-a-pen rp 1)))

(defun %plaque-name (rp name w)
  "NAME (already display-cased) clipped to the plaque's W-pixel
interior via FIT-TITLE (src/view.lisp), measured with the rastport's
real font."
  (fit-title name (lambda (s) (amiga.gfx:text-length rp s)) (- w 2)))

(defun %chrome-frames (rp game l)
  "The picture frame + drop shadow around the view, the location
plaque under it, and the white message page shell (the page ends a
small gap above the effect strip — see %AMIGA-DRAW-BAND)."
  (let* ((bx (ui-layout-bx l))
         (by (ui-layout-by l))
         (w (ui-layout-fp-w l))
         (h (ui-layout-fp-h l))
         (py (ui-layout-plaque-y l))
         (pb (ui-layout-plaque-b l))    ; plaque bottom
         (pg (ui-layout-page-b l))      ; log page bottom
         (lx (ui-layout-log-x l))
         (r (ui-layout-right l)))
    ;; View + plaque drop shadow (down-right, BT2 style), +VIEW-SHADOW+
    ;; deep — the same weight as the message page's beside it, and
    ;; shallower than +ROSTER-GAP+ so a row of clear grey is left
    ;; between the shadow and the roster's CHARACTER header.
    (amiga.gfx:set-a-pen rp 0)
    (amiga.gfx:rect-fill rp (+ bx w 1) (+ by 1)
                         (+ bx w +view-shadow+) (+ pb +view-shadow+))
    (amiga.gfx:rect-fill rp (+ bx 1) (1+ pb)
                         (+ bx w +view-shadow+) (+ pb +view-shadow+))
    ;; white picture frame around the view
    (amiga.gfx:set-a-pen rp 1)
    (%chrome-rect rp (1- bx) (1- by) (+ bx w) (+ by h))
    ;; location plaque: black block, white border, centered map name;
    ;; the block is two pixels taller than a text line so the glyphs
    ;; sit clear of both border rows
    (amiga.gfx:set-a-pen rp 0)
    (amiga.gfx:rect-fill rp (1- bx) py (+ bx w) pb)
    (amiga.gfx:set-a-pen rp 1)
    (%chrome-rect rp (1- bx) py (+ bx w) pb)
    (let ((name (%plaque-name rp (title-case (map-title (game-map game))) w)))
      (let ((tw (amiga.gfx:text-length rp name)))
        (amiga.gfx:move-to rp (+ bx (max 0 (floor (- w tw) 2)))
                           (+ py 2 (ui-layout-base l)))
        (amiga.gfx:gfx-text rp name)))
    ;; message page: white sheet with a black outline and shadow
    (amiga.gfx:set-a-pen rp 0)
    (amiga.gfx:rect-fill rp (+ (- lx 4) 2) (+ by 1) (+ r 2) (+ pg 2))
    (amiga.gfx:set-a-pen rp 1)
    (amiga.gfx:rect-fill rp (- lx 4) (1- by) r pg)
    (amiga.gfx:set-a-pen rp 0)
    (%chrome-rect rp (- lx 4) (1- by) r pg)
    (amiga.gfx:set-a-pen rp 1)))

;;; ---------------------------------------------------------------------
;;; Drawing

(defun %amiga-door-rect (rp cx cy hw hh)
  (amiga.gfx:set-a-pen rp 3)
  (amiga.gfx:draw-line rp (- cx hw) (- cy hh) (+ cx hw) (- cy hh))
  (amiga.gfx:draw-line rp (- cx hw) (+ cy hh) (+ cx hw) (+ cy hh))
  (amiga.gfx:draw-line rp (- cx hw) (- cy hh) (- cx hw) (+ cy hh))
  (amiga.gfx:draw-line rp (+ cx hw) (- cy hh) (+ cx hw) (+ cy hh))
  (amiga.gfx:set-a-pen rp 1))

(defun %amiga-draw-fp (rp game ox oy w h &optional walls back)
  "Draw the first-person view into the rastport at (OX,OY): blitted
wall graphics when WALLS (the loaded piece bitmaps) is available and
the viewport has its full asset size, the wireframe otherwise.  With
BACK (an offscreen bitmap at least W x H in the display's format, see
%ALLOC-FP-BACKBUFFER) the frame is composed there and reaches the
screen as ONE blit; without it every backdrop fill and wall piece
paints the visible window in turn, which the eye catches as flicker
on any machine fast enough (specs/step-animation.md discusses the one
place that painting-as-motion was ever welcome)."
  (if back
      (amiga.gfx:with-bitmap-rastport (brp back)
        (%amiga-compose-fp brp game 0 0 w h walls)
        (amiga.gfx:blt-bitmap-rastport back 0 0 rp ox oy w h))
      (%amiga-compose-fp rp game ox oy w h walls)))

(defun %alloc-fp-backbuffer (rp l)
  "The offscreen bitmap %AMIGA-DRAW-FP composes the view frame in:
sized to layout L's viewport and allocated with the window's bitmap as
friend, so it lives in the display's native format and the final blit
is a plain copy.  NIL when the allocation fails — the view then paints
the window directly, as it always did."
  (handler-case
      (let ((friend (amiga.gfx:rastport-bitmap rp)))
        (amiga.gfx:alloc-bitmap (ui-layout-fp-w l) (ui-layout-fp-h l)
                                (amiga.gfx:get-bitmap-attr
                                 friend amiga.gfx:+bma-depth+)
                                :friend friend))
    (error () nil)))

(defun %amiga-compose-fp (rp game ox oy w h walls)
  "The view composition %AMIGA-DRAW-FP wraps: the blitted view starts
from the ceiling/floor backdrop (black where the pack has none); the
walls carve the perspective on top of it."
  (let ((slices (compute-view (game-map game) (game-x game) (game-y game)
                              (game-facing game) (render-view-depth game)))
        (planes (view-planes w h)))
    (if (and walls (= w *fp-view-width*) (= h *fp-view-height*))
        (progn
          ;; ceiling above the horizon, floor below, then the walls
          ;; cookie-cut on top so the corners they don't cover let the
          ;; backdrop show through.  Outdoors (a non-:DARK zone) the sky
          ;; and ground are a flat fill in pens +ART-PEN-SKY+/-GROUND+,
          ;; whose colour %APPLY-DAYTIME-PALETTE has set for the hour —
          ;; that is the whole day/night effect.  Indoor/dark zones keep
          ;; their pack's opaque ceiling.iff/floor.iff (black where none):
          ;; there is no sky underground and no hour to track.
          (let ((outdoor (not (dungeon-map-dark (game-map game)))))
            (loop for key in '((:ceiling) (:floor))
                  for pen in (list +art-pen-sky+ +art-pen-ground+)
                  for (x y pw ph) in (backdrop-rects planes)
                  do (let ((entry (and (not outdoor) (gethash key walls))))
                       (if entry
                           (amiga.gfx:blt-bitmap-rastport (car (svref entry 0))
                                                          0 0 rp
                                                          (+ ox x) (+ oy y)
                                                          pw ph)
                           (progn
                             (amiga.gfx:set-a-pen rp (if outdoor pen 0))
                             (amiga.gfx:rect-fill rp (+ ox x) (+ oy y)
                                                  (+ ox x pw -1)
                                                  (+ oy y ph -1)))))))
          ;; The wall of night: when darkness truncated the view at an
          ;; open front, the backdrop must not show a lit corridor
          ;; receding beyond it — black out the plane past the last
          ;; visible cell before the walls go on top.
          (let ((s (car (last slices))))
            (when (and s
                       (eq (view-slice-front s) :open)
                       (game-dark-p game))
              (destructuring-bind (qx0 qy0 qx1 qy1)
                  (aref planes (1+ (view-slice-depth s)))
                (amiga.gfx:set-a-pen rp 0)
                (amiga.gfx:rect-fill rp (+ ox qx0) (+ oy qy0)
                                     (+ ox qx1) (+ oy qy1)))))
          (amiga.gfx:set-a-pen rp 1)
          (dolist (rec (view-blit-list slices planes))
            (destructuring-bind (piece style x y pw ph sx) rec
              (let ((variants (gethash piece walls)))
                (when variants
                  ;; the wall's building style picks the piece variant;
                  ;; a base-only pack always lands on index 0.  SX is
                  ;; the piece's own x offset: a flank the nearer walls
                  ;; partly hide blits only its visible right-hand part
                  ;; (BltMaskBitMapRastPort offsets the mask with the
                  ;; source, so the cookie cut follows).
                  (destructuring-bind (bm . mask)
                      (svref variants (mod style (length variants)))
                    (if mask
                        (amiga.gfx:blt-mask-bitmap-rastport
                         bm sx 0 rp (+ ox x) (+ oy y) pw ph mask)
                        (amiga.gfx:blt-bitmap-rastport
                         bm sx 0 rp (+ ox x) (+ oy y) pw ph))))))))
        (progn
          (amiga.gfx:set-a-pen rp 0)
          (amiga.gfx:rect-fill rp ox oy (+ ox w -1) (+ oy h -1))
          (amiga.gfx:set-a-pen rp 1)
          (dolist (prim (view-display-list slices planes))
            (ecase (first prim)
              (:line (destructuring-bind (x0 y0 x1 y1) (rest prim)
                       (amiga.gfx:draw-line rp (+ ox x0) (+ oy y0)
                                            (+ ox x1) (+ oy y1))))
              (:door (destructuring-bind (cx cy hw hh) (rest prim)
                       (%amiga-door-rect rp (+ ox cx) (+ oy cy)
                                         hw hh)))))))))

;;; ---------------------------------------------------------------------
;;; Wall-piece assets (M3): the data/gfx ILBMs loaded into offscreen
;;; bitmaps once per session.  RTG-safe: the window's own bitmap is the
;;; AllocBitMap friend and sets the depth, so the pieces live in the
;;; display's native format and every blit copies all its planes;
;;; pixels go in as chunky bytes (WRITE-CHUNKY), never as planes.

;;; The active tile pack directory *GFX-DIR* lives in view.lisp (it is
;;; platform-independent: the manifest and the asset generator use it
;;; on the host too); PLAY-AMIGA's :GFX-DIR argument rebinds it.

;;; ---------------------------------------------------------------------
;;; The planar fast path.
;;;
;;; ILBM plane rows and BitMap plane rows share a layout (MSB-first,
;;; word-padded), so a piece never becomes chunky: the rows go through
;;; AMIGA.GFX:WRITE-PLANES into a SCRATCH bitmap allocated with no
;;; friend and without BMF_DISPLAYABLE — a standard planar BitMap in
;;; ordinary RAM, the one kind whose planes may legally be written —
;;; and the blitter then converts it into the friend bitmap's real
;;; format, so an RTG display never sees a plane poke.  The piece
;;; bitmaps the renderer blits every frame stay RTG-safe by
;;; construction (AllocBitMap with a friend, OS blits only).

(defvar *wall-load-planar* t
  "Load wall pieces through the planar fast path (%POKE-PLANES) rather
than decoding to chunky pens and calling WriteChunkyPixels.  Bound to
NIL to fall back — the chunky path is the portable one, and is what
the host renderer and the asset tools use regardless.")

(defun %poke-planes (bitmap img)
  "Copy PLANAR-IMAGE IMG's bitplane rows into the scratch BITMAP's
planes (AMIGA.GFX:WRITE-PLANES — one contiguous copy per plane when
the strides agree, row-by-row at the destination stride otherwise;
never a VM dispatch per byte, which was the bulk of a pack load on a
68020).  BITMAP must be at least as deep as IMG (its extra planes are
left as allocated, i.e. cleared) and its rows at least as wide."
  (amiga.gfx:write-planes bitmap
                          (loop for p below (planar-image-depth img)
                                collect (planar-image-plane img p))
                          (planar-image-row-bytes img)
                          (planar-image-height img)))

(defun %planar-piece-mask (img)
  "IMG's cookie-cut mask in chip RAM, or NIL for a fully painted piece
(a plain opaque blit).  One mask fold: the same bytes answer the
does-it-need-a-mask question (%MASK-OPAQUE-P) and become the chip
plane — PLANAR-IMAGE-TRANSPARENT-P followed by PLANAR-MASK-BYTES
would fold every plane twice."
  (multiple-value-bind (bytes row-bytes) (planar-mask-bytes img)
    (when (and bytes
               (not (%mask-opaque-p bytes row-bytes
                                    (planar-image-width img)
                                    (planar-image-height img))))
      (amiga.exec:alloc-chip-bytes bytes))))

(defun %load-wall-assets (rp log)
  "Load the active tile pack (*GFX-DIR*): every wall piece the active
draw distance can show (*DRAW-DEPTH*) into an offscreen bitmap — the
pieces at deeper levels must still be PRESENT (the pack contract is
the full set whatever this machine draws), they are just not decoded —
plus the optional floor.iff / ceiling.iff backdrops
under the keys (:FLOOR) / (:CEILING).  Returns (VALUES WALLS PALETTE):
a hash of piece key -> vector of (BITMAP . MASK) entries — index 0 the
base piece file, further entries the optional -v1.iff/-v2.iff/...
style variants, probed in order until one is missing (see
WALL-PIECE-VARIANT-FILE; the renderer picks per wall with
\(MOD STYLE COUNT)) — and the pack's CMAP palette (palette.iff's when
present, else the first wall piece's), or (VALUES NIL NIL) — falling
back to the wireframe view — when the pack is missing, unreadable, or
mis-sized.  MASK is a chip-RAM cookie-cut plane for a piece that uses
pen 0 (transparent), else NIL (a plain opaque blit); the backdrops are
always opaque."
  (let* ((walls (make-hash-table :test #'equal))
         (palette nil)
         (friend (amiga.gfx:rastport-bitmap rp))
         (depth (max 2 (amiga.gfx:get-bitmap-attr friend
                                                  amiga.gfx:+bma-depth+)))
         (drawn (%draw-depth))  ; distance levels this machine blits
         (planes (view-planes *fp-view-width* *fp-view-height*)))
    (labels ((add-entry (key bm mask)
               ;; into the hash the moment the bitmap exists, so an
               ;; error later in the load still frees it via
               ;; %FREE-WALL-ASSETS
               (let ((entry (cons bm mask))
                     (old (gethash key walls)))
                 (setf (gethash key walls)
                       (if old
                           (concatenate 'vector old (vector entry))
                           (vector entry)))
                 entry))
             (build-mask (img)
               ;; A piece that uses pen 0 (the transparent key) gets a
               ;; cookie-cut mask in chip RAM so the backdrop shows
               ;; through; a fully-painted piece needs none.
               (when (image-transparent-p img)
                 (amiga.exec:alloc-chip-bytes
                  (mask-bytes (image-width img)
                              (image-height img)
                              (image-pixels img)))))
             (check-size (file iw ih w h)
               ;; Blits copy W x H from the bitmap wherever the piece's
               ;; slot sits, so a mis-sized pack file would read past
               ;; the bitmap's edges — reject it here, loudly.
               (unless (and (= iw w) (= ih h))
                 (error "~A is ~Dx~D, its slot needs ~Dx~D (see ~
PRINT-TILE-MANIFEST)"
                        file iw ih w h)))
             (load-piece-chunky (key file w h maskable)
               (let ((img (read-ilbm file)))
                 (check-size file (image-width img) (image-height img) w h)
                 (let ((bm (amiga.gfx:alloc-bitmap w h depth
                                                   :friend friend)))
                   (add-entry key bm (when maskable (build-mask img)))
                   (unless palette
                     (setf palette (image-palette img)))
                   (amiga.gfx:with-bitmap-rastport (brp bm)
                     (amiga.gfx:write-chunky brp 0 0 w h
                                             (image-pixels img))))))
             (load-piece-planar (key file w h maskable)
               ;; The fast path: an ILBM plane row and a BitMap plane
               ;; row have the same layout, so the piece never becomes
               ;; chunky — the rows go straight into a scratch planar
               ;; BitMap and the blitter moves them into the piece's
               ;; own (friend, display-format) bitmap.  That skips both
               ;; the per-pixel fold and WriteChunkyPixels.
               (let ((img (read-ilbm-planar file)))
                 (check-size file (planar-image-width img)
                             (planar-image-height img) w h)
                 (let ((bm (amiga.gfx:alloc-bitmap w h depth
                                                   :friend friend)))
                   (add-entry key bm (when maskable
                                       (%planar-piece-mask img)))
                   (unless palette
                     (setf palette (planar-image-palette img)))
                   ;; scratch is plain (no friend, not displayable), so
                   ;; it is a standard planar BitMap we may poke; it is
                   ;; allocated at the piece's depth so the blit copies
                   ;; every plane, the unused high ones cleared
                   (let ((scratch (amiga.gfx:alloc-bitmap w h depth)))
                     (unwind-protect
                          (progn
                            (%poke-planes scratch img)
                            (amiga.gfx:with-bitmap-rastport (brp bm)
                              (amiga.gfx:blt-bitmap-rastport
                               scratch 0 0 brp 0 0 w h)))
                       (amiga.gfx:free-bitmap scratch))))))
             (load-piece (key file w h &optional maskable)
               (if *wall-load-planar*
                   (load-piece-planar key file w h maskable)
                   (load-piece-chunky key file w h maskable))))
      (dlog-timed ("wall pack ~A" *gfx-dir*)
       (handler-case
          (progn
            ;; Every piece of the FULL set is checked for existence —
            ;; the pack contract does not shrink with the machine's
            ;; draw distance, or a pack missing a deep piece would pass
            ;; here and fail only on someone else's faster machine.
            ;; Only the depths this machine will actually blit are then
            ;; decoded into bitmaps, which is where the load time and
            ;; the chip RAM go.
            (dolist (piece (wall-piece-names))
              (let ((file (concatenate 'string *gfx-dir*
                                       (wall-piece-file piece))))
                (unless (probe-file file)
                  (error "missing wall asset ~A" file))
                (when (< (second piece) drawn)
                  (destructuring-bind (x y w h) (wall-piece-rect planes piece)
                    (declare (ignore x y))
                    (load-piece piece file w h t)
                    ;; optional per-house style variants beside the base
                    ;; piece, probed in order until one is missing — the
                    ;; pack decides how many looks (and at which depths)
                    ;; it pays the load time for
                    (loop for v from 1
                          for vfile = (concatenate
                                       'string *gfx-dir*
                                       (wall-piece-variant-file piece v))
                          while (probe-file vfile)
                          do (load-piece piece vfile w h t))))))
            ;; The backdrops are optional and always opaque: a pack
            ;; without them keeps the black ceiling/floor.
            (destructuring-bind (ceiling floor) (backdrop-rects planes)
              (dolist (entry (list (list '(:ceiling) "ceiling.iff" ceiling)
                                   (list '(:floor) "floor.iff" floor)))
                (destructuring-bind (key name (x y w h)) entry
                  (declare (ignore x y))
                  (let ((file (concatenate 'string *gfx-dir* name)))
                    (when (probe-file file)
                      (load-piece key file w h))))))
            ;; palette.iff (any size) overrides the pack palette.
            (let ((file (concatenate 'string *gfx-dir* "palette.iff")))
              (when (probe-file file)
                (setf palette (image-palette (read-ilbm file)))))
            (values walls palette))
        (error (e)
          (%free-wall-assets walls)
          (dlog "wall pack ~A failed: ~A" *gfx-dir* e)
          (when log
            (log-message log (format nil "No wall graphics (~A); ~
wireframe view." e)))
          (values nil nil)))))))

;;; The layout reads its text metrics (line height, character cell)
;;; from the rastport font, but the profiles' region budgets are tuned
;;; for topaz 8 (10px lines, 8px cells).  RTG Workbenches often default
;;; to a bigger system font, which would shrink the viewport below the
;;; wall assets' size — so the game selects topaz 8 (a ROM font) on its
;;; rastport explicitly.

(defun %with-game-font (fn)
  "Call FN with the topaz 8 TextFont (NIL when unavailable — the
layout then uses whatever the rastport carries); closes the font after
FN returns."
  (let ((font (amiga.gfx:open-font "topaz.font" 8)))
    (unwind-protect
        (funcall fn font)
      (when font (amiga.gfx:close-font font)))))

(defun %game-rastport (win font)
  "The window's rastport with the game font selected and JAM1 drawing —
text paints glyphs only, so it sits on the grey chrome and the white
page without a background-color box around every character."
  (let ((rp (amiga.intuition:window-rastport win)))
    (when font (amiga.gfx:set-font rp font))
    (amiga.gfx:set-drmd rp amiga.gfx:+jam1+)
    rp))

(defun %free-wall-assets (walls)
  "Free every variant's piece bitmap and cookie-cut mask; safe with
NIL."
  (when walls
    (maphash (lambda (piece entries)
               (declare (ignore piece))
               (loop for entry across entries
                     do (amiga.gfx:free-bitmap (car entry))
                        (when (cdr entry)
                          (amiga:free-chip (cdr entry)))))
             walls))
  nil)

;;; The tile-pack cache policy lives in view.lisp (it is pure list
;;; work, and host-testable); exec's free-memory probe is the one part
;;; that has to be here.

(defun %free-memory ()
  "Free system RAM in bytes — AvailMem(MEMF_ANY) — or NIL when the
probe fails (then the caller must not gate on memory)."
  (handler-case (amiga.exec:avail-mem)
    (error () nil)))

(defun %pack-cache-free-all (cache)
  "Free every pack in CACHE; returns NIL (the empty cache)."
  (%pack-cache-drop cache #'%free-wall-assets))

;;; ---------------------------------------------------------------------
;;; The mouse pointer.  An own screen starts with unset sprite colors —
;;; the pointer would be invisible black on black — so the game always
;;; shows an explicit SetPointer sprite of its own.  The hover states:
;;; an open hand (the built-in *HAND-POINTER-ART*) while nothing under
;;; the pointer reacts, a pointing finger (*POINT-POINTER-ART*)
;;; whenever the mouse rests on a click target (*HOTSPOTS*), and over
;;; the first-person view's click-to-walk zones the arrow of the move
;;; a click would make — turn left/right on the side quarters, walk
;;; forward on the middle, back-step on its bottom band (the zones
;;; name their cursor at registration, see %REGISTER-MOVE-ZONES) —
;;; tracked off the IntuiTicks heartbeat (~10/s), which also re-checks
;;; a resting mouse after a redraw moved the targets; no
;;; IDCMP_MOUSEMOVE flood on a 14MHz 68020.  All are overridable per
;;; campaign by a pointer.iff / pointer-click.iff /
;;; pointer-forward.iff / pointer-back.iff / pointer-turn-left.iff /
;;; pointer-turn-right.iff in the zone's tile pack (16 px wide at
;;; most, pens 1-3; the hand's CMAP entries 1-3 become screen colors
;;; 17-19 — one sprite palette serves them all — the hot spot is the
;;; topmost-leftmost inked pixel).  The sprite is
;;; re-shown after every palette change: Picasso96/RTG latches the
;;; pointer image and its colors at SetPointer time, so setting colors
;;; 17-19 alone can leave an already-shown pointer black.  During the
;;; loads that take real seconds at 14MHz (tile packs on zone travel,
;;; a game load, first-sight ILBM images) the pointer switches to a
;;; busy hourglass (*BUSY-POINTER-ART* in ilbm.lisp): the frame in the
;;; outline pen, the sand — top-bulb remainder, falling stream, heap
;;; below — in the skin pen.

(defvar *game-pointer* nil
  "The neutral in-game pointer (the hand) as (CHIP HEIGHT XOFF YOFF) —
chip-RAM sprite data shown on the game window — or NIL outside a
session (the system default pointer then applies).")

(defvar *point-pointer* nil
  "The click-target pointer (the pointing finger) as (CHIP HEIGHT XOFF
YOFF), or NIL outside a session.")

(defvar *move-pointers* '()
  "The directional pointers for the view's click-to-walk zones: a
plist CURSOR -> (CHIP HEIGHT XOFF YOFF) for :FORWARD, :BACK,
:TURN-LEFT and :TURN-RIGHT; empty outside a session.")

(defvar *pointer-hot* nil
  "The hover state: NIL while nothing under the mouse reacts (the
hand shows), :POINT on a plain click target (the pointing finger), or
a move zone's cursor tag — :FORWARD, :BACK, :TURN-LEFT, :TURN-RIGHT —
on the view's walk zones (the matching arrow).")

(defvar *pointer-window* nil
  "The game window while a session runs.  Draw code deep below the
event loop (the image cache) brackets its slow first-sight loads with
the busy pointer through this.")

(defun %pointer-chip (image)
  "IMAGE as chip-RAM sprite data plus SET-POINTER geometry: returns
\(CHIP HEIGHT XOFF YOFF); the caller frees CHIP (AMIGA:FREE-CHIP)."
  (multiple-value-bind (chip height)
      (amiga.intuition:make-pointer-sprite (pointer-sprite-rows image))
    (multiple-value-bind (hx hy) (pointer-hotspot image)
      (list chip height (- hx) (- hy)))))

(defun %apply-standard-pointer (win)
  "Show the pointer matching the current hover state on WIN — a
directional arrow over one of the view's move zones, the pointing
finger over any other click target, the hand elsewhere — or the
system default when none is loaded."
  (let ((ptr (or (when *pointer-hot*
                   (or (getf *move-pointers* *pointer-hot*)
                       *point-pointer*))
                 *game-pointer*)))
    (if ptr
        (destructuring-bind (chip height xoff yoff) ptr
          (amiga.intuition:set-pointer win chip height 16 xoff yoff))
        (amiga.intuition:clear-pointer win))))

(defun %track-pointer-hot (win x y)
  "Retarget the pointer as the mouse at window pixel (X,Y) crosses
click-target boundaries: a move zone's directional arrow, the
pointing finger on any other target, the hand off all of them.
Called off IntuiTicks; only a state change touches SetPointer.  While
the busy pointer is up only the state updates — the busy bracket's
unwind re-applies the right sprite."
  (let ((hot (multiple-value-bind (key cursor) (%hotspot-at x y)
               (cond (cursor) (key :point)))))
    (unless (eq hot *pointer-hot*)
      (setf *pointer-hot* hot)
      (unless *busy-pointer-active*
        (%apply-standard-pointer win)))))

(defun %pointer-image (name fallback)
  "The pointer art from the tile pack's file NAME (pointer.iff /
pointer-click.iff) when the pack ships one (campaign-configurable),
else the built-in art from FALLBACK.  A file that will not load or
breaks the sprite constraints logs to the trace and falls back."
  (let ((file (and *gfx-dir* (concatenate 'string *gfx-dir* name))))
    (or (when (and file (probe-file file))
          (handler-case
              (let ((img (read-ilbm file)))
                (pointer-sprite-rows img) ; validate geometry and pens
                img)
            (error (e)
              (dlog "~A ~A rejected: ~A" name file e)
              nil)))
        (funcall fallback))))

(defun %move-pointer-list ()
  "The loaded directional pointers as a plain list, palette keys
dropped."
  (loop for entry on *move-pointers* by #'cddr
        collect (second entry)))

(defun %ensure-standard-pointer (scr win display)
  "(Re)build the standard pointers for the current tile pack — the
hand, the pointing finger and the four move-zone arrows — and show
the one the hover state calls for: art from %POINTER-IMAGE, the
hand's palette entries 1-3 into the sprite colors (screen colors
17-19; the hardware sprite has one palette, all pointers share it) —
on our own screen only; a Workbench window keeps the Workbench
pointer colors."
  (let ((img (%pointer-image "pointer.iff" #'hand-pointer-image))
        (point (%pointer-image "pointer-click.iff" #'point-pointer-image))
        (moves (list :forward (%pointer-image "pointer-forward.iff"
                                              #'forward-pointer-image)
                     :back (%pointer-image "pointer-back.iff"
                                           #'back-pointer-image)
                     :turn-left (%pointer-image "pointer-turn-left.iff"
                                                #'turn-left-pointer-image)
                     :turn-right (%pointer-image "pointer-turn-right.iff"
                                                 #'turn-right-pointer-image))))
    (when (and scr (eq display :screen))
      (let ((vp (amiga.intuition:screen-viewport scr)))
        (loop for i from 1 to 3
              for rgb = (and (< i (length (image-palette img)))
                             (aref (image-palette img) i))
              when rgb
                do (amiga.gfx:set-rgb4 vp (+ 16 i)
                                       (floor (first rgb) 17)
                                       (floor (second rgb) 17)
                                       (floor (third rgb) 17)))))
    (let ((old (list* *game-pointer* *point-pointer*
                      (%move-pointer-list))))
      (setf *game-pointer* (%pointer-chip img)
            *point-pointer* (%pointer-chip point)
            *move-pointers* (loop for entry on moves by #'cddr
                                  nconc (list (first entry)
                                              (%pointer-chip
                                               (second entry)))))
      (%apply-standard-pointer win)
      ;; the old sprite data is off the hardware only after the new
      ;; SetPointer above, so it frees last
      (dolist (ptr old)
        (when ptr (amiga:free-chip (first ptr)))))))

(defun %free-standard-pointer (win)
  "Drop the standard pointers at session end: back to the system
default, sprite data freed."
  (let ((all (list* *game-pointer* *point-pointer*
                    (%move-pointer-list))))
    (when (remove nil all)
      (amiga.intuition:clear-pointer win))
    (dolist (ptr all)
      (when ptr (amiga:free-chip (first ptr)))))
  (setf *game-pointer* nil
        *point-pointer* nil
        *move-pointers* '()
        *pointer-hot* nil))

(defun %make-busy-pointer-chip ()
  "The busy hourglass (*BUSY-POINTER-ART*) as chip-RAM sprite data
ready for SET-POINTER; returns (VALUES CHIP HEIGHT).  The caller frees
CHIP (AMIGA:FREE-CHIP) after the pointer moves off it."
  (destructuring-bind (chip height xoff yoff)
      (%pointer-chip (busy-pointer-image))
    (declare (ignore xoff yoff))        ; busy centers, no art hot spot
    (values chip height)))

(defvar *busy-pointer-active* nil
  "Non-NIL while the busy pointer is up — nested loads (a game load
that then swaps tile packs) keep the outer pointer instead of flashing
it off halfway.")

(defun %call-with-busy-pointer (win fn)
  "Show the busy hourglass pointer on WIN while FN runs; puts the
standard pointer (the hand — or the system default outside a session)
back after and frees the sprite data.  Reentrant: a nested call runs
FN directly under the already-shown pointer."
  (if *busy-pointer-active*
      (funcall fn)
      (multiple-value-bind (chip height) (%make-busy-pointer-chip)
        (unwind-protect
            (let ((*busy-pointer-active* t))
              (amiga.intuition:set-pointer win chip height 16 -7 -6)
              (funcall fn))
          (%apply-standard-pointer win)
          (amiga:free-chip chip)))))

;;; The image cache: effects-band icons (effect :image), location
;;; pictures (the location op's :image) and character portraits (hero
;;; class :image) — arbitrary ILBMs loaded lazily on first draw into a
;;; per-session cache keyed by the resolved path (map-relative, like
;;; zone tile packs), with the wall-piece bitmap recipe: window-depth
;;; friend bitmap, chunky upload, chip-RAM cookie-cut mask when the
;;; image uses pen 0.  A file that will not load logs once and its
;;; user falls back (text label, first-person view).

(defun %upload-planar (img friend depth)
  "PLANAR-IMAGE IMG as a fresh offscreen friend-format bitmap — the
wall-piece planar recipe: the plane rows go into a scratch planar
BitMap and the blitter moves them into the friend-format bitmap —
no per-pixel chunky fold, no WriteChunkyPixels (a location picture
cost ~30s on a 68020 through the chunky path)."
  (let* ((w (planar-image-width img))
         (h (planar-image-height img))
         (bm (amiga.gfx:alloc-bitmap w h depth :friend friend))
         (scratch (amiga.gfx:alloc-bitmap w h depth)))
    (unwind-protect
         (progn
           (%poke-planes scratch img)
           (amiga.gfx:with-bitmap-rastport (brp bm)
             (amiga.gfx:blt-bitmap-rastport scratch 0 0 brp 0 0 w h)))
      (amiga.gfx:free-bitmap scratch))
    bm))

(defun %load-image (rp path)
  "Load the ILBM at PATH into an offscreen bitmap; returns the cache
entry (BITMAP WIDTH HEIGHT MASK FRAMES RECT), MASK NIL for an opaque
image, and the mask goes to chip RAM in one POKE-BYTES instead of a
poke per byte.  FRAMES/RECT carry the optional in-place animation
shipped beside PATH (see IMAGE-FRAME-FILES): FRAMES a vector of
(BITMAP . MASK) conses, element 0 the base image itself, and RECT the
(X Y W H) box bounding every pixel any frame changes — the anim
stepper re-blits only that box.  Both NIL for a still image (no frame
files, or frames identical to the base).  A mis-sized frame signals,
like a mis-sized pack piece: a broken pack should say so, not animate
garbage."
  (let* ((img (read-ilbm-planar path))
         (w (planar-image-width img))
         (h (planar-image-height img))
         (friend (amiga.gfx:rastport-bitmap rp))
         (depth (max 2 (amiga.gfx:get-bitmap-attr friend
                                                  amiga.gfx:+bma-depth+)
                     (planar-image-depth img)))
         (bm (%upload-planar img friend depth))
         (mask (%planar-piece-mask img))
         (frames '())
         (rect nil)
         (ok nil))
    (unwind-protect
         (progn
           (dolist (file (image-frame-files path))
             (let ((fimg (read-ilbm-planar file)))
               (unless (and (= w (planar-image-width fimg))
                            (= h (planar-image-height fimg)))
                 (error "~A is ~Dx~D, its base frame ~A is ~Dx~D"
                        file (planar-image-width fimg)
                        (planar-image-height fimg) path w h))
               (push (cons (%upload-planar fimg friend depth)
                           (%planar-piece-mask fimg))
                     frames)
               (setf rect (%rect-union rect (planar-diff-rect img fimg)))))
           (setf ok t))
      ;; a bad frame file must not leak the bitmaps loaded before it
      ;; (the caller logs the error and marks the path :missing)
      (unless ok
        (dolist (f frames)
          (amiga.gfx:free-bitmap (car f))
          (when (cdr f) (amiga:free-chip (cdr f))))
        (amiga.gfx:free-bitmap bm)
        (when mask (amiga:free-chip mask))))
    ;; frames that never differ from the base animate nothing: drop
    ;; them so the stepper has no record to chew on
    (when (and frames (null rect))
      (dolist (f frames)
        (amiga.gfx:free-bitmap (car f))
        (when (cdr f) (amiga:free-chip (cdr f))))
      (setf frames '()))
    (list bm w h mask
          (when frames
            (coerce (cons (cons bm mask) (nreverse frames)) 'vector))
          rect)))

(defun %cached-image (rp images path log)
  "The cached entry (BITMAP WIDTH HEIGHT MASK FRAMES RECT) for the
ILBM at PATH,
loading it on first sight, or NIL — PATH is NIL, or the file would
not load (said once in the log).  A first-sight load reads an ILBM
from disk — seconds at 14MHz (a location picture on entering a shop),
so it runs under the busy pointer."
  (when path
    (let ((entry (gethash path images)))
      (flet ((load-it ()
               (handler-case
                   (setf (gethash path images) (%load-image rp path))
                 (error (e)
                   (when log
                     (log-message log (format nil "No image ~A (~A)."
                                              path e)))
                   (setf (gethash path images) :missing)
                   nil))))
        (cond ((eq entry :missing) nil)
              (entry entry)
              (*pointer-window*
               (%call-with-busy-pointer *pointer-window* #'load-it))
              (t (load-it)))))))

(defun %effect-icon (rp images game effect log)
  "EFFECT's cached icon entry (BITMAP WIDTH HEIGHT MASK FRAMES RECT),
or NIL — no :image, or the file would not load (the band falls back to
nothing)."
  (%cached-image rp images (effect-image-path game effect) log))

(defun %free-images (images)
  "Free the cached image bitmaps and masks — animation frames too
(their element 0 is the entry's own bitmap/mask, freed once); safe
with NIL."
  (when images
    (maphash (lambda (path entry)
               (declare (ignore path))
               (unless (eq entry :missing)
                 (amiga.gfx:free-bitmap (first entry))
                 (when (fourth entry)
                   (amiga:free-chip (fourth entry)))
                 (let ((frames (fifth entry)))
                   (when frames
                     (loop for i from 1 below (length frames)
                           for f = (aref frames i)
                           do (amiga.gfx:free-bitmap (car f))
                              (when (cdr f)
                                (amiga:free-chip (cdr f))))))))
             images))
  nil)

;;; In-place animation: an image with -fN frame files beside it (see
;;; IMAGE-FRAME-FILES and the FRAMES/RECT slots of the cache entry)
;;; cycles on the INTUITICKS heartbeat.  The two picture renderers
;;; register a blit record for every animated image they draw; the
;;; INTUITICKS handler advances the frame counter every few ticks and
;;; re-blits each record's changed rectangle IN PLACE — never through
;;; REDRAW, whose monolithic repaint (picture + band + log + roster)
;;; is exactly what a 14MHz 68020 cannot afford ten times a second.
;;; REDRAW clears the records before it paints, so a page that draws
;;; no picture or band (the map, help, an overlay menu) leaves the
;;; stepper with nothing to blit and the animation stops by itself.

(defvar *anim-blits* '()
  "Blit records for the animated images on screen, rebuilt by every
REDRAW.  (:PICTURE FRAMES SX SY DX DY W H): blit the current frame's
SX/SY W-x-H sub-rectangle — the dirty rect, pre-cropped to the visible
window — opaque to DX/DY.  (:ICON FRAMES X Y W H): grey-wipe the band
slot and cookie-cut the whole current frame back over it (frames may
disagree about which pixels are transparent, so a masked blit alone
could not erase the previous frame).")

(defvar *anim-step* 0
  "The animation frame counter: every record shows its frame
(MOD *ANIM-STEP* frame-count), so all animations stay in lockstep and
a redraw mid-cycle repaints the frame the stepper is showing rather
than snapping back to frame 0.")

(defconstant +anim-ticks-per-step+ 3
  "INTUITICKS per animation step.  Ticks arrive ~10/s, so 3 gives the
~3 frames/s shuffle of the classic monster portraits — and a third of
the blit cost of stepping on every tick.")

(defun %anim-frame (frames)
  "The (BITMAP . MASK) cons FRAMES shows at the current step."
  (aref frames (mod *anim-step* (length frames))))

(defun %anim-blit-step (rp)
  "One animation heartbeat: advance the frame counter and re-blit
every record's changed rectangle in place."
  (incf *anim-step*)
  (dolist (r *anim-blits*)
    (ecase (first r)
      (:picture
       (destructuring-bind (frames sx sy dx dy w h) (rest r)
         (amiga.gfx:blt-bitmap-rastport (car (%anim-frame frames))
                                        sx sy rp dx dy w h)))
      (:icon
       (destructuring-bind (frames x y w h) (rest r)
         (let ((f (%anim-frame frames)))
           (amiga.gfx:set-a-pen rp 2)
           (amiga.gfx:rect-fill rp x y (+ x w -1) (+ y h -1))
           (if (cdr f)
               (amiga.gfx:blt-mask-bitmap-rastport (car f) 0 0 rp x y w h
                                                   (cdr f))
               (amiga.gfx:blt-bitmap-rastport (car f) 0 0 rp x y w h))
           (amiga.gfx:set-a-pen rp 1)))))))

(defun %amiga-draw-picture (rp images path l log)
  "Draw the ILBM at PATH in the view slot — black backdrop, the image
centered (center-cropped when it overhangs the viewport).  Returns T,
or NIL — no PATH, or the file would not load — so the caller can fall
back to the first-person view.  Pictures blit opaque: over the black
backdrop a pen-0 pixel and a masked-out pixel look the same.  An
animated picture draws its current frame and registers the visible
part of its dirty rect with the stepper."
  (let ((entry (%cached-image rp images path log)))
    (when entry
      (destructuring-bind (bm iw ih mask frames rect) entry
        (declare (ignore mask))
        (let* ((ox (ui-layout-bx l))
               (oy (ui-layout-by l))
               (w (ui-layout-fp-w l))
               (h (ui-layout-fp-h l))
               (bw (min iw w))
               (bh (min ih h))
               (sx (max 0 (floor (- iw w) 2)))
               (sy (max 0 (floor (- ih h) 2)))
               (dx (+ ox (max 0 (floor (- w iw) 2))))
               (dy (+ oy (max 0 (floor (- h ih) 2)))))
          (when frames
            (setf bm (car (%anim-frame frames)))
            (destructuring-bind (rx ry rw rh) rect
              (let ((ix0 (max rx sx))
                    (iy0 (max ry sy))
                    (ix1 (min (+ rx rw) (+ sx bw)))
                    (iy1 (min (+ ry rh) (+ sy bh))))
                ;; a center-crop may push the moving part off screen
                ;; entirely — then there is nothing to step
                (when (and (< ix0 ix1) (< iy0 iy1))
                  (push (list :picture frames ix0 iy0
                              (+ dx (- ix0 sx)) (+ dy (- iy0 sy))
                              (- ix1 ix0) (- iy1 iy0))
                        *anim-blits*)))))
          (amiga.gfx:set-a-pen rp 0)
          (amiga.gfx:rect-fill rp ox oy (+ ox w -1) (+ oy h -1))
          (amiga.gfx:set-a-pen rp 1)
          (amiga.gfx:blt-bitmap-rastport bm sx sy rp dx dy bw bh)
          t)))))

;;; Message-log lines render in the microfont's condensed small face
;;; (microfont.lisp) — 6px per character against topaz 8's 8px, so
;;; the narrow column holds more text, in the bold condensed manner
;;; of the Bard's Tale screens.  Each distinct display line is rasterized once
;;; (black on the white page) into an offscreen bitmap and blitted on
;;; every redraw — one OS call per line instead of a chunky upload,
;;; which matters at 14MHz.  The cache is per session, keyed by the
;;; (already truncated) line text.

(defconstant +log-line-cache-limit+ 128
  "Cache entries before the log-line bitmaps are flushed wholesale —
far above the distinct lines ever on screen at once.")

(defun %log-line-bitmap (rp lines text)
  "The cached (BITMAP . WIDTH) for the log display line TEXT, LINES
being the session's cache; renders and uploads it on first sight."
  (or (gethash text lines)
      (progn
        (when (>= (hash-table-count lines) +log-line-cache-limit+)
          (%free-log-lines lines)
          (clrhash lines))
        (multiple-value-bind (pens w h)
            (microfont-small-line text 0 1) ; black glyphs on the white page
          (let* ((friend (amiga.gfx:rastport-bitmap rp))
                 (depth (max 2 (amiga.gfx:get-bitmap-attr
                                friend amiga.gfx:+bma-depth+)))
                 (bm (amiga.gfx:alloc-bitmap w h depth :friend friend)))
            (amiga.gfx:with-bitmap-rastport (brp bm)
              (amiga.gfx:write-chunky brp 0 0 w h pens))
            (setf (gethash text lines) (cons bm w)))))))

(defun %free-log-lines (lines)
  "Free the cached log-line bitmaps; safe with NIL."
  (when lines
    (maphash (lambda (text entry)
               (declare (ignore text))
               (amiga.gfx:free-bitmap (car entry)))
             lines))
  nil)

(defun %map-glyph (rp px py cell ch pen)
  "Draw CH with PEN centered in the CELL-pixel map cell at (PX,PY),
in the microfont's compact small face — a 30x30 city at lores draws
7px cells, which the bold face's 8px advance cannot enter.  Cells
under 6px carry no glyph.  Only the glyph's own pixels are set: a
chunky upload would stamp its background rectangle over the wall
lines framing the cell."
  (when (>= cell +microfont-small-advance+)
    (amiga.gfx:set-a-pen rp pen)
    (let ((rows (microfont-small-glyph ch))
          (gx (+ px (max 1 (floor (- cell +microfont-small-width+) 2))))
          (gy (+ py (max 1 (floor (- cell +microfont-glyph-height+) 2)))))
      (dotimes (row +microfont-glyph-height+)
        (let ((bits (aref rows row)))
          (dotimes (col +microfont-small-width+)
            (when (logbitp (- +microfont-small-width+ 1 col) bits)
              (amiga.gfx:write-pixel rp (+ gx col) (+ gy row)))))))))

(defun %amiga-draw-map-region (rp game ox oy cell x0 y0 vw vh full
                               &optional legend)
  "Draw automap cells [X0,X0+VW) x [Y0,Y0+VH) at (OX,OY), CELL pixels
per cell — black ink on the grey page, doors and the party amber.
FULL non-NIL draws everything (debug); otherwise only what the
party's knowledge holds.  LEGEND is the MAP-LEGEND-ENTRIES list —
each entry's marker draws amber on its cell.  Walls come from
MAP-EDGE-RUNS: one draw-line per straight stretch instead of one per
cell edge, which is what makes a 30x30 city map open in a blink
instead of seconds at 14MHz."
  (amiga.gfx:set-a-pen rp 2)
  (amiga.gfx:rect-fill rp ox oy (+ ox (* cell vw)) (+ oy (* cell vh)))
  (let ((map (game-map game))
        (k (game-knowledge game)))
    ;; walls, then doors — merged runs, one pen switch each
    (multiple-value-bind (wall-runs door-runs)
        (map-edge-runs map (unless full k) x0 y0 vw vh)
      (flet ((draw-runs (runs)
               (dolist (r runs)
                 (destructuring-bind (horizontal ex ey len) r
                   (let ((px (+ ox (* ex cell)))
                         (py (+ oy (* ey cell))))
                     (if horizontal
                         (amiga.gfx:draw-line rp px py
                                              (+ px (* len cell)) py)
                         (amiga.gfx:draw-line rp px py
                                              px (+ py (* len cell)))))))))
        (amiga.gfx:set-a-pen rp 0)
        (draw-runs wall-runs)
        (amiga.gfx:set-a-pen rp 3)
        (draw-runs door-runs)))
    ;; feature glyphs, black — direct scan of the packed feature bytes
    (let ((features (dungeon-map-features map))
          (mw (dungeon-map-width map)))
      (when (>= cell +microfont-small-advance+)
        (dotimes (ry vh)
          (let ((row (+ (* (+ y0 ry) mw) x0)))
            (dotimes (rx vw)
              (let ((c (aref features (+ row rx))))
                (unless (zerop c)
                  (when (or full (cell-explored-p k (+ x0 rx) (+ y0 ry)))
                    (%map-glyph rp (+ ox (* rx cell)) (+ oy (* ry cell))
                                cell (code-char c) 0)))))))))
    ;; legend markers, amber on their cells
    (dolist (e legend)
      (destructuring-bind (marker x y title) e
        (declare (ignore title))
        (when (and (<= x0 x) (< x (+ x0 vw))
                   (<= y0 y) (< y (+ y0 vh)))
          (%map-glyph rp (+ ox (* (- x x0) cell)) (+ oy (* (- y y0) cell))
                      cell marker 3))))
    ;; party marker: filled block + facing tick, when inside the region
    (let ((gx (game-x game))
          (gy (game-y game)))
      (when (and (<= x0 gx) (< gx (+ x0 vw))
                 (<= y0 gy) (< gy (+ y0 vh)))
        (let* ((px (+ ox (* (- gx x0) cell)))
               (py (+ oy (* (- gy y0) cell)))
               (cx (+ px (floor cell 2)))
               (cy (+ py (floor cell 2)))
               (r (max 1 (floor cell 4)))
               (f (game-facing game)))
          (amiga.gfx:set-a-pen rp 3)
          (amiga.gfx:rect-fill rp (- cx r) (- cy r) (+ cx r) (+ cy r))
          (amiga.gfx:draw-line rp cx cy
                               (+ cx (* (aref *dir-dx* f)
                                        (- (floor cell 2) 1)))
                               (+ cy (* (aref *dir-dy* f)
                                        (- (floor cell 2) 1))))
          (amiga.gfx:set-a-pen rp 1))))))

(defun %amiga-draw-band (rp game l &optional icons log)
  "The effect strip below the log page, on the grey chrome (a small
gap separates it from the page above; :BAND-HEIGHT sets the strip's
height per profile — hires keeps it 20px, clearing the 16px icons with
a 4px margin, while lores' strip is 16px, flush with the icons, so the
log page above gets the room).
One slot per active effect, laid out left to right in effect order —
no labels; casting/expiry is announced in the log.  An effect's slot
shows its icon (ICONS is the session's icon cache, see %EFFECT-ICON),
except that an effect carrying a :COMPASS payload shows the live
compass rose instead: the diamond with the amber needle pointing at
the party's facing, the facing letter beside it.  The rose thus sits
wherever the granting spell/item/song sits in the strip, not at a
fixed spot.  An effect with neither icon nor :COMPASS shows nothing."
  (let* ((ox (ui-layout-log-x l))
         (right (ui-layout-right l))
         (band-y (ui-layout-band-y l))
         (band-h (ui-layout-band-h l))
         (bottom (+ band-y band-h -1))
         (cw (ui-layout-cw l))
         (x (- ox 4)))
    ;; the strip is bare chrome: grey-wipe it (covers icon churn)
    (amiga.gfx:set-a-pen rp 2)
    (amiga.gfx:rect-fill rp (- ox 4) band-y right bottom)
    (dolist (e (game-effects game))
      (cond
        ((getf (effect-payload e) :compass)
         ;; the live rose in this effect's slot
         (let* ((size (min band-h 16))
                (cx (+ x (floor size 2)))
                (cy (+ band-y (floor band-h 2)))
                (ri (max 3 (1- (floor size 2))))
                (f (game-facing game)))
           (when (<= (+ x size 2 cw -1) right)
             (amiga.gfx:set-a-pen rp 0)
             (amiga.gfx:draw-line rp cx (- cy ri) (+ cx ri) cy)
             (amiga.gfx:draw-line rp (+ cx ri) cy cx (+ cy ri))
             (amiga.gfx:draw-line rp cx (+ cy ri) (- cx ri) cy)
             (amiga.gfx:draw-line rp (- cx ri) cy cx (- cy ri))
             (amiga.gfx:set-a-pen rp 3)
             (amiga.gfx:draw-line rp cx cy
                                  (+ cx (* (aref *dir-dx* f) (1- ri)))
                                  (+ cy (* (aref *dir-dy* f) (1- ri))))
             ;; the facing letter, amber, beside the rose
             (amiga.gfx:move-to rp (+ x size 2) (+ cy 3))
             (amiga.gfx:gfx-text rp (string (char "NESW" f)))
             (incf x (+ size 2 cw 4)))))
        (t
         (let ((entry (and icons (%effect-icon rp icons game e log))))
           (when entry
             (destructuring-bind (bm iw ih mask frames rect) entry
               (declare (ignore rect))
               (when frames
                 ;; the icon's current animation frame — and its slot
                 ;; goes to the stepper (the whole 16px icon: at that
                 ;; size a dirty-rect blit saves nothing)
                 (let ((f (%anim-frame frames)))
                   (setf bm (car f) mask (cdr f))))
               (when (and (<= (+ x iw -1) right) (<= ih band-h))
                 (let ((y (+ band-y (max 0 (floor (- band-h ih) 2)))))
                   (when frames
                     (push (list :icon frames x y iw ih) *anim-blits*))
                   (if mask
                       (amiga.gfx:blt-mask-bitmap-rastport
                        bm 0 0 rp x y iw ih mask)
                       (amiga.gfx:blt-bitmap-rastport
                        bm 0 0 rp x y iw ih)))
                 (incf x (+ iw 2)))))))))
    (amiga.gfx:set-a-pen rp 1)))

(defun %put-microfont-line (rp lines-cache text x y)
  "One microfont display line at (X,Y): the cached bitmap blit when
LINES-CACHE is given, a direct chunky upload otherwise (correct but
slower — good enough for tests)."
  (if lines-cache
      (destructuring-bind (bm . bw) (%log-line-bitmap rp lines-cache text)
        (amiga.gfx:blt-bitmap-rastport bm 0 0 rp x y
                                       bw +microfont-line-height+))
      (multiple-value-bind (pens pw ph) (microfont-small-line text 0 1)
        (amiga.gfx:write-chunky rp x y pw ph pens))))

(defun %put-microfont-small-text (rp text x y &optional (fg 0) (bg 2))
  "One small-face line at (X,Y) in arbitrary pens — FG glyphs on a BG
field, the default being black on the chrome grey (the map page).
Uncached: the map and its legend redraw only on demand, so the chunky
upload costs nothing worth a bitmap cache (that is %PUT-MICROFONT-LINE's
job for the log, which repaints every frame)."
  (multiple-value-bind (pens w h) (microfont-small-line text fg bg)
    (amiga.gfx:write-chunky rp x y w h pens)))

(defun %amiga-draw-log (rp log l &optional lines-cache)
  "Message log: trailing lines, newest at the bottom, black microfont
text on the white page (the shell — outline and shadow — is
%CHROME-FRAMES's; the strip below the page belongs to
%AMIGA-DRAW-BAND).  LINES-CACHE is the session's rendered-line bitmap
cache (see %LOG-LINE-BITMAP)."
  (let* ((ox (ui-layout-log-x l))
         (oy (ui-layout-by l))
         (w (ui-layout-log-w l))
         (h (- (ui-layout-page-b l) oy))
         (lh +microfont-line-height+)
         (n (max 1 (floor (- h 2) lh)))
         (max-chars (max 4 (floor (- w 4) +microfont-small-advance+)))
         ;; Each message is a paragraph led by one blank line; long ones
         ;; wrap onto full-width continuation lines.  Keep the trailing N
         ;; display lines so the newest stays at the bottom.
         (wrapped (mapcan (lambda (m) (wrap-message m max-chars))
                          (log-recent log n)))
         (lines (last wrapped n)))
    ;; page interior (inside the black outline)
    (amiga.gfx:set-a-pen rp 1)
    (amiga.gfx:rect-fill rp (- ox 3) oy (+ ox w -1) (+ oy h -1))
    (let ((y (+ oy (- h (* (length lines) lh)) -1)))
      (dolist (m lines)
        (%put-microfont-line rp lines-cache
                             (if (> (length m) max-chars)
                                 (subseq m 0 max-chars)
                                 m)
                             ox y)
        (incf y lh)))
    (amiga.gfx:set-a-pen rp 1)))

(defun %amiga-draw-transcript (rp log l base &optional lines-cache)
  "A combat round's own page: only the messages said since BASE (the
log length when the round began), top-aligned on a cleared white page
— each round turns a new page instead of running into the walkabout
log.  A round longer than the page keeps its newest lines.
LINES-CACHE as in %AMIGA-DRAW-LOG."
  (let* ((ox (ui-layout-log-x l))
         (oy (ui-layout-by l))
         (w (ui-layout-log-w l))
         (h (- (ui-layout-page-b l) oy))
         (lh +microfont-line-height+)
         (n (max 1 (floor (- h 2) lh)))
         (max-chars (max 4 (floor (- w 4) +microfont-small-advance+)))
         (wrapped (mapcan (lambda (m) (wrap-message m max-chars))
                          (log-since log base)))
         ;; WRAP-MESSAGE leads every message with a blank spacer line;
         ;; the page's first message needs none
         (wrapped (if (and wrapped (equal (first wrapped) ""))
                      (rest wrapped)
                      wrapped))
         (lines (if (> (length wrapped) n) (last wrapped n) wrapped)))
    ;; page interior (inside the black outline)
    (amiga.gfx:set-a-pen rp 1)
    (amiga.gfx:rect-fill rp (- ox 3) oy (+ ox w -1) (+ oy h -1))
    (let ((y (1+ oy)))
      (dolist (m lines)
        (%put-microfont-line rp lines-cache
                             (if (> (length m) max-chars)
                                 (subseq m 0 max-chars)
                                 m)
                             ox y)
        (incf y lh)))
    (amiga.gfx:set-a-pen rp 1)))

(defun %amiga-draw-scrollbar (rp x y w h start end n)
  "A vertical scrollbar beside a windowed menu list: a center rail
down the trough, the thumb the visible START..END share of the N
items (END exclusive), never thinner than a clickable nub.  The
trough above the thumb registers as u, below it as d — the mouse
keeps scrolling without the marker rows the lists used to spend."
  (let* ((y1 (+ y h -1))
         (x1 (+ x w -1))
         (ty0 (+ y (floor (* h start) n)))
         (ty1 (min y1 (max (+ ty0 3)
                           (+ y (1- (ceiling (* h end) n)))))))
    (amiga.gfx:set-a-pen rp 0)
    (let ((cx (+ x (floor w 2))))
      (amiga.gfx:rect-fill rp cx y cx y1))      ; the rail
    (amiga.gfx:rect-fill rp x ty0 x1 ty1)       ; the thumb
    ;; the click zones reach a little left of the bar — a thin target
    ;; is a poor target
    (when (> ty0 y)
      (%hotspot #\u (- x 3) y (1+ x1) (1- ty0)))
    (when (< ty1 y1)
      (%hotspot #\d (- x 3) (1+ ty1) (1+ x1) y1))))

(defun %amiga-draw-takeover (rp lines log l &optional lines-cache)
  "An interaction taking over the message area — a location's menu or
the character sheet: LINES (small-face microfont, wrapped) from the
top of the white page.  The menu owns the whole page — the log-tail split the
page used to carry under a rule read poorly in practice, so game
feedback waits for the page to close.  The page interior repaints
wholesale — the takeover's 'cls' — so switching pages never leaves
stale text.  A windowed list on the page (the generator just ran, so
*MENU-SCROLL* is its geometry) gets a scrollbar along the page's
right edge.  LINES-CACHE as in %AMIGA-DRAW-LOG."
  (declare (ignore log))
  (let* ((ox (ui-layout-log-x l))
         (oy (ui-layout-by l))
         (w (ui-layout-log-w l))
         (h (- (ui-layout-page-b l) oy))
         (lh +microfont-line-height+)
         (rows (max 1 (floor (- h 2) lh)))
         (scroll (shiftf *menu-scroll* nil))
         (bar-w (if scroll 3 0))
         (max-chars (max 4 (floor (- w 4 (if scroll (+ bar-w 3) 0))
                                  +microfont-small-advance+)))
         ;; a page that overflows packs its one-option-per-row footer
         ;; hints onto shared rows, then gives up its blank spacer
         ;; lines, before it truncates content (the lores shop page is
         ;; the tight case) — see FIT-MENU-LINES
         (menu (fit-menu-lines lines rows max-chars))
         (n-menu (min (length menu) rows)))
    ;; page interior: the takeover's cls
    (amiga.gfx:set-a-pen rp 1)
    (amiga.gfx:rect-fill rp (- ox 3) oy (+ ox w -1) (+ oy h -1))
    (let ((y (1+ oy))
          (n 0))
      (dolist (line menu)
        (when (< n n-menu)
          (let ((text (menu-line-text line)))
            (%put-microfont-line rp lines-cache
                                 (if (> (length text) max-chars)
                                     (subseq text 0 max-chars)
                                     text)
                                 ox y)
            ;; a click on an option row / a footer hint is its key
            (%register-line-hotspots line ox y +microfont-small-advance+
                                     lh (- ox 3) (+ ox w -1)))
          (incf y lh)
          (incf n))))
    (when scroll
      (destructuring-bind (start end n) scroll
        (%amiga-draw-scrollbar rp (- (+ ox w) 1 bar-w) (1+ oy)
                               bar-w (- h 2) start end n)))
    (amiga.gfx:set-a-pen rp 1)))

(defun %amiga-hero-row (rp game l y hero index)
  "One roster table row at baseline Y, Bard's Tale columns: number,
name, armor class (with equipment and spell effects), then
max/current hit points and max/current spell points, and the class
code.  The current points are picked out in white; a downed hero
keeps the normal pens and shows DEAD in the hit-point columns instead
of numbers, and an ailing one shows HERO-CONDITION-CODE where its
maximum would stand.  Columns come from the profile's ROSTER-COLS character
cells; the numbers are right-aligned in their column (ROSTER-CELL) so
their digits line up down the table."
  (let* ((ox (ui-layout-bx l))
         (cw (ui-layout-cw l))
         (cols (display-profile-roster-cols *display-profile*))
         ;; the name column runs up to one cell short of the AC column
         (name-w (- (getf cols :ac) 1 (getf cols :name)))
         (down (not (hero-alive-p hero))))
    (labels ((col (cell pen text)
               (amiga.gfx:set-a-pen rp pen)
               (amiga.gfx:move-to rp (+ ox (* cw cell)) y)
               (amiga.gfx:gfx-text rp text))
             (num (cell pen text)          ; right-aligned number column
               (col (roster-cell cell text) pen text)))
      (col (getf cols :no) 0 (format nil "~D" (1+ index)))
      ;; a banked level: a white up-arrow in the spare cell between
      ;; the number and the name, standing until the sheet's 'l'
      ;; takes the rise — topaz has no arrow glyph, so the row
      ;; draws its own (tip and head over a straight shaft)
      (when (hero-level-up-pending-p hero)
        (let ((mid (+ ox (* cw (1+ (getf cols :no))) (floor cw 2))))
          (amiga.gfx:set-a-pen rp 1)
          (amiga.gfx:rect-fill rp mid (- y 6) mid (1- y))
          (amiga.gfx:rect-fill rp (1- mid) (- y 5) (1+ mid) (- y 5))
          (amiga.gfx:rect-fill rp (- mid 2) (- y 4) (+ mid 2) (- y 4))))
      (col (getf cols :name) 0
           (let ((name (hero-name hero)))
             (if (> (length name) name-w) (subseq name 0 name-w) name)))
      (num (getf cols :ac) 0
           (format nil "~D" (hero-effective-ac hero game)))
      ;; a downed hero's hit points give way to DEAD, right-aligned
      ;; under the HIT heading (it runs one cell into the column gap);
      ;; an ailing one gives up the MAX figure the same way, to the code
      ;; for the worst thing about them (STON, PARA, INSA, POIS) while
      ;; the current points stay where they were — what is wrong with a
      ;; hero belongs in the table, and this is the column that can say
      ;; it without a column of its own
      (if down
          (num (getf cols :hit) 0 "DEAD")
          (progn
            (num (getf cols :hit) 0
                 (or (hero-condition-code hero)
                     (format nil "~D" (hero-max-hp hero))))
            (num (getf cols :hpts) 1
                 (format nil "~D" (hero-hp hero)))))
      (num (getf cols :spl) 0 (format nil "~D" (hero-max-sp hero)))
      (num (getf cols :spts) 1 (format nil "~D" (hero-sp hero)))
      (col (getf cols :cl) 0 (hero-class-abbrev hero)))
    (amiga.gfx:set-a-pen rp 1)))

(defun %amiga-party (rp game l &optional clickable)
  "Party roster table, Bard's Tale style: a header row and one numbered
row per SLOT — +PARTY-LIMIT+ of them, whether the party fills them or
not — right under the location plaque (there is no status line), black
on the grey chrome with the current hit/spell points in white.  The
row number is the key that opens that hero's character sheet;
CLICKABLE non-NIL also registers each FILLED row as a click target for
its digit — passed only when digits currently mean the roster (not
while a menu model or a location eats them).

An empty slot draws its number and nothing else.  The roster is what
the layout is tightest against, so the rows the party has not claimed
should be as visible as the ones it has — a six-strong party that drew
six rows said nothing about whether the seventh still fits.

Rows advance by the layout's ROW-H, the glyph box itself, so the table
sets solid the way a printed roster does — see the note above
UI-LAYOUT."
  (let* ((ox (ui-layout-bx l))
         (oy (ui-layout-hdr-y l))
         (row-h (ui-layout-row-h l)))
    (amiga.gfx:set-a-pen rp 2)
    (amiga.gfx:rect-fill rp ox oy
                         (ui-layout-right l)
                         (+ (ui-layout-party-y l)
                            (* row-h +party-limit+) -1))
    (amiga.gfx:set-a-pen rp 0)
    (let ((y (+ oy (ui-layout-base l)))
          (cw (ui-layout-cw l))
          (cols (display-profile-roster-cols *display-profile*)))
      (labels ((col (cell text)
                 (amiga.gfx:move-to rp (+ ox (* cw cell)) y)
                 (amiga.gfx:gfx-text rp text))
               ;; a number column's heading sits over its values, so it
               ;; is right-aligned in the same field they are
               (num (cell text) (col (roster-cell cell text) text)))
        (col (getf cols :name) "CHARACTER")
        (num (getf cols :ac)   "AC")
        (num (getf cols :hit)  "HIT")
        (num (getf cols :hpts) "PTS")
        (num (getf cols :spl)  "SPL")
        (num (getf cols :spts) "PTS")
        (col (getf cols :cl)   "CL")))
    (let ((y (+ (ui-layout-party-y l) (ui-layout-base l)))
          (row-y (ui-layout-party-y l))
          (party (game-party game))
          (cw (ui-layout-cw l))
          (cols (display-profile-roster-cols *display-profile*)))
      (dotimes (i +party-limit+)
        (let ((h (nth i party)))
          (if h
              (progn
                (%amiga-hero-row rp game l y h i)
                (when (and clickable (< i 9))
                  (%hotspot (digit-char (1+ i))
                            ox row-y (ui-layout-right l)
                            (+ row-y row-h -1))))
              ;; an unfilled slot still shows its number: the roster is
              ;; the layout's tightest constraint, and a bare "7" is
              ;; what makes the reserved row visible on a party that
              ;; has not claimed it yet (the guest slot usually).  The
              ;; row stays inert — no hotspot, so a click there means
              ;; nothing rather than opening a sheet that is not there.
              (progn
                (amiga.gfx:set-a-pen rp 0)
                (amiga.gfx:move-to rp (+ ox (* cw (getf cols :no))) y)
                (amiga.gfx:gfx-text rp (format nil "~D" (1+ i))))))
        (incf y row-h)
        (incf row-y row-h)))
    (amiga.gfx:set-a-pen rp 1)))

(defun %help-page-box (l lines)
  "The help page's box and row budget on layout L for the microfont
LINES: values (PX PY PW PH ROWS).  The page sits centered on the
content area, as wide as the longest line wants (plus the scrollbar
gutter) and as tall as the reference or the area affords — ROWS is
how many small-face rows that height holds, and a reference longer
than ROWS windows and scrolls (see %AMIGA-DRAW-HELP)."
  (let* ((ox (ui-layout-bx l))
         (oy (ui-layout-by l))
         (lh +microfont-line-height+)
         (cw +microfont-small-advance+)
         (py (+ oy 8))
         (want (+ 16 6 (* cw (reduce #'max lines :key #'length))))
         (pw (min want (- (ui-layout-right l) ox 4)))
         (ph (min (- (ui-layout-bottom l) py 4)
                  (+ (* lh (length lines)) 12)))
         (px (+ ox (max 0 (floor (- (ui-layout-right l) ox pw) 2)))))
    (values px py pw ph (floor (- ph 8) lh))))

(defun %help-scroll (l top char)
  "The help page's scroll offset after key CHAR (u/d — MENU-SCROLL
over the page's row window), or NIL when CHAR does not scroll or the
whole reference fits the page."
  (let ((lines (help-lines t)))
    (multiple-value-bind (px py pw ph rows) (%help-page-box l lines)
      (declare (ignore px py pw ph))
      (menu-scroll top char (length lines) rows))))

(defun %amiga-draw-help (rp l top &optional lines-cache)
  "The help page ('h' or '?'): the key-mapping reference (HELP-LINES)
on a white parchment page over the grey chrome, like the character
sheet, in the microfont's condensed small face — topaz held barely
half the reference on the 200-line layout.  A reference still taller
than the page windows at scroll offset TOP: u/d turn it a page at a
time, and the scrollbar down the page's right edge clicks as the same
two keys.  LINES-CACHE as in %AMIGA-DRAW-LOG."
  (let ((lines (help-lines t)))         ; this UI wires map U/D scroll
    (multiple-value-bind (px py pw ph rows) (%help-page-box l lines)
      (amiga.gfx:set-a-pen rp 2)
      (amiga.gfx:rect-fill rp (ui-layout-bx l) (ui-layout-by l)
                           (ui-layout-right l) (ui-layout-bottom l))
      ;; page shadow, sheet, outline — the character-sheet look
      (amiga.gfx:set-a-pen rp 0)
      (amiga.gfx:rect-fill rp (+ px 2) (+ py 2) (+ px pw 2) (+ py ph 2))
      (amiga.gfx:set-a-pen rp 1)
      (amiga.gfx:rect-fill rp px py (+ px pw) (+ py ph))
      (amiga.gfx:set-a-pen rp 0)
      (%chrome-rect rp px py (+ px pw) (+ py ph))
      (multiple-value-bind (start end above below)
          (menu-window (length lines) top rows)
        (let ((max-chars (floor (- pw 16) +microfont-small-advance+))
              (y (+ py 5)))
          (dolist (text (subseq lines start end))
            (%put-microfont-line rp lines-cache
                                 (if (> (length text) max-chars)
                                     (subseq text 0 max-chars)
                                     text)
                                 (+ px 8) y)
            (incf y +microfont-line-height+)))
        (when (or above below)
          (%amiga-draw-scrollbar rp (+ px pw -5) (+ py 2)
                                 3 (- ph 4)
                                 start end (length lines)))))
    (amiga.gfx:set-a-pen rp 1)))

(defun %amiga-draw-sheet (rp game index l)
  "The INDEXth party member's stat block on a full white parchment
page over the grey chrome.  The play flow shows the sheet as a
message-area takeover instead (HERO-SHEET-LINES + the portrait in the
view column — see REDRAW); this full-page overlay variant stays for
front-ends that want it."
  (let* ((ox (ui-layout-bx l))
         (oy (ui-layout-by l))
         (lh (ui-layout-lh l))
         (cw (ui-layout-cw l))
         (hero (nth index (game-party game)))
         (px (+ ox (* 2 cw)))           ; the page
         (py (+ oy 8))
         (pw (min (* 40 cw) (- (ui-layout-right l) px (* 2 cw))))
         (ph (+ (* lh 10) 12)))
    (amiga.gfx:set-a-pen rp 2)
    (amiga.gfx:rect-fill rp ox oy (ui-layout-right l) (ui-layout-bottom l))
    ;; page shadow, sheet, outline
    (amiga.gfx:set-a-pen rp 0)
    (amiga.gfx:rect-fill rp (+ px 2) (+ py 2) (+ px pw 2) (+ py ph 2))
    (amiga.gfx:set-a-pen rp 1)
    (amiga.gfx:rect-fill rp px py (+ px pw) (+ py ph))
    (amiga.gfx:set-a-pen rp 0)
    (%chrome-rect rp px py (+ px pw) (+ py ph))
    (let ((y (+ py 4 (ui-layout-base l)))
          (max-chars (floor (- pw 16) cw)))
      (labels ((line (text)
                 (amiga.gfx:move-to rp (+ px 8) y)
                 (amiga.gfx:gfx-text rp (if (> (length text) max-chars)
                                            (subseq text 0 max-chars)
                                            text))
                 (incf y lh)))
        (line (format nil "Character ~D of ~D" (1+ index)
                      (length (game-party game))))
        (incf y (floor lh 2))
        (when hero
          (dolist (text (hero-summary-lines hero game))
            (line text)))
        (incf y (floor lh 2))
        (line "1-7 view another   Esc back")))
    (amiga.gfx:set-a-pen rp 1)))

(defun %menu-page-box (l)
  "The overlay menu page's box on layout L: values (PX PY PW PH).
The page is a dialog: four fifths of the content width, the leftover
split evenly so it sits centered between the content edges, running
from just below the content top down to the roster header.  Every
picker lists names — save slots, spells, songs, pack items — and a
name has no room in the lores view column, so they all take the same
box rather than one shape per menu."
  (let* ((ox (ui-layout-bx l))
         (py (+ (ui-layout-by l) 4))
         (pw (floor (* 4 (- (ui-layout-right l) ox)) 5))
         (px (+ ox (max 0 (floor (- (ui-layout-right l) ox pw) 2)))))
    (values px py pw (- (ui-layout-hdr-y l) 4 py))))

(defun %amiga-draw-page (rp menu-lines l &optional lines-cache)
  "An overlay menu page (cast, use, sing, save slots): MENU-LINES on
a white dialog centered over the page (%MENU-PAGE-BOX), in the
microfont's condensed small face — the same type as the message log
and the location takeover, and small enough that a long save-slot or
spell list fits the lo-res page without truncation.  The roster below
stays live, so hit/spell points move as the party acts, but the
dialog covers the message area: the caller skips drawing the log
under it and repaints (FRESH-PLAY) when the picker closes — nothing
worth watching happens beside a picker while it is open.  A windowed
list on the page gets a scrollbar along its right edge (see
%AMIGA-DRAW-SCROLLBAR).  LINES-CACHE as in %AMIGA-DRAW-LOG.
Locations and the character sheet use the message-area takeover
instead (%AMIGA-DRAW-TAKEOVER)."
  (multiple-value-bind (px py pw ph) (%menu-page-box l)
    (let* ((lh +microfont-line-height+)
           (cw +microfont-small-advance+)
           (max-lines (floor (- ph 8) lh))
           (scroll (shiftf *menu-scroll* nil))
           (bar-w (if scroll 3 0))
           (max-chars (floor (- pw 16 (if scroll (+ bar-w 3) 0)) cw))
           (lines (fit-menu-lines menu-lines max-lines max-chars)))
      ;; page shadow, sheet, outline — same look as the character sheet
      (amiga.gfx:set-a-pen rp 0)
      (amiga.gfx:rect-fill rp (+ px 2) (+ py 2) (+ px pw 2) (+ py ph 2))
      (amiga.gfx:set-a-pen rp 1)
      (amiga.gfx:rect-fill rp px py (+ px pw) (+ py ph))
      (amiga.gfx:set-a-pen rp 0)
      (%chrome-rect rp px py (+ px pw) (+ py ph))
      (let ((y (+ py 4))
            (n 0))
        (dolist (line lines)
          (when (< n max-lines)
            (let ((text (menu-line-text line)))
              (%put-microfont-line rp lines-cache
                                   (if (> (length text) max-chars)
                                       (subseq text 0 max-chars)
                                       text)
                                   (+ px 8) y)
              ;; a click on an option row / a footer hint is its key
              (%register-line-hotspots line (+ px 8) y cw lh
                                       (1+ px) (+ px pw -1)))
            (incf y lh)
            (incf n))))
      ;; a windowed list (the generator just ran — *MENU-SCROLL* is its
      ;; geometry) gets the scrollbar along the page's right edge
      (when scroll
        (destructuring-bind (start end n) scroll
          (%amiga-draw-scrollbar rp (- (+ px pw) 3 bar-w) (+ py 4)
                                 bar-w (- ph 8) start end n)))
      (amiga.gfx:set-a-pen rp 1))))

(defun %amiga-draw-confirm (rp lines l &optional lines-cache)
  "A small confirmation box centered on the inner window — the quit
question and its options (QUIT-CONFIRM-LINES) in the microfont's
condensed small face on a white page, drawn on top of whatever page is
already up.  The box is sized to its text; option rows click as their
keys, exactly as on %AMIGA-DRAW-PAGE.  LINES-CACHE as in
%AMIGA-DRAW-LOG."
  (let* ((lh +microfont-line-height+)
         (cw +microfont-small-advance+)
         (widest (reduce #'max lines
                         :key (lambda (line)
                                (length (menu-line-text line)))
                         :initial-value 0))
         (pw (min (+ (* widest cw) 16)
                  (- (ui-layout-right l) (ui-layout-bx l) 8)))
         (ph (+ (* lh (length lines)) 8))
         (px (+ (ui-layout-bx l)
                (max 0 (floor (- (ui-layout-right l) (ui-layout-bx l) pw) 2))))
         (py (+ (ui-layout-by l)
                (max 0 (floor (- (ui-layout-bottom l) (ui-layout-by l) ph) 2))))
         (max-chars (max 1 (floor (- pw 16) cw))))
    ;; page shadow, sheet, outline — the character-sheet look
    (amiga.gfx:set-a-pen rp 0)
    (amiga.gfx:rect-fill rp (+ px 2) (+ py 2) (+ px pw 2) (+ py ph 2))
    (amiga.gfx:set-a-pen rp 1)
    (amiga.gfx:rect-fill rp px py (+ px pw) (+ py ph))
    (amiga.gfx:set-a-pen rp 0)
    (%chrome-rect rp px py (+ px pw) (+ py ph))
    (let ((y (+ py 4)))
      (dolist (line lines)
        (let ((text (menu-line-text line)))
          (%put-microfont-line rp lines-cache
                               (if (> (length text) max-chars)
                                   (subseq text 0 max-chars)
                                   text)
                               (+ px 8) y)
          (%register-line-hotspots line (+ px 8) y cw lh
                                   (1+ px) (+ px pw -1)))
        (incf y lh)))
    (amiga.gfx:set-a-pen rp 1)))

;;; ---------------------------------------------------------------------
;;; The automap page's geometry.  The policy — show the map whole while
;;; it fits legibly, else keep a legible cell and window it — is
;;; MAP-PAGE-WINDOW's, in map.lisp where the host suite can check it.
;;; What belongs here is only the page's pixel budget: the legend's
;;; column, the gutter carrying the scrollbar, and the footer.
;;;
;;; The legend's column is reserved BEFORE the cell size is picked.  It
;;; used to be whatever the map happened to leave over, which held only
;;; while the page was height-bound; the moment the height stops
;;; deciding the cell size, an unreserved column is one the map grows
;;; straight over.

(defconstant +map-legend-cells+ 12
  "Small-face character cells the automap page keeps beside the map for
the found-locations legend.")

(defconstant +map-legend-gutter+ 11
  "Pixels between the automap and its legend column: the vertical
scrollbar's 3, with a clear margin either side.")

(defun %map-page-geometry (l map)
  "How the automap page draws MAP on layout L: values (CELL VW VH), the
page's own pixel budget put to MAP-PAGE-WINDOW.  Cells under
+MICROFONT-SMALL-ADVANCE+ carry no feature glyph (%MAP-GLYPH), so that
is the floor the policy works to."
  (map-page-window map
                   (- (ui-layout-right l) (ui-layout-bx l)
                      +map-legend-gutter+
                      (* +map-legend-cells+ +microfont-small-advance+))
                   (- (ui-layout-bottom l) (ui-layout-by l)
                      (* 2 +microfont-line-height+) 6)
                   +microfont-small-advance+))

(defun %map-scroll (game l top char)
  "The automap's scroll offset after key CHAR (u/d), or NIL when the
page shows the map whole or CHAR is no scroll key.  TOP NIL is a page
still centered on the party: the window it is showing right now is
where its first scroll starts from."
  (let ((map (game-map game)))
    (multiple-value-bind (cell vw vh) (%map-page-geometry l map)
      (declare (ignore cell))
      (map-page-scroll (or top
                           (nth-value 1 (map-viewport map (game-x game)
                                                      (game-y game) vw vh)))
                       char (dungeon-map-height map) vh))))

(defun %amiga-draw-map-legend (rp entries lx lw top bottom)
  "The found-locations legend beside the full map: MARKER NAME per
entry in the small face, black on the grey page, the marker amber to
match its map cell.  ENTRIES is the MAP-LEGEND-ENTRIES list.  A name
wider than the column wraps onto continuation lines aligned past the
marker — the full name always shows instead of losing its tail to the
column edge.  Draws the entries that fit whole between TOP and BOTTOM
in LW pixels."
  (let* ((y top)
         (marker-w (* 2 +microfont-small-advance+))
         (name-chars (max 1 (floor (- lw marker-w)
                                   +microfont-small-advance+))))
    (flet ((line (text fg x)
             (%put-microfont-small-text rp text x y fg)))
      (when (<= (+ y +microfont-line-height+) bottom)
        (line "Found:" 0 lx)
        (incf y (+ +microfont-line-height+ 2)))
      (dolist (e entries)
        (destructuring-bind (marker x0 y0 title) e
          (declare (ignore x0 y0))
          (let ((rows (wrap-text (title-case title) name-chars)))
            (when (> (+ y (* (length rows) +microfont-line-height+))
                     bottom)
              (return))
            (line (string marker) 3 lx)
            (dolist (row rows)
              (line row 0 (+ lx marker-w))
              (incf y +microfont-line-height+))))))))

(defun %amiga-draw-map-page (rp game l full &optional top)
  "Full map mode ('m'): the automap over the whole inner area — black
ink on the grey page — at the cell size and window %MAP-PAGE-GEOMETRY
allows.  A map too tall for the page shows a window of itself with a
scrollbar down its right edge: TOP is the scroll offset in cells (u/d
move it — see %MAP-SCROLL), and NIL keeps the window centered on the
party.  The reserved column right of the map carries the legend of
found locations; the two-line footer carries what the play page has
no room for: the zone title, the party position — plus the facing
while a compass burns — and the game clock (keys are on the help
page).  Every glyph on the page — cells, legend and footer — is the
microfont's compact small face, so the map reads as one plate instead
of two type sizes and the bold face's 8px advance never crowds the
legend column."
  (let* ((bx (ui-layout-bx l))
         (by (ui-layout-by l))
         (right (ui-layout-right l))
         (lh +microfont-line-height+)
         (map (game-map game))
         (mw (dungeon-map-width map))
         (mh (dungeon-map-height map))
         (legend (map-legend-entries map (game-knowledge game)
                                     :full full)))
    ;; clear the whole inner area (the play panes underneath) to the
    ;; grey page the map draws on.  The play chrome's white view frame
    ;; sits one pixel left of BX and above BY, and the message page's
    ;; black outline/drop shadow bleed to BY-1 and RIGHT+2 (see
    ;; %CHROME-FRAMES); reach those edge pixels too, or they survive as
    ;; a stray white L at the top-left and black lines along the top and
    ;; right.  The extra margin stays well inside the ornate ring (pad
    ;; >= 10 when it is drawn).
    (amiga.gfx:set-a-pen rp 2)
    (amiga.gfx:rect-fill rp (1- bx) (1- by) (+ right 2)
                         (ui-layout-bottom l))
    (multiple-value-bind (cell vw vh) (%map-page-geometry l map)
      (multiple-value-bind (x0 y0 w h)
          (map-viewport map (game-x game) (game-y game) vw vh)
        ;; a scrolled page shows the window the player put it at; an
        ;; unscrolled one stays centered on the party
        (when top
          (setf y0 (max 0 (min top (- mh h)))))
        (%amiga-draw-map-region rp game bx by cell x0 y0 w h full legend)
        ;; the scrollbar rides the gutter between map and legend, and
        ;; only while there is map above or below the window to reach
        (when (< h mh)
          (%amiga-draw-scrollbar rp (+ bx (* w cell) 4) by 3 (* h cell)
                                 y0 (+ y0 h) mh))
        ;; the legend, in its reserved column right of the gutter
        (let* ((lx (+ bx (* w cell) +map-legend-gutter+))
               (lw (- right lx)))
          (when (and legend (>= lw (* 7 +microfont-small-advance+)))
            (%amiga-draw-map-legend rp legend lx lw by (+ by (* h cell)))))))
    (let* ((y1 (- (ui-layout-bottom l) (* 2 lh)))  ; upper footer line
           (y2 (- (ui-layout-bottom l) lh))        ; lower footer line
           (clock (clock-line game))
           (clock-w (microfont-small-text-width clock))
           (place (format nil "~A  (~D,~D)~@[ ~A~]"
                          (title-case (map-title map))
                          (game-x game) (game-y game)
                          (when (compass-active-p game)
                            (dir-keyword (game-facing game)))))
           (place-max (max 0 (floor (- right bx clock-w
                                       +microfont-small-advance+)
                                    +microfont-small-advance+))))
      (%put-microfont-small-text rp (if (> (length place) place-max)
                                        (subseq place 0 place-max)
                                        place)
                                 bx y1)
      (%put-microfont-small-text rp clock (- right clock-w) y1)
      (%put-microfont-small-text rp (format nil "~Dx~D map~@[  FULL~]"
                                            mw mh full)
                                 bx y2)
      ;; the day-band, under the clock: "It's Morning." etc.
      (let ((band (time-of-day-line game)))
        (%put-microfont-small-text
         rp band (- right (microfont-small-text-width band)) y2))
      (amiga.gfx:set-a-pen rp 1))))

;;; The Game menu.  Item numbers (the bar counts as an item) are decoded
;;; from the MENUPICK code below: Save 0, Load 1, Quit 3.
(defparameter *menu-entries*
  (list (list amiga.gadtools:+nm-title+ "Game")
        (list amiga.gadtools:+nm-item+ "Save" :commkey "S")
        (list amiga.gadtools:+nm-item+ "Load" :commkey "L")
        :bar
        (list amiga.gadtools:+nm-item+ "Quit" :commkey "Q")))

(defconstant +menu-null+ #xFFFF)

(defun %menu-item-number (code)
  "The item number packed into a MENUPICK code (FULLMENUNUM layout:
menu bits 0-4, item bits 5-10, sub-item bits 11-15)."
  (logand (ash code -5) #x3F))

;;; ---------------------------------------------------------------------
;;; Display: Workbench window or own custom screen

(defun %set-pen-rgb (vp pen rgb)
  "Load RGB — an (R G B) of 0-255 components, as READ-ILBM's CMAPs
carry them — into color register PEN, scaled to SET-RGB4 nibbles."
  (amiga.gfx:set-rgb4 vp pen
                      (floor (first rgb) 17)
                      (floor (second rgb) 17)
                      (floor (third rgb) 17)))

(defun %game-screen-palette (scr)
  "Assert every engine-owned color register on the custom screen: the
transparent/UI pens 0-4, the mouse pointer's 17-19 (sprite 0 — unset
they render the pointer invisibly black) and the shared figure core
24-31.  The pack's own pens start black until %APPLY-PACK-PALETTE
fills them.

These are the pens no tile pack may move, so travelling art — monster
sprites, hero portraits, effect icons — reads the same in every zone;
see src/palette.lisp for the contract and why it is split this way."
  (let ((vp (amiga.intuition:screen-viewport scr))
        (depth (display-profile-screen-depth *display-profile*)))
    (loop for pen from 0 below (ash 1 depth)
          for fixed = (fixed-pen-color pen)
          do (%set-pen-rgb vp pen (or fixed '(0 0 0))))
    ;; The mouse-pointer sprite reads registers 17-19 whatever the
    ;; screen's depth, so they are set even on the 16-color profile
    ;; where they are past the end of the loop above — left unset the
    ;; pointer renders invisibly black.
    (loop for (pen rgb) in *pointer-pens*
          do (%set-pen-rgb vp pen rgb))))

(defun %apply-pack-palette (scr palette)
  "Load the tile pack's own colors from PALETTE, a CMAP vector of
(R G B) lists (see READ-ILBM).

Only PACK-PENS are taken — the sky, the ground and the art pens.  Every
other entry in the pack's CMAP is ignored on purpose: those registers
belong to the engine and were asserted once by %GAME-SCREEN-PALETTE, so
a pack whose palette.iff is stale, hand-edited or simply wrong cannot
recolor the UI text, the mouse pointer, or the monsters standing in
front of its walls.  The generator writes the fixed entries into a
pack's CMAP anyway (ART-PACK-PALETTE), because an artist opening the
file should see the true screen — but the file is not what decides."
  (when palette
    (let ((vp (amiga.intuition:screen-viewport scr)))
      (dolist (pen (pack-pens (display-profile-screen-depth
                               *display-profile*)))
        (let ((rgb (and (< pen (length palette)) (aref palette pen))))
          (when rgb (%set-pen-rgb vp pen rgb)))))))

(defun %apply-daytime-palette (scr game)
  "Tint the sky (+ART-PEN-SKY+) and ground (+ART-PEN-GROUND+) colour
registers for GAME's current day-band — the outdoor day/night effect
\(see SKY-COLOR-FOR).  A palette-only change: two SET-RGB4 calls repaint
the whole sky, no redraw of the view.

Indoor zones (ZONE :DARK ...) are skipped — there is no sky underground,
so those pens keep the colour %APPLY-PACK-PALETTE loaded from the pack.
Runs AFTER %APPLY-PACK-PALETTE (which would otherwise reset pens 5/6 to
the pack's noon colours) and again whenever the band turns."
  (let ((map (game-map game)))
    (unless (dungeon-map-dark map)
      (let ((vp (amiga.intuition:screen-viewport scr))
            (band (game-time-of-day game)))
        (%set-pen-rgb vp +art-pen-sky+
                      (sky-color-for (dungeon-map-sky map) band))
        (%set-pen-rgb vp +art-pen-ground+
                      (ground-color-for (dungeon-map-ground map) band))))))

(defun %call-with-game-window (display fn)
  "Open DISPLAY per the active *DISPLAY-PROFILE* and call FN with the
screen and window.  :WINDOW — a window on the public (Workbench)
screen, the development default.  :SCREEN — an own custom screen (the
profile's nominal geometry, chosen RTG-aware through
AMIGA.GFX:BEST-MODE-ID) covered by a borderless backdrop window.
Both the mode asked for and the screen actually opened go to the debug
log, so a display that came out unexpected can be told from a layout
that did."
  (let ((p *display-profile*))
    (ecase display
      (:window
       (amiga.intuition:with-pub-screen (scr)
         (amiga.intuition:with-window
             (win :title "Lambda's Tale"
                  :left 0 :top 0
                  :width (display-profile-win-width p)
                  :height (display-profile-win-height p)
                  :idcmp +game-idcmp+)
           (funcall fn scr win))))
      (:screen
       ;; Ask the database for a mode that fits the tallest display the
       ;; profile would accept (SCREEN-ASK-HEIGHT — NOT the layout's
       ;; height, which on a machine with an RTG board would land the
       ;; game on the graphics card; see the note there), then ask that
       ;; mode how tall its display really is and open the screen at
       ;; that height (SCREEN-HEIGHT-FOR).  A PAL machine answers 256
       ;; and gets its native display; an NTSC one answers 200 and
       ;; nothing moves.  The window below stays the layout's size
       ;; either way, so the game is laid out identically on both and
       ;; only the black band under it differs.  All three numbers go to
       ;; the debug log — a display that came out unexpected is then a
       ;; fact in the log rather than a guess.
       (let* ((ask-h (screen-ask-height p))
              (mode (amiga.gfx:best-mode-id
                     :width (display-profile-screen-width p)
                     :height ask-h
                     :depth (display-profile-screen-depth p)))
              (display-h (and mode
                              (amiga.intuition:display-mode-height mode)))
              (screen-h (screen-height-for p display-h)))
         (dlog "screen: asked ~Dx~Dx~D -> mode $~:[????????~;~:*~8,'0X~], ~
display ~:[unknown~;~:*~D~] rows, opening ~D"
               (display-profile-screen-width p)
               ask-h
               (display-profile-screen-depth p)
               mode display-h screen-h)
         (amiga.intuition:with-screen
             (scr :width (display-profile-screen-width p)
                  :height screen-h
                  :depth (display-profile-screen-depth p)
                  :title "Lambda's Tale"
                  :mode-id mode)
           (dlog "screen: opened ~Dx~D"
                 (amiga.intuition:screen-width scr)
                 (amiga.intuition:screen-height scr))
           (%game-screen-palette scr)
           ;; :TITLE NIL, not merely omitted: on a backdrop window any
           ;; WA_Title — including OPEN-WINDOW's default — still costs
           ;; a title bar (border-top), and the screen carries one.
           ;; The window is the LAYOUT's height, not the screen's: on a
           ;; display with rows to spare the game sits in the top of
           ;; the screen and the rest stays screen background.
           (amiga.intuition:with-window
               (win :title nil
                    :left 0 :top 0
                    :width (amiga.intuition:screen-width scr)
                    :height (min (display-profile-screen-height p)
                                 (amiga.intuition:screen-height scr))
                    :screen scr
                    :flags (logior amiga.intuition:+wflg-borderless+
                                   amiga.intuition:+wflg-backdrop+
                                   amiga.intuition:+wflg-activate+)
                    :idcmp +game-idcmp+)
             ;; the game owns the whole display: put the OS screen bar
             ;; behind the backdrop window (Bard's Tale has no title
             ;; bar).  This must run AFTER the window is open —
             ;; ShowTitle rearranges the layers of the backdrop windows
             ;; that exist at call time, and on RTG-promoted screens
             ;; (Picasso96 BestModeIDA modes) a backdrop window opened
             ;; after the call still sat behind the bar layer: the
             ;; title bar stayed visible in play even with
             ;; SA_ShowTitle FALSE at OpenScreen.
             (amiga.intuition:show-title scr nil)
             (funcall fn scr win))))))))

;;; ---------------------------------------------------------------------
;;; The game proper

(defun play-amiga (map-file
                   &key (display :screen) (profile *display-profile*)
                     gfx-dir draw-depth draw-flanks)
  "Interactive walkabout on MAP-FILE.  Loads the campaign.lisp next to
the map file (classes, monsters, items, party) when present — a
designer's own world directory brings its own campaign; the engine has
no default world.
DISPLAY is :screen (the default: an own custom screen, RTG-aware) or
:window (a development view on the Workbench screen).  PROFILE is a
DISPLAY-PROFILE or its name — :lores or :hires — and sets the screen
geometry, viewport and default tile pack.  A zone may declare its own
pack with (ZONE :GFX DIR) — see ZONE-GFX-DIR — swapped in when travel
enters it; GFX-DIR here overrides both (precedence: GFX-DIR, then the
zone's :GFX, then the profile's pack).  A pack is a directory of
wall-piece ILBMs plus the optional floor.iff / ceiling.iff /
palette.iff (see PRINT-TILE-MANIFEST for the contract, which depends
on the profile); the pack's colors show on the custom screen — a
Workbench window keeps the Workbench palette.
DRAW-DEPTH is the speed knob for slower machines: how many of the
+VIEW-DEPTH+ (4) distance levels the first-person view draws, 1-4,
defaulting to the profile's own.  Each level dropped is up to three
fewer wall blits per frame and ten fewer piece files at load time; the
corridor then ends that much nearer, in the backdrop.  It changes only
what is DRAWN — the automap still records everything the party could
see, and darkness still overrides it downward.
DRAW-FLANKS is the matching sideways knob: how many cells to each
open side the view draws of a facing row of houses (0 to +VIEW-FLANKS+
\(8), defaulting to the profile's own — 1, the classic immediate
neighbor).  Raising it fills a distant street's horizon with the
houses that stand there, one more blit per drawn cell per frame;
the flank pieces are reused, so it costs no load time and no pack
files.  0 draws no neighbor houses at all — the original Bard's
Tale's lone far house.  Like DRAW-DEPTH it changes only what is
drawn, never what the automap records.
Keys: W forward, S back-step, A/D turn, M map mode (M/Esc leaves it,
F toggles the debug full view there), H or ? the help page (the key
reference, in the microfont; taller than the page it scrolls on U/D
or the scrollbar — H/Esc leaves), 1-7 open a party member's character sheet
(1-7 switch heroes there, E opens the hero's pack page — digits
toggle an item on/off, class-unfit items are marked (u), P hands
an item to another party member (1-9 the item, then 1-7 who receives
it) — and Esc leaves), C cast a spell (pick
caster/spell/target by number, Esc backs out), Q/Esc quit; in combat
every round opens on the party-level choice — A attack the enemy, F
flee (the whole party or nobody; only there) — then the round-orders
page asks one hero at a time — A attack, D defend,
C cast, P play, U use, Esc undo the previous pick — and then reviews
the orders under \"Is this OK?\", where Y fights the round and N asks
again from the first hero; +/- set the transcript speed, each round
message lingering COMBAT-MESSAGE-DELAY seconds, and a won fight shows
the campaign's treasure picture for *VICTORY-LINGER* seconds; in a
location (shop) 1-9 choose,
S/B switch sell/buy, Esc back/leave — the location menu, the
character sheet and the round orders take over the message area, with
the location's :image / the hero's portrait / the enemy's portrait in
the view column when the campaign ships one.  A menu list longer than a page (a deep shop stock, a full
pack on the sheet) scrolls: U/D move the window, digits pick within
it, and the scrollbar on the page's right edge clicks — above the
thumb a window up, below it down.  Shift-S / Shift-L (and the
menu strip's Save/Load, right mouse button) open the save-slot
picker: 1-9 pick a slot, N names a new save (saves/NAME.sav), Esc
cancels; Quit sits in the menu strip too.
Everything key-driven clicks too: the view walks (left/right quarters
turn, the middle steps forward, its bottom band back), a roster row
opens that character sheet, a menu's numbered rows pick and its
bracket hints ([S]ell, [Esc] back) act as their keys, and the
map/help/sheet pages close on a click outside a target — see
*HOTSPOTS*."
  (load-campaign map-file)
  (with-display-profile (profile)
   ;; The overrides bind INSIDE the profile binding: WITH-DISPLAY-PROFILE
   ;; has just set *GFX-DIR* and *DRAW-DEPTH* to the profile's defaults,
   ;; and these arguments override those defaults.
   (let* ((*gfx-dir* (or gfx-dir *gfx-dir*))
          (*draw-depth* (or draw-depth *draw-depth*))
          (*draw-flanks* (or draw-flanks *draw-flanks*))
         (map (load-map-file map-file))
         (game nil)
         (log nil)
         (mode :play)       ; :play, :map (full map view), :sheet or :help
         (full nil)         ; omniscient map (debug), map mode only
         (map-top nil)      ; map mode: the scrolled window's top cell
                            ; row, or NIL while it follows the party
         (sheet-hero 0)     ; party index shown in :sheet mode
         (sheet-top 0)      ; current sheet page's scroll offset (u/d)
         (magic nil)        ; MAGIC-VIEW while the spells/songs page is up
         (ordering nil)     ; sheet: picking the hero's new slot (o)
         (changing nil)     ; sheet: picking the hero's new art (c)
         (sheet-notes nil)  ; the just-taken rise's messages, up as a
                            ; page of their own (see LEVEL-NOTES-LINES)
         (equipv nil)       ; EQUIP-VIEW while the pack page is open
         (tradev nil)       ; TRADE-VIEW while the trade page is open
         (help-prior-mode :play) ; mode to return to when help closes
         (help-top 0)       ; help page: the scrolled window's top line
         (locv nil)         ; the location's view (MAKE-LOCATION-VIEW)
                            ; while inside one; NIL for stateless menus
         (castv nil)        ; CAST-VIEW while the cast menu is open
         (usev nil)         ; USE-VIEW while the use menu is open
         (singv nil)        ; SING-VIEW while the sing menu is open
         (savem nil)        ; SAVE-MENU while the save/load picker is open
         (saves-prior-mode :play) ; mode to return to when the picker closes
         (zone-dirty nil)   ; party traveled: the chrome needs a repaint
         (ordersv nil)      ; COMBAT-ORDERS while a round is picked
         (pacing nil)       ; a combat round is running: pace messages
         (round-base nil)   ; log length when that round began — its
                            ; transcript draws on a page of its own
         (pace-fn nil)      ; draws one paced transcript beat (set once
                            ; the window exists)
         (idle-base nil)    ; internal-real-time the idle clock last
                            ; consumed, or NIL when not standing idle
                            ; (see the INTUITICKS handler's living clock)
         (anim-ticks 0)     ; INTUITICKS since the last animation step
                            ; (see +ANIM-TICKS-PER-STEP+)
         (quitting nil)     ; the quit confirmation is up (see REQUEST-QUIT)
         (over nil))
    (dlog "play-amiga ~A display ~S profile ~S draw-depth ~D draw-flanks ~D"
          map-file display (display-profile-name *display-profile*)
          (%draw-depth) (%draw-flanks))
    (labels ((wire (g)
               (setf log (attach-message-log g))
               (attach-sounds g)
               (setf locv (make-location-view g))
               (setf castv nil)
               (setf usev nil)
               (setf singv nil)
               (setf equipv nil)
               (setf tradev nil)
               (setf savem nil)
               (setf ordersv nil)
               ;; pace the combat transcript: linger on each message a
               ;; round says (the log handler above already caught it)
               (on-event g :message
                         (lambda (gm text) (declare (ignore gm text))
                           (when (and pacing pace-fn
                                      (plusp (combat-message-delay)))
                             (funcall pace-fn))))
               (on-event g :enter-location
                         (lambda (gm loc) (declare (ignore loc))
                           (setf locv (make-location-view gm))))
               (on-event g :leave-location
                         (lambda (gm loc) (declare (ignore gm loc))
                           (setf locv nil)))
               (on-event g :enter-zone
                         (lambda (gm map) (declare (ignore gm map))
                           (setf zone-dirty t)))
               (on-event g :combat-start
                         (lambda (gm monsters)
                           (declare (ignore gm monsters))
                           (setf ordersv (make-combat-orders))))
               (on-event g :combat-end
                         (lambda (gm result)
                           (declare (ignore gm result))
                           (setf ordersv nil)))
               (on-event g :game-won
                         (lambda (gm) (declare (ignore gm))
                           (setf over :won)
                           (log-message log "You win!  Press Q.")))
               (on-event g :party-defeated
                         (lambda (gm) (declare (ignore gm))
                           (setf over :lost)
                           (log-message log "Game over.  Press Q.")))
               g))
      (dlog-timed ("new-game + specials")
        (setf game (wire (new-game map
                                   :party (when (fboundp 'default-party)
                                            (funcall 'default-party)))))
        (trigger-special game))
      (%with-game-font
       (lambda (font)
         (%call-with-game-window
          display
          (lambda (scr win)
            ;; (%dlog-elapsed-ms 0) = ms since clamiga launch: the
            ;; internal-time epoch is anchored at process start.
            (dlog "display open [launch+~D ms]" (%dlog-elapsed-ms 0))
            (amiga.gadtools:with-visual-info (vi scr)
              (amiga.gadtools:with-menus (menu *menu-entries* vi win)
                (let* ((rp (%game-rastport win font))
                       (l (%amiga-layout win rp))
                       (fp-back (%alloc-fp-backbuffer rp l))
                                        ; offscreen frame compose (view
                                        ; tears without it; NIL = draw
                                        ; direct when RAM is that tight)
                       (walls nil)      ; loaded piece bitmaps
                       (walls-dir nil)  ; the pack they came from
                       (walls-pal nil)  ; ... and its CMAP palette
                       (pack-cache '()) ; inactive packs, MRU first
                       (icons (make-hash-table :test #'equal))
                                        ; image cache: effect icons,
                                        ; location pictures, portraits
                       (log-lines (make-hash-table :test #'equal)))
                                        ; rendered log-line bitmap cache
               (labels ((effective-gfx-dir ()
                          ;; precedence: the explicit :GFX-DIR argument,
                          ;; then the zone's (ZONE :GFX ...), then the
                          ;; profile's pack (bound into *GFX-DIR*)
                          (or gfx-dir (zone-gfx-dir game) *gfx-dir*))
                        (ensure-walls ()
                          ;; (re)load the wall bitmaps when the wanted
                          ;; pack changed — first draw, zone travel
                          ;; (:GFX zones), load-game.  Pack colors only
                          ;; on our own screen — a Workbench window has
                          ;; no say over the Workbench palette.  The
                          ;; load reads a directory of ILBMs — seconds
                          ;; at 14MHz — so the busy pointer is up, and
                          ;; the pack we are leaving goes to the cache
                          ;; (*GFX-CACHE-PACKS*) rather than the bin,
                          ;; which makes coming back free.
                          (let ((want (effective-gfx-dir)))
                            (unless (equal want walls-dir)
                              (%call-with-busy-pointer win
                               (lambda ()
                                 ;; *GFX-DIR* is the wanted pack for the
                                 ;; whole swap: the loader resolves the
                                 ;; piece files through it, and so does
                                 ;; the pack's optional pointer.iff.
                                 (let ((*gfx-dir* want))
                                   (when walls-dir
                                     (setf pack-cache
                                           (%pack-cache-put pack-cache
                                                            walls-dir
                                                            walls walls-pal)))
                                   (multiple-value-bind (w pal rest)
                                       (%pack-cache-take pack-cache want)
                                     (setf pack-cache rest)
                                     (if w
                                         (progn
                                           (dlog "wall pack ~A from cache" want)
                                           (setf walls w walls-pal pal))
                                         (multiple-value-setq (walls walls-pal)
                                           (%load-wall-assets rp log))))
                                   (setf walls-dir want)
                                   (setf pack-cache
                                         (%pack-cache-trim
                                          pack-cache #'%free-wall-assets
                                          (when (eq *gfx-cache-packs* :auto)
                                            (%free-memory))))
                                   (when (eq display :screen)
                                     (%apply-pack-palette scr walls-pal)
                                     ;; ...then override the sky/ground
                                     ;; pens for the current hour
                                     (%apply-daytime-palette scr game))
                                   ;; the pack may carry its own
                                   ;; pointer.iff; re-showing also
                                   ;; re-latches the sprite colors
                                   ;; after the palette change (RTG)
                                   (%ensure-standard-pointer
                                    scr win display)))))))
                        (clear-inner ()
                          ;; Grey-wipe the content area (a bit beyond
                          ;; it, to catch the frames and shadows) when
                          ;; the map/sheet page was underneath.
                          (amiga.gfx:set-a-pen rp 2)
                          (amiga.gfx:rect-fill rp
                                               (- (ui-layout-bx l) 2)
                                               (- (ui-layout-by l) 2)
                                               (+ (ui-layout-right l) 2)
                                               (ui-layout-bottom l))
                          (amiga.gfx:set-a-pen rp 1))
                        (fresh-play (&optional (target-mode :play))
                          ;; back to the play page (or, when a picker was
                          ;; opened over the map/sheet view via the menu
                          ;; strip, back to that view): chrome + panes
                          (setf mode target-mode)
                          (clear-inner)
                          (%chrome-frames rp game l)
                          (redraw))
                        (leave-map ()
                          (fresh-play))
                        (open-sheet (i)
                          ;; '1'-'7' from play: show that roster slot if
                          ;; it holds a hero, else stay put — the
                          ;; carousel starts over on its stat page
                          (when (nth i (game-party game))
                            (setf sheet-hero i
                                  sheet-top 0
                                  magic nil
                                  equipv nil
                                  tradev nil
                                  ordering nil
                                  changing nil
                                  sheet-notes nil
                                  mode :sheet)
                            (redraw)))
                        (sheet-next ()
                          ;; the carousel's page turn off the pack
                          ;; page: to the spells/songs page when the
                          ;; hero has one, else back to the stat block
                          (let ((hero (nth sheet-hero
                                           (game-party game))))
                            (setf equipv nil
                                  magic (when (and hero
                                                   (hero-magic-p hero))
                                          (make-magic-view hero))
                                  sheet-top 0)))
                        (magic-page-act (key)
                          ;; the spells/songs page: the shared model
                          ;; eats the keys (a digit opens that entry's
                          ;; card, c casts / p plays from the card,
                          ;; u/d scroll, n turns the carousel, Esc
                          ;; backs out to the stat block)
                          (let ((r (magic-act game magic key)))
                            (cond ((member r '(:next :cancelled))
                                   ;; the carousel's last page rounds
                                   ;; back to the stat block
                                   (setf magic nil sheet-top 0)
                                   (redraw))
                                  ;; a tune played or a spell resolved
                                  ;; — leave the sheet so the log shows
                                  ;; what happened
                                  ((eq r :done)
                                   (setf magic nil sheet-top 0)
                                   (leave-sheet))
                                  ;; the spell still wants a target:
                                  ;; hand over to the cast menu, which
                                  ;; lives outside the sheet
                                  ((and (consp r) (eq (first r) :cast))
                                   (setf magic nil sheet-top 0)
                                   (leave-sheet)
                                   (setf castv (second r))
                                   (redraw))
                                  (t (redraw)))))
                        (leave-sheet ()
                          ;; the sheet lives in the panes (takeover +
                          ;; portrait) — no chrome to repair
                          (setf magic nil)
                          (setf equipv nil)
                          (setf tradev nil)
                          (setf ordering nil)
                          (setf changing nil)
                          (setf sheet-notes nil)
                          (setf mode :play)
                          (redraw))
                        (open-help ()
                          ;; 'h'/'?' from play or map mode: remember
                          ;; where to return; the reference opens at
                          ;; its head every time
                          (setf help-prior-mode mode
                                help-top 0
                                mode :help)
                          (redraw))
                        (leave-help ()
                          (fresh-play help-prior-mode))
                        (menus-idle-p ()
                          ;; no menu model — and no confirmation — is
                          ;; eating the keys
                          (not (or quitting
                                   savem castv usev singv equipv tradev
                                   (game-location game))))
                        (%shop-picking-p ()
                          ;; the shop is asking who shops: digits mean
                          ;; the roster, so its rows may click
                          (let ((loc (game-location game)))
                            (and loc
                                 (eq (location-kind loc) :shop)
                                 locv
                                 (null (shop-view-hero locv)))))
                        (idle-clock ()
                          ;; one heartbeat of the living-world clock: drip
                          ;; game time forward while the party stands idle
                          ;; in free exploration (see time.lisp).  IDLE-BASE
                          ;; resets whenever we are NOT idling — combat, a
                          ;; location, a modal picker, the map/help/sheet
                          ;; pages — so a paused stretch never pours in on
                          ;; the next tick.
                          (let ((now (get-internal-real-time)))
                            (cond
                              ((not (and *idle-clock-rate*
                                         (eq mode :play)
                                         (not (game-combat game))
                                         (menus-idle-p)))
                               (setf idle-base nil))
                              ((null idle-base)
                               (setf idle-base now))
                              (t
                               (let ((mins (idle-minutes-elapsed
                                            (- now idle-base))))
                                 (when (plusp mins)
                                   ;; consume only the whole-minute part;
                                   ;; the sub-minute remainder carries on
                                   (incf idle-base (idle-minutes-cost mins))
                                   (let ((band (game-time-of-day game))
                                         (effects (length
                                                   (game-effects game))))
                                     (advance-time game mins)
                                     ;; repaint only when the party would
                                     ;; SEE it: the living world found the
                                     ;; loitering party (the wandering
                                     ;; vigil, see MAYBE-IDLE-ENCOUNTER —
                                     ;; the redraw opens the combat page),
                                     ;; the sky re-tints on a band turn,
                                     ;; or a worn-off effect (or sunrise/
                                     ;; sunset) has just logged a line
                                     (when (or (maybe-idle-encounter
                                                game mins)
                                               (not (eq band
                                                        (game-time-of-day
                                                         game)))
                                               (/= effects
                                                   (length (game-effects
                                                            game))))
                                       (redraw)))))))))
                        (redraw ()
                          ;; every redraw re-registers what animates on
                          ;; the page it paints — a page with no
                          ;; picture or band (map, help, an overlay
                          ;; menu) thereby stops the anim stepper
                          (setf *anim-blits* '())
                          ;; travel switched zones: swap in the zone's
                          ;; tile pack (and sound pack) and repaint the
                          ;; chrome first (the plaque carries the title)
                          (when zone-dirty
                            (setf zone-dirty nil)
                            (ensure-walls)
                            (amiga-sound-open (zone-sfx-dir game))
                            (clear-inner)
                            (%chrome-frames rp game l))
                          ;; re-tint the sky/ground for the hour: an
                          ;; action may have turned the day-band without
                          ;; a zone change, and it costs two SET-RGB4
                          (when (eq display :screen)
                            (%apply-daytime-palette scr game))
                          ;; the click targets are rebuilt with the
                          ;; frame: a full-page catch-all (leave) first
                          ;; where the mode has one, the renderers'
                          ;; specific targets on top of it
                          (setf *hotspots* '())
                          ;; scroll geometry too: the generators below
                          ;; refill it, and a page without a windowed
                          ;; list (the item card) must not inherit the
                          ;; last frame's scrollbar
                          (setf *menu-scroll* nil)
                          (when (member mode '(:map :help :sheet))
                            (%hotspot :esc
                                      (ui-layout-bx l) (ui-layout-by l)
                                      (ui-layout-right l)
                                      (ui-layout-bottom l)))
                          (cond
                            ((eq mode :map)
                             (%amiga-draw-map-page rp game l full map-top))
                            ((eq mode :help)
                             (%amiga-draw-help rp l help-top log-lines))
                            (t
                             ;; The view column: an overlay menu page
                             ;; (save picker, cast/use/sing), else the
                             ;; takeover's picture (the location's
                             ;; :image, the sheet hero's portrait, the
                             ;; enemy's portrait in combat) with the
                             ;; live first-person view as the fallback
                             ;; when there is no picture.
                             ;; every picker draws as one centered
                             ;; dialog over the log column too — see
                             ;; the page renderer
                             (cond (savem
                                    (%amiga-draw-page
                                     rp (save-menu-lines game savem) l
                                     log-lines))
                                   (castv
                                    (%amiga-draw-page
                                     rp (cast-lines game castv) l
                                     log-lines))
                                   (usev
                                    (%amiga-draw-page
                                     rp (use-lines game usev) l
                                     log-lines))
                                   (singv
                                    (%amiga-draw-page
                                     rp (sing-lines game singv) l
                                     log-lines))
                                   (t
                                    (let ((picture
                                            (cond ((eq mode :sheet)
                                                   (let ((hero (nth sheet-hero
                                                                    (game-party
                                                                     game))))
                                                     (when hero
                                                       (hero-image-path
                                                        game hero))))
                                                  ((game-location game)
                                                   (location-image-path
                                                    game))
                                                  ;; in a fight: the
                                                  ;; enemy the party
                                                  ;; faces
                                                  ((game-combat game)
                                                   (combat-image-path
                                                    game))
                                                  ;; facing a house door
                                                  ;; on the street: its
                                                  ;; facade
                                                  (t
                                                   (facing-location-image-path
                                                    game)))))
                                      (unless (%amiga-draw-picture
                                               rp icons picture l log)
                                        (%amiga-draw-fp rp game
                                                        (ui-layout-bx l)
                                                        (ui-layout-by l)
                                                        (ui-layout-fp-w l)
                                                        (ui-layout-fp-h l)
                                                        walls fp-back)))
                                    (%amiga-draw-band rp game l icons log)))
                             ;; click-to-walk zones on the view — only
                             ;; while W/A/S/D actually walk (no menu or
                             ;; location eating keys, not in combat)
                             (when (and (eq mode :play) (menus-idle-p)
                                        (not (game-combat game))
                                        (not over))
                               (%register-move-zones l))
                             ;; The message area: taken over by the
                             ;; character sheet, the round-orders page
                             ;; or the location's menu (log tail below
                             ;; the rule), else the log.  A dialog page
                             ;; covers it — nothing to draw under it
                             ;; (FRESH-PLAY repaints when the picker
                             ;; closes).
                             (cond ((or savem castv usev singv))
                                   ((eq mode :sheet)
                                    (%amiga-draw-takeover
                                     rp (cond (sheet-notes
                                               (level-notes-lines
                                                sheet-notes))
                                              (equipv
                                               (equip-lines game equipv))
                                              (tradev
                                               (trade-lines game tradev))
                                              (magic
                                               (magic-lines game magic))
                                              (t
                                               (hero-sheet-lines
                                                game sheet-hero
                                                sheet-top ordering
                                                changing)))
                                     log l log-lines))
                                   (ordersv
                                    ;; combat: the round orders, with
                                    ;; the transcript running on under
                                    ;; them
                                    (%amiga-draw-takeover
                                     rp (combat-orders-lines game ordersv)
                                     log l log-lines))
                                   ((game-location game)
                                    (%amiga-draw-takeover
                                     rp
                                     (location-lines
                                      game
                                      (or locv
                                          (setf locv
                                                (make-location-view game))))
                                     log l log-lines))
                                   (t
                                    (%amiga-draw-log rp log l log-lines)))
                             ;; roster rows click as their digits when
                             ;; digits mean the roster: sheet picks in
                             ;; free exploration, and the shop's bare
                             ;; who-is-shopping prompt (its page lists
                             ;; no rows of its own — see SHOP-LINES)
                             (%amiga-party rp game l
                                           (and (or (menus-idle-p)
                                                    (%shop-picking-p))
                                                (not (game-combat game))
                                                (not over)))))
                          ;; the quit confirmation floats over whatever
                          ;; page is up and owns the frame's click
                          ;; targets while it waits: the box's own rows
                          ;; on top of a page-wide catch-all that reads
                          ;; as "no" — a click beside the box backs out
                          ;; the way Esc does
                          (when quitting
                            ;; nothing animates behind a modal box —
                            ;; the anim stepper would blit the view's
                            ;; dirty rectangles straight over it
                            (setf *anim-blits* '()
                                  *hotspots* '())
                            (%hotspot #\n
                                      (ui-layout-bx l) (ui-layout-by l)
                                      (ui-layout-right l)
                                      (ui-layout-bottom l))
                            (%amiga-draw-confirm rp (quit-confirm-lines) l
                                                 log-lines)))
                        (%step (relative)
                          ;; Only a refused step speaks: the view (and
                          ;; the door cue) already show a door passage,
                          ;; and plain steps stay quiet so the log
                          ;; tracks events, not every footfall.
                          (case (move-party game relative)
                            (:blocked (say game "You bump into a wall."))))
                        (pace ()
                          ;; one combat-transcript beat: show the round's
                          ;; own page and the roster (hp/sp move as the
                          ;; round plays), then linger on it
                          (if round-base
                              (%amiga-draw-transcript rp log l round-base
                                                      log-lines)
                              (%amiga-draw-log rp log l log-lines))
                          (%amiga-party rp game l nil)
                          (sleep (combat-message-delay)))
                        (fight (thunk)
                          ;; run one round (or a flee attempt) with the
                          ;; paced transcript on a page of its own, then
                          ;; open fresh orders when combat goes on
                          (setf ordersv nil)
                          (setf round-base (log-length log))
                          (let ((result
                                  (unwind-protect
                                      (progn (setf pacing t)
                                             (funcall thunk))
                                    (setf pacing nil))))
                            (if (game-combat game)
                                (progn
                                  (setf ordersv (make-combat-orders))
                                  (redraw))
                                (progn
                                  ;; a won fight lingers on its spoils:
                                  ;; the campaign's treasure picture
                                  ;; over the victory transcript (xp,
                                  ;; gold, the found item), a beat to
                                  ;; read before play resumes
                                  (when (eq result :victory)
                                    (let ((picture (victory-image-path
                                                    game)))
                                      (when (and picture
                                                 (%amiga-draw-picture
                                                  rp icons picture l log))
                                        (%amiga-party rp game l nil)
                                        (sleep *victory-linger*))))
                                  ;; combat over: the enemy portrait
                                  ;; gives the view column back to the
                                  ;; walls, so repaint the chrome under
                                  ;; it too
                                  (fresh-play)))))
                        (open-cast (in-combat)
                          (if (some #'hero-caster-p (alive-heroes game))
                              (setf castv
                                    (make-cast-view :in-combat in-combat))
                              (log-message log "No one here can cast."))
                          (redraw))
                        (cast-menu-act (c)
                          (let ((key (if (eq c :esc) #\Escape c)))
                            (when (characterp key)
                              (case (cast-act game castv key)
                                ((:done :cancelled)
                                 (setf castv nil)
                                 (fresh-play)
                                 (return-from cast-menu-act nil)))))
                          (redraw)
                          nil)
                        (open-use ()
                          (if (some #'usable-items (alive-heroes game))
                              (setf usev (make-use-view))
                              (log-message log
                                           "No one carries anything to use."))
                          (redraw))
                        (use-menu-act (c)
                          (let ((key (if (eq c :esc) #\Escape c)))
                            (when (characterp key)
                              (case (use-act game usev key)
                                ((:done :cancelled)
                                 (setf usev nil)
                                 (fresh-play)
                                 (return-from use-menu-act nil)))))
                          (redraw)
                          nil)
                        (open-sing (in-combat)
                          (if (some #'hero-singer-p (alive-heroes game))
                              (setf singv
                                    (make-sing-view :in-combat in-combat))
                              (log-message log "No one here can play."))
                          (redraw))
                        (sing-menu-act (c)
                          (let ((key (if (eq c :esc) #\Escape c)))
                            (when (characterp key)
                              (case (sing-act game singv key)
                                ((:done :cancelled)
                                 (setf singv nil)
                                 (fresh-play)
                                 (return-from sing-menu-act nil)))))
                          (redraw)
                          nil)
                        (open-saves (menu-mode)
                          ;; S/L keys and the GadTools Save/Load items
                          ;; all land here; the picker draws over the
                          ;; view column like a shop page.  The menu
                          ;; strip can trigger this from :map/:sheet
                          ;; mode too, so remember where to return.
                          (setf saves-prior-mode mode)
                          (setf savem (make-save-menu menu-mode)
                                mode :play)
                          (clear-inner)
                          (%chrome-frames rp game l)
                          (redraw))
                        (saves-act (c)
                          (let* ((key (if (eq c :esc) #\Escape c))
                                 (r (when (characterp key)
                                      (save-menu-act game savem key))))
                            (cond ((eq r :closed)
                                   (setf savem nil)
                                   (fresh-play saves-prior-mode))
                                  ((and (consp r) (eq (first r) :save))
                                   ;; a save file the host side holds
                                   ;; open, a full disk — the log gets
                                   ;; the news, the session plays on
                                   (handler-case
                                       (progn
                                         (ensure-save-dir)
                                         (save-game game (second r))
                                         (log-message
                                          log
                                          (format nil "Saved ~A."
                                                  (second r))))
                                     (error (e)
                                       (log-message
                                        log
                                        (format nil "Save failed: ~A" e))))
                                   (setf savem nil)
                                   (fresh-play saves-prior-mode))
                                  ((and (consp r) (eq (first r) :load))
                                   (handler-case
                                       (progn
                                         (%call-with-busy-pointer win
                                          (lambda ()
                                            (setf game
                                                  (wire (load-game
                                                         (second r))))
                                            (setf over nil
                                                  mode :play
                                                  zone-dirty nil)
                                            ;; loading may land in a
                                            ;; zone with its own tile
                                            ;; pack — swap packs and
                                            ;; repaint the chrome (the
                                            ;; plaque carries the zone
                                            ;; name)
                                            (ensure-walls)
                                            (amiga-sound-open
                                             (zone-sfx-dir game))))
                                         (clear-inner)
                                         (%chrome-frames rp game l)
                                         (log-message log "Game loaded.")
                                         (redraw))
                                     (error (e)
                                       ;; WIRE (which closes the
                                       ;; picker) may not have run —
                                       ;; close it ourselves and play
                                       ;; on with whatever game we hold
                                       (log-message
                                        log
                                        (format nil "Load failed: ~A" e))
                                       (setf savem nil)
                                       (fresh-play saves-prior-mode))))
                                  (t (redraw))))
                          nil)
                        (request-quit ()
                          ;; Q, Esc and the menu strip's Quit ask before
                          ;; the session ends — on the game's own screen
                          ;; that tears the display down, and Esc is the
                          ;; key a player reaches for to back out of a
                          ;; page.  The endgame page asks for Q itself,
                          ;; so there it quits straight away.
                          (cond (over :quit)
                                (t (setf quitting t)
                                   (redraw)
                                   nil)))
                        (act (c)
                          "Handle key C; :quit means leave the event loop."
                          (dlog "key ~S mode ~S" c mode)
                          (cond (quitting
                                 ;; the confirmation eats every key until
                                 ;; it is answered
                                 (case (quit-confirm-act c)
                                   (:quit :quit)
                                   (:cancel (setf quitting nil)
                                            (fresh-play mode)
                                            nil)
                                   (t nil)))
                                ((eq (act-key c) :quit) (request-quit))
                                (t nil)))
                        (act-key (c)
                          "Route key C to the page that owns it; :QUIT
means the player asked to leave (ACT confirms it)."
                          (let ((lc (if (characterp c) (char-downcase c) c)))
                            (cond ((eq mode :map)
                                   (cond ((eql lc #\q) :quit)
                                         ((or (eql lc #\m) (eql c :esc))
                                          (leave-map)
                                          nil)
                                         ((eql lc #\f)
                                          (setf full (not full))
                                          (redraw)
                                          nil)
                                         ;; a map too tall for the page
                                         ;; windows it: u/d walk the
                                         ;; window, and the scrollbar's
                                         ;; trough clicks as the same
                                         ;; two keys
                                         ((member lc '(#\u #\d))
                                          (let ((next (%map-scroll
                                                       game l map-top lc)))
                                            (when next
                                              (setf map-top next)
                                              (redraw)))
                                          nil)
                                         ((or (eql lc #\h) (eql c #\?))
                                          (open-help)
                                          nil)))
                                  ((eq mode :help)
                                   (cond ((eql lc #\q) :quit)
                                         ((or (eql lc #\h) (eql c #\?)
                                              (eql c :esc))
                                          (leave-help)
                                          nil)
                                         ;; a reference taller than the
                                         ;; page windows it: u/d turn a
                                         ;; page, the scrollbar's trough
                                         ;; clicks as the same two keys
                                         ((member lc '(#\u #\d))
                                          (let ((next (%help-scroll
                                                       l help-top lc)))
                                            (when next
                                              (setf help-top next)
                                              (redraw)))
                                          nil)))
                                  ((eq mode :sheet)
                                   (cond (sheet-notes
                                          ;; the rise's page: any key
                                          ;; (or the NEXT row's click)
                                          ;; turns back to the sheet
                                          (setf sheet-notes nil)
                                          (redraw)
                                          nil)
                                         ((eql lc #\q) :quit)
                                         (equipv
                                          ;; the pack page: the shared
                                          ;; model eats the keys (digits
                                          ;; toggle or pick, P gives,
                                          ;; u/d scroll, Esc backs out
                                          ;; a page at a time and
                                          ;; finally to the sheet)
                                          (let ((key (if (eq c :esc)
                                                         #\Escape
                                                         c)))
                                            (when (characterp key)
                                              (case (equip-act game equipv
                                                               key)
                                                (:cancelled
                                                 (setf equipv nil))
                                                (:next
                                                 (sheet-next))))
                                            (redraw))
                                          nil)
                                         (tradev
                                          ;; the trade page: the shared
                                          ;; model eats the keys (a
                                          ;; digit picks who receives,
                                          ;; digits then type the sum,
                                          ;; Return trades, Esc backs
                                          ;; out a page at a time)
                                          (let ((key (if (eq c :esc)
                                                         #\Escape
                                                         c)))
                                            (when (characterp key)
                                              (case (trade-act game tradev
                                                               key)
                                                ((:done :cancelled)
                                                 (setf tradev nil))))
                                            (redraw))
                                          nil)
                                         (ordering
                                          ;; the marching-order pick: a
                                          ;; digit (key or roster click)
                                          ;; is the hero's new slot and
                                          ;; the sheet follows them
                                          ;; there; Esc cancels
                                          (cond ((eql c :esc)
                                                 (setf ordering nil)
                                                 (redraw))
                                                ((and (characterp c)
                                                      (digit-char-p c)
                                                      (<= 1
                                                          (digit-char-p c)
                                                          (length
                                                           (game-party
                                                            game))))
                                                 (when (move-hero
                                                        game sheet-hero
                                                        (1- (digit-char-p
                                                             c)))
                                                   (setf sheet-hero
                                                         (1- (digit-char-p
                                                              c))))
                                                 (setf ordering nil)
                                                 (redraw)))
                                          nil)
                                         (changing
                                          ;; the class pick: a digit
                                          ;; takes that art off the
                                          ;; offered list; Esc cancels.
                                          ;; The list is read fresh, so
                                          ;; it is the one just drawn.
                                          (let* ((hero
                                                  (nth sheet-hero
                                                       (game-party game)))
                                                 (targets
                                                  (and hero
                                                       (hero-class-change-targets
                                                        hero))))
                                            (cond ((eql c :esc)
                                                   (setf changing nil)
                                                   (redraw))
                                                  ((and (characterp c)
                                                        (digit-char-p c)
                                                        (<= 1
                                                            (digit-char-p c)
                                                            (length
                                                             targets)))
                                                   (change-class
                                                    game hero
                                                    (nth (1- (digit-char-p c))
                                                         targets))
                                                   (setf sheet-top 0)
                                                   (setf changing nil)
                                                   (redraw))))
                                          nil)
                                         (magic
                                          ;; the spells/songs page: the
                                          ;; shared model eats the keys
                                          ;; — digits pick a card
                                          ;; there, so Q alone quits
                                          (let ((key (if (eq c :esc)
                                                         #\Escape
                                                         c)))
                                            (when (characterp key)
                                              (magic-page-act key)))
                                          nil)
                                         ((eql c :esc) (leave-sheet) nil)
                                         ((and (characterp c)
                                               (digit-char-p c)
                                               (<= 1 (digit-char-p c)
                                                   +party-limit+))
                                          (open-sheet (1- (digit-char-p c)))
                                          nil)
                                         ((or (eql lc #\n) (eql lc #\i))
                                          ;; NEXT turns the carousel to
                                          ;; the pack page ('i' stays
                                          ;; as the old shortcut there)
                                          (let ((hero (nth sheet-hero
                                                           (game-party
                                                            game))))
                                            (when hero
                                              (setf equipv
                                                    (make-equip-view hero)
                                                    sheet-top 0)
                                              (redraw)))
                                          nil)
                                         ((eql lc #\p)
                                          ;; pool the party's gold onto
                                          ;; the sheet hero
                                          (let ((hero (nth sheet-hero
                                                           (game-party
                                                            game))))
                                            (when hero
                                              (pool-gold game hero)
                                              (redraw)))
                                          nil)
                                         ((eql lc #\t)
                                          ;; trade gold away from the
                                          ;; sheet hero (pointless with
                                          ;; one member)
                                          (let ((hero (nth sheet-hero
                                                           (game-party
                                                            game))))
                                            (when (and hero
                                                       (rest (game-party
                                                              game)))
                                              (setf tradev
                                                    (make-trade-view hero))
                                              (redraw)))
                                          nil)
                                         ((eql lc #\o)
                                          ;; change the marching order
                                          ;; (pointless with one member)
                                          (when (rest (game-party game))
                                            (setf ordering t)
                                            (redraw))
                                          nil)
                                         ((eql lc #\l)
                                          ;; a banked level rises here —
                                          ;; one level per press, its
                                          ;; messages on a page of
                                          ;; their own (the takeover
                                          ;; hides the log they land in)
                                          (let ((hero (nth sheet-hero
                                                           (game-party
                                                            game)))
                                                (mark (log-length log)))
                                            (when (and hero
                                                       (advance-level
                                                        game hero))
                                              (setf sheet-notes
                                                    (log-since log mark))
                                              (redraw)))
                                          nil)
                                         ((eql lc #\c)
                                          ;; the second ladder: pick an
                                          ;; art to take up
                                          (let ((hero (nth sheet-hero
                                                           (game-party
                                                            game))))
                                            (when (and hero
                                                       (hero-can-change-class-p
                                                        hero))
                                              (setf changing t)
                                              (redraw)))
                                          nil)
                                         ((characterp c)
                                          ;; u/d scroll a long stat block
                                          (let ((top (hero-sheet-scroll
                                                      game sheet-hero
                                                      sheet-top c)))
                                            (when top
                                              (setf sheet-top top)
                                              (redraw)))
                                          nil)))
                                  (savem
                                   ;; save/load picker: the shared model
                                   ;; eats every key — digits pick slots
                                   ;; and letters are name characters,
                                   ;; so S/L/q cannot leak through
                                   (saves-act c))
                                  (castv
                                   ;; cast menu: the shared model eats
                                   ;; every key (digits pick, Esc backs
                                   ;; out) — see spells.lisp
                                   (cast-menu-act c))
                                  (usev
                                   ;; use menu: same shape — see items.lisp
                                   (use-menu-act c))
                                  (singv
                                   ;; sing menu: same shape — see songs.lisp
                                   (sing-menu-act c))
                                  (ordersv
                                   ;; combat round orders: every hero
                                   ;; picks in turn (see combat.lisp);
                                   ;; Q still quits, everything else
                                   ;; feeds the model
                                   (if (eql lc #\q)
                                       :quit
                                       (let* ((key (if (eq c :esc)
                                                       #\Escape
                                                       c))
                                              (r (when (characterp key)
                                                   (combat-orders-act
                                                    game ordersv key))))
                                         (cond ((eq r :flee)
                                                (fight
                                                 (lambda ()
                                                   (attempt-flee game))))
                                               ((and (consp r)
                                                     (eq (first r) :fight))
                                                (fight
                                                 (lambda ()
                                                   (combat-round
                                                    game (second r)))))
                                               (t (redraw)))
                                         nil)))
                                  ((game-location game)
                                   ;; inside a shop: the shared model
                                   ;; handles the keys (Esc backs out /
                                   ;; leaves; Q still quits the game)
                                   (if (eql lc #\q)
                                       :quit
                                       (let ((key (if (eq c :esc)
                                                      #\Escape
                                                      c)))
                                         (when (characterp key)
                                           (location-act
                                            game
                                            (or locv
                                                (setf locv
                                                      (make-location-view
                                                       game)))
                                            key))
                                         ;; the location lives in the
                                         ;; panes too — leaving needs
                                         ;; only a redraw (a trapdoor
                                         ;; travel sets zone-dirty and
                                         ;; redraw repaints the chrome)
                                         (redraw)
                                         nil)))
                                  ((or (eql lc #\q) (eql c :esc)) :quit)
                                  (over nil) ; game ended: only Q/Esc react
                                  ((game-combat game)
                                   ;; combat without an orders page —
                                   ;; a scripted start: open one
                                   (setf ordersv (make-combat-orders))
                                   (redraw)
                                   nil)
                                  ((eql c #\S) (open-saves :save) nil)
                                  ((eql c #\L) (open-saves :load) nil)
                                  ((and (characterp c) (digit-char-p c)
                                        (<= 1 (digit-char-p c) +party-limit+))
                                   (open-sheet (1- (digit-char-p c)))
                                   nil)
                                  (t
                                   (case lc
                                     (#\w (%step :forward) (redraw))
                                     (#\s (%step :back) (redraw))
                                     (#\a (turn-left game) (redraw))
                                     (#\d (turn-right game) (redraw))
                                     (#\c (open-cast nil))
                                     (#\u (open-use))
                                     (#\p (open-sing nil))
                                     (#\h (open-help))
                                     (#\? (open-help))
                                     ;; the map opens centered on the
                                     ;; party every time — a window
                                     ;; scrolled away last visit says
                                     ;; nothing about where the party
                                     ;; stands now
                                     (#\m (setf mode :map
                                                map-top nil)
                                          (redraw)))
                                   nil)))))
                 (let ((*pointer-window* win))
                  (unwind-protect
                     (progn
                       ;; the hand pointer is up before the first
                       ;; (busy-bracketed) tile-pack load, so the busy
                       ;; pointer has something to restore to
                       (%ensure-standard-pointer scr win display)
                       (ensure-walls)
                       (amiga-sound-open (zone-sfx-dir game))
                       (dlog-timed ("chrome + first frame")
                         (%chrome-bg rp win l)
                         (%chrome-frames rp game l)
                         (setf pace-fn #'pace)
                         ;; a fresh session's animations start at frame
                         ;; 0 — keeps the unattended autoplay sessions
                         ;; deterministic
                         (setf *anim-step* 0)
                         (redraw))
                       (dlog "first frame up [launch+~D ms]"
                             (%dlog-elapsed-ms 0))
                       (amiga.intuition:event-loop win
                         (amiga.intuition:+idcmp-closewindow+ (msg)
                           (return))
                         (amiga.intuition:+idcmp-menupick+ (msg)
                           (let ((code (amiga.intuition:msg-code msg)))
                             (unless (= code +menu-null+)
                               (case (%menu-item-number code)
                                 (0 (open-saves :save))
                                 (1 (open-saves :load))
                                 ;; Quit asks the same question the Q
                                 ;; key does — see REQUEST-QUIT
                                 (3 (when (eq (request-quit) :quit)
                                      (return)))))))
                         (amiga.intuition:+idcmp-vanillakey+ (msg)
                           ;; letter case from the Shift qualifier, not
                           ;; Caps Lock — 's' must step back, never open
                           ;; the save picker — and the digit keys from
                           ;; their raw position, so a shift wedged down
                           ;; on the host cannot take hero selection out
                           ;; of play (see src/keys.lisp)
                           (let ((c (vanilla-key-char
                                     (amiga.intuition:msg-code msg)
                                     (amiga.intuition:msg-qualifier msg)
                                     (amiga.intuition:msg-raw-key msg))))
                             (when (eq (act c) :quit)
                               (return))))
                         (amiga.intuition:+idcmp-mousebuttons+ (msg)
                           ;; a left click acts as the key registered
                           ;; under the pointer (see *HOTSPOTS*)
                           (when (= (amiga.intuition:msg-code msg)
                                    amiga.intuition:+selectdown+)
                             (let ((c (%hotspot-at
                                       (amiga.intuition:msg-mouse-x msg)
                                       (amiga.intuition:msg-mouse-y msg))))
                               (dlog "click ~Dx~D -> ~S"
                                     (amiga.intuition:msg-mouse-x msg)
                                     (amiga.intuition:msg-mouse-y msg)
                                     c)
                               (when (and c (eq (act c) :quit))
                                 (return)))))
                         (amiga.intuition:+idcmp-intuiticks+ (msg)
                           ;; hover feedback: the pointing finger over
                           ;; a click target, the hand elsewhere
                           (%track-pointer-hot
                            win
                            (amiga.intuition:msg-mouse-x msg)
                            (amiga.intuition:msg-mouse-y msg))
                           ;; the living-world clock: while the party
                           ;; stands idle in free exploration, drip game
                           ;; time forward at *IDLE-CLOCK-RATE* (see
                           ;; time.lisp) so the sky cycles, magic
                           ;; regenerates and effects burn down without a
                           ;; step.  Off in combat, a location or any
                           ;; modal picker — and the base resets there so
                           ;; a paused stretch never dumps in at once.
                           (idle-clock)
                           ;; sweep old news off the message board —
                           ;; only while the board itself is showing
                           ;; (a takeover page or combat transcript
                           ;; owns the pane and its line marks)
                           (when (and (eq mode :play)
                                      (menus-idle-p)
                                      (not (game-combat game))
                                      (not over)
                                      (not pacing)
                                      (expire-messages log))
                             (%amiga-draw-log rp log l log-lines))
                           ;; the animation heartbeat: re-blit the
                           ;; registered dirty rectangles in place —
                           ;; never a REDRAW (see *ANIM-BLITS*)
                           (when (and *anim-blits*
                                      (>= (incf anim-ticks)
                                          +anim-ticks-per-step+))
                             (setf anim-ticks 0)
                             (%anim-blit-step rp))
                           (when *autoplay*
                             ;; a scripted entry is a key, or a
                             ;; (:click X Y) resolved through the same
                             ;; hotspot map as a real button event
                             (let* ((entry (pop *autoplay*))
                                    (c (if (and (consp entry)
                                                (eq (first entry) :click))
                                           (%hotspot-at (second entry)
                                                        (third entry))
                                           entry)))
                               (when (and c (eq (act c) :quit))
                                 (return)))))))
                   (%free-standard-pointer win)
                   (amiga-sound-close)
                   (when fp-back
                     (amiga.gfx:free-bitmap fp-back))
                   (setf *anim-blits* '()   ; records hold freed bitmaps
                         fp-back nil
                         walls (%free-wall-assets walls)
                         pack-cache (%pack-cache-free-all pack-cache)
                         icons (%free-images icons)
                         log-lines (%free-log-lines log-lines))))))))))))))))
