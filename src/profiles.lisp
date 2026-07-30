;;; Lambda's Tale — display profiles.
;;;
;;; A DISPLAY-PROFILE bundles everything that depends on the screen a
;;; target machine can show: screen/window geometry, the first-person
;;; viewport the wall assets are drawn for, the tile-pack directory and
;;; the layout tuning (chrome pads, log/compass band, roster columns).
;;; The engine and the renderers read the profile — and the view.lisp
;;; specials it binds — at call time, so a new target (say a big RTG
;;; screen) is a new profile plus an asset pack, not new code.
;;;
;;; This file is platform-independent: the asset generator and the
;;; tile-pack manifest use profiles on the host too, so it loads before
;;; view.lisp (whose viewport specials initialize from the default
;;; profile).

(in-package :tale)

(defparameter *engine-dir* cl-user::*lambda-tale-engine-root*
  "Absolute path of the engine's own directory (trailing separator
included), computed by src/load.lisp from *LOAD-TRUENAME*.  Engine
assets — the profiles' default tile packs under data/ — resolve here;
everything a GAME owns (maps, campaigns, zone packs, saves) resolves
against the working directory or the map file instead.")

(defun engine-path (relative)
  "RELATIVE, an engine-relative path like \"data/gfx/\", as an
absolute path under *ENGINE-DIR*."
  (concatenate 'string *engine-dir* relative))

(defstruct (display-profile (:constructor %make-display-profile))
  name                        ; :lores / :hires
  screen-width screen-height  ; the LAYOUT's size — what the game is
                              ; drawn for, and the size of the backdrop
                              ; window whatever display it lands on.
                              ; The custom SCREEN may come out taller:
                              ; see SCREEN-HEIGHT-FOR
  screen-max-height           ; how tall that screen may grow to match
                              ; the display (NIL = never grow)
  screen-depth                ; bitplanes; pens 0-3 UI, 4..2^depth-1 pack
  win-width win-height        ; window-mode size (:display :window)
  fp-width fp-height          ; first-person viewport the assets match
  gfx-dir                     ; the profile's default tile pack
  draw-depth                  ; default *DRAW-DEPTH*: how many depth
                              ; levels the view draws (see view.lisp).
                              ; Only a DEFAULT — the profile describes
                              ; a screen, not a CPU, and PLAY-AMIGA's
                              ; :DRAW-DEPTH overrides it per machine
  draw-flanks                 ; default *DRAW-FLANKS*: how many cells
                              ; sideways the view draws of a facing
                              ; house row (see view.lisp); a default
                              ; like DRAW-DEPTH, overridden per machine
                              ; by PLAY-AMIGA's :DRAW-FLANKS
  pad-x pad-y                 ; chrome ring pads (full-screen backdrop)
  view-gap                    ; px between the view column and the log
  band-height                 ; effects + compass band at the log's foot
  roster-cols)                ; roster columns as a plist of char cells:
                              ;   :no :name :ac :hit :hpts :spl :spts :cl
                              ;   (the Bard's Tale table — max/current
                              ;   hit and spell points, class code)

;;; The roster's numeric columns (AC, HIT, PTS, SPL, PTS) are
;;; right-aligned inside a fixed field so values of different width
;;; line up under one another and under their heading:
;;;
;;;    AC HIT PTS         AC  HIT PTS
;;;     4  20  20   not    4  20  20
;;;    10   7 112         10  7   112
;;;
;;; Three cells hold every value the engine produces — hit and spell
;;; points reach the hundreds, armor class the tens (with a sign when
;;; a spell shield drives it below zero) — and the profiles leave at
;;; least four cells between numeric columns, so the field never
;;; touches its neighbour.
(defconstant +roster-num-cells+ 3
  "Width, in character cells, of a right-aligned roster number column.")

;;; The roster's rows set solid (see the note above UI-LAYOUT), which
;;; is what makes the table fit — but a table that also began flush
;;; under the plaque read as one block with it.  This gap is the
;;; saving spent back where it shows: enough clear grey that the
;;; heading reads as the roster's own, not as the plaque's underline.
;;; It lives here rather than in the Amiga front-end because it is
;;; layout tuning like the pads and the column cells, and because the
;;; host suite has to be able to check the whole column against the
;;; 200-line screen — see the ":lores fills its 200 lines" test.
(defconstant +roster-gap+ 4
  "Pixels of clear space between the location plaque and the roster
header row.")

;;; The drop shadow the view and its plaque cast down-right eats into
;;; that gap: it hangs +VIEW-SHADOW+ rows below the plaque's bottom, so
;;; it has to stay strictly shallower than +ROSTER-GAP+ or it runs into
;;; the roster header and the two read as one heavy band — the more so
;;; against the bright $AAA chrome.  Named here beside the gap it must
;;; respect, so the host suite can hold the two against each other
;;; without opening a screen (see "the view's shadow clears the roster
;;; header").
(defconstant +view-shadow+ 2
  "Depth, in pixels, of the drop shadow under the view and its plaque.")

(defun roster-cell (cell text &optional (width +roster-num-cells+))
  "The character cell TEXT starts at when right-aligned in the
WIDTH-cell field that begins at CELL.  Text too wide for the field
starts at CELL itself, so an outsized value runs into the gap after
its column rather than backing up into the column before it."
  (+ cell (max 0 (- width (length text)))))

;;; The classic 640x256 PAL hires presentation: 16 colors, the 240x130
;;; viewport the original M3 wall packs were drawn for.
(defparameter *hires-profile*
  (%make-display-profile
   :name :hires
   ;; already the full PAL height, so nothing to grow into
   :screen-width 640 :screen-height 256 :screen-max-height nil
   :screen-depth 4
   :win-width 640 :win-height 256
   :fp-width 240 :fp-height 130
   :gfx-dir (engine-path "data/gfx-hires/")
   ;; One level short of the full view (+VIEW-DEPTH+ is 4; this file
   ;; loads before view.lisp, so the constant cannot be named here).
   ;; Hires blits roughly twice the lores pixel area per frame, and the
   ;; deepest level is the one that costs a blit to show almost nothing
   ;; — an 8x8 front wall — so it is the cheapest thing to give up.
   ;; Raise it with :DRAW-DEPTH 4 on a machine that can afford it.
   :draw-depth 3
   ;; The classic single flank per side; :DRAW-FLANKS up to 8 fills a
   ;; distant street with houses on a machine with blit headroom.
   :draw-flanks 1
   :pad-x 12 :pad-y 10 :view-gap 12 :band-height 20
   :roster-cols '(:no 0 :name 2 :ac 22 :hit 27 :hpts 32
                  :spl 38 :spts 43 :cl 48)))

;;; The ECS target: 320x200 lores, 5 bitplanes (32 colors) — the
;;; Bard's Tale presentation.  Half the chip-RAM/DMA cost of hires and
;;; near-square pixels for the art.  The 120px viewport gives the view
;;; column 2/5 of the 300px content span and the message log the other
;;; 3/5 — the text matters more than the picture, and the log column
;;; always takes whatever the profile's FP-WIDTH leaves over, so the
;;; split is a profile knob, not engine code.
;;;
;;; The LAYOUT is 200 lines, not PAL's 256, so one design serves both
;;; machines: an NTSC Amiga has no 256-line mode to fall back on, and a
;;; layout that only fit PAL would cost US machines the bottom of the
;;; roster.  The SCREEN, though, grows to whatever the display it
;;; landed on can show, up to SCREEN-MAX-HEIGHT — 256 on PAL, 200 on
;;; NTSC (see SCREEN-HEIGHT-FOR).  The game still lays out in the top
;;; 200 lines either way; what the extra buys is that a PAL machine
;;; gets its native display instead of a short screen an emulator or a
;;; scan doubler then has to rescale.
;;;
;;; That budget is what sizes the viewport: 181 rows inside the chrome
;;; pads (10 above, 9 below — BOTTOM is the last usable row), of which
;;; the furniture under the view — plaque (13), +ROSTER-GAP+ (4),
;;; header and seven roster rows (8 x 8) — takes 81, leaving 100.  So
;;; the wall assets are drawn for 100 and the column comes out exact:
;;; roster row 7 ends on the last usable row, with the chrome ring's
;;; two clear pixels below it.
;;;
;;; There is no slack left to spend, and none to be won back: a
;;; viewport too tall for its column does not shrink, it drops the
;;; view to the wireframe (%AMIGA-COMPOSE-FP only blits pieces at
;;; their full asset size), so growing FP-HEIGHT costs the wall
;;; graphics outright.  Anything that wants a pixel here has to take
;;; it from the roster or the plaque.  The suite checks the fit both
;;; ways (search "NTSC" and "fills its 200 lines" in
;;; tests/run-tests.lisp).
(defparameter *lores-profile*
  (%make-display-profile
   :name :lores
   :screen-width 320 :screen-height 200 :screen-max-height 256
   :screen-depth 5
   :win-width 320 :win-height 200
   :fp-width 120 :fp-height 100
   :gfx-dir (engine-path "data/gfx/")
   :draw-depth 4                        ; the full view — the smaller
                                        ; viewport can afford it
   :draw-flanks 1                       ; the classic single flank; see
                                        ; the hires profile's note
   ;; The effect strip is the icons' own 16 pixels and not one more:
   ;; the four the hires profile spends on clearance buy the message
   ;; page above it a whole extra row of small-face type, and the page
   ;; is where the shop and the character sheet run out of room.  Do
   ;; not go below 16 — %AMIGA-DRAW-BAND skips an icon taller than the
   ;; strip, so a shorter one shows no effects at all.
   :pad-x 10 :pad-y 10 :view-gap 12 :band-height 16
   :roster-cols '(:no 0 :name 2 :ac 15 :hit 19 :hpts 23
                  :spl 27 :spts 31 :cl 35)))

(defparameter *display-profiles*
  (list *lores-profile* *hires-profile*))

;;; ---------------------------------------------------------------------
;;; How tall a screen to open.
;;;
;;; The layout height and the screen height are not the same question.
;;; The layout is fixed — the game is drawn for it, and it must fit the
;;; shortest display the game claims to support (NTSC's 200 lines).  The
;;; screen is free to be as tall as the display actually is, and it
;;; should be: a screen shorter than its display is one an emulator's
;;; auto-zoom crops to and rescales, and one a real monitor letterboxes.
;;;
;;; So the profile names both, and this function picks: the layout
;;; height, grown to the display's own height where the display has more
;;; to give, never past SCREEN-MAX-HEIGHT.  On PAL that is 200 -> 256; on
;;; NTSC the display measures 200 and nothing moves.  No PAL/NTSC test
;;; anywhere — the display database is asked and answers for whatever
;;; the machine actually is, RTG included.

(defun screen-height-for (profile display-height)
  "The height to open PROFILE's custom screen at on a display of
DISPLAY-HEIGHT rows (NIL when the display database would not say).

Never shorter than the profile's layout, never taller than its
SCREEN-MAX-HEIGHT, and otherwise the display's own height — so the
screen matches the machine while the game keeps laying out in the top
SCREEN-HEIGHT rows."
  (let ((base (display-profile-screen-height profile))
        (cap (display-profile-screen-max-height profile)))
    (if (and display-height cap)
        (max base (min cap display-height))
        base)))

;;; What to ASK the display database for is a third question again, and
;;; not the layout's height either.  BestModeIDA matches on the nominal
;;; size it is handed, so the size handed to it decides which display
;;; the game lands on — and the layout's 200 rows are the wrong ask.
;;; On a machine carrying both a chipset display and an RTG board,
;;; 320x200 is an EXACT hit on a 320x200 RTG mode, and BestModeIDA
;;; rightly prefers it to PAL's 320x256; the game then plays on the
;;; graphics card, where the picture is resampled to the monitor rather
;;; than shown as pixels.  Asking for the tallest display the profile
;;; would accept puts the chipset mode back in front on such a machine
;;; and changes nothing on any other: an NTSC or RTG-only machine has
;;; no 320x256 to answer with and returns its own best fit, which
;;; SCREEN-HEIGHT-FOR then clamps back to the layout.
;;;
;;; This is a mode-selection knob, not a size: the screen still opens
;;; at SCREEN-HEIGHT-FOR's answer, never at the height asked here.

(defun screen-ask-height (profile)
  "The display height to hand BestModeIDA for PROFILE — the tallest
display the profile would accept (SCREEN-MAX-HEIGHT), falling back to
its layout height when it would not grow at all."
  (or (display-profile-screen-max-height profile)
      (display-profile-screen-height profile)))

(defvar *display-profile* *lores-profile*
  "The active display profile — :lores, the ECS target, by default.
PLAY-AMIGA's :PROFILE argument and WITH-DISPLAY-PROFILE rebind it
together with the view.lisp viewport specials.")

(defun find-display-profile (designator)
  "Resolve DESIGNATOR — a DISPLAY-PROFILE or its :NAME keyword — to a
profile from *DISPLAY-PROFILES*."
  (etypecase designator
    (display-profile designator)
    (symbol (or (find designator *display-profiles*
                      :key #'display-profile-name)
                (error "Unknown display profile ~S (have ~{~S~^, ~})"
                       designator
                       (mapcar #'display-profile-name
                               *display-profiles*))))))

(defmacro with-display-profile ((designator) &body body)
  "Run BODY with *DISPLAY-PROFILE* and the viewport/pack/draw-distance
specials (*FP-VIEW-WIDTH*, *FP-VIEW-HEIGHT*, *GFX-DIR*, *DRAW-DEPTH*,
*DRAW-FLANKS*) bound from DESIGNATOR's profile, so the layout, the
asset loader/generator and the manifest all agree on one target.

*DRAW-DEPTH* and *DRAW-FLANKS* are bound to the profile's DEFAULTS, so
a caller that wants its own draw distance or width must rebind them
INSIDE this macro (PLAY-AMIGA's :DRAW-DEPTH/:DRAW-FLANKS do) — an
outer binding would be overwritten here."
  (let ((p (gensym "PROFILE")))
    `(let* ((,p (find-display-profile ,designator))
            (*display-profile* ,p)
            (*fp-view-width* (display-profile-fp-width ,p))
            (*fp-view-height* (display-profile-fp-height ,p))
            (*gfx-dir* (display-profile-gfx-dir ,p))
            (*draw-depth* (display-profile-draw-depth ,p))
            (*draw-flanks* (display-profile-draw-flanks ,p)))
       ,@body)))
