;;; Lambda's Tale — the help screen, the quit confirmation and the
;;; magic-at-work page: three small text pages both front-ends draw.
;;;
;;; The status line is gone (the layout gives its space to the party
;;; roster), so the key reference lives on its own page under the H
;;; (or ?) key.  Pure text — both front-ends draw HELP-LINES verbatim
;;; (the SHOP-LINES pattern) and the host test suite checks it.

(in-package :tale)

(defun help-lines (&optional map-scroll-p menu-strip-p)
  "The key-mapping reference as a list of text lines.  MAP-SCROLL-P is
true only from a front-end that wires U/D scrolling into its map view
(the Amiga UI does, via MAP-PAGE-SCROLL; the host UI's MAP-ACT does
not) — it picks which map-view line to show so the page never
advertises a key the calling front-end does not honor.  MENU-STRIP-P
says the same about the mouse: only the Amiga UI hangs a menu strip
off the right button (*MENU-STRIP*), so only it names one here."
  (append
   (list "*** Keys ***"
         ""
         "W forward    S step back"
         "A turn left  D turn right"
         "M map view   H/? this help"
         "C cast spell"
         "U use item"
         "P play a song"
         "E magic at work (effects)"
         "1-7 character sheet"
         "    N there: next page (pack,"
         "      spells/songs, back around)"
         "    1-9 on the spells/songs page"
         "      opens that card; C casts"
         "      it there, P plays a song"
         "    P there: pool gold on hero"
         "    T there: trade gold to another"
         "    O there: marching order"
         "    L there: take a level (^ = due)"
         "    C there: change class"
         "Shift-S save  Shift-L load"
         "Q or Esc quit (asks first)"
         ""
         "Combat: every round opens with"
         "        F fight / R run (all)"
         "        then each hero picks:"
         "        A attack  D defend"
         "        C cast  P play  Esc undo"
         "        Y fight / N redo  +/- speed"
         "        E magic at work, any time"
         "Shop/menus: 1-9 pick  Esc back"
         "            U/D scroll long lists"
         (if map-scroll-p
             "Map view: U/D scroll  F reveal"
             "Map view: F reveal")
         "Mouse: click to walk and pick")
   ;; the pages that open out of nothing (map, help, cast, play, use,
   ;; save, load, quit) have no click of their own — where a menu strip
   ;; carries them, say so, or the mouse looks like it cannot get there
   (when menu-strip-p
     (list "       right button: menu strip"))
   (list ""
         "H or Esc: back")))

;;; ---------------------------------------------------------------------
;;; The quit confirmation
;;;
;;; Q, Esc and the menu strip's Quit all ask before they end the
;;; session — on the game's own screen quitting tears the display down,
;;; and Esc is the key a player reaches for to back out of a page, so a
;;; stray press must not cost an unsaved run.  Same shape as every other
;;; page: a LINES generator both front-ends draw (the option rows carry
;;; their keys, so they click) and an ACT that reads the key.  The
;;; endgame page is exempt — it asks for Q itself.

(defun quit-confirm-lines ()
  "The quit confirmation as menu lines: the question, then its two
options — one per row, each picked by the key its first letter names."
  (list "*** Quit ***"
        ""
        "Really quit the game?"
        "Unsaved progress is lost."
        ""
        (menu-option #\y "Yes, quit")
        (menu-option #\n "No, keep playing")))

(defun quit-confirm-act (key)
  "KEY answered on the quit confirmation: :QUIT when it confirms (Y),
:CANCEL when it backs out (N or Esc), NIL when the page ignores it —
the page eats every other key, so nothing leaks through to the game
while it waits for an answer."
  (let ((c (if (characterp key) (char-downcase key) key)))
    (cond ((eql c #\y) :quit)
          ((or (eql c #\n) (eql c #\Escape) (eq c :esc)) :cancel)
          (t nil))))
;;; ---------------------------------------------------------------------
;;; Magic at work: the effects strip, spelled out.
;;;
;;; The strip shows an icon per working and the log said what was cast;
;;; minutes later a player wants to know what is still holding and what
;;; it is doing for them — is the shield up, how long does the torch
;;; have, is the song the one that quickens the sword or the one that
;;; mends.  This page answers, on the road and in a fight alike (the E
;;; key in both front-ends; the Amiga's effects band clicks to it too):
;;; every active effect by name, what it works in the phrases the spell
;;; and song cards use — the same *EFFECT-PHRASES* read from the same
;;; keys, so what the card promised and what the page reports can never
;;; disagree — and how long it has.  The WORKINGS-VIEW is the page's
;;; scroll state (a long list windows like the pack page), WORKINGS-
;;; LINES renders it, WORKINGS-ACT reads the keys; a page with nothing
;;; at work says so.

(defstruct (workings-view (:constructor %make-workings-view))
  (top 0))              ; scroll offset into the page

(defun make-workings-view ()
  (%make-workings-view))

(defun effect-payload-lines (effect)
  "EFFECT's payload read back as player's phrases, one per line, in the
vocabulary's order: the payload keys are the spec keys' stand-ins
\(*TIMED-EFFECT-KEYS* pairs them), so the phrase is looked up by the
spec key the payload key stands for.  A payload marker that names no
timed key (the :SONG flag) says nothing."
  (let ((lines '()))
    (dolist (entry *timed-effect-keys*)
      (let ((value (getf (effect-payload effect) (second entry))))
        (when value
          (let ((phrase (%effect-phrase (first entry) value)))
            (when phrase (push phrase lines))))))
    (nreverse lines)))

(defun effect-time-left-text (game effect)
  "How long EFFECT has, in player's words: \"43 minutes left\", \"a
minute left\", or \"until dispelled\" for one that burns until removed."
  (let ((at (effect-expires-at effect)))
    (cond ((null at) "until dispelled")
          (t (let ((left (max 1 (- at (game-time game)))))
               (if (= left 1)
                   "a minute left"
                   (format nil "~D minutes left" left)))))))

(defun %workings-page-lines (game)
  "The whole page, unwindowed: the head, then each working — its name
\(a song marked as one), its phrases and its time left indented under
it — or the one line that says nothing is at work.  Every row is kept
inside +TAKEOVER-COLUMNS+ by the phrases' own discipline (see
*EFFECT-PHRASES*) and by naming the working alone on its row."
  (append
   (list "*** Magic at Work ***" "")
   (if (null (game-effects game))
       (list "Nothing is at work.")
       (loop for e in (game-effects game)
             append (cons (format nil "~A~:[~; (song)~]"
                                  (effect-label e)
                                  (getf (effect-payload e) :song))
                          (mapcar (lambda (line)
                                    (concatenate 'string "  " line))
                                  (append (effect-payload-lines e)
                                          (list (effect-time-left-text
                                                 game e)))))))))

(defun workings-lines (game view)
  "The magic-at-work page as menu lines (the SHOP-LINES pattern) —
the whole page windowed at VIEW's scroll offset over +TAKEOVER-ROWS+
rows, the pack page's way (EQUIP-LINES): a long list of workings turns
as one document, and a short one shows whole."
  (menu-scrolled-lines (%workings-page-lines game)
                       (workings-view-top view)
                       (lambda (i line) (declare (ignore i)) line)
                       +takeover-rows+))

(defun workings-act (game view char)
  "Apply key CHAR to the magic-at-work page: u/d turn a windowed page,
E or Esc close it.  Returns :CLOSED when the page should come down,
else NIL — the page is read, not picked from, so every other key is
ignored."
  (cond ((member char '(#\e #\E #\Escape)) :closed)
        (t (let ((top (menu-scroll (workings-view-top view) char
                                   (length (%workings-page-lines game))
                                   +takeover-rows+)))
             (when top (setf (workings-view-top view) top))
             nil))))
