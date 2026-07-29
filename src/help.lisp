;;; Lambda's Tale — the help screen and the quit confirmation: two
;;; small text pages both front-ends draw.
;;;
;;; The status line is gone (the layout gives its space to the party
;;; roster), so the key reference lives on its own page under the H
;;; (or ?) key.  Pure text — both front-ends draw HELP-LINES verbatim
;;; (the SHOP-LINES pattern) and the host test suite checks it.

(in-package :tale)

(defun help-lines ()
  "The key-mapping reference as a list of text lines."
  (list "*** Keys ***"
        ""
        "W forward    S step back"
        "A turn left  D turn right"
        "M map view   H/? this help"
        "C cast spell"
        "U use item"
        "P play a song"
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
        "Shift-S save  Shift-L load"
        "Q or Esc quit (asks first)"
        ""
        "Combat: every round opens with"
        "        A attack / F flee (all)"
        "        then each hero picks:"
        "        A attack  D defend"
        "        C cast  P play  Esc undo"
        "        Y fight / N redo  +/- speed"
        "Shop/menus: 1-9 pick  Esc back"
        "            U/D scroll long lists"
        "Map view: F full map (debug)"
        "Mouse: click to walk and pick"
        ""
        "H or Esc: back"))

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
