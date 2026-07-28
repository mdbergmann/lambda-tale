;;; Lambda's Tale — the help screen: the key mappings as text lines.
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
        "    I there: inventory (equip/pass/throw/inspect)"
        "    P there: pool gold on hero"
        "    T there: trade gold to another"
        "    O there: marching order"
        "Shift-S save  Shift-L load"
        "Q or Esc quit"
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
