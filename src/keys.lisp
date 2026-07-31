;;; Lambda's Tale — keyboard input normalization.
;;;
;;; The Amiga UI receives keys as IDCMP_VANILLAKEY Code+Qualifier
;;; pairs.  The keymap has already been applied to Code, so Caps Lock
;;; upcases letters exactly like Shift — but the game's controls are
;;; case-sensitive ('s' steps back, Shift-S opens the save picker), so
;;; a stuck or host-desynced Caps Lock (a classic emulator condition)
;;; would silently turn every back-step into the save screen.  The fix:
;;; for letters, derive the case from the Shift qualifier alone.
;;;
;;; The digit keys have the same enemy one worse: a SHIFT wedged down
;;; on the host (the FS-UAE classic — a screenshot chord steals the
;;; key-up) rides the qualifier into the keymap itself, so the digit
;;; row arrives as punctuation and hero selection silently dies.  No
;;; character-level rule can undo that — the layouts disagree about
;;; what shift-1 is — but the raw POSITION codes never move: 0x01-0x0A
;;; is the digit row and the numeric pad is its own block on every
;;; Amiga keymap (some, like the French, even put the digits BEHIND
;;; shift — served by the same rule).  So when the front-end can hand
;;; over the raw key (AMIGA.INTUITION:MSG-RAW-KEY), the digit keys
;;; translate from position alone and the qualifiers can say what
;;; they like.
;;;
;;; Pure functions, loaded on both platforms so the host suite tests
;;; them; the qualifier bits are IEQUALIFIER_LSHIFT/RSHIFT from
;;; devices/inputevent.h (mirrored in AMIGA.INTUITION, which only
;;; exists on the Amiga build).

(in-package :tale)

(defconstant +key-shift-mask+ #x0003
  "IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT.")

(defun raw-key-digit (raw)
  "The digit character raw keycode RAW names by its POSITION — the
top row (0x01-0x0A) or the numeric pad — or NIL for any other key (a
NIL RAW included).  Position-coded means keymap- and qualifier-blind:
a wedged Shift changes what the keymap makes of the key, never where
the key is."
  (case raw
    (#x01 #\1) (#x02 #\2) (#x03 #\3) (#x04 #\4) (#x05 #\5)
    (#x06 #\6) (#x07 #\7) (#x08 #\8) (#x09 #\9) (#x0A #\0)
    (#x0F #\0)
    (#x1D #\1) (#x1E #\2) (#x1F #\3)
    (#x2D #\4) (#x2E #\5) (#x2F #\6)
    (#x3D #\7) (#x3E #\8) (#x3F #\9)))

(defun vanilla-key-char (code qualifier &optional raw)
  "Map an IDCMP_VANILLAKEY CODE+QUALIFIER to the key ACT expects:
:ESC for the Escape code, otherwise a character whose case — for
letters — reflects the Shift qualifier, not Caps Lock.  RAW, when the
front-end can supply it (MSG-RAW-KEY), is the position-coded raw key
behind the event: a digit key translates from it alone, so a wedged
host Shift cannot turn the roster digits into punctuation — see the
file head."
  (or (raw-key-digit raw)
      (if (= code 27)
          :esc
          (let ((ch (code-char code)))
            (cond ((not (both-case-p ch)) ch)
                  ((logtest qualifier +key-shift-mask+) (char-upcase ch))
                  (t (char-downcase ch)))))))
