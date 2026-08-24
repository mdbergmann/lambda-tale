;;; Lambda's Tale — bard songs.
;;;
;;; Song types are campaign data, not engine facts: the campaign
;;; registers them with DEFINE-SONG in its campaign.lisp; the engine
;;; only knows the mechanics.  Singers (hero classes with :SINGER T)
;;; pay TUNES — one charge per song, one charge per level when rested
;;; (Bard's Tale songs-per-day), refilled with a drink at a :TAVERN
;;; location (see locations.lisp).
;;;
;;; A song is always a timed effect over the shared vocabulary
;;; (*TIMED-EFFECT-KEYS* in game.lisp — :buff-ac, :light, :compass,
;;; :buff-damage, :regen-sp, :extra-attacks, :combat-heal ... plus
;;; :duration; keys combine, so one tune can quicken sp on the road
;;; AND arms in a fight), and only ONE song plays at a time: its
;;; effect carries a :SONG marker in the payload, and striking up a
;;; new song displaces the old — the Bard's Tale rule.
;;;
;;; The sing interaction is modeled here too, platform-free (the
;;; CAST-VIEW pattern): a SING-VIEW holds the menu state, SING-LINES
;;; renders it as text lines and SING-ACT maps a key press to
;;; mechanics — both front-ends drive the same model.  In combat,
;;; (:sing SONG) is a party action beside :attack and (:cast ...).

(in-package :tale)

(defstruct (song-type (:constructor %make-song-type))
  name                ; symbol, e.g. TRAVELLERS-TUNE
  title               ; display string, e.g. "travellers tune"
  (level 1)           ; minimum singer level
  effect              ; timed effect plist, e.g. (:buff-ac 2 :duration 60)
                      ; or (:regen-sp 2 :extra-attacks 1 :duration 60)
  image               ; effects-band icon file, or NIL
  description         ; player-facing lore line for the song card, or
                      ; NIL — the card then speaks from the effect spec
                      ; alone (EFFECT-SUMMARY-LINES)
  notes)              ; designer notes (canon effect text, stand-in
                      ; remarks) — carried as data so generated
                      ; songbooks show them; no mechanics.  NOT the
                      ; card's text: notes may carry development
                      ; caveats, :DESCRIPTION is what a player reads

(defvar *song-types* (make-hash-table :test 'eq))
(defvar *song-names* '()
  "Song names in registration order — the stable order of the menus.")

(defparameter %song-meta-keys '(:title :level :image :description :notes)
  "DEFINE-SONG's non-effect keywords; everything else in the argument
plist is the effect spec.")

(defun define-song (name &rest args)
  "Register song type NAME (a symbol).  Campaign data calls this.
ARGS is a plist: :TITLE, :LEVEL (minimum singer level, default 1),
:IMAGE (the effects-band icon), :DESCRIPTION (the player-facing lore
line the song card shows, over and above the effect it derives from
the spec itself), :NOTES (a designer-facing string —
canon effect text, stand-in remarks — data, not mechanics, so
generated songbooks can surface it; never shown on the card, which is
why :DESCRIPTION is its own field) — and the effect spec itself, one
or more keys of the TIMED vocabulary (*TIMED-EFFECT-KEYS* in game.lisp;
keys combine into one effect record).  Songs are always timed, so
:DURATION (game minutes, or :INDEFINITE) is required.  TITLE defaults
to the downcased name (TRAVELLERS-TUNE -> \"travellers tune\")."
  (loop for tail on args by #'cddr
        unless (and (keywordp (first tail)) (cdr tail))
          do (error "define-song ~S: malformed argument plist at ~S"
                    name tail))
  (let ((notes (getf args :notes))
        (description (getf args :description)))
    (when (and notes (not (stringp notes)))
      (error "define-song ~S: :notes must be a string (got ~S)"
             name notes))
    (when (and description (not (stringp description)))
      (error "define-song ~S: :description must be a string (got ~S)"
             name description)))
  (let ((spec (loop for (key value) on args by #'cddr
                    unless (member key %song-meta-keys)
                      append (list key value))))
    (check-effect-spec "define-song" name spec :timed-only t)
    (setf (gethash name *song-types*)
          (%make-song-type
           :name name
           :title (or (getf args :title)
                      (string-downcase (substitute #\Space #\- (string name))))
           :level (or (getf args :level) 1)
           :effect spec
           :image (getf args :image)
           :description (getf args :description)
           :notes (getf args :notes))))
  ;; keep the registration order; a re-registration keeps its spot
  (unless (member name *song-names*)
    (setf *song-names* (append *song-names* (list name))))
  name)

(defun find-song-type (name)
  (or (gethash name *song-types*)
      (error "Unknown song ~S (register it with DEFINE-SONG)" name)))

(defun song-title (name)
  (song-type-title (find-song-type name)))

(defun song-description (name)
  "The song's player-facing lore line (the card's own words), or NIL."
  (song-type-description (find-song-type name)))

(defun song-card-lines (hero name)
  "The song card for NAME — the level it opens at, HERO's tunes in
hand (a song costs one charge, so the card answers 'can I?' the way
the spell card does), and what the tune actually does: the campaign's
:DESCRIPTION when it wrote one, else the effect read back out of the
spec (EFFECT-SUMMARY-LINES).  The sheet's spells/songs page shows this
when a digit picks a row."
  (let ((type (find-song-type name)))
    (append
     (list (format nil "*** ~A ***" (song-title name)) "")
     (list (format nil "Level ~D   ~A"
                   (song-type-level type) (hero-tunes-text hero))
           "")
     (if (song-type-description type)
         (list (song-type-description type))
         (effect-summary-lines (song-type-effect type))))))

(defun song-known-p (hero name)
  "Does HERO know song NAME?  A singer who has reached the song's
level — every singer learns every song of their level."
  (and (hero-singer-p hero)
       (>= (hero-level hero) (song-type-level (find-song-type name)))
       t))

(defun songs-for-hero (hero)
  "The songs HERO knows, in registration order."
  (remove-if-not (lambda (name) (song-known-p hero name))
                 *song-names*))

(defun hero-sings-with (hero)
  "The item kind HERO's class must have equipped to play a song (the
class's :SINGS-WITH, e.g. :INSTRUMENT), or NIL when the class sings
bare-handed."
  (hero-class-property (hero-class hero) :sings-with))

(defun hero-song-tool (hero)
  "The equipped item HERO plays their songs on — the :SINGS-WITH kind
in hand — or NIL when the class needs none or the hands are empty."
  (let ((kind (hero-sings-with hero)))
    (and kind (equipped-of-kind hero kind))))

(defun hero-tireless-p (hero)
  "Does HERO's song tool spend no tune?  True when the instrument in
hand was registered :TIRELESS (DEFINE-ITEM) — the singer plays as often
as they like and HERO-TUNES never falls.  A class that sings without a
tool never is: the trait rides on the instrument, not the singer."
  (let ((tool (hero-song-tool hero)))
    (and tool (item-type-tireless (find-item-type tool)) t)))

(defun hero-tunes-text (hero)
  "HERO's tunes for a menu row: \"Tunes 2/3\", or \"Tireless\" when the
instrument in hand spends none — the count would still show, and a
singer with none left who plays on regardless reads as a bug.  Short
on purpose: the who-plays rows carry it in parentheses beside the name
inside +TAKEOVER-COLUMNS+."
  (if (hero-tireless-p hero)
      "Tireless"
      (format nil "Tunes ~D/~D" (hero-tunes hero) (hero-max-tunes hero))))

(defun song-refusal (hero name)
  "Why HERO cannot play NAME right now, as a short line for the song
card — or NIL when they can: known, an instrument in hand when the
class needs one, and a tune left to spend (unless the instrument is
tireless, HERO-TIRELESS-P).  SONG-PLAYABLE-P answers the yes/no of the
same rule, and SING-SONG turns it down for the same three reasons in
the log's own words.  The card is +TAKEOVER-COLUMNS+ wide, so the
reasons stay short."
  (cond ((not (song-known-p hero name)) "Not in the book.")
        ((and (hero-sings-with hero) (null (hero-song-tool hero)))
         "No instrument in hand.")
        ((and (< (hero-tunes hero) 1) (not (hero-tireless-p hero)))
         "No tunes left.")
        (t nil)))

(defun song-playable-p (hero name)
  "Can HERO play NAME right now?  SONG-REFUSAL decides; this just asks
whether it had anything to say."
  (not (song-refusal hero name)))

(defun current-song (game)
  "The active song's EFFECT (the one whose payload carries the :SONG
marker), or NIL when no song plays."
  (find-if (lambda (e) (getf (effect-payload e) :song))
           (game-effects game)))

(defun sing-song (game hero name)
  "HERO strikes up song NAME.  Says why and returns NIL when the hero
does not know it, has no instrument in hand for a class that plays on
one (:SINGS-WITH) or has no tunes left — SONG-REFUSAL's three rules,
here in the log's own words; otherwise spends one tune (none on a
tireless instrument, HERO-TIRELESS-P), displaces any song already
playing, installs the song's timed effect (:SONG-marked), emits
:SONG-SUNG and returns T."
  (let ((type (find-song-type name)))
    (cond
      ((not (song-known-p hero name))
       (say game "~A does not know ~A." (hero-name hero)
            (song-type-title type))
       nil)
      ((and (hero-sings-with hero) (null (hero-song-tool hero)))
       (say game "~A has no ~(~A~) in hand - the music needs one."
            (hero-name hero) (hero-sings-with hero))
       nil)
      ((and (< (hero-tunes hero) 1) (not (hero-tireless-p hero)))
       (say game "~A has no tunes left - the tavern would help."
            (hero-name hero))
       nil)
      (t
       (unless (hero-tireless-p hero)
         (decf (hero-tunes hero)))
       (let ((old (current-song game)))
         (when old
           (setf (game-effects game)
                 (remove old (game-effects game)))))
       (say game "~A strikes up ~A!" (hero-name hero)
            (song-type-title type))
       (apply-effect-spec game (song-type-title type)
                          (song-type-effect type)
                          :image (song-type-image type)
                          :extra-payload '(:song t))
       (emit game :song-sung hero name)
       t))))

;;; ---------------------------------------------------------------------
;;; The sing interaction model (shared by both front-ends).

(defstruct (sing-view (:constructor %make-sing-view))
  hero                ; the chosen singer, or NIL while picking
  in-combat           ; T: committing fights one COMBAT-ROUND;
                      ; :ORDERS: committing returns the pick as a
                      ; round action (the combat-orders flow)
  (top 0))            ; scroll offset into the song list

(defun make-sing-view (&key in-combat hero)
  "HERO presets the singer (the combat-orders flow asks for one hero's
pick); NIL starts at the who-plays page."
  (%make-sing-view :in-combat in-combat :hero hero))

(defun %sing-commit (game view name)
  "Resolve the completed pick: sing directly; in combat fight one
round where the singer sings and everyone else attacks; in :ORDERS
mode hand the pick back as (:ACTION (:SING NAME))."
  (let ((hero (sing-view-hero view)))
    (cond
      ((eq (sing-view-in-combat view) :orders)
       (list :action (list :sing name)))
      ((sing-view-in-combat view)
       (combat-round game
                     (mapcar (lambda (h)
                               (if (eq h hero) (list :sing name) :attack))
                             (alive-heroes game)))
       :done)
      (t (sing-song game hero name)
         :done))))

(defun sing-lines (game view)
  "The current sing menu as a list of menu lines — the front-ends draw
these verbatim (the SHOP-LINES pattern); option rows carry their pick
key (see MENU-NUMBERED)."
  (let ((hero (sing-view-hero view)))
    (append
     (list "*** Play a Song ***" "")
     (if (null hero)
         (append
          (list "Who plays?" "")
          (let ((i 0))
            (mapcan (lambda (h)
                      (incf i)
                      (when (hero-singer-p h)
                        (list (menu-numbered
                               i (format nil "~D) ~A  (~A)"
                                         i (hero-name h)
                                         (hero-tunes-text h))))))
                    (game-party game))))
         ;; rows first, then a head that can carry the window's range
         (let ((rows (menu-scrolled-lines
                      (songs-for-hero hero) (sing-view-top view)
                      (lambda (i name)
                        (menu-numbered
                         i (format nil "~D) ~A" i (song-title name)))))))
           (append
            (menu-scroll-head
             (format nil "~A plays.  ~A"
                     (hero-name hero) (hero-tunes-text hero)))
            (list "")
            rows))))))

(defun sing-act (game view char)
  "Apply key CHAR to the sing menu.  Returns :DONE when a song
resolved (the front-end drops the view) — in :ORDERS mode
\(:ACTION (:SING ...)) instead — :CANCELLED on Esc at the top level,
else NIL."
  (let ((hero (sing-view-hero view))
        (digit (digit-char-p char)))
    (cond
      ;; picking the singer
      ((null hero)
       (cond ((and digit (<= 1 digit (length (game-party game))))
              (let ((h (nth (1- digit) (game-party game))))
                (when (and (hero-singer-p h) (hero-alive-p h))
                  (setf (sing-view-hero view) h)))
              nil)
             ((eql char #\Escape) :cancelled)
             (t nil)))
      ;; picking the song
      (t
       (cond (digit
              (let ((name (menu-window-pick (songs-for-hero hero)
                                            (sing-view-top view) digit)))
                (when name
                  (%sing-commit game view name))))
             ((eql char #\Escape)
              (setf (sing-view-hero view) nil
                    (sing-view-top view) 0)
              nil)
             (t
              (let ((top (menu-scroll (sing-view-top view) char
                                      (length (songs-for-hero hero)))))
                (when top (setf (sing-view-top view) top)))
              nil))))))
