;;; Lambda's Tale — engine events and story flags.
;;;
;;; The engine never hard-codes story facts: it emits events, the story
;;; (campaign data) and the front-end subscribe.  Everything the game
;;; wants to tell the player travels as a :MESSAGE event, so front-ends
;;; subscribe once and stay independent of what generates the text.
;;;
;;; Events the engine emits:
;;;   :message TEXT          something to show the player
;;;   :enter-cell X Y        the party entered a cell (move or teleport)
;;;   :enter-zone MAP        the party traveled to another zone (map)
;;;   :enter-location LOC    the party entered a location (shop, ...)
;;;   :leave-location LOC    ... and left it again
;;;   :blocked DIR           the party bumped into a wall
;;;   :door DIR              the party stepped through a door
;;;   :combat-start MONSTERS combat began
;;;   :combat-end RESULT     combat ended (:victory, :defeat or :fled)
;;;   :hit MONSTER DMG       the party landed a blow (melee or spell)
;;;   :slay MONSTER          ... and it felled the monster
;;;   :miss HERO MONSTER     a hero's swing missed
;;;   :hero-hurt HERO DMG    a hero took damage and stands
;;;   :hero-died HERO        a hero dropped to 0 hp
;;;   :party-defeated        the last hero fell
;;;   :level-up HERO         a hero rose a level
;;;   :loot ITEM HERO        a fallen monster's item found after a fight
;;;   :coin AMOUNT           gold changed hands in a shop (buy, sell)
;;; plus :spell-cast, :song-sung, :item-used, :item-passed, :drink,
;;; :hero-revived, :party-joined, :time-band, :sunrise, :sunset and
;;; :effect-expired from their subsystems — ATTACH-SOUNDS (sound.lisp)
;;; maps a subset of all these to the sound-cue vocabulary.
;;; Story specials can emit arbitrary further topics via the EVENT op
;;; (see specials.lisp); front-ends and campaign code subscribe alike.

(in-package :tale)

(defun on-event (game topic handler)
  "Subscribe HANDLER, a function of GAME and the event's arguments, to
TOPIC (a keyword).  Handlers on one topic run in subscription order."
  (let ((entry (assoc topic (game-handlers game))))
    (if entry
        (setf (cdr entry) (append (cdr entry) (list handler)))
        (setf (game-handlers game)
              (append (game-handlers game)
                      (list (cons topic (list handler)))))))
  topic)

(defun emit (game topic &rest args)
  "Emit event TOPIC: call each subscribed handler with GAME and ARGS.
With the debug log enabled, every emission leaves a trace line: the
topic, the arguments (briefly), the handler count and the time the
handlers took."
  (let ((handlers (cdr (assoc topic (game-handlers game)))))
    (if *debug-log-stream*
        (let ((start (get-internal-real-time)))
          (dolist (h handlers)
            (apply h game args))
          (%dlog "event ~S~{ ~A~} handlers=~D [~D ms]"
                 topic (mapcar #'%dlog-brief args) (length handlers)
                 (%dlog-elapsed-ms start)))
        (dolist (h handlers)
          (apply h game args))))
  (values))

(defun say (game control &rest args)
  "Emit a :MESSAGE event with the FORMAT-ted text."
  (emit game :message (apply #'format nil control args)))

;;; ---------------------------------------------------------------------
;;; Message log: the Bard's Tale-style text column.  Front-ends attach
;;; one to a game and render its trailing lines, newest at the bottom.

(defstruct (message-log (:constructor %make-message-log))
  (lines '())         ; newest first, each entry (stamp . text) — the
                      ; stamp is the GET-INTERNAL-REAL-TIME of logging,
                      ; EXPIRE-MESSAGES's yardstick
  (limit 100))

(defun attach-message-log (game &key (limit 100))
  "Subscribe a fresh MESSAGE-LOG to GAME's :MESSAGE events and return
it.  The log keeps the most recent LIMIT messages."
  (let ((log (%make-message-log :limit limit)))
    (on-event game :message
              (lambda (g text)
                (declare (ignore g))
                (log-message log text)))
    log))

(defun log-message (log text)
  "Append TEXT to LOG, dropping the oldest line beyond the limit."
  (push (cons (get-internal-real-time) text) (message-log-lines log))
  (let ((tail (nthcdr (1- (message-log-limit log))
                      (message-log-lines log))))
    (when (consp tail)
      (setf (cdr tail) nil)))
  text)

(defun log-recent (log n)
  "The last N messages logged to LOG, oldest first — ready to draw top
to bottom with the newest line at the bottom."
  (let ((lines (message-log-lines log)))
    (mapcar #'cdr (reverse (subseq lines 0 (min n (length lines)))))))

(defun log-length (log)
  "How many messages LOG currently holds (bounded by its limit)."
  (length (message-log-lines log)))

(defun log-since (log mark)
  "The messages logged after the point where LOG held MARK lines,
oldest first — the window a combat round's own transcript page shows."
  (let ((lines (message-log-lines log)))
    (mapcar #'cdr (reverse (subseq lines 0 (max 0 (- (length lines) mark)))))))

(defparameter *message-ttl* 60
  "Seconds a line lingers on the message board before EXPIRE-MESSAGES
sweeps it — old news clears itself even while the party stands still.
NIL keeps every line until the ring's LIMIT drops it.")

(defun expire-messages (log &optional (now (get-internal-real-time)))
  "Drop LOG's lines logged more than *MESSAGE-TTL* seconds before NOW.
Returns true when any line went, so a front-end's idle beat can redraw
the board exactly then.  Call it only while the plain log is showing:
a combat transcript's mark counts lines (LOG-SINCE), so a sweep
mid-round would shift the round's window."
  (when *message-ttl*
    (let* ((cutoff (- now (* *message-ttl* internal-time-units-per-second)))
           (lines (message-log-lines log))
           (kept (remove-if (lambda (entry) (< (car entry) cutoff))
                            lines)))
      (setf (message-log-lines log) kept)
      (< (length kept) (length lines)))))

(defun wrap-text (text width)
  "Split TEXT into a list of lines at most WIDTH characters long,
breaking at spaces; a word longer than WIDTH breaks mid-word.  Always
returns at least one line (\"\" wraps to (\"\"))."
  (let ((width (max 1 width))
        (lines '()))
    (loop
      (when (<= (length text) width)
        (push text lines)
        (return))
      (let ((break (position #\Space text :from-end t :end (1+ width))))
        (if (and break (plusp break))
            (setf lines (cons (subseq text 0 break) lines)
                  text (subseq text (1+ break)))
            (setf lines (cons (subseq text 0 width) lines)
                  text (subseq text width)))))
    (nreverse lines)))

(defun wrap-message (text width)
  "Wrap TEXT like WRAP-TEXT to at most WIDTH characters per line, with a
leading blank line separating it from the previous message so
consecutive messages read as distinct paragraphs in the log."
  (cons "" (wrap-text text (max 1 width))))

;;; ---------------------------------------------------------------------
;;; Structured menu lines.  The menu generators (SHOP-LINES, CAST-LINES,
;;; SAVE-MENU-LINES, ...) return text lines; a line that stands for a
;;; pickable option carries the key that picks it as (TEXT . KEY), so a
;;; pointing front-end can turn a click on the line into that key press
;;; without parsing the text.  Plain informational lines stay strings.
;;; The pages name only their own keys — plain words whose first letter
;;; is the key ("Sell", "Pool gold"), one option per row, each row a
;;; MENU-OPTION so it clicks; the common navigation (digit picks, Esc,
;;; u/d scrolling, +/- speed) lives on the help screen.  The older
;;; bracket-hint convention ("[S]ell  [Esc] back") is still understood:
;;; MENU-KEY-SPANS locates the tokens for per-segment hotspots, and
;;; FIT-MENU-LINES packs such rows together when a page runs out of
;;; rows.

(defun menu-option (key text)
  "TEXT as a menu line that key KEY picks."
  (cons text key))

(defun menu-numbered (i text)
  "TEXT as menu option row I, picked by the digit key I (1-9); rows
past 9 stay plain text — no key reaches them (the models only accept
single-digit picks)."
  (if (<= 1 i 9) (menu-option (digit-char i) text) text))

(defconstant +takeover-columns+ 27
  "Character cells of the narrowest (lores) message-area takeover
column.  The takeover generators design their lines against this
width; MENU-NEXT-OPTION centers on it.")

(defun menu-next-option ()
  "The NEXT row that pages the character-sheet carousel (sheet, pack,
spells/songs — 'n' or a click): the word centered on the lores
takeover column, standing alone on the page's last row.  An option
line's click target is its whole row, so the padding clicks too."
  (menu-option #\n (format nil "~v@A"
                           (floor (+ +takeover-columns+ 4) 2)
                           "NEXT")))

(defun menu-line-text (line)
  "The display text of a menu line (string or (TEXT . KEY))."
  (if (consp line) (car line) line))

(defun menu-line-key (line)
  "The key a menu line stands for, or NIL for a plain line — and for a
packed option row, whose several keys are MENU-LINE-SPANS' business."
  (let ((k (if (consp line) (cdr line) nil)))
    (if (atom k) k nil)))

(defun menu-line-spans (line)
  "The per-option click targets of a packed option row (see
%PACK-OPTION-RUNS) — a list of (START END KEY) character-cell spans,
END exclusive, in the shape MENU-KEY-SPANS returns.  NIL for every
other kind of line: a whole-row option answers to MENU-LINE-KEY and a
plain footer's bracket hints to MENU-KEY-SPANS."
  (let ((k (if (consp line) (cdr line) nil)))
    (if (consp k) k nil)))

(defun menu-texts (lines)
  "LINES with each menu line reduced to its display text."
  (mapcar #'menu-line-text lines))

(defun hint-line-p (line)
  "True for a plain footer hint row — a string with a bracket hint in
it (\"[S]ell\", \"[Esc] back\").  Option lines carrying a key are not
hint rows even when their text brackets something."
  (and (stringp line) (find #\[ line) t))

(defun %hint-segments (text)
  "TEXT split at its option gaps — runs of two or more spaces — into
the whole hints: \"[1-9] buy  [S]ell\" gives (\"[1-9] buy\" \"[S]ell\")."
  (let ((segments '())
        (len (length text))
        (start 0))
    (loop while (< start len)
          do (let ((gap (search "  " text :start2 start)))
               (cond (gap
                      (when (> gap start)
                        (push (subseq text start gap) segments))
                      (setf start (+ gap 2))
                      (loop while (and (< start len)
                                       (char= (char text start) #\Space))
                            do (incf start)))
                     (t
                      (push (subseq text start) segments)
                      (setf start len)))))
    (nreverse segments)))

(defun wrap-hint-line (text width)
  "Wrap a footer hint line at its option boundaries — the two-space
gaps — so a row always holds whole \"[S]ell\"-style options and an
option is never split mid-hint the way plain WRAP-TEXT would split it.
A single option wider than WIDTH falls back to WRAP-TEXT."
  (let ((rows '())
        (row nil))
    (flet ((flush ()
             (when row
               (push row rows)
               (setf row nil))))
      (dolist (seg (%hint-segments text))
        (cond ((> (length seg) width)
               (flush)
               (dolist (part (wrap-text seg width))
                 (push part rows)))
              ((null row)
               (setf row seg))
              ((<= (+ (length row) 2 (length seg)) width)
               (setf row (concatenate 'string row "  " seg)))
              (t
               (flush)
               (setf row seg))))
      (flush))
    (or (nreverse rows) (list ""))))

(defun wrap-menu-line (line width)
  "Wrap a menu line like WRAP-TEXT, carrying its key (when it has one)
onto every wrapped row — a click on any row of a wrapped option picks
it.  A footer hint row (HINT-LINE-P) wraps at its option boundaries
instead, so hints never break mid-option.  A plain informational line
that OVERFLOWS the page and carries a two-space gap — the shop
header's \"NAME buys.  Gold: N gp\", any tabular row — breaks at its
gaps the same way, each segment keeping a whole row, instead of
mid-sentence where WRAP-TEXT happens to land; a line that fits passes
through untouched, gaps and all."
  (let ((key (menu-line-key line))
        (text (menu-line-text line)))
    (cond ((and (null key) (hint-line-p line))
           (wrap-hint-line text width))
          ((and (null key)
                (> (length text) width)
                (search "  " (string-trim " " text)))
           (wrap-hint-line (string-trim " " text) width))
          (t
           (mapcar (lambda (row) (if key (menu-option key row) row))
                   (wrap-text text width))))))

(defun %pack-hint-runs (lines width)
  "LINES with each run of consecutive hint rows re-packed onto as few
rows as WIDTH allows (whole options only) — the fallback for a page
too short to list one option per row."
  (let ((out '())
        (run '()))
    (flet ((flush ()
             (when run
               (let ((joined (format nil "~{~A~^  ~}" (nreverse run))))
                 (dolist (row (wrap-hint-line joined width))
                   (push row out)))
               (setf run nil))))
      (dolist (line lines)
        (if (hint-line-p line)
            (push line run)
            (progn (flush) (push line out))))
      (flush))
    (nreverse out)))

(defun %pack-options (options width)
  "OPTIONS — menu lines that each carry a key — packed onto rows of at
most WIDTH cells, whole options only.  Each row comes back as a packed
line (TEXT . SPANS): the option texts joined by the two-space gap, with
the cell span each one occupies paired to its key so a pointing
front-end can still tell which option the pointer is over.  The texts
are trimmed as they join — that is what lets the centered NEXT row give
up its padding and share a row rather than fall off the page."
  (let ((rows '())
        (row "")
        (spans '()))
    (labels ((flush ()
               (when (plusp (length row))
                 (push (cons row (nreverse spans)) rows)
                 (setf row "" spans '())))
             (place (text key)
               (let ((start (if (zerop (length row)) 0 (+ (length row) 2))))
                 (setf row (if (zerop (length row))
                               text
                               (concatenate 'string row "  " text)))
                 (push (list start (+ start (length text)) key) spans))))
      (dolist (option options)
        (let ((text (string-trim " " (menu-line-text option)))
              (key (menu-line-key option)))
          (unless (zerop (length text))
            ;; a row already holding something gives way when the next
            ;; option would run past the page's width
            (when (and (plusp (length row))
                       (> (+ (length row) 2 (length text)) width))
              (flush))
            (place text key))))
      (flush))
    (nreverse rows)))

(defun %packable-option-p (line)
  "True for a menu row the packer may share with its neighbours: an
option row whose key is a LETTER.  A digit-keyed row is a pick out of
a list — a shop's stock, a pack, a spell book — and keeps a row to
itself: its number has to read down the column against the numbers
above and below it, and a stock item that ran on into the page's
commands would read as one of them."
  (let ((key (menu-line-key line)))
    (and (characterp key) (not (digit-char-p key)))))

(defun %pack-run-minimally (options width overflow)
  "OPTIONS — a run of two or more packable rows — packed just far
enough to save OVERFLOW rows: the shortest prefix whose packing pays
that much shares rows (%PACK-OPTIONS) and the rest keep their own, so
the squeeze starts at the run's head and the page's foot — the
sheet's NEXT — is the last row to give up standing alone.  When even
the whole run packed cannot save enough, all of it packs.  Returns
\(VALUES ROWS SAVED)."
  (loop with n = (length options)
        for j from 2 to n
        for packed = (%pack-options (subseq options 0 j) width)
        for saved = (- j (length packed))
        when (or (>= saved overflow) (= j n))
          return (values (append packed (nthcdr j options)) saved)))

(defun %pack-option-runs (lines width overflow)
  "LINES with runs of two or more consecutive packable option rows
(%PACKABLE-OPTION-P) put onto shared WIDTH-wide rows until OVERFLOW
rows are saved — the last squeeze a page can make before it starts
losing rows off its foot, taken in page order and no further than the
page actually needs (%PACK-RUN-MINIMALLY): once the overflow is paid,
the rows that follow keep standing one option each.  A lone option
row passes through untouched, so a NEXT standing by itself on a page
that fits keeps the padding that centers it."
  (let ((out '())
        (run '()))
    (flet ((flush ()
             (when run
               (let ((options (nreverse run)))
                 (dolist (row (if (and (rest options) (plusp overflow))
                                  (multiple-value-bind (rows saved)
                                      (%pack-run-minimally options width
                                                           overflow)
                                    (decf overflow saved)
                                    rows)
                                  options))
                   (push row out)))
               (setf run nil))))
      (dolist (line lines)
        (if (%packable-option-p line)
            (push line run)
            (progn (flush) (push line out))))
      (flush))
    (nreverse out)))

(defun %drop-info-rows (lines overflow)
  "LINES with the first OVERFLOW plain informational rows dropped — the
rows that carry no key, no spans and no bracket hint: the page's title
and header, and never a pick, a command or a hint.  The true last
resort, after every squeeze: a page that still overflows loses its
FOOT to the caller's truncation, and the foot is the navigation — a
wrapped header or stock row must cost the garnish at the head, not the
keys at the bottom."
  (loop for line in lines
        if (and (plusp overflow)
                (null (menu-line-key line))
                (null (menu-line-spans line))
                (not (hint-line-p line)))
          do (decf overflow)
        else collect line))

(defun fit-menu-lines (lines rows width)
  "LINES wrapped for a ROWS x WIDTH page: each line through
WRAP-MENU-LINE, then squeezed progressively while they overflow —
first each run of consecutive footer hint rows packs onto shared rows
(whole options only, see %PACK-HINT-RUNS), then the blank spacer rows
go, then the page's command rows pack (%PACK-OPTION-RUNS — the
letter options only, in page order and only as far as the overflow
demands, so the foot unpacks first when there is room; a numbered
list row keeps its own row).  The
generators emit one option per row (the Bard's Tale look), so a roomy
page lists the options vertically and a tight page — the lores shop and
character sheet — packs them onto shared rows instead of running its
navigation off the bottom edge.  As the last resort the plain
informational rows drop from the head (%DROP-INFO-ROWS), so a page a
wrapped line pushed over can never lose its picks or its command foot.
A page that still overflows even then is the caller's to truncate."
  (let ((all (mapcan (lambda (line) (wrap-menu-line line width))
                     lines)))
    (when (> (length all) rows)
      (setf all (%pack-hint-runs all width)))
    (when (> (length all) rows)
      (setf all (delete-if (lambda (line)
                             (equal (menu-line-text line) ""))
                           all)))
    (when (> (length all) rows)
      (setf all (%pack-option-runs all width (- (length all) rows))))
    (when (> (length all) rows)
      (setf all (%drop-info-rows all (- (length all) rows))))
    all))

;;; Menu scrolling: a list longer than a page shows a full-page window
;;; of it.  u/d move the window (help-screen knowledge, no rows spent
;;; on hints) and a pointing front-end draws a clickable scrollbar
;;; beside the page from *MENU-SCROLL*.  Digits pick within the
;;; visible window (row 1 is the window's first row), which also keeps
;;; every item of a long list reachable with the single-digit keys the
;;; models speak.  The window math is pure and shared: the *-LINES
;;; renderers and the *-ACT key handlers both go through MENU-WINDOW,
;;; so what is drawn and what a digit picks can never disagree.

(defconstant +menu-page-size+ 7
  "Menu rows a scrolling list may occupy at once.  At most this many
items show whole; a longer list windows to this many rows and scrolls
with u/d (or the front-end's scrollbar).  Seven keeps every
party-sized list (+PARTY-LIMIT+ rows) un-scrolled.")

(defconstant +book-page-size+ 8
  "Entries the character sheet's spells/songs page windows at once —
one more than +MENU-PAGE-SIZE+, because the book rows are nearly all
that page carries: its head, the entries, a spacer and NEXT are the
lores takeover page's eleven rows exactly.  The pack and shop lists
spend those rows on their own command footers and stay at
+MENU-PAGE-SIZE+.")

(defun menu-window (n top &optional (page +menu-page-size+))
  "The visible window of an N-item list scrolled to offset TOP:
values (START END ABOVE-P BELOW-P), END exclusive.  A list of at most
PAGE items shows whole; a longer one shows PAGE items starting at TOP
(clamped so the window never runs off either end), ABOVE-P/BELOW-P
telling whether hidden items remain above/below."
  (if (<= n page)
      (values 0 n nil nil)
      (let* ((start (max 0 (min top (- n page))))
             (end (+ start page)))
        (values start end (> start 0) (< end n)))))

(defun menu-window-pick (items top digit &optional (page +menu-page-size+))
  "The element of ITEMS that DIGIT picks in the window at TOP — digit 1
is the window's first visible row — or NIL when the digit runs past
the window.  Returns (values ITEM ABSOLUTE-INDEX)."
  (multiple-value-bind (start end) (menu-window (length items) top page)
    (let ((index (+ start (1- digit))))
      (when (and (<= 1 digit) (< index end))
        (values (nth index items) index)))))

(defun menu-scroll (top char n &optional (page +menu-page-size+))
  "The scroll offset after key CHAR on an N-item list at TOP: u/U a
window up, d/D a window down (each clamped at the ends), or NIL when
CHAR is no scroll key or the list does not scroll."
  (when (and (characterp char) (> n page))
    (multiple-value-bind (start) (menu-window n top page)
      (case char
        ((#\u #\U) (max 0 (- start page)))
        ((#\d #\D) (min (- n page) (+ start page)))
        (t nil)))))

(defvar *menu-scroll* nil
  "Scroll geometry of the last list MENU-SCROLLED-LINES windowed —
(START END N), END exclusive — or NIL when that list fit its page
whole.  A pointing front-end clears it before generating a page's
lines and draws a scrollbar from what it finds afterwards: the thumb
spans the START..END share of the N items, a click above the thumb
scrolls a window up (u), below it down (d).  The keyboard needs no
rows for this — u/d live on the help screen.")

(defun menu-scrolled-lines (items top render &optional (page +menu-page-size+))
  "Menu rows for ITEMS scrolled to TOP: each visible item rendered by
RENDER — called with the 1-based display row number and the item, and
returning a menu line (see MENU-NUMBERED).  The section occupies at
most PAGE rows; a longer list windows to PAGE rows and reports its
scroll geometry through *MENU-SCROLL* for the front-ends' scrollbars."
  (multiple-value-bind (start end above below)
      (menu-window (length items) top page)
    (setf *menu-scroll* (when (or above below)
                          (list start end (length items))))
    (let ((i 0))
      (mapcar (lambda (item)
                (incf i)
                (funcall render i item))
              (subseq items start end)))))

(defun %menu-token-key (token)
  "The key a footer bracket token names: a single character stands for
itself, \"Esc\" and \"Return\" for those keys; range tokens (\"1-9\")
and anything else give NIL — the numbered option lines carry those."
  (cond ((= (length token) 1) (char token 0))
        ((string-equal token "Esc") #\Escape)
        ((string-equal token "Return") #\Return)
        (t nil)))

(defun menu-key-spans (text)
  "The clickable key hints in a footer line, by the generators' bracket
convention (\"[1-9] buy  [S]ell  [Esc] back\"): a list of
(START END KEY), END exclusive, each span running from its '[' over the
following words up to the next hint (two spaces or the next '[') so the
hint's label is part of its click target.  Range tokens and unmatched
brackets yield no span."
  (let ((spans '())
        (len (length text))
        (i 0))
    (loop
      (let ((open (position #\[ text :start i)))
        (unless open (return))
        (let ((close (position #\] text :start (1+ open))))
          (unless close (return))
          (let ((key (%menu-token-key (subseq text (1+ open) close)))
                ;; the span runs to the next hint: a "  " gap or '['
                (end (1+ close)))
            (loop while (and (< end len)
                             (char/= (char text end) #\[)
                             (not (and (char= (char text end) #\Space)
                                       (< (1+ end) len)
                                       (char= (char text (1+ end))
                                              #\Space))))
                  do (incf end))
            (loop while (and (> end (1+ close))
                             (char= (char text (1- end)) #\Space))
                  do (decf end))
            (when key
              (push (list open end key) spans))
            (setf i (1+ close))))))
    (nreverse spans)))

;;; ---------------------------------------------------------------------
;;; Story flags: arbitrary EQUAL-comparable keys the story sets and tests.
;;; Flags live in the save game, so anything stored must print readably.

(defun flag (game key)
  "The value of story flag KEY, or NIL when unset."
  (gethash key (game-flags game)))

(defun set-flag (game key &optional (value t))
  "Set story flag KEY to VALUE (default T)."
  (setf (gethash key (game-flags game)) value))

(defun clear-flag (game key)
  "Remove story flag KEY."
  (remhash key (game-flags game))
  (values))
