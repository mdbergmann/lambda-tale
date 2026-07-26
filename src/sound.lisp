;;; Lambda's Tale — sound: 8SVX samples and the cue layer.
;;;
;;; The engine speaks sound the way it speaks tiles: a fixed vocabulary
;;; of names (*SOUND-NAMES*, the wall-piece-names of audio), a pack — a
;;; directory of NAME.8svx files a zone declares with (ZONE :SFX DIR) —
;;; and a front-end that decides what a cue turns into.  PLAY-SOUND
;;; funnels every cue through *SOUND-BACKEND*: NIL (the default, and
;;; the host front-end's setting) is silence, the Amiga front-end
;;; installs a function that plays the pack's sample for the cue, and
;;; the test suite installs a recorder.  ATTACH-SOUNDS turns the
;;; engine's events into cues the same way ATTACH-MESSAGE-LOG turns
;;; them into log lines — mechanics never call PLAY-SOUND directly,
;;; they emit; a pack may ship any subset of the vocabulary and the
;;; missing cues stay silent.
;;;
;;; The file format is IFF 8SVX (signed 8-bit samples — exactly what
;;; Paula plays), read whole like src/ilbm.lisp reads ILBM, sharing its
;;; big-endian byte plumbing.  Compression 0 (none) and 1 (Fibonacci-
;;; delta) are understood; WRITE-8SVX writes uncompressed — it exists
;;; so asset generators and the test suite need no second writer.

(in-package :tale)

;;; ---------------------------------------------------------------------
;;; The sample model

(defstruct (sound (:constructor %make-sound))
  (rate 8000)   ; samples per second
  bytes)        ; (unsigned-byte 8) vector of two's-complement samples

(defun make-sound (rate bytes)
  "A SOUND playing the (UNSIGNED-BYTE 8) vector BYTES — signed 8-bit
samples in two's-complement — at RATE samples per second."
  (%make-sound :rate rate :bytes bytes))

;;; ---------------------------------------------------------------------
;;; Fibonacci-delta decompression (8SVX.doc appendix; sCompression 1).
;;; Two header bytes (a pad and the seed value), then two 4-bit delta
;;; codes per byte, high nibble first — each code indexes the delta
;;; table and moves the running sample value.

(defparameter *fibonacci-deltas*
  #(-34 -21 -13 -8 -5 -3 -2 -1 0 1 2 3 5 8 13 21))

(defun %unpack-fibonacci (src pos len file)
  "Decode LEN bytes of Fibonacci-delta data at SRC[POS] into the
2*(LEN-2) sample bytes they carry.  Signals on truncated data."
  (when (< len 3)
    (error "8SVX ~A: Fibonacci-delta BODY too short (~D bytes)" file len))
  (when (> (+ pos len) (length src))
    (error "8SVX ~A: truncated Fibonacci-delta BODY" file))
  (let* ((n (* 2 (- len 2)))
         (out (make-array n :element-type '(unsigned-byte 8)))
         (seed (aref src (1+ pos)))
         (v (if (> seed 127) (- seed 256) seed))
         (i 0))
    (flet ((emit (code)
             (setf v (max -128 (min 127 (+ v (svref *fibonacci-deltas* code)))))
             (setf (aref out i) (logand v 255))
             (incf i)))
      (loop for p from (+ pos 2) below (+ pos len)
            do (let ((byte (aref src p)))
                 (emit (ash byte -4))
                 (emit (logand byte 15)))))
    out))

;;; ---------------------------------------------------------------------
;;; Reading and writing IFF 8SVX

(defun read-8svx (file)
  "Read an IFF 8SVX FILE; returns a SOUND.  Understands uncompressed
and Fibonacci-delta BODYs; signals a clear error on anything that is
not a well-formed one-octave 8SVX."
  (dlog-timed ("sound ~A" file)
    (%read-8svx file)))

(defun %read-8svx (file)
  (let ((bytes
          (with-open-file (s file :element-type '(unsigned-byte 8))
            (let ((v (make-array (file-length s)
                                 :element-type '(unsigned-byte 8))))
              (read-sequence v s)
              v))))
    (when (or (< (length bytes) 12)
              (string/= (%chunk-id bytes 0) "FORM")
              (string/= (%chunk-id bytes 8) "8SVX"))
      (error "8SVX ~A: not an IFF FORM 8SVX file" file))
    (let ((form-end (min (length bytes) (+ 8 (%u32be bytes 4))))
          (pos 12)
          (rate nil)
          (compression 0)
          (samples nil))
      (loop while (<= (+ pos 8) form-end)
            do (let* ((id (%chunk-id bytes pos))
                      (len (%u32be bytes (+ pos 4)))
                      (data (+ pos 8)))
                 (when (> (+ data len) (length bytes))
                   (error "8SVX ~A: chunk ~A (~D bytes) runs past end of file"
                          file id len))
                 (cond
                   ((string= id "VHDR")
                    (when (< len 16)
                      (error "8SVX ~A: VHDR chunk too short" file))
                    (setf rate (%u16be bytes (+ data 12))
                          compression (aref bytes (+ data 15)))
                    (unless (member compression '(0 1))
                      (error "8SVX ~A: unsupported compression ~D ~
(only sCmpNone/sCmpFibDelta)" file compression))
                    (when (zerop rate)
                      (error "8SVX ~A: VHDR samples-per-second is zero" file)))
                   ((string= id "BODY")
                    (unless rate
                      (error "8SVX ~A: BODY before VHDR" file))
                    (setf samples
                          (if (= compression 1)
                              (%unpack-fibonacci bytes data len file)
                              (subseq bytes data (+ data len)))))
                   (t nil))                 ; skip NAME, ANNO, CHAN, ...
                 (setf pos (+ data len (mod len 2)))))  ; chunks pad to even
      (unless rate
        (error "8SVX ~A: no VHDR chunk" file))
      (unless samples
        (error "8SVX ~A: no BODY chunk" file))
      (%make-sound :rate rate :bytes samples))))

(defun %u32be-list (n)
  (list (ldb (byte 8 24) n) (ldb (byte 8 16) n)
        (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun write-8svx (file sound)
  "Write SOUND to FILE as a one-shot, one-octave, uncompressed IFF
8SVX.  Returns FILE."
  (let* ((samples (sound-bytes sound))
         (n (length samples))
         (body-pad (mod n 2))
         ;; FORM size: "8SVX" + VHDR header (8) + VHDR body (20)
         ;;          + BODY header (8) + samples + pad
         (form-size (+ 4 8 20 8 n body-pad))
         (header '()))
    (flet ((emit-string (s) (dolist (c (coerce s 'list))
                              (push (char-code c) header)))
           (emit-u32 (v) (dolist (b (%u32be-list v)) (push b header))))
      (emit-string "FORM") (emit-u32 form-size)
      (emit-string "8SVX")
      (emit-string "VHDR") (emit-u32 20)
      (emit-u32 n)                     ; oneShotHiSamples
      (emit-u32 0)                     ; repeatHiSamples
      (emit-u32 32)                    ; samplesPerHiCycle (conventional)
      (push (ldb (byte 8 8) (sound-rate sound)) header)   ; samplesPerSec
      (push (ldb (byte 8 0) (sound-rate sound)) header)
      (push 1 header)                  ; ctOctave
      (push 0 header)                  ; sCompression = none
      (emit-u32 65536)                 ; volume, fixed-point 1.0
      (emit-string "BODY") (emit-u32 n))
    (with-open-file (s file :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence (coerce (nreverse header) 'vector) s)
      (write-sequence samples s)
      (when (plusp body-pad)
        (write-byte 0 s))))
  file)

;;; ---------------------------------------------------------------------
;;; The cue vocabulary — the names a sound pack may ship, each a
;;; NAME.8svx in the zone's (ZONE :SFX DIR) directory.  A pack ships
;;; any subset; missing cues are silent.  HIT.8svx doubles as the
;;; pack's probe file (see ZONE-SFX-DIR) — every pack should carry it.

(defparameter *sound-names*
  '(hit        ; the party lands a blow
    slay       ; ... and it fells the monster
    miss       ; ... or it misses
    hurt       ; a monster lands a blow on a hero
    death      ; a hero falls
    combat     ; an encounter begins
    victory    ; combat won
    defeat     ; the party is beaten
    door       ; the party steps through a door
    blocked    ; the party bumps a wall
    cast       ; a spell is cast
    song       ; a bard strikes up a song
    level      ; a hero rises a level
    coin       ; gold changes hands (buy, sell)
    drink)     ; a tavern drink
  "The engine's sound-cue vocabulary, in no particular order.  The
names are the pack contract: a zone's :SFX directory holds NAME.8svx
for each cue it wants audible.")

;;; ---------------------------------------------------------------------
;;; The backend hook

(defvar *sound-backend* nil
  "How a cue becomes audible: NIL (the default) is silence, else a
function of one argument — the cue name, a symbol from *SOUND-NAMES* —
called on every PLAY-SOUND.  The Amiga front-end installs a Paula
player; the test suite installs a recorder.")

(defun play-sound (name)
  "Sound cue NAME through *SOUND-BACKEND* — a no-op when no backend is
installed.  Returns NAME."
  (when *sound-backend*
    (funcall *sound-backend* name))
  name)

;;; ---------------------------------------------------------------------
;;; Event wiring — the sound analogue of ATTACH-MESSAGE-LOG.

(defun attach-sounds (game)
  "Subscribe the sound cues to GAME's events: each mapped event plays
its *SOUND-NAMES* cue through *SOUND-BACKEND*.  Attach once per game,
next to the message log.  Returns GAME."
  (flet ((cue (topic name)
           (on-event game topic
                     (lambda (g &rest args)
                       (declare (ignore g args))
                       (play-sound name)))))
    (cue :hit 'hit)
    (cue :slay 'slay)
    (cue :miss 'miss)
    (cue :hero-hurt 'hurt)
    (cue :hero-died 'death)
    (cue :combat-start 'combat)
    (cue :door 'door)
    (cue :blocked 'blocked)
    (cue :spell-cast 'cast)
    (cue :song-sung 'song)
    (cue :level-up 'level)
    (cue :coin 'coin)
    (cue :drink 'drink)
    (on-event game :combat-end
              (lambda (g result)
                (declare (ignore g))
                (case result
                  (:victory (play-sound 'victory))
                  (:defeat (play-sound 'defeat))))))
  game)

;;; ---------------------------------------------------------------------
;;; Sound packs

(defun zone-sfx-dir (game)
  "The current zone's declared sound pack, or NIL: the map's
(ZONE :SFX DIR), resolved like ZONE-GFX-DIR — relative to the map
file's directory when the pack lives there, else relative to the game
directory.  The probe is the hit.8svx every pack should hold."
  (let* ((map (game-map game))
         (sfx (dungeon-map-sfx map)))
    (when sfx
      (let ((local (%resolve-map-path (dungeon-map-name map) sfx)))
        (if (probe-file (concatenate 'string local "hit.8svx"))
            local
            sfx)))))

(defun load-sound-pack (dir)
  "Read DIR's cue samples: an alist of (NAME . SOUND) for every
*SOUND-NAMES* cue whose NAME.8svx exists there, in vocabulary order.
Cues without a file are simply absent — they stay silent."
  (dlog-timed ("sound pack ~A" dir)
    (let ((pack '()))
      (dolist (name *sound-names* (nreverse pack))
        (let ((file (concatenate 'string dir
                                 (string-downcase (symbol-name name))
                                 ".8svx")))
          (when (probe-file file)
            (push (cons name (read-8svx file)) pack)))))))
