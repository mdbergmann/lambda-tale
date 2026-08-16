;;; Lambda's Tale — the Amiga sound backend.
;;;
;;; Turns the cue layer (src/sound.lisp) audible on AmigaOS: a zone's
;;; sound pack is read with READ-8SVX, every sample uploaded to chip
;;; RAM once, and one audio.device channel (cl-amiga's AMIGA.AUDIO
;;; module) plays the cues — a new cue cuts the one still sounding,
;;; which on a single channel is the honest Bard's Tale behavior.
;;; Everything degrades to silence: no :SFX zone, no pack files, no
;;; free channel (or no audio.device at all) simply leave
;;; *SOUND-BACKEND* uninstalled.
;;;
;;; AMIGA-SOUND-OPEN is idempotent per directory, so the front-end
;;; calls it wherever travel may have changed the zone (the same spots
;;; that swap the tile pack); AMIGA-SOUND-CLOSE gives channel and chip
;;; RAM back on the way out of PLAY-AMIGA.
;;;
;;; Below the cues sits the background-music layer: a location's
;;; (:MUSIC FILE) looping on a second, quieter channel while the party
;;; stands inside — see AMIGA-MUSIC-PLAY and friends at the bottom.

(require "amiga/audio")
(require "amiga/exec")

(in-package :tale)

(defstruct (amiga-sfx (:constructor %make-amiga-sfx))
  dir       ; the pack directory this state was loaded from
  audio     ; the AMIGA.AUDIO handle
  samples)  ; alist NAME -> (CHIP-PTR LENGTH PERIOD), vocabulary order

(defvar *amiga-sfx* nil
  "The open Amiga sound-pack state, or NIL while silent.")

(defun %amiga-sfx-play (name)
  "The *SOUND-BACKEND* the Amiga front-end installs: play cue NAME's
uploaded sample, cutting whatever still sounds.  Unshipped cues are
silent."
  (let ((entry (and *amiga-sfx*
                    (assoc name (amiga-sfx-samples *amiga-sfx*)))))
    (when entry
      (amiga.audio:play-sample (amiga-sfx-audio *amiga-sfx*)
                               (first (cdr entry))
                               (second (cdr entry))
                               :period (third (cdr entry))))))

(defun %amiga-sfx-upload (pack)
  "Upload PACK's samples ((NAME . SOUND) from LOAD-SOUND-PACK) to chip
RAM: an alist NAME -> (CHIP-PTR LENGTH PERIOD).  Paula plays words, so
an odd sample loses its last byte; one shorter than a word is dropped."
  (let ((uploaded '()))
    (dolist (entry pack (nreverse uploaded))
      (let* ((sound (cdr entry))
             (bytes (sound-bytes sound))
             (len (- (length bytes) (mod (length bytes) 2))))
        (when (plusp len)
          (push (list (car entry)
                      (amiga.exec:alloc-chip-bytes
                       (if (= len (length bytes))
                           bytes
                           (subseq bytes 0 len)))
                      len
                      (amiga.audio:period-for-rate (sound-rate sound)))
                uploaded))))))

(defun amiga-sound-open (dir)
  "Make DIR's sound pack the audible one, replacing whatever pack was
open; a NIL DIR (a zone without sounds) just closes.  Idempotent while
DIR is already the open pack — call it freely wherever travel may have
switched zones.  Returns the state, or NIL when staying silent (no
pack files, no free channel, no audio.device)."
  (unless (and *amiga-sfx* (equal dir (amiga-sfx-dir *amiga-sfx*)))
    (amiga-sound-close)
    (when dir
      (let ((pack (load-sound-pack dir)))
        (when pack
          (let ((audio (amiga.audio:open-audio)))
            (if audio
                (dlog-timed ("sound pack ~A to chip RAM" dir)
                  (setf *amiga-sfx*
                        (%make-amiga-sfx :dir dir :audio audio
                                         :samples (%amiga-sfx-upload pack)))
                  (setf *sound-backend* #'%amiga-sfx-play))
                (dlog "sound: no audio.device channel — staying silent")))))))
  *amiga-sfx*)

(defun amiga-sound-close ()
  "Silence and free everything AMIGA-SOUND-OPEN holds: the cue backend,
the chip-RAM samples, the audio.device channel."
  (when *amiga-sfx*
    (let ((state *amiga-sfx*))
      (setf *sound-backend* nil
            *amiga-sfx* nil)
      (amiga.audio:close-audio (amiga-sfx-audio state))
      (dolist (entry (amiga-sfx-samples state))
        (amiga:free-chip (second entry)))))
  nil)

;;; ---------------------------------------------------------------------
;;; Background music — a location's (:MUSIC FILE) looped while the
;;; party stands inside (see LOCATION-MUSIC-PATH).  Its own channel at
;;; a precedence anything louder may steal, its own chip-RAM sample,
;;; so the cue channel above keeps cutting cues the Bard's Tale way
;;; while the tune plays under them.  The same silence discipline: an
;;; unreadable file, no free channel or no chip RAM just stay quiet.

(defconstant +music-volume+ 40
  "The loop's Paula volume (0..64) — under the cues' full 64, so a
door or a coin still speaks over the tune.")

(defconstant +music-precedence+ -64
  "The music channel's allocation precedence: quiet background, first
to lose its channel to anyone louder.")

(defstruct (amiga-music (:constructor %make-amiga-music))
  file      ; the 8SVX file this state holds, or whose load failed
  audio     ; the AMIGA.AUDIO handle (its own channel), or NIL
  chip      ; the sample in chip RAM, or NIL when staying silent
  len
  period)

(defvar *amiga-music* nil
  "The open background-music state, or NIL while no tune is loaded.")

(defun amiga-music-play (file)
  "Loop FILE (an 8SVX) as background music until AMIGA-MUSIC-STOP —
FILE already being the loaded tune starts it over from the top.  A
sample past the device's write ceiling is cut to it (the loop just
comes around early).  Returns the state, or NIL when staying silent."
  (unless (and *amiga-music* (equal file (amiga-music-file *amiga-music*)))
    (amiga-music-close)
    (let ((sound (handler-case (read-8svx file)
                   (error (e)
                     (dlog "music ~A: ~A — staying silent" file e)
                     nil))))
      ;; a failed read still leaves a state carrying the file name, so
      ;; re-entering the same location does not re-read a bad file
      (setf *amiga-music* (%make-amiga-music :file file))
      (when sound
        (let* ((bytes (sound-bytes sound))
               (len (min (- (length bytes) (mod (length bytes) 2))
                         amiga.audio:+max-sample-bytes+)))
          (when (plusp len)
            (let ((audio (amiga.audio:open-audio
                          :precedence +music-precedence+)))
              (if audio
                  ;; a tune is dear chip RAM (up to the write ceiling)
                  ;; — a machine too tight for it stays silent, it
                  ;; does not lose the session
                  (let ((chip (handler-case
                                  (amiga.exec:alloc-chip-bytes
                                   (if (= len (length bytes))
                                       bytes
                                       (subseq bytes 0 len)))
                                (error (e)
                                  (dlog "music ~A: ~A — staying silent"
                                        file e)
                                  (amiga.audio:close-audio audio)
                                  nil))))
                    (when chip
                      (setf (amiga-music-audio *amiga-music*) audio
                            (amiga-music-chip *amiga-music*) chip
                            (amiga-music-len *amiga-music*) len
                            (amiga-music-period *amiga-music*)
                            (amiga.audio:period-for-rate
                             (sound-rate sound)))))
                  (dlog "music: no audio.device channel — staying silent"))))))))
  (when (and *amiga-music* (amiga-music-chip *amiga-music*))
    (amiga.audio:play-sample (amiga-music-audio *amiga-music*)
                             (amiga-music-chip *amiga-music*)
                             (amiga-music-len *amiga-music*)
                             :period (amiga-music-period *amiga-music*)
                             :volume +music-volume+
                             :cycles 0)
    *amiga-music*))

(defun amiga-music-sync (game)
  "Match the loop to where GAME stands: inside a location that names
:MUSIC the tune plays (from the top), anywhere else it stops.  The
call a save-game load needs by hand — LOAD-GAME re-enters the saved
location before WIRE attaches its :ENTER-LOCATION handler, so nothing
else starts the tune."
  (let ((path (location-music-path game)))
    (if path
        (amiga-music-play path)
        (amiga-music-stop))))

(defun amiga-music-stop ()
  "Silence the loop but keep the tune loaded — stepping back in
through the same door must not re-read the file."
  (when (and *amiga-music* (amiga-music-audio *amiga-music*))
    (amiga.audio:stop-sample (amiga-music-audio *amiga-music*)))
  nil)

(defun amiga-music-close ()
  "Free everything AMIGA-MUSIC-PLAY holds: the channel and the
chip-RAM sample."
  (when *amiga-music*
    (let ((state *amiga-music*))
      (setf *amiga-music* nil)
      (when (amiga-music-audio state)
        (amiga.audio:close-audio (amiga-music-audio state)))
      (when (amiga-music-chip state)
        (amiga:free-chip (amiga-music-chip state)))))
  nil)
