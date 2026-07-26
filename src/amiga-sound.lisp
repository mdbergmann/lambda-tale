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
