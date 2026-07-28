;;; Lambda's Tale — the microfont: the engine's own pixel faces.
;;;
;;; The engine brings its own type instead of an Amiga system font,
;;; printable ASCII 32-126, drawn in the bold manner of the classic
;;; Bard's Tale screens, in two faces:
;;;
;;; - the display face: 7x7 glyphs on an 8x8 cell (topaz 8's width),
;;;   slab serifs where the box allows — kept for display-size use;
;;; - the small face: 5x7 glyphs on a 6x8 cell, the condensed bold
;;;   cut the engine's pages actually set — the message log, the
;;;   takeover and overlay menus and the whole map page.  The real
;;;   Bard's Tale II text measures ~6px per character at the same
;;;   height, which is what this face reproduces.
;;;
;;; MICROFONT-LINE / MICROFONT-SMALL-LINE render a text line into a
;;; chunky pen buffer (row-major (unsigned-byte 8) vector) that the
;;; Amiga front-end uploads with AMIGA.GFX:WRITE-CHUNKY — either
;;; straight into the window or into a cached offscreen bitmap (see
;;; %LOG-LINE-BITMAP in amiga-ui.lisp).  Pure pixel math, no OS calls,
;;; so the host test suite covers the glyphs and the layout.

(in-package :tale)

(defconstant +microfont-glyph-width+ 7)
(defconstant +microfont-glyph-height+ 7)
(defconstant +microfont-advance+ 8)      ; glyph + 1 column spacing
(defconstant +microfont-line-height+ 8)  ; glyph + 1 row spacing

;;; Each glyph is 7 rows, top to bottom, of 7 bits — #b1000000 is the
;;; leftmost pixel.  Index = char-code - 32.
(defparameter *microfont-glyphs*
  (vector
   ;; space
   #(#b0000000 #b0000000 #b0000000 #b0000000 #b0000000 #b0000000 #b0000000)
   ;; !
   #(#b0011000 #b0011000 #b0011000 #b0011000 #b0011000 #b0000000 #b0011000)
   ;; "
   #(#b0110110 #b0110110 #b0100100 #b0000000 #b0000000 #b0000000 #b0000000)
   ;; #
   #(#b0110110 #b0110110 #b1111111 #b0110110 #b1111111 #b0110110 #b0110110)
   ;; $
   #(#b0011000 #b0111111 #b1101000 #b0111110 #b0001011 #b1111110 #b0011000)
   ;; %
   #(#b1100001 #b1100011 #b0000110 #b0011000 #b0110000 #b1100011 #b1000011)
   ;; &
   #(#b0111000 #b1101100 #b1101100 #b0111000 #b1101101 #b1100110 #b0111011)
   ;; '
   #(#b0011000 #b0011000 #b0110000 #b0000000 #b0000000 #b0000000 #b0000000)
   ;; (
   #(#b0001100 #b0011000 #b0110000 #b0110000 #b0110000 #b0011000 #b0001100)
   ;; )
   #(#b0110000 #b0011000 #b0001100 #b0001100 #b0001100 #b0011000 #b0110000)
   ;; *
   #(#b0000000 #b0011000 #b1101011 #b0111110 #b1101011 #b0011000 #b0000000)
   ;; +
   #(#b0000000 #b0011000 #b0011000 #b1111110 #b0011000 #b0011000 #b0000000)
   ;; ,
   #(#b0000000 #b0000000 #b0000000 #b0000000 #b0011000 #b0011000 #b0110000)
   ;; -
   #(#b0000000 #b0000000 #b0000000 #b1111110 #b0000000 #b0000000 #b0000000)
   ;; .
   #(#b0000000 #b0000000 #b0000000 #b0000000 #b0000000 #b0011000 #b0011000)
   ;; /
   #(#b0000011 #b0000110 #b0001100 #b0011000 #b0110000 #b1100000 #b1000000)
   ;; 0
   #(#b0111100 #b1100110 #b1100110 #b1100110 #b1100110 #b1100110 #b0111100)
   ;; 1
   #(#b0011000 #b0111000 #b0011000 #b0011000 #b0011000 #b0011000 #b1111110)
   ;; 2
   #(#b0111100 #b1100110 #b0000110 #b0001100 #b0011000 #b0110000 #b1111111)
   ;; 3
   #(#b0111100 #b1100110 #b0000110 #b0011100 #b0000110 #b1100110 #b0111100)
   ;; 4
   #(#b0001100 #b0011100 #b0111100 #b1101100 #b1111111 #b0001100 #b0011110)
   ;; 5
   #(#b1111110 #b1100000 #b1111100 #b0000110 #b0000110 #b1100110 #b0111100)
   ;; 6
   #(#b0011100 #b0110000 #b1100000 #b1111100 #b1100110 #b1100110 #b0111100)
   ;; 7
   #(#b1111110 #b0000110 #b0001100 #b0011000 #b0110000 #b0110000 #b0110000)
   ;; 8
   #(#b0111100 #b1100110 #b1100110 #b0111100 #b1100110 #b1100110 #b0111100)
   ;; 9
   #(#b0111100 #b1100110 #b1100110 #b0111110 #b0000110 #b0001100 #b0111000)
   ;; :
   #(#b0000000 #b0011000 #b0011000 #b0000000 #b0011000 #b0011000 #b0000000)
   ;; ;
   #(#b0000000 #b0011000 #b0011000 #b0000000 #b0011000 #b0011000 #b0110000)
   ;; <
   #(#b0000110 #b0011000 #b0110000 #b1100000 #b0110000 #b0011000 #b0000110)
   ;; =
   #(#b0000000 #b0000000 #b1111110 #b0000000 #b1111110 #b0000000 #b0000000)
   ;; >
   #(#b0110000 #b0011000 #b0001100 #b0000110 #b0001100 #b0011000 #b0110000)
   ;; ?
   #(#b0111100 #b1100110 #b0000110 #b0001100 #b0011000 #b0000000 #b0011000)
   ;; @
   #(#b0111100 #b1100110 #b1101110 #b1101110 #b1101100 #b1100000 #b0111110)
   ;; A
   #(#b0011000 #b0111100 #b1100110 #b1100110 #b1111110 #b1100110 #b1110111)
   ;; B
   #(#b1111100 #b0110110 #b0110110 #b0111100 #b0110110 #b0110110 #b1111100)
   ;; C
   #(#b0111100 #b1100110 #b1100000 #b1100000 #b1100000 #b1100110 #b0111100)
   ;; D
   #(#b1111100 #b0110110 #b0110011 #b0110011 #b0110011 #b0110110 #b1111100)
   ;; E
   #(#b1111111 #b0110011 #b0110000 #b0111100 #b0110000 #b0110011 #b1111111)
   ;; F
   #(#b1111111 #b0110011 #b0110000 #b0111100 #b0110000 #b0110000 #b1111000)
   ;; G
   #(#b0111100 #b1100110 #b1100000 #b1101111 #b1100110 #b1100110 #b0111110)
   ;; H
   #(#b1110111 #b0110110 #b0110110 #b0111110 #b0110110 #b0110110 #b1110111)
   ;; I
   #(#b0111100 #b0011000 #b0011000 #b0011000 #b0011000 #b0011000 #b0111100)
   ;; J
   #(#b0011110 #b0001100 #b0001100 #b0001100 #b0001100 #b1101100 #b0111000)
   ;; K
   #(#b1110011 #b0110110 #b0111100 #b0111000 #b0111100 #b0110110 #b1110011)
   ;; L
   #(#b1111000 #b0110000 #b0110000 #b0110000 #b0110000 #b0110011 #b1111111)
   ;; M
   #(#b1100011 #b1110111 #b1111111 #b1101011 #b1100011 #b1100011 #b1110111)
   ;; N
   #(#b1100011 #b1110011 #b1101011 #b1100111 #b1100011 #b1100011 #b1100011)
   ;; O
   #(#b0111100 #b1100110 #b1100110 #b1100110 #b1100110 #b1100110 #b0111100)
   ;; P
   #(#b1111110 #b0110011 #b0110011 #b0111110 #b0110000 #b0110000 #b1111000)
   ;; Q
   #(#b0111100 #b1100110 #b1100110 #b1100110 #b1101110 #b1100110 #b0111101)
   ;; R
   #(#b1111110 #b0110011 #b0110011 #b0111110 #b0110110 #b0110011 #b1110011)
   ;; S
   #(#b0111110 #b1100000 #b1100000 #b0111100 #b0000110 #b0000110 #b1111100)
   ;; T
   #(#b1111111 #b1101011 #b0011000 #b0011000 #b0011000 #b0011000 #b0111100)
   ;; U
   #(#b1110111 #b0110110 #b0110110 #b0110110 #b0110110 #b0110110 #b0111100)
   ;; V
   #(#b1110111 #b0110110 #b0110110 #b0110110 #b0011100 #b0011100 #b0011000)
   ;; W
   #(#b1100011 #b1100011 #b1101011 #b1101011 #b1101011 #b1111111 #b0110110)
   ;; X
   #(#b1110111 #b0110110 #b0011100 #b0011000 #b0011100 #b0110110 #b1110111)
   ;; Y
   #(#b1110111 #b0110110 #b0110110 #b0011100 #b0011000 #b0011000 #b0111100)
   ;; Z
   #(#b1111111 #b0000110 #b0001100 #b0011000 #b0110000 #b1100000 #b1111111)
   ;; [
   #(#b0111100 #b0110000 #b0110000 #b0110000 #b0110000 #b0110000 #b0111100)
   ;; backslash
   #(#b1000000 #b1100000 #b0110000 #b0011000 #b0001100 #b0000110 #b0000011)
   ;; ]
   #(#b0111100 #b0001100 #b0001100 #b0001100 #b0001100 #b0001100 #b0111100)
   ;; ^
   #(#b0011000 #b0111100 #b1100110 #b0000000 #b0000000 #b0000000 #b0000000)
   ;; _
   #(#b0000000 #b0000000 #b0000000 #b0000000 #b0000000 #b0000000 #b1111111)
   ;; `
   #(#b0110000 #b0011000 #b0001100 #b0000000 #b0000000 #b0000000 #b0000000)
   ;; a
   #(#b0000000 #b0000000 #b0111100 #b0000110 #b0111110 #b1100110 #b0111011)
   ;; b
   #(#b1100000 #b1100000 #b1111100 #b1100110 #b1100110 #b1100110 #b1111100)
   ;; c
   #(#b0000000 #b0000000 #b0111100 #b1100110 #b1100000 #b1100110 #b0111100)
   ;; d
   #(#b0000110 #b0000110 #b0111110 #b1100110 #b1100110 #b1100110 #b0111110)
   ;; e
   #(#b0000000 #b0000000 #b0111100 #b1100110 #b1111110 #b1100000 #b0111110)
   ;; f
   #(#b0011100 #b0110110 #b0110000 #b1111000 #b0110000 #b0110000 #b1111000)
   ;; g
   #(#b0000000 #b0111110 #b1100110 #b1100110 #b0111110 #b0000110 #b0111100)
   ;; h
   #(#b1100000 #b1100000 #b1101100 #b1110110 #b1100110 #b1100110 #b1110111)
   ;; i
   #(#b0011000 #b0000000 #b0111000 #b0011000 #b0011000 #b0011000 #b0111100)
   ;; j
   #(#b0001100 #b0000000 #b0011100 #b0001100 #b0001100 #b1101100 #b0111000)
   ;; k
   #(#b1100000 #b1100000 #b1100110 #b1101100 #b1111000 #b1101100 #b1100111)
   ;; l
   #(#b0111000 #b0011000 #b0011000 #b0011000 #b0011000 #b0011000 #b0111100)
   ;; m
   #(#b0000000 #b0000000 #b1111111 #b1101011 #b1101011 #b1101011 #b1101011)
   ;; n
   #(#b0000000 #b0000000 #b1101100 #b1110110 #b1100110 #b1100110 #b1110111)
   ;; o
   #(#b0000000 #b0000000 #b0111100 #b1100110 #b1100110 #b1100110 #b0111100)
   ;; p
   #(#b0000000 #b0000000 #b1111100 #b1100110 #b1111100 #b1100000 #b1111000)
   ;; q
   #(#b0000000 #b0000000 #b0111110 #b1100110 #b0111110 #b0000110 #b0001111)
   ;; r
   #(#b0000000 #b0000000 #b1101110 #b0111011 #b0110000 #b0110000 #b1111000)
   ;; s
   #(#b0000000 #b0000000 #b0111110 #b1100000 #b0111100 #b0000110 #b1111100)
   ;; t
   #(#b0110000 #b0110000 #b1111100 #b0110000 #b0110000 #b0110011 #b0011110)
   ;; u
   #(#b0000000 #b0000000 #b1100110 #b1100110 #b1100110 #b1100111 #b0111011)
   ;; v
   #(#b0000000 #b0000000 #b1110111 #b0110110 #b0110110 #b0011100 #b0011000)
   ;; w
   #(#b0000000 #b0000000 #b1100011 #b1101011 #b1101011 #b1111111 #b0110110)
   ;; x
   #(#b0000000 #b0000000 #b1100110 #b0111100 #b0011000 #b0111100 #b1100110)
   ;; y
   #(#b0000000 #b0000000 #b1100110 #b1100110 #b0111110 #b0000110 #b0111100)
   ;; z
   #(#b0000000 #b0000000 #b1111110 #b0001100 #b0011000 #b0110000 #b1111110)
   ;; {
   #(#b0001100 #b0011000 #b0011000 #b0110000 #b0011000 #b0011000 #b0001100)
   ;; |
   #(#b0011000 #b0011000 #b0011000 #b0011000 #b0011000 #b0011000 #b0011000)
   ;; }
   #(#b0110000 #b0011000 #b0011000 #b0001100 #b0011000 #b0011000 #b0110000)
   ;; ~
   #(#b0000000 #b0000000 #b0000000 #b0111011 #b1101110 #b0000000 #b0000000)))

(defparameter *microfont-fallback*
  #(#b1111111 #b1000001 #b1000001 #b1000001 #b1000001 #b1000001 #b1111111)
  "A hollow box, drawn for any character outside ASCII 32-126.")

(defun microfont-glyph (char)
  "The 7-row bit pattern for CHAR, or the fallback box."
  (let ((i (- (char-code char) 32)))
    (if (and (>= i 0) (< i (length *microfont-glyphs*)))
        (aref *microfont-glyphs* i)
        *microfont-fallback*)))

;;; The small face: compact 5x7 glyphs on a 6px advance, drawn bold —
;;; two-pixel stems wherever five columns allow, one-pixel strokes
;;; only where a diagonal needs the room — in the condensed manner of
;;; the actual Bard's Tale II screens (measured off the Amiga
;;; original: ~6px per character at the same 7px height).  This is
;;; the engine's page face: the message log, the takeover and overlay
;;; menus and the whole map page set in it; the wide bold face above
;;; remains for display-size use.  Same row-of-bits layout, #b10000
;;; leftmost.

(defconstant +microfont-small-width+ 5)
(defconstant +microfont-small-advance+ 6)  ; glyph + 1 column spacing

(defparameter *microfont-small-glyphs*
  (vector
   ;; space
   #(#b00000 #b00000 #b00000 #b00000 #b00000 #b00000 #b00000)
   ;; !
   #(#b01100 #b01100 #b01100 #b01100 #b01100 #b00000 #b01100)
   ;; "
   #(#b11011 #b11011 #b00000 #b00000 #b00000 #b00000 #b00000)
   ;; #
   #(#b01010 #b01010 #b11111 #b01010 #b11111 #b01010 #b01010)
   ;; $
   #(#b00100 #b01111 #b10100 #b01110 #b00101 #b11110 #b00100)
   ;; %
   #(#b11001 #b11010 #b00010 #b00100 #b01000 #b01011 #b10011)
   ;; &
   #(#b01100 #b10010 #b10100 #b01000 #b10101 #b10010 #b01101)
   ;; '
   #(#b00110 #b00110 #b01100 #b00000 #b00000 #b00000 #b00000)
   ;; (
   #(#b00011 #b00110 #b01100 #b01100 #b01100 #b00110 #b00011)
   ;; )
   #(#b11000 #b01100 #b00110 #b00110 #b00110 #b01100 #b11000)
   ;; *
   #(#b00000 #b00100 #b10101 #b01110 #b10101 #b00100 #b00000)
   ;; +
   #(#b00000 #b00100 #b00100 #b11111 #b00100 #b00100 #b00000)
   ;; ,
   #(#b00000 #b00000 #b00000 #b00000 #b01100 #b01100 #b11000)
   ;; -
   #(#b00000 #b00000 #b00000 #b11111 #b00000 #b00000 #b00000)
   ;; .
   #(#b00000 #b00000 #b00000 #b00000 #b00000 #b01100 #b01100)
   ;; /
   #(#b00001 #b00011 #b00110 #b01100 #b11000 #b10000 #b00000)
   ;; 0
   #(#b01110 #b11011 #b11011 #b11011 #b11011 #b11011 #b01110)
   ;; 1
   #(#b00110 #b01110 #b00110 #b00110 #b00110 #b00110 #b01111)
   ;; 2
   #(#b01110 #b11011 #b00011 #b00110 #b01100 #b11000 #b11111)
   ;; 3
   #(#b11110 #b00011 #b00011 #b01110 #b00011 #b00011 #b11110)
   ;; 4
   #(#b00111 #b01111 #b11011 #b11011 #b11111 #b00011 #b00011)
   ;; 5
   #(#b11111 #b11000 #b11110 #b00011 #b00011 #b11011 #b01110)
   ;; 6
   #(#b01110 #b11000 #b11110 #b11011 #b11011 #b11011 #b01110)
   ;; 7
   #(#b11111 #b00011 #b00011 #b00110 #b00110 #b01100 #b01100)
   ;; 8
   #(#b01110 #b11011 #b11011 #b01110 #b11011 #b11011 #b01110)
   ;; 9
   #(#b01110 #b11011 #b11011 #b01111 #b00011 #b00011 #b01110)
   ;; :
   #(#b00000 #b01100 #b01100 #b00000 #b01100 #b01100 #b00000)
   ;; ;
   #(#b00000 #b01100 #b01100 #b00000 #b01100 #b01100 #b11000)
   ;; <
   #(#b00011 #b00110 #b01100 #b11000 #b01100 #b00110 #b00011)
   ;; =
   #(#b00000 #b00000 #b11111 #b00000 #b11111 #b00000 #b00000)
   ;; >
   #(#b11000 #b01100 #b00110 #b00011 #b00110 #b01100 #b11000)
   ;; ?
   #(#b01110 #b11011 #b00011 #b00110 #b01100 #b00000 #b01100)
   ;; @
   #(#b01110 #b10001 #b00001 #b01101 #b10101 #b10101 #b01110)
   ;; A
   #(#b01110 #b11011 #b11011 #b11111 #b11011 #b11011 #b11011)
   ;; B
   #(#b11110 #b11011 #b11011 #b11110 #b11011 #b11011 #b11110)
   ;; C
   #(#b01110 #b11011 #b11000 #b11000 #b11000 #b11011 #b01110)
   ;; D
   #(#b11110 #b11011 #b11011 #b11011 #b11011 #b11011 #b11110)
   ;; E
   #(#b11111 #b11000 #b11000 #b11110 #b11000 #b11000 #b11111)
   ;; F
   #(#b11111 #b11000 #b11000 #b11110 #b11000 #b11000 #b11000)
   ;; G
   #(#b01111 #b11000 #b11000 #b11011 #b11011 #b11011 #b01111)
   ;; H
   #(#b11011 #b11011 #b11011 #b11111 #b11011 #b11011 #b11011)
   ;; I
   #(#b11111 #b00100 #b00100 #b00100 #b00100 #b00100 #b11111)
   ;; J
   #(#b01111 #b00011 #b00011 #b00011 #b00011 #b11011 #b01110)
   ;; K
   #(#b11011 #b11011 #b11110 #b11100 #b11110 #b11011 #b11011)
   ;; L
   #(#b11000 #b11000 #b11000 #b11000 #b11000 #b11000 #b11111)
   ;; M
   #(#b10001 #b11011 #b11111 #b10101 #b10001 #b10001 #b10001)
   ;; N
   #(#b10001 #b11001 #b11101 #b10111 #b10011 #b10001 #b10001)
   ;; O
   #(#b01110 #b11011 #b11011 #b11011 #b11011 #b11011 #b01110)
   ;; P
   #(#b11110 #b11011 #b11011 #b11110 #b11000 #b11000 #b11000)
   ;; Q
   #(#b01110 #b11011 #b11011 #b11011 #b11011 #b01110 #b00011)
   ;; R
   #(#b11110 #b11011 #b11011 #b11110 #b11011 #b11011 #b11011)
   ;; S
   #(#b01111 #b11000 #b11000 #b01110 #b00011 #b00011 #b11110)
   ;; T
   #(#b11111 #b10101 #b00100 #b00100 #b00100 #b00100 #b01110)
   ;; U
   #(#b11011 #b11011 #b11011 #b11011 #b11011 #b11011 #b01110)
   ;; V
   #(#b11011 #b11011 #b11011 #b11011 #b11011 #b01110 #b00100)
   ;; W
   #(#b10001 #b10001 #b10001 #b10101 #b11111 #b11011 #b10001)
   ;; X
   #(#b11011 #b11011 #b01110 #b00100 #b01110 #b11011 #b11011)
   ;; Y
   #(#b11011 #b11011 #b11011 #b01110 #b00100 #b00100 #b01110)
   ;; Z
   #(#b11111 #b00011 #b00110 #b01100 #b11000 #b11000 #b11111)
   ;; [
   #(#b01111 #b01100 #b01100 #b01100 #b01100 #b01100 #b01111)
   ;; backslash
   #(#b10000 #b11000 #b01100 #b00110 #b00011 #b00001 #b00000)
   ;; ]
   #(#b11110 #b00110 #b00110 #b00110 #b00110 #b00110 #b11110)
   ;; ^
   #(#b00100 #b01110 #b11011 #b00000 #b00000 #b00000 #b00000)
   ;; _
   #(#b00000 #b00000 #b00000 #b00000 #b00000 #b00000 #b11111)
   ;; `
   #(#b01100 #b00110 #b00000 #b00000 #b00000 #b00000 #b00000)
   ;; a
   #(#b00000 #b00000 #b01110 #b00011 #b01111 #b11011 #b01111)
   ;; b
   #(#b11000 #b11000 #b11110 #b11011 #b11011 #b11011 #b11110)
   ;; c
   #(#b00000 #b00000 #b01111 #b11000 #b11000 #b11000 #b01111)
   ;; d
   #(#b00011 #b00011 #b01111 #b11011 #b11011 #b11011 #b01111)
   ;; e
   #(#b00000 #b00000 #b01110 #b11011 #b11111 #b11000 #b01111)
   ;; f
   #(#b00111 #b01100 #b11110 #b01100 #b01100 #b01100 #b01100)
   ;; g
   #(#b00000 #b01111 #b11011 #b11011 #b01111 #b00011 #b11110)
   ;; h
   #(#b11000 #b11000 #b11110 #b11011 #b11011 #b11011 #b11011)
   ;; i
   #(#b00110 #b00000 #b01110 #b00110 #b00110 #b00110 #b01111)
   ;; j
   #(#b00011 #b00000 #b00011 #b00011 #b00011 #b11011 #b01110)
   ;; k
   #(#b11000 #b11000 #b11011 #b11110 #b11100 #b11110 #b11011)
   ;; l
   #(#b01110 #b00110 #b00110 #b00110 #b00110 #b00110 #b01111)
   ;; m
   #(#b00000 #b00000 #b11011 #b11111 #b10101 #b10101 #b10101)
   ;; n
   #(#b00000 #b00000 #b11110 #b11011 #b11011 #b11011 #b11011)
   ;; o
   #(#b00000 #b00000 #b01110 #b11011 #b11011 #b11011 #b01110)
   ;; p
   #(#b00000 #b00000 #b11110 #b11011 #b11011 #b11110 #b11000)
   ;; q
   #(#b00000 #b00000 #b01111 #b11011 #b11011 #b01111 #b00011)
   ;; r
   #(#b00000 #b00000 #b11011 #b11101 #b11000 #b11000 #b11000)
   ;; s
   #(#b00000 #b00000 #b01111 #b11000 #b01110 #b00011 #b11110)
   ;; t
   #(#b01100 #b01100 #b11110 #b01100 #b01100 #b01101 #b00110)
   ;; u
   #(#b00000 #b00000 #b11011 #b11011 #b11011 #b11011 #b01111)
   ;; v
   #(#b00000 #b00000 #b11011 #b11011 #b11011 #b01110 #b00100)
   ;; w
   #(#b00000 #b00000 #b10101 #b10101 #b10101 #b11111 #b01010)
   ;; x
   #(#b00000 #b00000 #b11011 #b01110 #b00100 #b01110 #b11011)
   ;; y
   #(#b00000 #b00000 #b11011 #b11011 #b01111 #b00011 #b01110)
   ;; z
   #(#b00000 #b00000 #b11111 #b00110 #b01100 #b11000 #b11111)
   ;; {
   #(#b00011 #b00110 #b00110 #b01100 #b00110 #b00110 #b00011)
   ;; |
   #(#b01100 #b01100 #b01100 #b01100 #b01100 #b01100 #b01100)
   ;; }
   #(#b11000 #b01100 #b01100 #b00110 #b01100 #b01100 #b11000)
   ;; ~
   #(#b00000 #b00000 #b01000 #b10101 #b00010 #b00000 #b00000)))

(defparameter *microfont-small-fallback*
  #(#b11111 #b10001 #b10001 #b10001 #b10001 #b10001 #b11111)
  "The 5-wide hollow box for characters outside ASCII 32-126.")

(defun microfont-small-glyph (char)
  "The small face's 7-row bit pattern for CHAR, or the fallback box."
  (let ((i (- (char-code char) 32)))
    (if (and (>= i 0) (< i (length *microfont-small-glyphs*)))
        (aref *microfont-small-glyphs* i)
        *microfont-small-fallback*)))

(defun microfont-text-width (text)
  "Pixel width of TEXT in the bold face (one 8px cell per character)."
  (* +microfont-advance+ (length text)))

(defun microfont-small-text-width (text)
  "Pixel width of TEXT in the small face (one 6px cell per character)."
  (* +microfont-small-advance+ (length text)))

(defun %microfont-render (text fg bg width glyph-fn glyph-width advance)
  "The line renderer both faces share: (VALUES PENS W H) where PENS is
a row-major (unsigned-byte 8) vector of W x H pen indices — glyph
pixels FG on a field of BG.  W is ADVANCE per character or the
explicit WIDTH (longer text is cut, shorter padded with BG); H is
+MICROFONT-LINE-HEIGHT+ — both faces are seven rows on an eight-row
cell."
  (let* ((w (max 1 (or width (* advance (length text)))))
         (h +microfont-line-height+)
         (pens (make-array (* w h) :element-type '(unsigned-byte 8)
                                   :initial-element bg)))
    (dotimes (i (length text))
      (let ((x0 (* i advance)))
        (when (>= x0 w) (return))
        (let ((rows (funcall glyph-fn (char text i))))
          (dotimes (row +microfont-glyph-height+)
            (let ((bits (aref rows row)))
              (dotimes (col glyph-width)
                (let ((x (+ x0 col)))
                  (when (and (< x w)
                             (logbitp (- glyph-width 1 col) bits))
                    (setf (aref pens (+ (* row w) x)) fg)))))))))
    (values pens w h)))

(defun microfont-line (text fg bg &key width)
  "TEXT rendered as a chunky pen buffer in the bold face — see
%MICROFONT-RENDER for the values.  The Amiga front-end feeds this
straight to AMIGA.GFX:WRITE-CHUNKY."
  (%microfont-render text fg bg width #'microfont-glyph
                     +microfont-glyph-width+ +microfont-advance+))

(defun microfont-small-line (text fg bg &key width)
  "TEXT rendered as a chunky pen buffer in the compact small face —
the map page's type (its 6px advance keeps the legend and footer out
of the map's way where the bold face's 8px would crowd it)."
  (%microfont-render text fg bg width #'microfont-small-glyph
                     +microfont-small-width+ +microfont-small-advance+))
