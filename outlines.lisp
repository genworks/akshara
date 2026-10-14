;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; Shaped text onto a cl-pdf page as filled glyph outlines: each
;;; glyph's TrueType contours (quadratic Béziers, read through zpb-ttf)
;;; become cubic curves in the page's content stream, placed by the
;;; shaper's advances and offsets.  No font is embedded and the text
;;; is not selectable; that comes with glyph-indexed TrueType
;;; embedding in cl-pdf.  This is the proof that the shaping is right.

(in-package #:akshara)

(export '(glyph-outline-path draw-shaped-glyphs draw-shaped-text show-shaped-glyphs show-shaped-text))

(defun glyph-outline-path (font gid scale x y)
  "Add the outline of GID to the current path, its origin at (X Y) in
page units, font units scaled by SCALE."
  (let ((glyph (zpb-ttf:index-glyph gid (font-loader font))))
    (zpb-ttf:do-contours (contour glyph)
      (when (plusp (length contour))
        (let ((first t))
          (zpb-ttf:do-contour-segments (p0 p1 p2) contour
            (let ((x0 (+ x (* scale (zpb-ttf:x p0)))) (y0 (+ y (* scale (zpb-ttf:y p0))))
                  (x2 (+ x (* scale (zpb-ttf:x p2)))) (y2 (+ y (* scale (zpb-ttf:y p2)))))
              (when first
                (pdf:move-to (float x0 1d0) (float y0 1d0))
                (setf first nil))
              (if p1
                  (let* ((x1 (+ x (* scale (zpb-ttf:x p1)))) (y1 (+ y (* scale (zpb-ttf:y p1))))
                         ;; the quadratic (p0 p1 p2) as a cubic
                         (c1x (+ x0 (* 2/3 (- x1 x0)))) (c1y (+ y0 (* 2/3 (- y1 y0))))
                         (c2x (+ x2 (* 2/3 (- x1 x2)))) (c2y (+ y2 (* 2/3 (- y1 y2)))))
                    (pdf:bezier-to (float c1x 1d0) (float c1y 1d0) (float c2x 1d0) (float c2y 1d0)
                                   (float x2 1d0) (float y2 1d0)))
                  (pdf:line-to (float x2 1d0) (float y2 1d0)))))
          (pdf:close-path))))))

(defun draw-shaped-glyphs (font glyphs x y size)
  "Fill the shaped GLYPHS (from `shape') at SIZE points with the pen
starting at (X Y), the baseline origin, in the current fill color.
Returns the pen's x after the last advance."
  (let ((scale (/ size (font-units-per-em font)))
        (pen-x x) (pen-y y))
    (loop for g across glyphs
          do (glyph-outline-path font (sg-gid g) scale
                                 (+ pen-x (* scale (sg-x-offset g)))
                                 (+ pen-y (* scale (sg-y-offset g))))
             (incf pen-x (* scale (sg-x-advance g)))
             (incf pen-y (* scale (sg-y-advance g))))
    (pdf:fill-path)
    pen-x))

(defun draw-shaped-text (font string x y size &rest shape-args)
  "Shape STRING with FONT and draw it; see `draw-shaped-glyphs'."
  (draw-shaped-glyphs font (apply #'shape font string shape-args) x y size))

;;; --- as text, through a glyph-indexed font --------------------------------
;;;
;;; The real thing: the glyphs shown with Tj/TJ in a font cl-pdf
;;; embeds (cl-pdf-glyphs.lisp), so the PDF carries the font program,
;;; the text is selectable and what the cmap names copies out.

(defun utf16-hex (string)
  "STRING as a PDF hex string in UTF-16BE with a byte order mark."
  (with-output-to-string (s)
    (write-string "<FEFF" s)
    (loop for ch across string
          for code = (char-code ch)
          do (if (< code #x10000)
                 (format s "~4,'0X" code)
                 (let ((v (- code #x10000)))
                   (format s "~4,'0X~4,'0X" (+ #xD800 (ash v -10)) (+ #xDC00 (logand v #x3FF))))))
    (write-char #\> s)))

(defun show-shaped-glyphs (font glyphs x y size pdf-font &key text)
  "Show the shaped GLYPHS as text at SIZE points from the baseline
origin (X Y), in PDF-FONT, the glyph font cl-pdf made of the same file
(`pdf:load-glyph-font').  Must be called inside `pdf:in-text-mode'.
Glyphs with no offset go into TJ arrays whose numbers correct the
viewer's advances to the shaper's; a glyph with an offset (a mark) is
placed by its own text matrix.  With TEXT, the string the glyphs were
shaped from, each cluster is wrapped in a marked-content span whose
ActualText is its characters, so the logical text copies and searches
out of the PDF whatever the glyph order (a reph, a pre-base matra, a
conjunct).  Returns the pen's x after the last advance."
  (let* ((upem (font-units-per-em font))
         (scale (/ size upem))
         (k (/ 1000 upem))
         (widths (pdf:glyph-widths (pdf::font-metrics pdf-font)))
         (pen-x x)
         (run nil)                      ; reversed TJ items for the run in progress
         (run-x nil)
         (group nil)                    ; reversed gids of the hex string in progress
         (n (length glyphs)))
    (pdf:set-font pdf-font size)
    (labels ((close-group ()
               (when group
                 (push (nreverse group) run)
                 (setf group nil)))
             (flush ()
               (close-group)
               (when run
                 (pdf:set-text-matrix 1 0 0 1 run-x y)
                 (pdf:show-glyphs-spaced (nreverse run))
                 (setf run nil run-x nil)))
             (show-one (g)
               (let* ((gid (sg-gid g))
                      (adv (* k (sg-x-advance g)))
                      (w (if (< gid (length widths)) (aref widths gid) 0)))
                 (if (and (zerop (sg-x-offset g)) (zerop (sg-y-offset g)) (zerop (sg-y-advance g)))
                     (let ((adjust (- w adv)))
                       (unless run-x (setf run-x pen-x))
                       (push gid group)
                       (when (> (abs adjust) 0.005)
                         (close-group)
                         (push adjust run)))
                     (progn
                       (flush)
                       (pdf:set-text-matrix 1 0 0 1
                                            (+ pen-x (* scale (sg-x-offset g)))
                                            (+ y (* scale (sg-y-offset g))))
                       (pdf:show-glyphs (list gid))))
                 (incf pen-x (* scale (sg-x-advance g))))))
      (if (null text)
          (loop for g across glyphs do (show-one g))
          ;; One span per cluster: the glyphs sharing a cluster value, whose
          ;; characters run from that value to the next cluster's.
          (let ((i 0))
            (loop while (< i n)
                  do (let* ((cluster (sg-cluster (aref glyphs i)))
                            (j (1+ i)))
                       (loop while (and (< j n) (= (sg-cluster (aref glyphs j)) cluster)) do (incf j))
                       (let* ((next (if (< j n) (sg-cluster (aref glyphs j)) (length text)))
                              (chars (subseq text (min cluster (length text)) (min (max cluster next) (length text)))))
                         (format pdf::*page-stream* "/Span <</ActualText ~a>> BDC~%" (utf16-hex chars))
                         (loop for m from i below j do (show-one (aref glyphs m)))
                         (flush)
                         (write-line "EMC" pdf::*page-stream*))
                       (setf i j)))))
      (flush))
    pen-x))

(defun show-shaped-text (font string x y size pdf-font &rest shape-args)
  "Shape STRING with FONT and show it as text with ActualText spans;
see `show-shaped-glyphs'."
  (show-shaped-glyphs font (apply #'shape font string shape-args) x y size pdf-font :text string))
