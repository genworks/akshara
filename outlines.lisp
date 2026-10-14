;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; Shaped text onto a cl-pdf page as filled glyph outlines: each
;;; glyph's TrueType contours (quadratic Béziers, read through zpb-ttf)
;;; become cubic curves in the page's content stream, placed by the
;;; shaper's advances and offsets.  No font is embedded and the text
;;; is not selectable; that comes with glyph-indexed TrueType
;;; embedding in cl-pdf.  This is the proof that the shaping is right.

(in-package #:akshara)

(export '(glyph-outline-path draw-shaped-glyphs draw-shaped-text))

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
