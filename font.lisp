;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; The font: a zpb-ttf loader for the cmap, metrics and outlines, and
;;; the layout tables (GDEF, GSUB, GPOS) read from the same file into
;;; octet vectors and parsed by layout.lisp.

(in-package #:akshara)

(defstruct (font (:constructor %make-font))
  path
  loader                                ; the zpb-ttf font-loader, kept open
  units-per-em
  glyph-count
  glyph-classes                         ; GDEF class-def, or NIL
  mark-attach-classes                   ; GDEF mark attachment class-def, or NIL
  mark-glyph-sets                       ; simple vector of coverage tables
  gsub                                  ; layout-table or NIL
  gpos                                  ; layout-table or NIL
  (advances (make-hash-table))          ; gid -> advance width in font units
  (cmap (make-hash-table)))             ; code point -> gid

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type t)
    (format stream "~a" (font-path font))))

(defun read-table-octets (loader tag)
  "The whole table TAG as an octet vector, or NIL when the font has no
such table."
  (when (zpb-ttf::table-info tag loader)
    (let* ((stream (zpb-ttf::input-stream loader))
           (size (zpb-ttf::table-size tag loader))
           (v (make-array size :element-type '(unsigned-byte 8))))
      (file-position stream (zpb-ttf::table-position tag loader))
      (read-sequence v stream)
      v)))

(defun load-font (path)
  "Open the TrueType font at PATH and read its layout tables.  The
font keeps its file open until `close-font'."
  (let* ((loader (zpb-ttf:open-font-loader path))
         (font (%make-font :path path :loader loader
                           :units-per-em (zpb-ttf:units/em loader)
                           :glyph-count (zpb-ttf:glyph-count loader))))
    (let ((gdef (read-table-octets loader "GDEF")))
      (when gdef (parse-gdef font gdef)))
    (let ((gsub (read-table-octets loader "GSUB")))
      (when gsub (setf (font-gsub font) (parse-layout-table gsub :gsub))))
    (let ((gpos (read-table-octets loader "GPOS")))
      (when gpos (setf (font-gpos font) (parse-layout-table gpos :gpos))))
    font))

(defun close-font (font)
  (when (font-loader font)
    (zpb-ttf:close-font-loader (font-loader font))
    (setf (font-loader font) nil)))

(defmacro with-font ((var path) &body body)
  `(let ((,var (load-font ,path)))
     (unwind-protect (progn ,@body)
       (close-font ,var))))

;;; --- GDEF ---------------------------------------------------------------

(defun parse-gdef (font v)
  (let* ((minor (u16 v 2))
         (class-def (offset16 v 4 0))
         (mark-attach (offset16 v 10 0))
         (mark-sets (and (>= minor 2) (offset16 v 12 0))))
    (when class-def
      (setf (font-glyph-classes font) (parse-class-def v class-def)))
    (when mark-attach
      (setf (font-mark-attach-classes font) (parse-class-def v mark-attach)))
    (when mark-sets
      (let* ((count (u16 v (+ mark-sets 2)))
             (sets (make-array count)))
        (dotimes (k count)
          (setf (aref sets k)
                (parse-coverage v (+ mark-sets (u32 v (+ mark-sets 4 (* 4 k)))))))
        (setf (font-mark-glyph-sets font) sets)))))

(defun glyph-class (font gid)
  "The GDEF class of GID: 1 base, 2 ligature, 3 mark, 4 component, 0
unclassified."
  (class-of-glyph (font-glyph-classes font) gid))

(defun mark-set-covers-p (font set-index gid)
  (let ((sets (font-mark-glyph-sets font)))
    (and sets (< set-index (length sets))
         (covered-p (aref sets set-index) gid))))

;;; --- cmap and metrics through zpb-ttf -------------------------------------

(defun glyph-id (font code)
  "The glyph index for the code point CODE, 0 when the font has none."
  (let ((cmap (font-cmap font)))
    (multiple-value-bind (gid found) (gethash code cmap)
      (if found
          gid
          (setf (gethash code cmap)
                (if (or (> code #xFFFF) (<= #xD800 code #xDFFF))
                    0   ; zpb-ttf reads the BMP format 4 subtable only
                    (let ((ch (code-char code)))
                      (if (zpb-ttf:glyph-exists-p ch (font-loader font))
                          (zpb-ttf:font-index (zpb-ttf:find-glyph ch (font-loader font)))
                          0))))))))

(defun advance-width (font gid)
  "The horizontal advance of GID in font units."
  (let ((advances (font-advances font)))
    (multiple-value-bind (adv found) (gethash gid advances)
      (if found
          adv
          (setf (gethash gid advances)
                (zpb-ttf:advance-width (zpb-ttf:index-glyph gid (font-loader font))))))))

(defun font-scripts (font &optional (table :gsub))
  "The script tags of the GSUB (or GPOS) table."
  (let ((lt (ecase table (:gsub (font-gsub font)) (:gpos (font-gpos font)))))
    (and lt (mapcar #'first (lt-scripts lt)))))

(defun font-features (font &optional (table :gsub))
  "The distinct feature tags of the GSUB (or GPOS) table, in order."
  (let ((lt (ecase table (:gsub (font-gsub font)) (:gpos (font-gpos font)))))
    (and lt (remove-duplicates (map 'list #'car (lt-features lt)) :test #'string= :from-end t))))
