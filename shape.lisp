;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; The shaping plan and the pipeline: text in, glyph ids with
;;; positions out.

(in-package #:akshara)

;;; --- the plan: which lookups, in which stages, with which masks ---------------
;;;
;;; The plan structure itself is defined in indic.lisp, which the
;;; reordering functions there read.  A lookup entry is (lookup mask
;;; per-syllable auto-zwnj auto-zwj).

(defparameter *non-global-masks*
  '(("rphf" . 2) ("pref" . 4) ("blwf" . 8) ("abvf" . 16) ("half" . 32) ("pstf" . 64) ("init" . 128)))

(defparameter *indic-basic-features*
  '("nukt" "akhn" "rphf" "rkrf" "pref" "blwf" "abvf" "half" "pstf" "vatu" "cjct")
  "Each in its own stage, in this order.")

(defparameter *indic-other-features*
  '("init" "pres" "abvs" "blws" "psts" "haln"))

(defparameter *common-gsub-features* '("calt" "clig" "liga" "rclt" "rlig")
  "HarfBuzz's common and horizontal GSUB features, one stage after the
shaper's own.")

(defparameter *indic-common-gsub-features* '("calt" "clig" "rclt" "rlig")
  "The same for the Indic shaper, which turns 'liga' off.")

(defparameter *gpos-features* '("abvm" "blwm" "mark" "mkmk" "curs" "dist" "kern"))

(defun script-tags (script)
  "The OpenType script tags tried for SCRIPT, the new-spec tag first."
  (ecase script
    (:devanagari '("dev2" "deva"))
    (:bengali '("bng2" "beng"))
    (:tamil '("tml2" "taml"))
    (:telugu '("tel2" "telu"))
    (:gujarati '("gjr2" "gujr"))
    ((nil) '("DFLT" "dflt" "latn"))))

(defun collect-stage (lt langsys features masks &key per-syllable (auto-joiners t))
  "The lookup entries of FEATURES (tags) in LANGSYS, sorted by lookup
index and merged where features share a lookup."
  (let ((entries (make-hash-table)))
    (dolist (tag features)
      (let ((mask (gethash tag masks 0)))
        (unless (zerop mask)
          (dolist (i (langsys-feature-lookups lt langsys tag))
            (let ((e (gethash i entries)))
              (if e
                  (setf (second e) (logior (second e) mask))
                  (setf (gethash i entries)
                        (list (aref (lt-lookups lt) i) mask per-syllable auto-joiners auto-joiners))))))))
    (let ((list (loop for e being the hash-values of entries collect e)))
      (sort list #'< :key (lambda (e) (lookup-index (first e)))))))

(defun merge-stages (&rest entry-lists)
  "One lookup list from several, as HarfBuzz compiles the features of
a stage: sorted by lookup index, a lookup two features share appearing
once with their masks or'd and the stricter joiner skipping of the two."
  (let ((entries (make-hash-table)))
    (dolist (list entry-lists)
      (loop for (lookup mask per-syllable auto-zwnj auto-zwj) in list
            do (let ((e (gethash (lookup-index lookup) entries)))
                 (if e
                     (setf (second e) (logior (second e) mask)
                           (fourth e) (and (fourth e) auto-zwnj)
                           (fifth e) (and (fifth e) auto-zwj))
                     (setf (gethash (lookup-index lookup) entries)
                           (list lookup mask per-syllable auto-zwnj auto-zwj))))))
    (sort (loop for e being the hash-values of entries collect e)
          #'< :key (lambda (e) (lookup-index (first e))))))

(defun make-plan (font script &optional language)
  (let* ((plan (%make-plan :font font :script script :config (indic-config script)))
         (gsub (font-gsub font))
         (gpos (font-gpos font))
         (tags (script-tags script))
         (masks (plan-feature-masks plan)))
    (when gsub
      (let ((entry (find-script gsub tags)))
        (setf (plan-gsub-langsys plan) (and entry (script-langsys entry language)))))
    (when gpos
      (let ((entry (find-script gpos tags)))
        (setf (plan-gpos-langsys plan) (and entry (script-langsys entry language)))))
    ;; Masks: a global feature present in either table is 1; a per-glyph
    ;; feature gets its bit when the GSUB has it.
    (flet ((note (tag table langsys)
             (when (and table (langsys-has-feature-p table langsys tag))
               (setf (gethash tag masks)
                     (or (cdr (assoc tag *non-global-masks* :test #'string=)) 1)))))
      (dolist (tag (append '("locl" "ccmp") *indic-basic-features* *indic-other-features*
                           (if (plan-config plan) *indic-common-gsub-features* *common-gsub-features*)))
        (note tag gsub (plan-gsub-langsys plan)))
      (dolist (tag *gpos-features*)
        (note tag gpos (plan-gpos-langsys plan))))
    (when (plan-config plan)
      (setf (plan-virama-gid plan) (glyph-id font (indic-config-virama (plan-config plan)))))
    ;; GSUB stages
    (when gsub
      (let ((ls (plan-gsub-langsys plan)))
        (setf (plan-gsub-stages plan)
              (if (plan-config plan)
                  (append
                   (list (cons (collect-stage gsub ls '("locl" "ccmp") masks :per-syllable t)
                               #'initial-reordering))
                   (loop for tag in *indic-basic-features*
                         for rest on *indic-basic-features*
                         collect (cons (collect-stage gsub ls (list tag) masks :per-syllable t :auto-joiners nil)
                                       (if (null (cdr rest)) #'final-reordering nil)))
                   (list (cons (merge-stages (collect-stage gsub ls *indic-other-features* masks :per-syllable t :auto-joiners nil)
                                             (collect-stage gsub ls *indic-common-gsub-features* masks))
                               nil)))
                  (list (cons (collect-stage gsub ls (append '("locl" "ccmp") *common-gsub-features*) masks)
                              nil))))))
    (when gpos
      (setf (plan-gpos-lookups plan)
            (collect-stage gpos (plan-gpos-langsys plan) *gpos-features* masks)))
    plan))

;;; --- the buffer from the text ------------------------------------------------

(defun guess-script (string)
  "The script of the first character of STRING in a block the Indic
shaper covers, or NIL."
  (loop for ch across string
        for code = (char-code ch)
        do (cond ((<= #x0900 code #x097F) (return :devanagari))
                 ((<= #x0980 code #x09FF) (return :bengali))
                 ((<= #x0A80 code #x0AFF) (return :gujarati))
                 ((<= #x0B80 code #x0BFF) (return :tamil))
                 ((<= #x0C00 code #x0C7F) (return :telugu)))
        finally (return nil)))

(defun normalize-codes (font codes)
  "Decompose what HarfBuzz decomposes, order nukta before virama, and
recompose the consonant-nukta pairs the font has glyphs for.  CODES is
a list of (code . cluster); the result is the same."
  (let ((out nil))
    ;; decompose
    (dolist (item codes)
      (let ((parts (decompose-code (car item))))
        (if (and parts (every (lambda (p) (plusp (glyph-id font p))) parts))
            (dolist (p parts) (push (cons p (cdr item)) out))
            (push item out))))
    (setf out (nreverse out))
    ;; canonical order of marks with a combining class
    (let ((v (coerce out 'vector)) (i 0))
      (loop while (< i (length v))
            do (if (zerop (combining-class (car (aref v i))))
                   (incf i)
                   (let ((j i))
                     (loop while (and (< j (length v)) (plusp (combining-class (car (aref v j))))) do (incf j))
                     (when (> (- j i) 1)
                       (let ((run (stable-sort (subseq v i j) #'< :key (lambda (x) (combining-class (car x))))))
                         (replace v run :start1 i)))
                     (setf i j))))
      (setf out (coerce v 'list)))
    ;; recompose
    (let ((result nil))
      (dolist (item out)
        (let ((prev (first result)))
          (if (and prev
                   (not (unicode-mark-p (car prev)))
                   (compose-codes (car prev) (car item))
                   (plusp (glyph-id font (compose-codes (car prev) (car item)))))
              (setf (first result) (cons (compose-codes (car prev) (car item)) (cdr prev)))
              (push item result))))
      (nreverse result))))

(defun fill-buffer (plan buf string)
  (let* ((font (plan-font plan))
         (script (plan-script plan))
         (codes (loop for ch across string
                      for i from 0
                      collect (cons (char-code ch) i))))
    (setf codes (normalize-codes font codes))
    ;; Clusters: a mark or joiner joins the cluster before it.
    (let ((cluster 0) (first t))
      (dolist (item codes)
        (let ((code (car item)))
          (if (and (not first) (continuation-p code))
              (setf (cdr item) cluster)
              (setf cluster (cdr item)))
          (setf first nil))))
    (dolist (item codes)
      (let* ((code (car item))
             (gid (glyph-id font code))
             (g (make-ginfo :gid gid :codepoint code :cluster (cdr item)
                            :props (gdef-props font gid)
                            :unicode-mark-p (unicode-mark-p code)
                            :continuation-p (continuation-p code))))
        (when script
          (multiple-value-bind (cat pos) (indic-properties script code)
            (setf (gi-category g) cat (gi-position g) pos)))
        (vector-push-extend g buf)))
    buf))

;;; --- positioning -----------------------------------------------------------------

(defun position-default (font buf)
  (loop for g across buf
        do (setf (gi-x-advance g) (advance-width font (gi-gid g))
                 (gi-y-advance g) 0
                 (gi-x-offset g) 0
                 (gi-y-offset g) 0)))

(defun zero-mark-widths (font buf adjust-offsets)
  (declare (ignore font))
  (loop for g across buf
        when (gi-mark-p g)
          do (when adjust-offsets
               (decf (gi-x-offset g) (gi-x-advance g))
               (decf (gi-y-offset g) (gi-y-advance g)))
             (setf (gi-x-advance g) 0 (gi-y-advance g) 0)))

(defun zero-default-ignorable-advances (buf)
  "HarfBuzz's hb_ot_zero_width_default_ignorables: the joiners and
other default-ignorable characters no lookup consumed take no space."
  (loop for g across buf
        when (default-ignorable-p g)
          do (setf (gi-x-advance g) 0 (gi-y-advance g) 0)))

(defun propagate-attachment-offsets (buf)
  (let ((len (fill-pointer buf)))
    (labels ((propagate (i depth)
               (let* ((g (aref buf i))
                      (chain (gi-attach-chain g)))
                 (unless (or (zerop chain) (zerop depth))
                   (setf (gi-attach-chain g) 0)
                   (let ((j (+ i chain)))
                     (when (< -1 j len)
                       (propagate j (1- depth))
                       (let ((base (aref buf j)))
                         (incf (gi-x-offset g) (gi-x-offset base))
                         (incf (gi-y-offset g) (gi-y-offset base))
                         (loop for k from j below i
                               do (decf (gi-x-offset g) (gi-x-advance (aref buf k)))
                                  (decf (gi-y-offset g) (gi-y-advance (aref buf k)))))))))))
      (dotimes (i len) (propagate i 64)))))

;;; --- the result ----------------------------------------------------------------------

(defstruct (shaped-glyph (:conc-name sg-))
  gid cluster x-advance y-advance x-offset y-offset)

(defmethod print-object ((g shaped-glyph) stream)
  (print-unreadable-object (g stream)
    (format stream "~d=~d@~d,~d+~d" (sg-gid g) (sg-cluster g) (sg-x-offset g) (sg-y-offset g) (sg-x-advance g))))

(defun run-stage (c entries)
  (loop for (lookup mask per-syllable auto-zwnj auto-zwj) in entries
        do (apply-lookup c lookup mask :per-syllable per-syllable :auto-zwnj auto-zwnj :auto-zwj auto-zwj)))

(defun shape (font string &key (script :guess) language)
  "Shape STRING with FONT: a vector of shaped-glyph, in visual order,
advances and offsets in font units.  SCRIPT is :devanagari, :bengali,
:tamil, :telugu, :gujarati, NIL for the default shaper, or :guess
\(from the text).  LANGUAGE is an OpenType language tag such as
\"SAN \"."
  (let* ((script (if (eq script :guess) (guess-script string) script))
         (plan (make-plan font script language))
         (buf (make-buffer (max 16 (length string)))))
    (fill-buffer plan buf string)
    (when (plan-config plan)
      (setup-syllables buf))
    (let ((c (%make-actx :font font :buffer buf :kind :gsub)))
      (dolist (stage (plan-gsub-stages plan))
        (run-stage c (car stage))
        (when (cdr stage) (funcall (cdr stage) plan buf))))
    (position-default font buf)
    (when (plan-gpos-lookups plan)
      (let ((c (%make-actx :font font :buffer buf :kind :gpos)))
        (run-stage c (plan-gpos-lookups plan))))
    ;; The default shaper zeroes mark advances by GDEF class; the Indic
    ;; shaper leaves them to the font (HarfBuzz's ZERO_WIDTH_MARKS_NONE),
    ;; since some Indic fonts class their spacing matras as marks.
    (unless (plan-config plan)
      (zero-mark-widths font buf (null (plan-gpos-lookups plan))))
    (zero-default-ignorable-advances buf)
    (propagate-attachment-offsets buf)
    ;; Default ignorables are shown as the space glyph, or dropped when
    ;; the font has none (HarfBuzz's hb_ot_hide_default_ignorables).
    (let ((space (glyph-id font #x20)))
      (coerce (loop for g across buf
                    for hidden = (default-ignorable-p g)
                    unless (and hidden (zerop space))
                      collect (make-shaped-glyph :gid (if hidden space (gi-gid g)) :cluster (gi-cluster g)
                                                 :x-advance (gi-x-advance g) :y-advance (gi-y-advance g)
                                                 :x-offset (gi-x-offset g) :y-offset (gi-y-offset g)))
              'vector))))

(defun shaped-width (glyphs)
  "The total advance of a shaped line, in font units."
  (loop for g across glyphs sum (sg-x-advance g)))

;;; --- hb-shape's text format, for the golden tests ------------------------------------

(defun hb-string (glyphs)
  "GLYPHS in the format hb-shape prints with --no-glyph-names:
[gid=cluster@x_offset,y_offset+x_advance|...], offsets left out when
both are zero, y_advance left out when zero."
  (with-output-to-string (s)
    (write-char #\[ s)
    (loop for g across glyphs
          for first = t then nil
          do (unless first (write-char #\| s))
             (format s "~d=~d" (sg-gid g) (sg-cluster g))
             (unless (and (zerop (sg-x-offset g)) (zerop (sg-y-offset g)))
               (format s "@~d,~d" (sg-x-offset g) (sg-y-offset g)))
             (format s "+~d" (sg-x-advance g))
             (unless (zerop (sg-y-advance g))
               (format s ",~d" (sg-y-advance g))))
    (write-char #\] s)))

(defun parse-hb-string (string)
  "The inverse of `hb-string': a vector of shaped-glyph."
  (let ((items (loop with start = 1
                     for bar = (position #\| string :start start)
                     collect (subseq string start (or bar (1- (length string))))
                     while bar
                     do (setf start (1+ bar)))))
    (map 'vector
         (lambda (item)
           (let* ((eq (position #\= item))
                  (at (position #\@ item))
                  (plus (position #\+ item))
                  (gid (parse-integer item :end eq))
                  (cluster (parse-integer item :start (1+ eq) :end (or at plus)))
                  (xo 0) (yo 0) (xa 0) (ya 0))
             (when at
               (let ((comma (position #\, item :start at)))
                 (setf xo (parse-integer item :start (1+ at) :end comma)
                       yo (parse-integer item :start (1+ comma) :end plus))))
             (let ((comma (position #\, item :start plus)))
               (setf xa (parse-integer item :start (1+ plus) :end comma))
               (when comma (setf ya (parse-integer item :start (1+ comma)))))
             (make-shaped-glyph :gid gid :cluster cluster :x-advance xa :y-advance ya :x-offset xo :y-offset yo)))
         items)))
