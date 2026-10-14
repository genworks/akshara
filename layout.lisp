;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; GSUB and GPOS: the script, feature and lookup lists, and every
;;; subtable type the shaper applies, parsed into structures.
;;;
;;; GSUB: 1 single, 2 multiple, 3 alternate, 4 ligature, 5 contextual,
;;; 6 chained contextual, 7 extension.  GPOS: 1 single, 2 pair, 3
;;; cursive (parsed, not applied), 4 mark-to-base, 5 mark-to-ligature,
;;; 6 mark-to-mark, 7 contextual, 8 chained contextual, 9 extension.
;;; Device and variation tables are skipped: this is a static-font
;;; shaper.

(in-package #:akshara)

(defstruct (layout-table (:conc-name lt-) (:constructor %make-layout-table))
  kind                                  ; :gsub or :gpos
  scripts                               ; list of (tag default-langsys langsys-alist)
  features                              ; simple vector of (tag . lookup-index-vector)
  lookups)                              ; simple vector of lookup

;;; A langsys is (required-feature-index-or-nil . feature-index-vector).

(defstruct (lookup (:constructor %make-lookup))
  index
  type                                  ; the real type (extensions unwrapped)
  flag
  mark-filtering-set                    ; index into GDEF's mark glyph sets, or NIL
  subtables)                            ; list

(defconstant +flag-right-to-left+ #x0001)
(defconstant +flag-ignore-base-glyphs+ #x0002)
(defconstant +flag-ignore-ligatures+ #x0004)
(defconstant +flag-ignore-marks+ #x0008)
(defconstant +flag-use-mark-filtering-set+ #x0010)
(defconstant +flag-ignore-flags+ #x000E)
(defconstant +flag-mark-attachment-type+ #xFF00)

(defun parse-layout-table (v kind)
  (let ((script-list (u16 v 4))
        (feature-list (u16 v 6))
        (lookup-list (u16 v 8)))
    (%make-layout-table :kind kind
                        :scripts (parse-script-list v script-list)
                        :features (parse-feature-list v feature-list)
                        :lookups (parse-lookup-list v lookup-list kind))))

(defun parse-langsys (v at)
  (let ((required (u16 v (+ at 2)))
        (count (u16 v (+ at 4))))
    (cons (if (= required #xFFFF) nil required)
          (u16-vector v (+ at 6) count))))

(defun parse-script-list (v at)
  (let ((count (u16 v at)))
    (loop for k below count
          collect (let* ((rec (+ at 2 (* 6 k)))
                         (tag (tag-string v rec))
                         (script (+ at (u16 v (+ rec 4))))
                         (default (offset16 v script script))
                         (ls-count (u16 v (+ script 2))))
                    (list tag
                          (and default (parse-langsys v default))
                          (loop for j below ls-count
                                collect (let ((r (+ script 4 (* 6 j))))
                                          (cons (tag-string v r)
                                                (parse-langsys v (+ script (u16 v (+ r 4))))))))))))

(defun parse-feature-list (v at)
  (let* ((count (u16 v at))
         (features (make-array count)))
    (dotimes (k count features)
      (let* ((rec (+ at 2 (* 6 k)))
             (feature (+ at (u16 v (+ rec 4))))
             (n (u16 v (+ feature 2))))
        (setf (aref features k)
              (cons (tag-string v rec) (u16-vector v (+ feature 4) n)))))))

(defun parse-lookup-list (v at kind)
  (let* ((count (u16 v at))
         (lookups (make-array count)))
    (dotimes (k count lookups)
      (let* ((lookup (+ at (u16 v (+ at 2 (* 2 k)))))
             (type (u16 v lookup))
             (flag (u16 v (+ lookup 2)))
             (n (u16 v (+ lookup 4)))
             (set (and (logtest flag +flag-use-mark-filtering-set+)
                       (u16 v (+ lookup 6 (* 2 n)))))
             (subtables (loop for j below n
                              collect (parse-subtable v (+ lookup (u16 v (+ lookup 6 (* 2 j)))) type kind))))
        (setf (aref lookups k)
              (%make-lookup :index k
                            :type (if subtables (subtable-type (first subtables)) type)
                            :flag flag
                            :mark-filtering-set set
                            :subtables subtables))))))

(defun parse-subtable (v at type kind)
  (ecase kind
    (:gsub (parse-gsub-subtable v at type))
    (:gpos (parse-gpos-subtable v at type))))

;;; --- shared pieces -----------------------------------------------------

(defun parse-lookup-records (v at count)
  "COUNT (sequence-index . lookup-index) pairs starting at AT."
  (loop for k below count
        collect (cons (u16 v (+ at (* 4 k))) (u16 v (+ at 2 (* 4 k))))))

(defstruct (rule (:constructor %make-rule))
  backtrack                             ; vector (glyphs, classes or coverages), nearest first
  input                                 ; vector, the first glyph excluded
  lookahead                             ; vector
  records)                              ; list of (sequence-index . lookup-index)

(defstruct (context (:conc-name ctx-) (:constructor %make-context))
  format                                ; 1 glyphs, 2 classes, 3 coverages
  chain-p
  kind                                  ; :gsub or :gpos
  coverage                              ; formats 1 and 2: the first glyph's coverage; 3: the first input coverage
  rule-sets                             ; formats 1 and 2: vector (by coverage index / class) of rule lists
  backtrack-class-def
  input-class-def
  lookahead-class-def
  rule)                                 ; format 3: the single rule

(defun parse-rule (v at chain-p classes-p)
  "A (Chain)Rule or (Chain)ClassRule at AT."
  (declare (ignore classes-p))
  (if chain-p
      (let* ((bt-count (u16 v at))
             (backtrack (u16-vector v (+ at 2) bt-count))
             (p (+ at 2 (* 2 bt-count)))
             (in-count (u16 v p))
             (input (u16-vector v (+ p 2) (max 0 (1- in-count))))
             (p2 (+ p 2 (* 2 (max 0 (1- in-count)))))
             (la-count (u16 v p2))
             (lookahead (u16-vector v (+ p2 2) la-count))
             (p3 (+ p2 2 (* 2 la-count)))
             (rec-count (u16 v p3)))
        (%make-rule :backtrack backtrack :input input :lookahead lookahead
                    :records (parse-lookup-records v (+ p3 2) rec-count)))
      (let* ((glyph-count (u16 v at))
             (rec-count (u16 v (+ at 2)))
             (input (u16-vector v (+ at 4) (max 0 (1- glyph-count)))))
        (%make-rule :backtrack #() :input input :lookahead #()
                    :records (parse-lookup-records v (+ at 4 (* 2 (max 0 (1- glyph-count)))) rec-count)))))

(defun parse-context-subtable (v at chain-p kind)
  (let ((format (u16 v at)))
    (flet ((rule-sets (offsets-at count)
             (let ((sets (make-array count :initial-element nil)))
               (dotimes (k count sets)
                 (let ((set (offset16 v (+ offsets-at (* 2 k)) at)))
                   (when set
                     (let ((n (u16 v set)))
                       (setf (aref sets k)
                             (loop for j below n
                                   collect (parse-rule v (+ set (u16 v (+ set 2 (* 2 j)))) chain-p (= format 2)))))))))))
      (ecase format
        (1 (let ((coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
                 (count (u16 v (+ at 4))))
             (%make-context :format 1 :chain-p chain-p :kind kind :coverage coverage
                            :rule-sets (rule-sets (+ at 6) count))))
        (2 (if chain-p
               (let ((coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
                     (bt (offset16 v (+ at 4) at))
                     (in (offset16 v (+ at 6) at))
                     (la (offset16 v (+ at 8) at))
                     (count (u16 v (+ at 10))))
                 (%make-context :format 2 :chain-p t :kind kind :coverage coverage
                                :backtrack-class-def (and bt (parse-class-def v bt))
                                :input-class-def (and in (parse-class-def v in))
                                :lookahead-class-def (and la (parse-class-def v la))
                                :rule-sets (rule-sets (+ at 12) count)))
               (let ((coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
                     (cd (offset16 v (+ at 4) at))
                     (count (u16 v (+ at 6))))
                 (%make-context :format 2 :chain-p nil :kind kind :coverage coverage
                                :input-class-def (and cd (parse-class-def v cd))
                                :rule-sets (rule-sets (+ at 8) count)))))
        (3 (flet ((coverages (p count)
                    (let ((a (make-array count)))
                      (dotimes (k count a)
                        (setf (aref a k) (parse-coverage v (+ at (u16 v (+ p (* 2 k))))))))))
             (if chain-p
                 (let* ((bt-count (u16 v (+ at 2)))
                        (backtrack (coverages (+ at 4) bt-count))
                        (p (+ at 4 (* 2 bt-count)))
                        (in-count (u16 v p))
                        (input (coverages (+ p 2) in-count))
                        (p2 (+ p 2 (* 2 in-count)))
                        (la-count (u16 v p2))
                        (lookahead (coverages (+ p2 2) la-count))
                        (p3 (+ p2 2 (* 2 la-count)))
                        (rec-count (u16 v p3)))
                   (%make-context :format 3 :chain-p t :kind kind
                                  :coverage (and (plusp in-count) (aref input 0))
                                  :rule (%make-rule :backtrack backtrack
                                                    :input (subseq input (min 1 in-count))
                                                    :lookahead lookahead
                                                    :records (parse-lookup-records v (+ p3 2) rec-count))))
                 (let* ((in-count (u16 v (+ at 2)))
                        (rec-count (u16 v (+ at 4)))
                        (input (coverages (+ at 6) in-count)))
                   (%make-context :format 3 :chain-p nil :kind kind
                                  :coverage (and (plusp in-count) (aref input 0))
                                  :rule (%make-rule :backtrack #()
                                                    :input (subseq input (min 1 in-count))
                                                    :lookahead #()
                                                    :records (parse-lookup-records v (+ at 6 (* 2 in-count)) rec-count)))))))))))

(defun parse-extension (v at kind)
  (let ((ext-type (u16 v (+ at 2)))
        (ext-offset (u32 v (+ at 4))))
    (parse-subtable v (+ at ext-offset) ext-type kind)))

;;; --- GSUB --------------------------------------------------------------

(defstruct (single-subst (:constructor %make-single-subst))
  coverage delta substitutes)

(defstruct (multiple-subst (:constructor %make-multiple-subst))
  coverage sequences)

(defstruct (alternate-subst (:constructor %make-alternate-subst))
  coverage alternates)

(defstruct (ligature-subst (:constructor %make-ligature-subst))
  coverage
  ligature-sets)                        ; vector by coverage index of list of (lig-glyph . component-vector)

(defun parse-gsub-subtable (v at type)
  (ecase type
    (1 (let ((format (u16 v at))
             (coverage (parse-coverage v (+ at (u16 v (+ at 2))))))
         (ecase format
           (1 (%make-single-subst :coverage coverage :delta (u16 v (+ at 4))))
           (2 (%make-single-subst :coverage coverage
                                  :substitutes (u16-vector v (+ at 6) (u16 v (+ at 4))))))))
    ((2 3) (let* ((coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
                  (count (u16 v (+ at 4)))
                  (seqs (make-array count)))
             (dotimes (k count)
               (let ((s (+ at (u16 v (+ at 6 (* 2 k))))))
                 (setf (aref seqs k) (u16-vector v (+ s 2) (u16 v s)))))
             (if (= type 2)
                 (%make-multiple-subst :coverage coverage :sequences seqs)
                 (%make-alternate-subst :coverage coverage :alternates seqs))))
    (4 (let* ((coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
              (count (u16 v (+ at 4)))
              (sets (make-array count)))
         (dotimes (k count)
           (let* ((set (+ at (u16 v (+ at 6 (* 2 k)))))
                  (n (u16 v set)))
             (setf (aref sets k)
                   (loop for j below n
                         collect (let* ((lig (+ set (u16 v (+ set 2 (* 2 j)))))
                                        (glyph (u16 v lig))
                                        (comp-count (u16 v (+ lig 2))))
                                   (cons glyph (u16-vector v (+ lig 4) (max 0 (1- comp-count)))))))))
         (%make-ligature-subst :coverage coverage :ligature-sets sets)))
    (5 (parse-context-subtable v at nil :gsub))
    (6 (parse-context-subtable v at t :gsub))
    (7 (parse-extension v at :gsub))
    (8 (list :reverse-chain-unsupported at))))

;;; --- GPOS --------------------------------------------------------------

(defun value-record-size (format)
  (* 2 (logcount format)))

(defun parse-value-record (v at format)
  "The value record at AT with FORMAT, as (x-placement y-placement
x-advance y-advance); device offsets are read past and ignored."
  (let ((p at) (xp 0) (yp 0) (xa 0) (ya 0))
    (when (logtest format #x01) (setf xp (s16 v p)) (incf p 2))
    (when (logtest format #x02) (setf yp (s16 v p)) (incf p 2))
    (when (logtest format #x04) (setf xa (s16 v p)) (incf p 2))
    (when (logtest format #x08) (setf ya (s16 v p)) (incf p 2))
    (list xp yp xa ya)))

(defstruct (single-pos (:constructor %make-single-pos))
  coverage values)                      ; values: a single value (format 1) or a vector

(defstruct (pair-pos (:constructor %make-pair-pos))
  format coverage
  pair-sets                             ; format 1: vector by coverage index of vector of (second v1 v2), sorted by second
  class-def1 class-def2 class1-count class2-count
  records                               ; format 2: 2-d array of (v1 . v2)
  format1 format2)                      ; the value formats (0 = no values)

(defstruct (cursive-pos (:constructor %make-cursive-pos))
  coverage entry-exit)

(defstruct (mark-base-pos (:constructor %make-mark-base-pos))
  mark-coverage base-coverage class-count
  mark-array                            ; vector of (class . (x . y))
  base-array)                           ; vector by base index of vector by class of (x . y) or NIL

(defstruct (mark-lig-pos (:constructor %make-mark-lig-pos))
  mark-coverage lig-coverage class-count
  mark-array
  lig-array)                            ; vector by lig index of vector by component of vector by class

(defstruct (mark-mark-pos (:constructor %make-mark-mark-pos))
  mark1-coverage mark2-coverage class-count
  mark1-array mark2-array)

(defun parse-anchor (v at)
  (and at (cons (s16 v (+ at 2)) (s16 v (+ at 4)))))

(defun parse-mark-array (v at)
  (let* ((count (u16 v at))
         (a (make-array count)))
    (dotimes (k count a)
      (let ((rec (+ at 2 (* 4 k))))
        (setf (aref a k)
              (cons (u16 v rec) (parse-anchor v (offset16 v (+ rec 2) at))))))))

(defun parse-anchor-matrix (v at count class-count)
  "COUNT rows of CLASS-COUNT anchor offsets starting at AT, relative to
the array at AT - 2 (the array's count precedes the rows)."
  (let ((base (- at 2))
        (rows (make-array count)))
    (dotimes (r count rows)
      (let ((row (make-array class-count)))
        (dotimes (c class-count)
          (setf (aref row c)
                (parse-anchor v (offset16 v (+ at (* 2 (+ (* r class-count) c))) base))))
        (setf (aref rows r) row)))))

(defun parse-gpos-subtable (v at type)
  (ecase type
    (1 (let ((format (u16 v at))
             (coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
             (vf (u16 v (+ at 4))))
         (ecase format
           (1 (%make-single-pos :coverage coverage :values (parse-value-record v (+ at 6) vf)))
           (2 (let* ((count (u16 v (+ at 6)))
                     (size (value-record-size vf))
                     (values (make-array count)))
                (dotimes (k count)
                  (setf (aref values k) (parse-value-record v (+ at 8 (* size k)) vf)))
                (%make-single-pos :coverage coverage :values values))))))
    (2 (let ((format (u16 v at))
             (coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
             (vf1 (u16 v (+ at 4)))
             (vf2 (u16 v (+ at 6))))
         (ecase format
           (1 (let* ((count (u16 v (+ at 8)))
                     (sets (make-array count))
                     (size1 (value-record-size vf1))
                     (size2 (value-record-size vf2)))
                (dotimes (k count)
                  (let* ((set (+ at (u16 v (+ at 10 (* 2 k)))))
                         (n (u16 v set))
                         (pairs (make-array n))
                         (rec-size (+ 2 size1 size2)))
                    (dotimes (j n)
                      (let ((rec (+ set 2 (* rec-size j))))
                        (setf (aref pairs j)
                              (list (u16 v rec)
                                    (parse-value-record v (+ rec 2) vf1)
                                    (parse-value-record v (+ rec 2 size1) vf2)))))
                    (setf (aref sets k) pairs)))
                (%make-pair-pos :format 1 :coverage coverage :pair-sets sets
                                :format1 vf1 :format2 vf2)))
           (2 (let* ((cd1 (parse-class-def v (+ at (u16 v (+ at 8)))))
                     (cd2 (parse-class-def v (+ at (u16 v (+ at 10)))))
                     (c1 (u16 v (+ at 12)))
                     (c2 (u16 v (+ at 14)))
                     (size1 (value-record-size vf1))
                     (size2 (value-record-size vf2))
                     (records (make-array (list c1 c2)))
                     (p (+ at 16)))
                (dotimes (i c1)
                  (dotimes (j c2)
                    (setf (aref records i j)
                          (cons (parse-value-record v p vf1)
                                (parse-value-record v (+ p size1) vf2)))
                    (incf p (+ size1 size2))))
                (%make-pair-pos :format 2 :coverage coverage
                                :class-def1 cd1 :class-def2 cd2
                                :class1-count c1 :class2-count c2
                                :records records :format1 vf1 :format2 vf2))))))
    (3 (let* ((coverage (parse-coverage v (+ at (u16 v (+ at 2)))))
              (count (u16 v (+ at 4)))
              (ee (make-array count)))
         (dotimes (k count)
           (let ((rec (+ at 6 (* 4 k))))
             (setf (aref ee k)
                   (cons (parse-anchor v (offset16 v rec at))
                         (parse-anchor v (offset16 v (+ rec 2) at))))))
         (%make-cursive-pos :coverage coverage :entry-exit ee)))
    (4 (let* ((mark-cov (parse-coverage v (+ at (u16 v (+ at 2)))))
              (base-cov (parse-coverage v (+ at (u16 v (+ at 4)))))
              (class-count (u16 v (+ at 6)))
              (mark-array (+ at (u16 v (+ at 8))))
              (base-array (+ at (u16 v (+ at 10)))))
         (%make-mark-base-pos :mark-coverage mark-cov :base-coverage base-cov
                              :class-count class-count
                              :mark-array (parse-mark-array v mark-array)
                              :base-array (parse-anchor-matrix v (+ base-array 2) (u16 v base-array) class-count))))
    (5 (let* ((mark-cov (parse-coverage v (+ at (u16 v (+ at 2)))))
              (lig-cov (parse-coverage v (+ at (u16 v (+ at 4)))))
              (class-count (u16 v (+ at 6)))
              (mark-array (+ at (u16 v (+ at 8))))
              (lig-array (+ at (u16 v (+ at 10))))
              (lig-count (u16 v lig-array))
              (ligs (make-array lig-count)))
         (dotimes (k lig-count)
           (let* ((attach (+ lig-array (u16 v (+ lig-array 2 (* 2 k)))))
                  (comp-count (u16 v attach)))
             (setf (aref ligs k) (parse-anchor-matrix v (+ attach 2) comp-count class-count))))
         (%make-mark-lig-pos :mark-coverage mark-cov :lig-coverage lig-cov
                             :class-count class-count
                             :mark-array (parse-mark-array v mark-array)
                             :lig-array ligs)))
    (6 (let* ((m1-cov (parse-coverage v (+ at (u16 v (+ at 2)))))
              (m2-cov (parse-coverage v (+ at (u16 v (+ at 4)))))
              (class-count (u16 v (+ at 6)))
              (m1-array (+ at (u16 v (+ at 8))))
              (m2-array (+ at (u16 v (+ at 10)))))
         (%make-mark-mark-pos :mark1-coverage m1-cov :mark2-coverage m2-cov
                              :class-count class-count
                              :mark1-array (parse-mark-array v m1-array)
                              :mark2-array (parse-anchor-matrix v (+ m2-array 2) (u16 v m2-array) class-count))))
    (7 (parse-context-subtable v at nil :gpos))
    (8 (parse-context-subtable v at t :gpos))
    (9 (parse-extension v at :gpos))))

(defun subtable-type (st)
  "The OpenType lookup type of the parsed subtable ST."
  (typecase st
    (single-subst 1) (multiple-subst 2) (alternate-subst 3) (ligature-subst 4)
    (single-pos 1) (pair-pos 2) (cursive-pos 3) (mark-base-pos 4) (mark-lig-pos 5) (mark-mark-pos 6)
    (context (if (eq (ctx-kind st) :gsub)
                 (if (ctx-chain-p st) 6 5)
                 (if (ctx-chain-p st) 8 7)))
    (t 0)))

;;; --- finding things ------------------------------------------------------

(defun find-script (lt tags)
  "The first of TAGS present in the layout table's script list, as its
(tag default-langsys langsys-alist) entry, or NIL."
  (loop for tag in tags
        for entry = (find tag (lt-scripts lt) :key #'first :test #'string=)
        when entry return entry))

(defun script-langsys (script-entry &optional language)
  "The langsys of SCRIPT-ENTRY for LANGUAGE (a tag), falling back to
the default langsys."
  (or (and language (cdr (assoc language (third script-entry) :test #'string=)))
      (second script-entry)))

(defun langsys-feature-lookups (lt langsys tag)
  "The lookup indices of every feature named TAG in LANGSYS, in order."
  (when langsys
    (loop for fi across (cdr langsys)
          for feature = (aref (lt-features lt) fi)
          when (string= (car feature) tag)
            append (coerce (cdr feature) 'list))))

(defun langsys-has-feature-p (lt langsys tag)
  (and langsys
       (loop for fi across (cdr langsys)
             thereis (string= (car (aref (lt-features lt) fi)) tag))))
