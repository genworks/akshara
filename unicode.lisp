;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; The Unicode facts the shaper needs about Devanagari and Bengali:
;;; the Indic syllabic category and position of each character (as
;;; HarfBuzz classifies them: C, V, N, H, M, SM, Ra, Symbol ...), which
;;; characters are marks, and the canonical decompositions the
;;; normalizer applies.  Tables are written out by hand from
;;; IndicSyllabicCategory.txt and IndicPositionalCategory.txt for the
;;; two blocks, so the library carries no Unicode database.

(in-package #:akshara)

;;; Indic positions, in HarfBuzz's order (the sort key of a syllable).
(defconstant +pos-start+ 0)
(defconstant +pos-ra-to-become-reph+ 1)
(defconstant +pos-pre-m+ 2)
(defconstant +pos-pre-c+ 3)
(defconstant +pos-base-c+ 4)
(defconstant +pos-after-main+ 5)
(defconstant +pos-above-c+ 6)
(defconstant +pos-before-sub+ 7)
(defconstant +pos-below-c+ 8)
(defconstant +pos-after-sub+ 9)
(defconstant +pos-before-post+ 10)
(defconstant +pos-post-c+ 11)
(defconstant +pos-after-post+ 12)
(defconstant +pos-smvd+ 13)
(defconstant +pos-end+ 14)

(defun in-ranges-p (code ranges)
  (loop for (lo hi) on ranges by #'cddr
        thereis (<= lo code hi)))

;;; Each entry: (low high category side); SIDE for matras is :pre :post
;;; :above or :below, for others NIL.
(defparameter *indic-table*
  '(;; Devanagari
    (#x0900 #x0902 :sm :above) (#x0903 #x0903 :sm :post)
    (#x0904 #x0914 :v nil)
    (#x0915 #x0939 :c nil)
    (#x093A #x093A :m :above) (#x093B #x093B :m :post)
    (#x093C #x093C :n nil)
    (#x093D #x093D :symbol nil)
    (#x093E #x093E :m :post) (#x093F #x093F :m :pre) (#x0940 #x0940 :m :post)
    (#x0941 #x0944 :m :below) (#x0945 #x0948 :m :above) (#x0949 #x094C :m :post)
    (#x094D #x094D :h nil)
    (#x094E #x094E :m :pre) (#x094F #x094F :m :post)
    (#x0951 #x0952 :a nil) (#x0953 #x0954 :sm :above)
    (#x0955 #x0955 :m :above) (#x0956 #x0957 :m :below)
    (#x0958 #x095F :c nil)
    (#x0960 #x0961 :v nil)
    (#x0962 #x0963 :m :below)
    (#x0966 #x096F :placeholder nil)
    (#x0972 #x0977 :v nil)
    (#x0978 #x097F :c nil)
    ;; Bengali
    (#x0980 #x0980 :placeholder nil)
    (#x0981 #x0981 :sm :above) (#x0982 #x0983 :sm :post)
    (#x0985 #x098C :v nil) (#x098F #x0990 :v nil) (#x0993 #x0994 :v nil)
    (#x0995 #x09A8 :c nil) (#x09AA #x09B0 :c nil) (#x09B2 #x09B2 :c nil) (#x09B6 #x09B9 :c nil)
    (#x09BC #x09BC :n nil)
    (#x09BD #x09BD :symbol nil)
    (#x09BE #x09BE :m :post) (#x09BF #x09BF :m :pre) (#x09C0 #x09C0 :m :post)
    (#x09C1 #x09C4 :m :below) (#x09C7 #x09C8 :m :pre) (#x09CB #x09CC :m :post)
    (#x09CD #x09CD :h nil)
    (#x09CE #x09CE :c nil)
    (#x09D7 #x09D7 :m :post)
    (#x09DC #x09DF :c nil)
    (#x09E0 #x09E1 :v nil)
    (#x09E2 #x09E3 :m :below)
    (#x09E6 #x09EF :placeholder nil)
    (#x09F0 #x09F1 :c nil)
    (#x09FC #x09FC :placeholder nil)
    (#x09FE #x09FE :sm :above)
    ;; Shared
    (#x0020 #x0020 :placeholder nil) (#x00A0 #x00A0 :placeholder nil) (#x00D7 #x00D7 :placeholder nil)
    (#x200C #x200C :zwnj nil) (#x200D #x200D :zwj nil)
    (#x2010 #x2015 :placeholder nil) (#x2022 #x2022 :placeholder nil)
    (#x25CC #x25CC :dottedcircle nil) (#x25FB #x25FE :placeholder nil)))

(defun ra-p (code)
  (member code '(#x0930 #x09B0 #x09F0)))

(defun indic-category (code)
  "The HarfBuzz Indic category of CODE and, for a matra, which side of
the consonant it is written on."
  (loop for (lo hi cat side) in *indic-table*
        when (<= lo code hi)
          do (return (values (if (and (eq cat :c) (ra-p code)) :ra cat) side))
        finally (return (values :x nil))))

(defun matra-position (script side)
  "Where a matra of SIDE sorts within a syllable of SCRIPT (HarfBuzz's
matra_position_indic)."
  (ecase side
    (:pre +pos-pre-m+)
    (:post (ecase script (:devanagari +pos-after-sub+) (:bengali +pos-after-post+)))
    (:above +pos-after-sub+)
    (:below +pos-after-sub+)))

(defparameter +consonant-categories+ '(:c :cs :ra :cm :v :placeholder :dottedcircle))

(defun indic-properties (script code)
  "The category and initial position of CODE for SCRIPT."
  (multiple-value-bind (cat side) (indic-category code)
    (values cat
            (cond ((member cat +consonant-categories+) +pos-base-c+)
                  ((eq cat :m) (matra-position script side))
                  ((member cat '(:sm :a :symbol)) +pos-smvd+)
                  (t +pos-end+)))))

;;; --- general category, as far as the shaper cares -------------------------

(defparameter *mark-ranges*
  '(#x0300 #x036F
    #x0900 #x0903 #x093A #x093C #x093E #x094F #x0951 #x0957 #x0962 #x0963
    #x0981 #x0983 #x09BC #x09BC #x09BE #x09C4 #x09C7 #x09C8 #x09CB #x09CD #x09D7 #x09D7
    #x09E2 #x09E3 #x09FE #x09FE
    #x1CD0 #x1CD2 #x1CD4 #x1CE8 #x1CED #x1CED #x1CF4 #x1CF4 #x1CF8 #x1CF9
    #xA8E0 #xA8F1 #xA8FF #xA8FF
    #xFE00 #xFE0F))

(defun unicode-mark-p (code)
  "Whether CODE is a combining mark (general category Mn, Mc or Me) in
the scripts this library covers."
  (in-ranges-p code *mark-ranges*))

(defun continuation-p (code)
  "Whether CODE continues the grapheme of the character before it, as
HarfBuzz forms clusters: marks and the joiners."
  (or (unicode-mark-p code) (<= #x200C code #x200D)))

(defun letter-or-mark-p (code)
  "Whether CODE's general category lies in Format..Nonspacing_Mark:
a letter, a mark or a format character."
  (or (unicode-mark-p code)
      (<= #x200C code #x200D)
      (and (< code char-code-limit) (alpha-char-p (code-char code)))))

(defun combining-class (code)
  "The canonical combining class: nukta 7, virama 9, everything else 0."
  (case code
    ((#x093C #x09BC) 7)
    ((#x094D #x09CD) 9)
    (t 0)))

;;; --- normalization ---------------------------------------------------------

(defparameter *decompositions*
  '((#x0929 #x0928 #x093C) (#x0934 #x0933 #x093C)
    (#x0958 #x0915 #x093C) (#x0959 #x0916 #x093C) (#x095A #x0917 #x093C)
    (#x095B #x091C #x093C) (#x095C #x0921 #x093C) (#x095D #x0922 #x093C)
    (#x095E #x092B #x093C) (#x095F #x092F #x093C)
    (#x09CB #x09C7 #x09BE) (#x09CC #x09C7 #x09D7)
    (#x09DF #x09AF #x09BC))
  "Canonical decompositions in the two blocks, less the three HarfBuzz
keeps whole (U+0931, U+09DC, U+09DD).")

(defparameter *compositions*
  '((#x0928 #x093C #x0929) (#x0930 #x093C #x0931) (#x0933 #x093C #x0934))
  "The pairs that recompose: canonical compositions not on the
exclusion list, and never a mark with a mark.")

(defun decompose-code (code)
  (cdr (assoc code *decompositions*)))

(defun compose-codes (a b)
  (third (find-if (lambda (e) (and (= (first e) a) (= (second e) b))) *compositions*)))
