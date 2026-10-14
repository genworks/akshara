;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

(defpackage #:akshara
  (:use #:common-lisp)
  (:documentation "OpenType shaping in Common Lisp: GSUB and GPOS read
through zpb-ttf, an Indic shaper (Devanagari and Bengali), glyph ids
with positions out.")
  (:export #:load-font
           #:close-font
           #:with-font
           #:font
           #:font-path
           #:font-loader
           #:font-units-per-em
           #:font-glyph-count
           #:font-scripts
           #:font-features
           #:glyph-id
           #:advance-width
           #:glyph-class
           #:shape
           #:shaped-glyph
           #:sg-gid
           #:sg-cluster
           #:sg-x-advance
           #:sg-y-advance
           #:sg-x-offset
           #:sg-y-offset
           #:hb-string
           #:parse-hb-string
           #:shaped-width))
