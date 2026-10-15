;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

(defsystem "akshara"
  :description "OpenType shaping: GSUB and GPOS through zpb-ttf, an Indic shaper for Devanagari, Bengali, Tamil, Telugu and Gujarati."
  :author "David Cooper"
  :license "MIT"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on ("zpb-ttf")
  :serial t
  :components ((:file "package")
               (:file "binary")
               (:file "layout")
               (:file "font")
               (:file "buffer")
               (:file "apply")
               (:file "unicode")
               (:file "indic")
               (:file "shape"))
  :in-order-to ((test-op (test-op "akshara/test"))))

(defsystem "akshara/pdf"
  :description "Shaped text onto a cl-pdf page as glyph outlines."
  :author "David Cooper"
  :license "MIT"
  :encoding :utf-8
  :depends-on ("akshara" "cl-pdf")
  :serial t
  :components ((:file "cl-pdf-glyphs")
               (:file "outlines")))

(defsystem "akshara/test"
  :description "The golden tests: (asdf:test-system \"akshara\")."
  :author "David Cooper"
  :license "MIT"
  :encoding :utf-8
  :depends-on ("akshara")
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (unless (uiop:symbol-call '#:akshara-test '#:run-tests)
               (error "akshara: tests failed"))))
