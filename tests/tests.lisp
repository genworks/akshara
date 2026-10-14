;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; The tests: (asdf:test-system "akshara"), or load "akshara/test" and
;;; call (akshara-test:run-tests), which prints every line that shapes
;;; differently from HarfBuzz and a summary, and returns T when all
;;; agree.
;;;
;;; The corpus is tests/golden/: <script>-lines.txt, one line of text
;;; per line, and <script>-hb.txt, what hb-shape made of each with the
;;; font in fonts/ (see make-golden.sh).  A line passes when akshara's
;;; glyph ids, clusters, advances and offsets are all the same.

(defpackage #:akshara-test
  (:use #:common-lisp)
  (:export #:run-tests #:compare-corpus #:*golden-directory*))

(in-package #:akshara-test)

(defparameter *golden-directory*
  (merge-pathnames "golden/" (make-pathname :name nil :type nil
                                            :defaults (or *load-truename* *compile-file-truename*))))

(defparameter *fonts-directory*
  (merge-pathnames "../fonts/" (make-pathname :name nil :type nil
                                              :defaults (or *load-truename* *compile-file-truename*))))

(defparameter *corpora*
  '((:devanagari "deva" "NotoSerifDevanagari-SemiBold.ttf")
    (:bengali "beng" "NotoSerifBengali-SemiBold.ttf")))

(defun read-lines (path)
  (with-open-file (in path :external-format :utf-8)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun compare-corpus (script &key (verbose t))
  "Shape every line of SCRIPT's corpus and compare with HarfBuzz;
return the number of lines that differ and the number of lines."
  (destructuring-bind (prefix font-file) (rest (assoc script *corpora*))
    (let* ((lines (read-lines (merge-pathnames (format nil "~a-lines.txt" prefix) *golden-directory*)))
           (golden (read-lines (merge-pathnames (format nil "~a-hb.txt" prefix) *golden-directory*)))
           (failures 0))
      (unless (= (length lines) (length golden))
        (error "~a: ~d lines but ~d golden results" prefix (length lines) (length golden)))
      (akshara:with-font (font (merge-pathnames font-file *fonts-directory*))
        (loop for line in lines
              for want in golden
              for n from 1
              do (let ((got (handler-case (akshara:hb-string (akshara:shape font line :script script))
                              (error (e) (format nil "ERROR: ~a" e)))))
                   (unless (string= got want)
                     (incf failures)
                     (when verbose
                       (format t "~&~a line ~d: ~a~%  got  ~a~%  want ~a~%" prefix n line got want))))))
      (values failures (length lines)))))

(defun hb-string-roundtrip-p ()
  (let ((s "[89=0+674|103=0@-76,0+0|31=2+258|34=17@6,7+0|5=3+100,-20]"))
    (string= s (akshara:hb-string (akshara:parse-hb-string s)))))

(defun run-tests ()
  (let ((ok t))
    (unless (hb-string-roundtrip-p)
      (format t "~&hb-string round trip failed~%")
      (setf ok nil))
    (dolist (corpus *corpora*)
      (multiple-value-bind (failures total) (compare-corpus (first corpus))
        (format t "~&~(~a~): ~d of ~d lines match HarfBuzz~%" (first corpus) (- total failures) total)
        (unless (zerop failures) (setf ok nil))))
    ok))
