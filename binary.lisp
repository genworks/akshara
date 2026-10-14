;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; Big-endian readers over an octet vector holding one OpenType table,
;;; and the two small structures every layout table is built from:
;;; coverage tables and class definitions.

(in-package #:akshara)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline u8 u16 s16 u32))

(defun u8 (v i)
  (declare (type octets v) (type fixnum i))
  (aref v i))

(defun u16 (v i)
  (declare (type octets v) (type fixnum i))
  (logior (ash (aref v i) 8) (aref v (1+ i))))

(defun s16 (v i)
  (declare (type octets v) (type fixnum i))
  (let ((n (u16 v i)))
    (if (>= n #x8000) (- n #x10000) n)))

(defun u32 (v i)
  (declare (type octets v) (type fixnum i))
  (logior (ash (u16 v i) 16) (u16 v (+ i 2))))

(defun tag-string (v i)
  "The four-character tag at I, as a string."
  (map 'string #'code-char (subseq v i (+ i 4))))

(defun u16-vector (v i count)
  "COUNT consecutive uint16 values starting at I, as a simple vector."
  (let ((a (make-array count)))
    (dotimes (k count a)
      (setf (aref a k) (u16 v (+ i (* 2 k)))))))

(defun offset16 (v i base)
  "The Offset16 at I, resolved against BASE; NIL when the offset is 0
(a null offset in OpenType)."
  (let ((o (u16 v i)))
    (if (zerop o) nil (+ base o))))

;;; --- coverage ----------------------------------------------------------
;;;
;;; A coverage table maps the glyphs a subtable applies to onto dense
;;; indices.  Format 1 lists the glyphs in order; format 2 lists ranges
;;; with the index of each range's first glyph.

(defstruct (coverage (:constructor %make-coverage))
  format
  glyphs                                ; format 1: sorted simple vector of gids
  ranges)                               ; format 2: simple vector of (start end start-index)

(defun parse-coverage (v offset)
  (let ((format (u16 v offset))
        (count (u16 v (+ offset 2))))
    (ecase format
      (1 (%make-coverage :format 1 :glyphs (u16-vector v (+ offset 4) count)))
      (2 (let ((ranges (make-array count)))
           (dotimes (k count)
             (let ((at (+ offset 4 (* 6 k))))
               (setf (aref ranges k)
                     (list (u16 v at) (u16 v (+ at 2)) (u16 v (+ at 4))))))
           (%make-coverage :format 2 :ranges ranges))))))

(defun coverage-index (coverage gid)
  "The coverage index of GID, or NIL when GID is not covered."
  (declare (type fixnum gid))
  (ecase (coverage-format coverage)
    (1 (let* ((glyphs (coverage-glyphs coverage))
              (lo 0) (hi (1- (length glyphs))))
         (loop while (<= lo hi)
               do (let* ((mid (ash (+ lo hi) -1))
                         (g (aref glyphs mid)))
                    (cond ((= g gid) (return mid))
                          ((< g gid) (setf lo (1+ mid)))
                          (t (setf hi (1- mid))))))))
    (2 (let* ((ranges (coverage-ranges coverage))
              (lo 0) (hi (1- (length ranges))))
         (loop while (<= lo hi)
               do (let* ((mid (ash (+ lo hi) -1))
                         (r (aref ranges mid)))
                    (cond ((< gid (first r)) (setf hi (1- mid)))
                          ((> gid (second r)) (setf lo (1+ mid)))
                          (t (return (+ (third r) (- gid (first r))))))))))))

(defun covered-p (coverage gid)
  (and coverage (coverage-index coverage gid) t))

;;; --- class definitions -------------------------------------------------

(defstruct (class-def (:constructor %make-class-def))
  format
  start                                 ; format 1
  classes                               ; format 1: simple vector
  ranges)                               ; format 2: simple vector of (start end class)

(defun parse-class-def (v offset)
  (let ((format (u16 v offset)))
    (ecase format
      (1 (let ((start (u16 v (+ offset 2)))
               (count (u16 v (+ offset 4))))
           (%make-class-def :format 1 :start start
                            :classes (u16-vector v (+ offset 6) count))))
      (2 (let* ((count (u16 v (+ offset 2)))
                (ranges (make-array count)))
           (dotimes (k count)
             (let ((at (+ offset 4 (* 6 k))))
               (setf (aref ranges k)
                     (list (u16 v at) (u16 v (+ at 2)) (u16 v (+ at 4))))))
           (%make-class-def :format 2 :ranges ranges))))))

(defun class-of-glyph (class-def gid)
  "The class of GID under CLASS-DEF; 0 when unlisted, or when CLASS-DEF
is NIL."
  (declare (type fixnum gid))
  (if (null class-def)
      0
      (ecase (class-def-format class-def)
        (1 (let ((i (- gid (class-def-start class-def)))
                 (classes (class-def-classes class-def)))
             (if (and (>= i 0) (< i (length classes))) (aref classes i) 0)))
        (2 (let* ((ranges (class-def-ranges class-def))
                  (lo 0) (hi (1- (length ranges))))
             (loop while (<= lo hi)
                   do (let* ((mid (ash (+ lo hi) -1))
                             (r (aref ranges mid)))
                        (cond ((< gid (first r)) (setf hi (1- mid)))
                              ((> gid (second r)) (setf lo (1+ mid)))
                              (t (return (third r)))))
                   finally (return 0)))))))
