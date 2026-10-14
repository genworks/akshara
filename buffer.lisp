;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; The shaping buffer: one record per glyph, carrying what the glyph
;;; came from (code point, cluster), what the shaper decided about it
;;; (category, position, syllable, feature mask), what the lookups did
;;; to it (GDEF class and the substituted / ligated / multiplied
;;; flags, ligature id and component) and where it ended up (advance,
;;; offset, attachment).  The conventions follow HarfBuzz's
;;; hb_glyph_info_t so that its output can be matched exactly.

(in-package #:akshara)

(defconstant +props-base+ #x02)
(defconstant +props-ligature+ #x04)
(defconstant +props-mark+ #x08)
(defconstant +props-substituted+ #x10)
(defconstant +props-ligated+ #x20)
(defconstant +props-multiplied+ #x40)
(defconstant +props-preserve+ (logior +props-substituted+ +props-ligated+ +props-multiplied+))

(defstruct (ginfo (:conc-name gi-) (:copier copy-ginfo))
  (gid 0 :type fixnum)
  (codepoint 0 :type fixnum)
  (cluster 0 :type fixnum)
  (mask 1 :type fixnum)
  (syllable 0 :type fixnum)
  (syllable-type nil)
  (category :x)                         ; Indic category keyword
  (position 0 :type fixnum)             ; Indic position
  (props 0 :type fixnum)
  (lig-id 0 :type fixnum)
  (lig-comp 0 :type fixnum)             ; component number for a mark/component; count for a ligature
  (lig-base-p nil)
  (unicode-mark-p nil)
  (continuation-p nil)
  (x-advance 0)
  (y-advance 0)
  (x-offset 0)
  (y-offset 0)
  (attach-chain 0 :type fixnum)
  (attach-type nil))

(defmethod print-object ((g ginfo) stream)
  (print-unreadable-object (g stream)
    (format stream "g~d U+~4,'0X c~d ~a/~d m~b~@[ ~a~]"
            (gi-gid g) (gi-codepoint g) (gi-cluster g) (gi-category g) (gi-position g) (gi-mask g)
            (and (gi-attach-type g) (list (gi-x-offset g) (gi-y-offset g))))))

(declaim (inline gi-mark-p gi-base-p gi-ligature-p gi-substituted-p gi-ligated-p gi-multiplied-p))
(defun gi-mark-p (g) (logtest (gi-props g) +props-mark+))
(defun gi-base-p (g) (logtest (gi-props g) +props-base+))
(defun gi-ligature-p (g) (logtest (gi-props g) +props-ligature+))
(defun gi-substituted-p (g) (logtest (gi-props g) +props-substituted+))
(defun gi-ligated-p (g) (logtest (gi-props g) +props-ligated+))
(defun gi-multiplied-p (g) (logtest (gi-props g) +props-multiplied+))
(defun gi-ligated-and-didnt-multiply-p (g) (and (gi-ligated-p g) (not (gi-multiplied-p g))))

(defun gi-lig-num-comps (g)
  "The number of components, 1 for anything but a ligature glyph."
  (if (and (gi-ligature-p g) (gi-lig-base-p g)) (gi-lig-comp g) 1))

(defun gi-lig-component (g)
  "The ligature component a mark is attached to, 0 when it is none or
when the glyph is itself the ligature."
  (if (gi-lig-base-p g) 0 (gi-lig-comp g)))

(defun gdef-props (font gid)
  "The glyph-property bits GDEF gives GID: base, ligature, or mark with
its attachment class in the high byte."
  (case (glyph-class font gid)
    (1 +props-base+)
    (2 +props-ligature+)
    (3 (logior +props-mark+
               (ash (class-of-glyph (font-mark-attach-classes font) gid) 8)))
    (t 0)))

;;; --- the buffer -----------------------------------------------------------

(defun make-buffer (&optional (size 16))
  (make-array size :adjustable t :fill-pointer 0))

(defun buffer-insert (buf index info)
  "Insert INFO before INDEX."
  (vector-push-extend info buf)
  (replace buf buf :start1 (1+ index) :start2 index :end2 (1- (fill-pointer buf)))
  (setf (aref buf index) info)
  buf)

(defun buffer-delete (buf index)
  (replace buf buf :start1 index :start2 (1+ index))
  (decf (fill-pointer buf))
  buf)

(defun buffer-replace-range (buf start end new-infos)
  "Replace the glyphs in [START, END) with the list NEW-INFOS."
  (let* ((old (- end start))
         (new (length new-infos))
         (delta (- new old))
         (len (fill-pointer buf)))
    (cond ((plusp delta)
           (dotimes (k delta) (vector-push-extend nil buf))
           (replace buf buf :start1 (+ end delta) :start2 end :end2 len))
          ((minusp delta)
           (replace buf buf :start1 (+ end delta) :start2 end :end2 len)
           (setf (fill-pointer buf) (+ len delta))))
    (loop for info in new-infos
          for i from start
          do (setf (aref buf i) info))
    buf))

(defun merge-clusters (buf start end)
  "Give the glyphs in [START, END) one cluster, the smallest among them,
extending the range so clusters stay monotone (HarfBuzz's cluster
level 0)."
  (when (< (1+ start) end)
    (let ((len (fill-pointer buf)))
      (let ((cluster (gi-cluster (aref buf start))))
        (loop for i from (1+ start) below end
              do (setf cluster (min cluster (gi-cluster (aref buf i)))))
        ;; Extend end
        (loop while (and (< end len)
                         (= (gi-cluster (aref buf (1- end))) (gi-cluster (aref buf end))))
              do (incf end))
        ;; Extend start
        (loop while (and (< 0 start)
                         (= (gi-cluster (aref buf (1- start))) (gi-cluster (aref buf start))))
              do (decf start))
        (loop for i from start below end
              do (setf (gi-cluster (aref buf i)) cluster))))))

(defun buffer-move (buf from to)
  "Move the glyph at FROM to index TO, shifting the glyphs between."
  (let ((g (aref buf from)))
    (cond ((< from to)
           (replace buf buf :start1 from :start2 (1+ from) :end2 (1+ to)))
          ((> from to)
           (replace buf buf :start1 (1+ to) :start2 to :end2 from)))
    (setf (aref buf to) g)))

(defun buffer-reverse-range (buf start end)
  (let ((v (subseq buf start end)))
    (loop for i from start below end
          for g across (nreverse v)
          do (setf (aref buf i) g))))
