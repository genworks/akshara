;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; Applying lookups to the buffer: the matcher that skips glyphs a
;;; lookup ignores, each subtable's apply method, nested lookups from
;;; contextual rules, and the would-apply test the Indic shaper uses
;;; to ask what a font would do with a glyph pair.  Written after the
;;; OpenType spec and HarfBuzz's hb-ot-layout-gsubgpos.hh, whose
;;; behaviour is the reference.

(in-package #:akshara)

(defstruct (actx (:constructor %make-actx))
  font
  buffer
  kind                                  ; :gsub or :gpos
  (idx 0 :type fixnum)
  (lookup-mask -1 :type fixnum)
  (lookup-flag 0 :type fixnum)
  mark-filtering-set
  per-syllable
  auto-zwnj
  auto-zwj
  (nesting 6)
  (zero-context nil))                   ; for would-apply

(declaim (inline cur buffer-len))
(defun cur (c) (aref (actx-buffer c) (actx-idx c)))
(defun buffer-len (c) (fill-pointer (actx-buffer c)))

(defun zwnj-p (g) (= (gi-codepoint g) #x200C))
(defun zwj-p (g) (= (gi-codepoint g) #x200D))
(defun default-ignorable-p (g) (<= #x200C (gi-codepoint g) #x200D))

;;; --- glyph properties against lookup flags -------------------------------

(defun glyph-property-ok-p (c info flag set)
  "Whether INFO is a glyph this lookup may act on or match, given the
lookup FLAG and mark filtering SET."
  (let ((props (gi-props info)))
    (cond ((/= 0 (logand props flag +flag-ignore-flags+)) nil)
          ((logtest props +props-mark+)
           (cond ((logtest flag +flag-use-mark-filtering-set+)
                  (and set (mark-set-covers-p (actx-font c) set (gi-gid info))))
                 ((logtest flag +flag-mark-attachment-type+)
                  (= (logand flag +flag-mark-attachment-type+)
                     (logand props +flag-mark-attachment-type+)))
                 (t t)))
          (t t))))

;;; --- the skipping matcher -------------------------------------------------
;;;
;;; A match spec is (match-fn . data): match-fn takes a glyph info and
;;; one element of DATA's sequence.  CONTEXT-P selects HarfBuzz's
;;; context iterator: any mask, joiners skipped.

(defun may-skip (c info flag set ignore-zwnj ignore-zwj)
  (cond ((not (glyph-property-ok-p c info flag set)) :yes)
        ((and (default-ignorable-p info)
              (or ignore-zwnj (not (zwnj-p info)))
              (or ignore-zwj (not (zwj-p info))))
         :maybe)
        (t :no)))

(defun may-match (info mask syllable match-fn datum)
  (cond ((or (not (logtest (gi-mask info) mask))
             (and (/= syllable 0) (/= syllable (gi-syllable info))))
         :no)
        ((null match-fn) :maybe)
        ((funcall match-fn info datum) :yes)
        (t :no)))

(defun iter-params (c context-p)
  "The mask, ignore-zwnj and ignore-zwj a HarfBuzz skipping iterator
would use: CONTEXT-P for backtrack and lookahead, else input."
  (let ((gpos (eq (actx-kind c) :gpos)))
    (values (if context-p -1 (actx-lookup-mask c))
            (or gpos (and context-p (actx-auto-zwnj c)))
            (or gpos context-p (actx-auto-zwj c)))))

(defun iter-syllable (c)
  (if (and (eq (actx-kind c) :gsub) (actx-per-syllable c))
      (gi-syllable (cur c))
      0))

(defun iter-next (c start count match-fn data &key context-p (flag (actx-lookup-flag c)) (set (actx-mark-filtering-set c)) (syllable (iter-syllable c)))
  "Match COUNT glyphs after START forward, each against the next element
of DATA (a vector, or NIL for any glyph) through MATCH-FN; return the
list of matched indices, or NIL."
  (multiple-value-bind (mask ignore-zwnj ignore-zwj) (iter-params c context-p)
    (let ((buf (actx-buffer c))
          (idx start)
          (positions nil)
          (len (buffer-len c)))
      (dotimes (k count (nreverse positions))
        (let ((datum (and data (aref data k)))
              (found nil))
          (loop while (< (1+ idx) len)
                do (incf idx)
                   (let* ((info (aref buf idx))
                          (skip (may-skip c info flag set ignore-zwnj ignore-zwj)))
                     (unless (eq skip :yes)
                       (let ((match (may-match info mask syllable match-fn datum)))
                         (cond ((or (eq match :yes) (and (eq match :maybe) (eq skip :no)))
                                (setf found t)
                                (return))
                               ((eq skip :no) (return)))))))
          (if found
              (push idx positions)
              (return nil)))))))

(defun iter-prev (c start count match-fn data &key context-p (flag (actx-lookup-flag c)) (set (actx-mark-filtering-set c)) (syllable (iter-syllable c)))
  "The backward twin of `iter-next': COUNT glyphs before START, nearest
first."
  (multiple-value-bind (mask ignore-zwnj ignore-zwj) (iter-params c context-p)
    (let ((buf (actx-buffer c))
          (idx start)
          (positions nil))
      (dotimes (k count (nreverse positions))
        (let ((datum (and data (aref data k)))
              (found nil))
          (loop while (> idx 0)
                do (decf idx)
                   (let* ((info (aref buf idx))
                          (skip (may-skip c info flag set ignore-zwnj ignore-zwj)))
                     (unless (eq skip :yes)
                       (let ((match (may-match info mask syllable match-fn datum)))
                         (cond ((or (eq match :yes) (and (eq match :maybe) (eq skip :no)))
                                (setf found t)
                                (return))
                               ((eq skip :no) (return)))))))
          (if found
              (push idx positions)
              (return nil)))))))

;;; Match functions for the three context formats.
(defun match-glyph (info value) (= (gi-gid info) value))
(defun match-coverage (info coverage) (covered-p coverage (gi-gid info)))
(defun make-class-matcher (class-def)
  (lambda (info value) (= (class-of-glyph class-def (gi-gid info)) value)))

;;; --- changing glyphs -------------------------------------------------------

(defun set-glyph-props (c info gid &key ligature component)
  "Give INFO the new glyph GID with HarfBuzz's _set_glyph_class: the
GDEF class of the new glyph plus the substituted / ligated /
multiplied history."
  (let ((props (logior (gi-props info) +props-substituted+)))
    (when ligature
      (setf props (logandc2 (logior props +props-ligated+) +props-multiplied+)))
    (when component
      (setf props (logior props +props-multiplied+)))
    (setf (gi-props info)
          (logior (logand props +props-preserve+) (gdef-props (actx-font c) gid)))
    (setf (gi-gid info) gid)
    info))

(defun replace-glyph (c gid)
  "Replace the current glyph with GID and step past it."
  (set-glyph-props c (cur c) gid)
  (incf (actx-idx c)))

;;; --- GSUB subtables ----------------------------------------------------------

(defgeneric apply-subtable (st c)
  (:documentation "Try ST at the current glyph of C; T when it applied,
with C's index moved past what it consumed."))

(defmethod apply-subtable ((st t) c)
  (declare (ignore c))
  nil)

(defmethod apply-subtable ((st single-subst) c)
  (let* ((info (cur c))
         (i (coverage-index (single-subst-coverage st) (gi-gid info))))
    (when i
      (replace-glyph c (if (single-subst-substitutes st)
                           (aref (single-subst-substitutes st) i)
                           (logand (+ (gi-gid info) (single-subst-delta st)) #xFFFF)))
      t)))

(defmethod apply-subtable ((st alternate-subst) c)
  (let* ((info (cur c))
         (i (coverage-index (alternate-subst-coverage st) (gi-gid info))))
    (when (and i (plusp (length (aref (alternate-subst-alternates st) i))))
      (replace-glyph c (aref (aref (alternate-subst-alternates st) i) 0))
      t)))

(defmethod apply-subtable ((st multiple-subst) c)
  (let* ((info (cur c))
         (i (coverage-index (multiple-subst-coverage st) (gi-gid info))))
    (when i
      (let* ((seq (aref (multiple-subst-sequences st) i))
             (count (length seq))
             (buf (actx-buffer c))
             (idx (actx-idx c)))
        (case count
          (1 (replace-glyph c (aref seq 0)))
          (0 (buffer-delete buf idx))
          (t (let ((lig-id (gi-lig-id info))
                   (outputs nil))
               (dotimes (k count)
                 (let ((g (copy-ginfo info)))
                   (when (zerop lig-id)
                     (setf (gi-lig-id g) 0 (gi-lig-comp g) k (gi-lig-base-p g) nil))
                   (set-glyph-props c g (aref seq k) :component t)
                   (push g outputs)))
               (buffer-replace-range buf idx (1+ idx) (nreverse outputs))
               (setf (actx-idx c) (+ idx count)))))
        t))))

(defvar *lig-id-counter* 0)
(defun allocate-lig-id ()
  (setf *lig-id-counter* (1+ (mod *lig-id-counter* 254))))

(defun match-input (c count data match-fn)
  "Match COUNT glyphs after the current one against DATA (a vector) with
MATCH-FN, honouring ligature component rules; return the list of
positions (the current index first) and the total component count, or
NIL."
  (let* ((buf (actx-buffer c))
         (first (cur c))
         (first-lig-id (gi-lig-id first))
         (first-lig-comp (gi-lig-component first))
         (positions (iter-next c (actx-idx c) count match-fn data)))
    (when (or (zerop count) positions)
      (let ((total 0))
        (dolist (p positions)
          (let* ((info (aref buf p))
                 (this-id (gi-lig-id info))
                 (this-comp (gi-lig-component info)))
            (if (and (/= first-lig-id 0) (/= first-lig-comp 0))
                (when (or (/= first-lig-id this-id) (/= first-lig-comp this-comp))
                  (return-from match-input nil))
                (when (and (/= this-id 0) (/= this-comp 0) (/= this-id first-lig-id))
                  (return-from match-input nil)))
            (incf total (gi-lig-num-comps info))))
        (incf total (gi-lig-num-comps first))
        (values (cons (actx-idx c) positions) total)))))

(defun ligate-input (c positions lig-glyph total-components)
  "Replace the glyphs at POSITIONS (the current one first) by LIG-GLYPH,
marking skipped glyphs between them as attached to its components."
  (let* ((buf (actx-buffer c))
         (start (first positions))
         (end (1+ (car (last positions))))
         (first (aref buf start))
         (is-base-ligature (gi-base-p first))
         (is-mark-ligature (gi-mark-p first)))
    (merge-clusters buf start end)
    (dolist (p (rest positions))
      (unless (gi-mark-p (aref buf p))
        (setf is-base-ligature nil is-mark-ligature nil)))
    (let* ((is-ligature (and (not is-base-ligature) (not is-mark-ligature)))
           (lig-id (if is-ligature (allocate-lig-id) 0))
           (last-num-components (gi-lig-num-comps first))
           (components-so-far last-num-components)
           (last-lig-id (gi-lig-id first)))
      (when is-ligature
        (setf (gi-lig-id first) lig-id
              (gi-lig-comp first) total-components
              (gi-lig-base-p first) t)
        (when (gi-unicode-mark-p first)
          (setf (gi-unicode-mark-p first) nil)))
      ;; Marked as ligated whatever the class: a base that took marks keeps
      ;; the base class but was ligated all the same (HarfBuzz's
      ;; replace_glyph_with_ligature).
      (set-glyph-props c first lig-glyph :ligature t)
      (unless is-ligature
        (setf (gi-props first) (logandc2 (gi-props first) +props-ligature+)))
      ;; Walk the components, re-attaching the marks between them.
      (let ((keep (list first))
            (i (1+ start)))
        (dolist (p (rest positions))
          (loop while (< i p)
                do (let ((g (aref buf i)))
                     (when is-ligature
                       (let* ((this-comp (let ((cc (gi-lig-component g))) (if (zerop cc) last-num-components cc)))
                              (new-comp (+ (- components-so-far last-num-components)
                                           (min this-comp last-num-components))))
                         (setf (gi-lig-id g) lig-id (gi-lig-comp g) new-comp (gi-lig-base-p g) nil)))
                     (push g keep)
                     (incf i)))
          (let ((comp (aref buf p)))
            (setf last-lig-id (gi-lig-id comp)
                  last-num-components (gi-lig-num-comps comp))
            (incf components-so-far last-num-components))
          (incf i))
        (setf keep (nreverse keep))
        (buffer-replace-range buf start end keep)
        (let ((new-idx (+ start (length keep))))
          ;; Marks after the ligature that belonged to the last component
          (when (and (not is-mark-ligature) (/= last-lig-id 0))
            (loop for j from new-idx below (buffer-len c)
                  for g = (aref buf j)
                  while (= (gi-lig-id g) last-lig-id)
                  do (let ((this-comp (gi-lig-component g)))
                       (when (zerop this-comp) (return))
                       (setf (gi-lig-id g) lig-id
                             (gi-lig-comp g) (+ (- components-so-far last-num-components)
                                                (min this-comp last-num-components))
                             (gi-lig-base-p g) nil))))
          (setf (actx-idx c) new-idx))))))

(defmethod apply-subtable ((st ligature-subst) c)
  (let* ((info (cur c))
         (i (coverage-index (ligature-subst-coverage st) (gi-gid info))))
    (when i
      (dolist (lig (aref (ligature-subst-ligature-sets st) i))
        (let ((components (cdr lig)))
          (if (zerop (length components))
              (progn (replace-glyph c (car lig))
                     (return t))
              (multiple-value-bind (positions total)
                  (match-input c (length components) components #'match-glyph)
                (when positions
                  (ligate-input c positions (car lig) total)
                  (return t)))))))))

;;; --- contextual subtables (GSUB 5, 6 and GPOS 7, 8) ------------------------

(defun context-match-fn (ctx which)
  "The match function and its datum transformer for WHICH (:backtrack
:input :lookahead) of the context CTX."
  (ecase (ctx-format ctx)
    (1 #'match-glyph)
    (2 (make-class-matcher (ecase which
                             (:backtrack (ctx-backtrack-class-def ctx))
                             (:input (ctx-input-class-def ctx))
                             (:lookahead (ctx-lookahead-class-def ctx)))))
    (3 #'match-coverage)))

(defun candidate-rules (ctx gid)
  "The rules of CTX whose first glyph can be GID."
  (ecase (ctx-format ctx)
    (1 (let ((i (coverage-index (ctx-coverage ctx) gid)))
         (and i (< i (length (ctx-rule-sets ctx))) (aref (ctx-rule-sets ctx) i))))
    (2 (when (covered-p (ctx-coverage ctx) gid)
         (let ((class (class-of-glyph (ctx-input-class-def ctx) gid)))
           (and (< class (length (ctx-rule-sets ctx))) (aref (ctx-rule-sets ctx) class)))))
    (3 (and (covered-p (ctx-coverage ctx) gid) (list (ctx-rule ctx))))))

(defun match-rule (c ctx rule)
  "Match RULE at the current glyph: the input positions (current first)
or NIL."
  (let* ((input (rule-input rule))
         (positions (if (zerop (length input))
                        (list (actx-idx c))
                        (match-input c (length input) input (context-match-fn ctx :input)))))
    (when positions
      (let ((backtrack (rule-backtrack rule))
            (lookahead (rule-lookahead rule)))
        (and (or (zerop (length backtrack))
                 (iter-prev c (actx-idx c) (length backtrack) (context-match-fn ctx :backtrack) backtrack :context-p t))
             (or (zerop (length lookahead))
                 (iter-next c (car (last positions)) (length lookahead) (context-match-fn ctx :lookahead) lookahead :context-p t))
             positions)))))

(defmethod apply-subtable ((st context) c)
  (let ((gid (gi-gid (cur c))))
    (dolist (rule (candidate-rules st gid))
      (let ((positions (match-rule c st rule)))
        (when positions
          (apply-nested-lookups c positions (rule-records rule))
          (return t))))))

(defun apply-nested-lookups (c positions records)
  "HarfBuzz's apply_lookup: run each (sequence-index . lookup) of
RECORDS at its matched position, keeping the positions right as nested
lookups change the buffer's length, and leave the index after the
match."
  (let* ((positions (coerce positions 'vector))
         (count (length positions))
         (end (1+ (aref positions (1- count)))))
    (dolist (rec records)
      (let ((seq-index (car rec))
            (lookup-index (cdr rec)))
        (when (< seq-index count)
          (let ((orig-len (buffer-len c)))
            (when (>= (aref positions seq-index) orig-len) (return))
            (setf (actx-idx c) (aref positions seq-index))
            (when (recurse-lookup c lookup-index)
              (let ((delta (- (buffer-len c) orig-len)))
                (unless (zerop delta)
                  (incf end delta)
                  (when (< end (aref positions seq-index))
                    (incf delta (- (aref positions seq-index) end))
                    (setf end (aref positions seq-index)))
                  (let ((next (1+ seq-index)))
                    (if (plusp delta)
                        (let ((new (make-array (+ count delta))))
                          (replace new positions :end2 next)
                          (replace new positions :start1 (+ next delta) :start2 next)
                          (setf positions new))
                        (progn
                          (setf delta (max delta (- next count)))
                          (decf next delta)
                          (replace positions positions :start1 (+ next delta) :start2 next)
                          (setf positions (subseq positions 0 (+ count delta)))
                          (setf next (+ next delta))))
                    (incf count delta)
                    (loop for j from (1+ seq-index) below next
                          do (setf (aref positions j) (1+ (aref positions (1- j)))))
                    (loop for j from next below count
                          do (incf (aref positions j) delta))))))))))
    (setf (actx-idx c) end)))

(defun recurse-lookup (c lookup-index)
  "Apply the lookup LOOKUP-INDEX once at the current glyph, under its
own flags."
  (when (plusp (actx-nesting c))
    (let* ((lt (ecase (actx-kind c) (:gsub (font-gsub (actx-font c))) (:gpos (font-gpos (actx-font c)))))
           (lookup (aref (lt-lookups lt) lookup-index))
           (saved-flag (actx-lookup-flag c))
           (saved-set (actx-mark-filtering-set c)))
      (setf (actx-lookup-flag c) (lookup-flag lookup)
            (actx-mark-filtering-set c) (lookup-mark-filtering-set lookup))
      (decf (actx-nesting c))
      (unwind-protect
           (dolist (st (lookup-subtables lookup) nil)
             (when (apply-subtable st c) (return t)))
        (incf (actx-nesting c))
        (setf (actx-lookup-flag c) saved-flag
              (actx-mark-filtering-set c) saved-set)))))

;;; --- GPOS subtables ------------------------------------------------------------

(defun add-value (info value)
  (destructuring-bind (xp yp xa ya) value
    (incf (gi-x-offset info) xp)
    (incf (gi-y-offset info) yp)
    (incf (gi-x-advance info) xa)
    (incf (gi-y-advance info) ya)))

(defmethod apply-subtable ((st single-pos) c)
  (let* ((info (cur c))
         (i (coverage-index (single-pos-coverage st) (gi-gid info))))
    (when i
      (let ((values (single-pos-values st)))
        (add-value info (if (vectorp values) (aref values i) values)))
      (incf (actx-idx c))
      t)))

(defmethod apply-subtable ((st pair-pos) c)
  (let* ((info (cur c))
         (i (coverage-index (pair-pos-coverage st) (gi-gid info))))
    (when i
      (let ((next (first (iter-next c (actx-idx c) 1 nil nil))))
        (when next
          (let* ((buf (actx-buffer c))
                 (second (aref buf next))
                 (pair (ecase (pair-pos-format st)
                         (1 (let ((set (aref (pair-pos-pair-sets st) i))
                                  (gid2 (gi-gid second)))
                              (let ((hit (find gid2 set :key #'first)))
                                (and hit (cons (second hit) (third hit))))))
                         (2 (let ((c1 (class-of-glyph (pair-pos-class-def1 st) (gi-gid info)))
                                  (c2 (class-of-glyph (pair-pos-class-def2 st) (gi-gid second))))
                              (and (< c1 (pair-pos-class1-count st))
                                   (< c2 (pair-pos-class2-count st))
                                   (aref (pair-pos-records st) c1 c2)))))))
            (when pair
              (add-value info (car pair))
              (add-value second (cdr pair))
              (setf (actx-idx c) (if (zerop (pair-pos-format2 st)) next (1+ next)))
              t)))))))

(defun attach-mark (c mark-array mark-index anchors glyph-pos)
  "Position the current glyph (a mark) on the glyph at GLYPH-POS, whose
anchors by class are the vector ANCHORS."
  (let* ((record (aref mark-array mark-index))
         (class (car record))
         (mark-anchor (cdr record))
         (base-anchor (and (< class (length anchors)) (aref anchors class))))
    (when (and mark-anchor base-anchor)
      (let ((info (cur c)))
        (setf (gi-x-offset info) (- (car base-anchor) (car mark-anchor))
              (gi-y-offset info) (- (cdr base-anchor) (cdr mark-anchor))
              (gi-attach-type info) :mark
              (gi-attach-chain info) (- glyph-pos (actx-idx c)))
        (incf (actx-idx c))
        t))))

(defun find-base-glyph (c)
  "The index of the glyph a mark at the current position attaches to:
backwards past marks, skipping later glyphs of a multiple
substitution so a mark attaches to its first."
  (let ((buf (actx-buffer c))
        (idx (actx-idx c)))
    (loop
      (let ((prev (first (iter-prev c idx 1 nil nil :flag +flag-ignore-marks+ :set nil :syllable 0))))
        (unless prev (return nil))
        (let ((g (aref buf prev)))
          (when (or (not (gi-multiplied-p g))
                    (zerop (gi-lig-component g))
                    (zerop prev)
                    (gi-mark-p (aref buf (1- prev)))
                    (not (gi-multiplied-p (aref buf (1- prev))))
                    (/= (gi-lig-id g) (gi-lig-id (aref buf (1- prev))))
                    (/= (gi-lig-component g) (1+ (gi-lig-component (aref buf (1- prev))))))
            (return prev))
          (setf idx prev))))))

(defmethod apply-subtable ((st mark-base-pos) c)
  (let* ((info (cur c))
         (mark-index (coverage-index (mark-base-pos-mark-coverage st) (gi-gid info))))
    (when mark-index
      (let ((base-pos (find-base-glyph c)))
        (when base-pos
          (let ((base-index (coverage-index (mark-base-pos-base-coverage st)
                                            (gi-gid (aref (actx-buffer c) base-pos)))))
            (when base-index
              (attach-mark c (mark-base-pos-mark-array st) mark-index
                           (aref (mark-base-pos-base-array st) base-index) base-pos))))))))

(defmethod apply-subtable ((st mark-lig-pos) c)
  (let* ((info (cur c))
         (mark-index (coverage-index (mark-lig-pos-mark-coverage st) (gi-gid info))))
    (when mark-index
      (let ((lig-pos (first (iter-prev c (actx-idx c) 1 nil nil :flag +flag-ignore-marks+ :set nil :syllable 0))))
        (when lig-pos
          (let* ((buf (actx-buffer c))
                 (lig (aref buf lig-pos))
                 (lig-index (coverage-index (mark-lig-pos-lig-coverage st) (gi-gid lig))))
            (when lig-index
              (let* ((components (aref (mark-lig-pos-lig-array st) lig-index))
                     (comp-count (length components))
                     (lig-id (gi-lig-id lig))
                     (mark-id (gi-lig-id info))
                     (mark-comp (gi-lig-component info))
                     (comp-index (if (and (/= lig-id 0) (= lig-id mark-id) (/= mark-comp 0))
                                     (1- (min mark-comp comp-count))
                                     (1- comp-count))))
                (when (and (plusp comp-count) (>= comp-index 0))
                  (attach-mark c (mark-lig-pos-mark-array st) mark-index
                               (aref components comp-index) lig-pos))))))))))

(defmethod apply-subtable ((st mark-mark-pos) c)
  (let* ((info (cur c))
         (mark1-index (coverage-index (mark-mark-pos-mark1-coverage st) (gi-gid info))))
    (when mark1-index
      (let ((prev (first (iter-prev c (actx-idx c) 1 nil nil
                                    :flag (logandc2 (actx-lookup-flag c) +flag-ignore-flags+)
                                    :syllable 0))))
        (when prev
          (let* ((buf (actx-buffer c))
                 (m2 (aref buf prev)))
            (when (gi-mark-p m2)
              (let ((id1 (gi-lig-id info)) (id2 (gi-lig-id m2))
                    (comp1 (gi-lig-component info)) (comp2 (gi-lig-component m2)))
                (when (if (= id1 id2)
                          (or (zerop id1) (= comp1 comp2))
                          (or (and (plusp id1) (zerop comp1)) (and (plusp id2) (zerop comp2))))
                  (let ((mark2-index (coverage-index (mark-mark-pos-mark2-coverage st) (gi-gid m2))))
                    (when mark2-index
                      (attach-mark c (mark-mark-pos-mark1-array st) mark1-index
                                   (aref (mark-mark-pos-mark2-array st) mark2-index) prev))))))))))))

;;; --- driving a lookup over the buffer -------------------------------------------

(defun apply-lookup (c lookup mask &key per-syllable (auto-zwnj t) (auto-zwj t))
  "Apply LOOKUP left to right over the whole buffer to the glyphs whose
mask meets MASK."
  (setf (actx-lookup-mask c) mask
        (actx-lookup-flag c) (lookup-flag lookup)
        (actx-mark-filtering-set c) (lookup-mark-filtering-set lookup)
        (actx-per-syllable c) per-syllable
        (actx-auto-zwnj c) auto-zwnj
        (actx-auto-zwj c) auto-zwj
        (actx-idx c) 0)
  (let ((applied nil))
    (loop while (< (actx-idx c) (buffer-len c))
          do (let ((info (cur c)))
               (if (and (logtest (gi-mask info) mask)
                        (glyph-property-ok-p c info (lookup-flag lookup) (lookup-mark-filtering-set lookup))
                        (dolist (st (lookup-subtables lookup) nil)
                          (when (apply-subtable st c) (return t))))
                   (setf applied t)
                   (incf (actx-idx c)))))
    applied))

;;; --- would-apply: what a lookup would do to a glyph sequence -------------------

(defgeneric would-apply-subtable (st gids zero-context)
  (:documentation "Whether ST applies to exactly the glyph sequence GIDS
(a list) with no other glyphs around; ZERO-CONTEXT forbids rules with
backtrack or lookahead."))

(defmethod would-apply-subtable ((st t) gids zero-context)
  (declare (ignore gids zero-context))
  nil)

(defmethod would-apply-subtable ((st single-subst) gids zero-context)
  (declare (ignore zero-context))
  (and (= (length gids) 1) (covered-p (single-subst-coverage st) (first gids))))

(defmethod would-apply-subtable ((st multiple-subst) gids zero-context)
  (declare (ignore zero-context))
  (and (= (length gids) 1) (covered-p (multiple-subst-coverage st) (first gids))))

(defmethod would-apply-subtable ((st alternate-subst) gids zero-context)
  (declare (ignore zero-context))
  (and (= (length gids) 1) (covered-p (alternate-subst-coverage st) (first gids))))

(defmethod would-apply-subtable ((st ligature-subst) gids zero-context)
  (declare (ignore zero-context))
  (let ((i (coverage-index (ligature-subst-coverage st) (first gids))))
    (and i
         (loop for lig in (aref (ligature-subst-ligature-sets st) i)
               thereis (and (= (length (cdr lig)) (1- (length gids)))
                            (every #'= (cdr lig) (rest gids)))))))

(defmethod would-apply-subtable ((st context) gids zero-context)
  (when (eq (ctx-kind st) :gsub)
    (loop for rule in (candidate-rules st (first gids))
          thereis (and (or (not zero-context)
                           (not (ctx-chain-p st))
                           (and (zerop (length (rule-backtrack rule)))
                                (zerop (length (rule-lookahead rule)))))
                       (= (length (rule-input rule)) (1- (length gids)))
                       (let ((fn (context-match-fn st :input)))
                         (loop for gid in (rest gids)
                               for datum across (rule-input rule)
                               always (funcall fn (make-ginfo :gid gid) datum)))))))

(defun lookup-would-substitute-p (lookup gids zero-context)
  (loop for st in (lookup-subtables lookup)
        thereis (would-apply-subtable st gids zero-context)))
