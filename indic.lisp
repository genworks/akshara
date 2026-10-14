;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; The Indic shaper: syllables, the reordering before and after the
;;; basic shaping features, and the per-glyph feature masks.  A port of
;;; the algorithm in HarfBuzz's hb-ot-shaper-indic.cc, which itself
;;; follows Microsoft's Devanagari/Bengali OpenType shaping
;;; specifications, kept close enough that the two agree glyph for
;;; glyph on the fonts in the test corpus.

(in-package #:akshara)

(defstruct (indic-config (:constructor %make-indic-config))
  script
  virama
  reph-pos                              ; :after-main :before-sub :after-sub :before-post :after-post
  blwf-mode)                            ; :pre-and-post or :post-only

(defparameter *indic-configs*
  (list (%make-indic-config :script :devanagari :virama #x094D :reph-pos :before-post :blwf-mode :pre-and-post)
        (%make-indic-config :script :bengali :virama #x09CD :reph-pos :after-sub :blwf-mode :pre-and-post)))

(defun indic-config (script)
  (find script *indic-configs* :key #'indic-config-script))

;;; The shaping plan: which lookups run in which stage with which
;;; masks.  Built by `make-plan' in shape.lisp; the reordering here
;;; reads its masks and asks its lookups what the font would do.

(defstruct (plan (:constructor %make-plan))
  font
  script                                ; :devanagari :bengali or NIL (the default shaper)
  config                                ; indic-config or NIL
  gsub-langsys
  gpos-langsys
  (feature-masks (make-hash-table :test #'equal))
  gsub-stages                           ; list of (lookup-entries . pause-function)
  gpos-lookups                          ; lookup-entries
  virama-gid)

(defun plan-feature-mask (plan tag)
  "The mask bit(s) of feature TAG: 1 for a global feature, its own bit
for a per-glyph one, 0 when the font lacks it."
  (gethash tag (plan-feature-masks plan) 0))

(defun plan-feature-lookups (plan tag)
  "The GSUB lookups of feature TAG in the plan's langsys."
  (let ((lt (font-gsub (plan-font plan))))
    (and lt (mapcar (lambda (i) (aref (lt-lookups lt) i))
                    (langsys-feature-lookups lt (plan-gsub-langsys plan) tag)))))

(defun consonant-p (g) (member (gi-category g) +consonant-categories+))
(defun joiner-p (g) (member (gi-category g) '(:zwj :zwnj)))
(defun halant-p (g) (eq (gi-category g) :h))

;;; --- syllables -------------------------------------------------------------
;;;
;;; The grammar of hb-ot-shaper-indic-machine.rl, matched here by a
;;; small pattern interpreter over the category sequence.  Patterns:
;;; a category keyword; (:cat k ...); (:seq p ...); (:alt p ...);
;;; (:opt p); (:star p); (:rep p min max).

(defun match-pattern (pat cats pos end)
  "Every position a match of PAT starting at POS can end at."
  (if (keywordp pat)
      (if (and (< pos end) (eq (aref cats pos) pat)) (list (1+ pos)) nil)
      (ecase (first pat)
        (:cat (if (and (< pos end) (member (aref cats pos) (rest pat))) (list (1+ pos)) nil))
        (:seq (let ((ends (list pos)))
                (dolist (p (rest pat) ends)
                  (setf ends (remove-duplicates
                              (loop for e in ends append (match-pattern p cats e end)))))))
        (:alt (remove-duplicates (loop for p in (rest pat) append (match-pattern p cats pos end))))
        (:opt (remove-duplicates (cons pos (match-pattern (second pat) cats pos end))))
        (:star (let ((all (list pos)) (frontier (list pos)))
                 (loop while frontier
                       do (let ((next (remove-if (lambda (e) (member e all))
                                                 (remove-duplicates
                                                  (loop for e in frontier append (match-pattern (second pat) cats e end))))))
                            (setf all (append all next) frontier next)))
                 all))
        (:rep (destructuring-bind (p min max) (rest pat)
                (let ((frontier (list pos)) (result (if (zerop min) (list pos) nil)))
                  (loop for n from 1 to max
                        while frontier
                        do (setf frontier (remove-duplicates
                                           (loop for e in frontier append (match-pattern p cats e end))))
                           (when (>= n min) (setf result (union result frontier))))
                  result))))))

(defparameter *c* '(:cat :c :ra))
(defparameter *n* `(:seq (:opt (:seq (:opt :zwnj) :rs)) (:opt (:seq :n (:opt :n)))))
(defparameter *z* '(:cat :zwj :zwnj))
(defparameter *reph* '(:alt (:seq :ra :h) :repha))
(defparameter *cn* `(:seq ,*c* (:opt :zwj) ,*n*))
(defparameter *symbol* '(:seq :symbol (:opt :n)))
(defparameter *matra-group* `(:seq (:star ,*z*) (:alt :m (:seq (:opt :sm) :mpst)) (:opt :n) (:opt :h)))
(defparameter *syllable-tail* `(:seq (:opt (:seq (:opt ,*z*) :sm (:opt :sm) (:opt :zwnj)))
                                     (:opt (:seq :a (:opt :a)))
                                     (:opt (:seq :vd (:opt :vd)))))
(defparameter *halant-group* `(:seq (:opt ,*z*) :h (:opt (:seq :zwj (:opt :n)))))
(defparameter *final-halant-group* `(:alt ,*halant-group* (:seq :h :zwnj)))
(defparameter *medial-group* '(:opt :cm))
(defparameter *halant-or-matra-group* `(:alt ,*final-halant-group* (:rep ,*matra-group* 0 4)))
(defparameter *complex-syllable-tail* `(:seq (:star (:seq ,*halant-group* ,*cn*)) ,*medial-group* ,*halant-or-matra-group* ,*syllable-tail*))

(defparameter *syllable-patterns*
  `((:consonant-syllable . (:seq (:opt (:cat :repha :cs)) ,*cn* ,*complex-syllable-tail*))
    (:vowel-syllable . (:seq (:opt ,*reph*) :v ,*n* (:alt :zwj ,*complex-syllable-tail*)))
    (:standalone-cluster . (:seq (:alt (:seq (:opt (:cat :repha :cs)) :placeholder) (:seq (:opt ,*reph*) :dottedcircle))
                                 ,*n* ,*complex-syllable-tail*))
    (:symbol-cluster . (:seq ,*symbol* ,*syllable-tail*))
    (:broken-cluster . (:seq (:opt ,*reph*) ,*n* ,*complex-syllable-tail*)))
  "In priority order; the longest match wins, ties to the first.")

(defun find-syllable (cats pos end)
  "The type and end of the syllable starting at POS."
  (let ((best-type :non-indic-cluster) (best-end (1+ pos)))
    (loop for (type . pat) in *syllable-patterns*
          do (let ((ends (match-pattern pat cats pos end)))
               (dolist (e ends)
                 (when (> e best-end)
                   (setf best-end e best-type type)))))
    (values best-type best-end)))

(defun setup-syllables (buf)
  "Number the syllables of the buffer and record each glyph's syllable
type."
  (let* ((len (fill-pointer buf))
         (cats (make-array len))
         (serial 0))
    (dotimes (i len) (setf (aref cats i) (gi-category (aref buf i))))
    (let ((pos 0))
      (loop while (< pos len)
            do (multiple-value-bind (type end) (find-syllable cats pos len)
                 (incf serial)
                 (loop for i from pos below end
                       do (setf (gi-syllable (aref buf i)) serial
                                (gi-syllable-type (aref buf i)) type))
                 (setf pos end))))
    buf))

(defun map-syllables (fn buf)
  "Call FN with the start and end of each syllable."
  (let ((len (fill-pointer buf)) (start 0))
    (loop while (< start len)
          do (let ((end (1+ start))
                   (syl (gi-syllable (aref buf start))))
               (loop while (and (< end len) (= (gi-syllable (aref buf end)) syl))
                     do (incf end))
               (funcall fn start end)
               (setf start end)))))

;;; --- consonant positions from the font ---------------------------------------

(defun would-substitute-p (plan tag gids)
  "Whether any lookup of feature TAG would act on exactly the glyphs
GIDS, with no context around them."
  (loop for lookup in (plan-feature-lookups plan tag)
        thereis (lookup-would-substitute-p lookup gids t)))

(defun consonant-position-from-face (plan consonant virama)
  "Where the font puts CONSONANT after a VIRAMA: a below-base form, a
post-base form, or the base position when it forms neither."
  (let ((cv (list consonant virama))
        (vc (list virama consonant)))
    (cond ((or (would-substitute-p plan "blwf" vc) (would-substitute-p plan "blwf" cv)
               (would-substitute-p plan "vatu" vc) (would-substitute-p plan "vatu" cv))
           +pos-below-c+)
          ((or (would-substitute-p plan "pstf" vc) (would-substitute-p plan "pstf" cv))
           +pos-post-c+)
          ((or (would-substitute-p plan "pref" vc) (would-substitute-p plan "pref" cv))
           +pos-post-c+)
          (t +pos-base-c+))))

(defun update-consonant-positions (plan buf)
  (let ((virama (plan-virama-gid plan)))
    (when (and virama (plusp virama))
      (loop for g across buf
            when (= (gi-position g) +pos-base-c+)
              do (setf (gi-position g) (consonant-position-from-face plan (gi-gid g) virama))))))

;;; --- initial reordering ------------------------------------------------------

(defun initial-reordering (plan buf)
  (update-consonant-positions plan buf)
  (map-syllables (lambda (start end)
                   (case (gi-syllable-type (aref buf start))
                     ((:consonant-syllable :broken-cluster :vowel-syllable :standalone-cluster)
                      (initial-reordering-consonant-syllable plan buf start end))))
                 buf))

(defun initial-reordering-consonant-syllable (plan buf start end)
  (let* ((config (plan-config plan))
         (mask-rphf (plan-feature-mask plan "rphf"))
         (mask-half (plan-feature-mask plan "half"))
         (mask-blwf (plan-feature-mask plan "blwf"))
         (mask-abvf (plan-feature-mask plan "abvf"))
         (mask-pstf (plan-feature-mask plan "pstf"))
         (base end)
         (has-reph nil))
    (flet ((info (i) (aref buf i)))
      ;; 1. Find the base consonant.
      (let ((limit start))
        (when (and (/= mask-rphf 0)
                   (<= (+ start 3) end)
                   (not (joiner-p (info (+ start 2))))
                   (would-substitute-p plan "rphf" (list (gi-gid (info start)) (gi-gid (info (1+ start))))))
          (incf limit 2)
          (loop while (and (< limit end) (joiner-p (info limit))) do (incf limit))
          (setf base start has-reph t))
        ;; Starting from the end, move backwards until a consonant that
        ;; has no below- or post-base form, or the first consonant.
        (let ((i end) (seen-below nil))
          (loop
            (decf i)
            (cond ((consonant-p (info i))
                   (when (and (/= (gi-position (info i)) +pos-below-c+)
                              (or (/= (gi-position (info i)) +pos-post-c+) seen-below))
                     (setf base i)
                     (return))
                   (when (= (gi-position (info i)) +pos-below-c+) (setf seen-below t))
                   (setf base i))
                  ((and (< start i) (eq (gi-category (info i)) :zwj) (halant-p (info (1- i))))
                   (return)))
            (unless (> i limit) (return))))
        ;; Ra + Halant with no other consonant: Ra is the base, not a reph.
        (when (and has-reph (= base start) (<= (- limit base) 2))
          (setf has-reph nil)))
      ;; Reorder characters: what is before the base is pre-base.
      (loop for i from start below base
            do (setf (gi-position (info i)) (min +pos-pre-c+ (gi-position (info i)))))
      (when (< base end) (setf (gi-position (info base)) +pos-base-c+))
      (when has-reph (setf (gi-position (info start)) +pos-ra-to-become-reph+))
      ;; Attach misc marks to the previous char so they move with it.
      (let ((last-pos +pos-start+))
        (loop for i from start below end
              do (let ((g (info i)))
                   (cond ((member (gi-category g) '(:zwj :zwnj :n :rs :cm :h))
                          (setf (gi-position g) last-pos)
                          (when (and (halant-p g) (= (gi-position g) +pos-pre-m+))
                            ;; Uniscribe doesn't move the Halant with a left matra.
                            (loop for j from i above start
                                  when (/= (gi-position (info (1- j))) +pos-pre-m+)
                                    do (setf (gi-position g) (gi-position (info (1- j))))
                                       (return))))
                         ((/= (gi-position g) +pos-smvd+)
                          (when (and (eq (gi-category g) :mpst) (> i start) (eq (gi-category (info (1- i))) :sm))
                            (setf (gi-position (info (1- i))) (gi-position g)))
                          (setf last-pos (gi-position g)))))))
      ;; Post-base consonants own anything before them since the last
      ;; consonant or matra.
      (let ((last base))
        (loop for i from (1+ base) below end
              do (cond ((consonant-p (info i))
                        (loop for j from (1+ last) below i
                              when (< (gi-position (info j)) +pos-smvd+)
                                do (setf (gi-position (info j)) (gi-position (info i))))
                        (setf last i))
                       ((eq (gi-category (info i)) :m) (setf last i)))))
      ;; Sort the syllable by position, stably, and merge the clusters
      ;; of what moved.
      (let* ((n (- end start))
             (orig (make-array n))
             (items (loop for i from start below end collect (cons (- i start) (info i)))))
        (setf items (stable-sort items #'< :key (lambda (item) (gi-position (cdr item)))))
        (loop for item in items
              for i from start
              do (setf (aref buf i) (cdr item)
                       (aref orig (- i start)) (car item)))
        ;; Find base again, and the left-matra range.
        (let ((first-left end) (last-left end))
          (setf base end)
          (loop for i from start below end
                do (cond ((= (gi-position (info i)) +pos-base-c+) (setf base i) (return))
                         ((= (gi-position (info i)) +pos-pre-m+)
                          (when (= first-left end) (setf first-left i))
                          (setf last-left i))))
          (when (< first-left last-left)
            (buffer-reverse-range buf first-left (1+ last-left))
            (let ((sub (subseq orig (- first-left start) (- (1+ last-left) start))))
              (loop for i from (- first-left start)
                    for o across (nreverse sub)
                    do (setf (aref orig i) o)))))
        (let ((b (if (< base end) base start)))
          (loop for i from b below end
                do (let ((o (aref orig (- i start))))
                     (when (/= o -1)
                       (let ((max i) (j (+ start o)))
                         (loop while (/= j i)
                               do (setf max (max max j))
                                  (let ((next (+ start (aref orig (- j start)))))
                                    (setf (aref orig (- j start)) -1)
                                    (setf j next)))
                         (when (/= i max)
                           (merge-clusters buf i (1+ max)))))))))
      ;; Setup masks.
      (loop for i from start below base
            while (= (gi-position (info i)) +pos-ra-to-become-reph+)
            do (setf (gi-mask (info i)) (logior (gi-mask (info i)) mask-rphf)))
      (let ((mask (logior mask-half (if (eq (indic-config-blwf-mode config) :pre-and-post) mask-blwf 0))))
        (loop for i from start below base
              do (setf (gi-mask (info i)) (logior (gi-mask (info i)) mask))))
      (let ((mask (logior mask-blwf mask-abvf mask-pstf)))
        (loop for i from (1+ base) below end
              do (setf (gi-mask (info i)) (logior (gi-mask (info i)) mask))))
      ;; ZWJ and ZWNJ effects: a ZWNJ disables the half form before it.
      (loop for i from (1+ start) below end
            when (joiner-p (info i))
              do (let ((non-joiner (eq (gi-category (info i)) :zwnj))
                       (j i))
                   (loop
                     (decf j)
                     (when non-joiner
                       (setf (gi-mask (info j)) (logandc2 (gi-mask (info j)) mask-half)))
                     (unless (and (> j start) (not (consonant-p (info j)))) (return))))))))

;;; --- final reordering --------------------------------------------------------

(defun final-reordering (plan buf)
  (map-syllables (lambda (start end)
                   (unless (eq (gi-syllable-type (aref buf start)) :non-indic-cluster)
                     (final-reordering-syllable plan buf start end)))
                 buf))

(defun final-reordering-syllable (plan buf start end)
  (let* ((config (plan-config plan))
         (virama (plan-virama-gid plan))
         (mask-pref (plan-feature-mask plan "pref"))
         (mask-init (plan-feature-mask plan "init"))
         (try-pref (/= mask-pref 0))
         (base start))
    (flet ((info (i) (aref buf i)))
      ;; A virama that was ligated and multiplied lost its class; give it back.
      (when (and virama (plusp virama))
        (loop for i from start below end
              do (let ((g (info i)))
                   (when (and (= (gi-gid g) virama) (gi-ligated-p g) (gi-multiplied-p g))
                     (setf (gi-category g) :h
                           (gi-props g) (logandc2 (gi-props g) (logior +props-ligated+ +props-multiplied+)))))))
      ;; Find the base again.
      (loop while (< base end)
            do (when (>= (gi-position (info base)) +pos-base-c+)
                 (when (and try-pref (< (1+ base) end))
                   (loop for i from (1+ base) below end
                         when (logtest (gi-mask (info i)) mask-pref)
                           do (unless (and (gi-substituted-p (info i)) (gi-ligated-and-didnt-multiply-p (info i)))
                                (setf base i)
                                (loop while (and (< base end) (halant-p (info base))) do (incf base))
                                (when (< base end) (setf (gi-position (info base)) +pos-base-c+))
                                (setf try-pref nil))
                              (return)))
                 (return))
               (incf base))
      (when (and (= base end) (< start base) (eq (gi-category (info (1- base))) :zwj))
        (decf base))
      (when (< base end)
        (loop while (and (< start base) (member (gi-category (info base)) '(:n :h)))
              do (decf base)))
      ;; Reorder matras: a pre-base matra goes after the last standalone
      ;; halant before the base.
      (when (and (< (1+ start) end) (< start base))
        (let ((new-pos (if (= base end) (- base 2) (1- base))))
          (loop while (and (> new-pos start) (not (member (gi-category (info new-pos)) '(:m :h))))
                do (decf new-pos))
          (if (and (halant-p (info new-pos)) (/= (gi-position (info new-pos)) +pos-pre-m+))
              (when (and (< (1+ new-pos) end) (eq (gi-category (info (1+ new-pos))) :zwj))
                (incf new-pos))
              (setf new-pos start))
          (if (and (< start new-pos) (/= (gi-position (info new-pos)) +pos-pre-m+))
              (loop for i from new-pos above start
                    when (= (gi-position (info (1- i))) +pos-pre-m+)
                      do (let ((old-pos (1- i)))
                           (when (and (< old-pos base) (<= base new-pos)) (decf base))
                           (buffer-move buf old-pos new-pos)
                           (merge-clusters buf new-pos (min end (1+ base)))
                           (decf new-pos)))
              (loop for i from start below base
                    when (= (gi-position (info i)) +pos-pre-m+)
                      do (merge-clusters buf i (min end (1+ base)))
                         (return)))))
      ;; Reorder reph.
      (when (and (< (1+ start) end)
                 (= (gi-position (info start)) +pos-ra-to-become-reph+)
                 (not (eq (eq (gi-category (info start)) :repha)
                          (and (gi-ligated-and-didnt-multiply-p (info start)) t))))
        (let ((reph-pos (indic-config-reph-pos config))
              (new-reph-pos nil))
          (block find
            (flet ((halant-before-base ()
                     (let ((p (1+ start)))
                       (loop while (and (< p base) (not (halant-p (info p)))) do (incf p))
                       (when (and (< p base) (halant-p (info p)))
                         (when (and (< (1+ p) base) (joiner-p (info (1+ p)))) (incf p))
                         (setf new-reph-pos p)
                         (return-from find)))))
              (unless (eq reph-pos :after-post)
                ;; 2. After the first explicit halant between the reph and the base.
                (halant-before-base)
                ;; 3. After the main consonant.
                (when (eq reph-pos :after-main)
                  (let ((p base))
                    (loop while (and (< (1+ p) end) (<= (gi-position (info (1+ p))) +pos-after-main+)) do (incf p))
                    (when (< p end) (setf new-reph-pos p) (return-from find))))
                ;; 4. Before the post-base consonants.
                (when (eq reph-pos :after-sub)
                  (let ((p base))
                    (loop while (and (< (1+ p) end)
                                     (not (member (gi-position (info (1+ p))) (list +pos-post-c+ +pos-after-post+ +pos-smvd+))))
                          do (incf p))
                    (when (< p end) (setf new-reph-pos p) (return-from find)))))
              ;; 5. As step 2.
              (halant-before-base)
              ;; 6. The end of the syllable, before syllable modifiers.
              (let ((p (1- end)))
                (loop while (and (> p start) (= (gi-position (info p)) +pos-smvd+)) do (decf p))
                ;; A reph that would land after Matra,Halant goes before the halant.
                (when (halant-p (info p))
                  (loop for i from (1+ base) below p
                        when (eq (gi-category (info i)) :m)
                          do (decf p) (return)))
                (setf new-reph-pos p))))
          (when (and new-reph-pos (> new-reph-pos start))
            (merge-clusters buf start (1+ new-reph-pos))
            (buffer-move buf start new-reph-pos)
            (when (and (< start base) (<= base new-reph-pos)) (decf base)))))
      ;; 'init' on a left matra at the start of a word.
      (when (and (/= mask-init 0) (= (gi-position (info start)) +pos-pre-m+))
        (when (or (zerop start)
                  (not (letter-or-mark-p (gi-codepoint (info (1- start))))))
          (setf (gi-mask (info start)) (logior (gi-mask (info start)) mask-init)))))))
