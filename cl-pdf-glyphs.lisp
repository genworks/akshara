;; Copyright (c) 2026 David Cooper
;; SPDX-License-Identifier: MIT

;;; Additions to cl-pdf: a glyph-indexed TrueType font.
;;;
;;; cl-pdf's `load-ttf-font' embeds a TrueType font as a Type0 font
;;; whose two-byte codes are Unicode code points, mapped to glyphs by
;;; a CIDToGIDMap built from the cmap; `draw-text' shows characters.
;;; A shaped line is a sequence of glyph ids, many of them (half
;;; forms, conjuncts, reph) with no code point at all, so this file
;;; adds the sibling: `load-glyph-font' makes a font whose codes ARE
;;; the glyph ids (CIDToGIDMap /Identity, widths by glyph, a ToUnicode
;;; CMap for the glyphs the cmap names, so simple text copies out of
;;; the PDF), and `show-glyphs' shows a sequence of glyph ids in it.
;;; The font program is embedded once per document, as it would be
;;; for the Unicode font.
;;;
;;; Everything here is new definitions in the PDF package; nothing of
;;; cl-pdf's is redefined.  It is written against cl-pdf 20231021 and
;;; meant to go upstream as ttu-glyph-font.lisp.

(in-package #:pdf)

(export '(ttu-glyph-font-metrics load-glyph-font glyph-font-name show-glyphs show-glyphs-spaced
          glyph-widths))

(defclass ttu-glyph-font-metrics (ttu-font-metrics)
  ((glyph-widths :accessor glyph-widths :initarg :glyph-widths
                 :documentation "A vector by glyph id of advance widths in 1/1000 em.")
   (glyph-codes :accessor glyph-codes :initarg :glyph-codes
                :documentation "A vector by glyph id of the code point the cmap maps to it, or NIL."))
  (:documentation "A TrueType font shown by glyph id rather than by
character: a Type0 font with Identity-H encoding and an identity
CIDToGIDMap."))

(defun glyph-font-name (ttf-font-metrics)
  "The name the glyph font of TTF-FONT-METRICS is registered under."
  (concatenate 'string (font-name ttf-font-metrics) "-GID"))

(defun load-glyph-font (ttf-file &key (source (load-ttf-font ttf-file)))
  "Load TTF-FILE as a glyph-indexed font and register it under
`glyph-font-name' of its metrics; return the metrics.  SOURCE is the
ttu-font-metrics of the same file (loaded here when not given), whose
descriptor and embedded program the glyph font shares.  Get the font
with (get-font (glyph-font-name source))."
  (let (widths codes)
    (zpb-ttf:with-font-loader (loader ttf-file)
      (let* ((count (zpb-ttf:glyph-count loader))
             (k (/ 1000 (zpb-ttf:units/em loader))))
        (setf widths (make-array count :initial-element 0)
              codes (make-array count :initial-element nil))
        (dotimes (gid count)
          (let ((g (zpb-ttf:index-glyph gid loader)))
            (when g
              (setf (aref widths gid) (round (* k (zpb-ttf:advance-width g))))
              (let ((code (zpb-ttf:code-point g)))
                (when (and code (<= 0 code #xFFFF))
                  (setf (aref codes gid) code))))))))
    (let ((fm (make-instance 'ttu-glyph-font-metrics :glyph-widths widths :glyph-codes codes)))
      (setf (font-name fm) (glyph-font-name source)
            (full-name fm) (full-name source)
            (family-name fm) (family-name source)
            (underline-position fm) (underline-position source)
            (underline-thickness fm) (underline-thickness source)
            (italic-angle fm) (italic-angle source)
            (italic-sin fm) (italic-sin source)
            (fixed-pitch-p fm) (fixed-pitch-p source)
            (font-bbox fm) (font-bbox source)
            (notice fm) (notice source)
            (ascender fm) (ascender source)
            (descender fm) (descender source)
            (leading fm) (leading source)
            (cap-height fm) (cap-height source)
            (x-height fm) (x-height source)
            (encoding-scheme fm) :unicode-encoding
            (characters fm) (characters source)
            (kernings fm) (make-hash-table)
            (encoding-vector fm) (encoding-vector source)
            (pdf-widths fm) (pdf-widths source)
            (min-code fm) (min-code source)
            (max-code fm) (max-code source)
            (c2g fm) (c2g source)
            (cid-widths fm) (cid-widths source)
            (binary-data fm) (binary-data source)
            (length1 fm) (length1 source)
            (gethash (string-downcase (font-name fm)) *font-metrics*) fm)
      fm)))

(defun to-unicode-cmap (fm)
  "The ToUnicode CMap of the glyph font FM: each glyph the cmap names,
as a bfchar entry."
  (with-output-to-string (s)
    (format s "/CIDInit /ProcSet findresource begin~%12 dict begin~%begincmap~%")
    (format s "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def~%")
    (format s "/CMapName /Adobe-Identity-UCS def~%/CMapType 2 def~%")
    (format s "1 begincodespacerange~%<0000> <FFFF>~%endcodespacerange~%")
    (let ((entries (loop for code across (glyph-codes fm)
                         for gid from 0
                         when code collect (cons gid code))))
      (loop while entries
            do (let ((block (loop repeat 100 while entries collect (pop entries))))
                 (format s "~d beginbfchar~%" (length block))
                 (dolist (e block)
                   (format s "<~4,'0x> <~4,'0x>~%" (car e) (cdr e)))
                 (format s "endbfchar~%"))))
    (format s "endcmap~%CMapName currentdict /CMap defineresource pop~%end~%end~%")))

(defmethod make-dictionary ((fm ttu-glyph-font-metrics) &key font encoding (embed *embed-fonts*))
  (declare (ignore font encoding))
  (let* ((descriptor (font-descriptor fm :embed embed :errorp nil))
         (cid-font (make-instance
                    'dictionary
                    :dict-values
                    `(("/Type" . "/Font")
                      ("/Subtype" . "/CIDFontType2")
                      ("/BaseFont" . ,(add-/ (font-name fm)))
                      ("/CIDSystemInfo"
                       . ,(make-instance
                           'dictionary
                           :dict-values
                           `(("/Registry" . ,(pdf-string "Adobe"))
                             ("/Ordering" . ,(pdf-string "Identity"))
                             ("/Supplement" . 0))))
                      ("/FontDescriptor" . ,descriptor)
                      ("/DW" . 1000)
                      ("/W" . ,(vector 0 (glyph-widths fm)))
                      ("/CIDToGIDMap" . "/Identity")))))
    (make-instance
     'dictionary
     :dict-values
     `(("/Type" . "/Font")
       ("/Subtype" . "/Type0")
       ("/BaseFont" . ,(add-/ (font-name fm)))
       ("/Encoding" . "/Identity-H")
       ("/DescendantFonts" . ,(vector (make-instance 'indirect-object :content cid-font)))
       ("/ToUnicode" . ,(make-instance
                         'indirect-object
                         :content (make-instance 'pdf-stream :content (to-unicode-cmap fm))))))))

;;; --- showing glyphs ---------------------------------------------------------

(defun write-glyph-string (gids)
  (write-char #\< *page-stream*)
  (map nil (lambda (gid) (format *page-stream* "~4,'0x" gid)) gids)
  (write-string "> " *page-stream*))

(defun show-glyphs (gids)
  "Show the glyph ids GIDS (a sequence of integers) in the current
font, which must be a glyph font (`load-glyph-font')."
  (write-glyph-string gids)
  (write-line "Tj" *page-stream*))

(defun show-glyphs-spaced (items)
  "Show ITEMS, a list of glyph id sequences and numbers, with TJ: a
number moves the pen back by that many thousandths of the font size."
  (write-string "[ " *page-stream*)
  (dolist (item items)
    (if (numberp item)
        (format *page-stream* "~,2f " item)
        (write-glyph-string item)))
  (write-line "] TJ" *page-stream*))

