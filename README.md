# akshara

OpenType shaping in portable Common Lisp: a reader for the GSUB and
GPOS tables, an Indic shaper for Devanagari, Bengali, Tamil, Telugu
and Gujarati, and glyph ids with positions out, so a Lisp program can
set those scripts correctly with nothing but the font file.  Named
for the akṣara, the written syllable, which is the unit the shaper
works in.

```lisp
(akshara:with-font (font "fonts/NotoSerifDevanagari-SemiBold.ttf")
  (akshara:shape font "वन्दे गुरोः"))
;; => #(#<86=0@0,0+516> #<248=1@0,0+301> #<75=3@0,0+485> #<40=3@-99,-1+0>
;;      #<3=5@0,0+276> #<59=6@0,0+547> #<34=6@6,7+0> #<84=8@0,0+398>
;;      #<43=8@0,0+258> #<105=8@0,0+265>)
```

Each result is a `shaped-glyph`: glyph id, cluster (the index of the
first character it came from), advance and offset in font units.
The half form of न, the ligature of श्र, the e-matra placed on द by the
font's anchors: what HarfBuzz produces, glyph for glyph, position for
position.  That is the library's definition of correct, and the test
suite enforces it.

Depends on [zpb-ttf](https://github.com/xach/zpb-ttf) for the file
and its cmap, metrics and outlines.  Tested on SBCL and Clozure CL.
MIT license.

## What it reads

- GSUB lookup types 1 (single), 2 (multiple), 3 (alternate), 4
  (ligature), 5 (contextual, all three formats), 6 (chained
  contextual, all three formats) and 7 (extension).
- GPOS lookup types 1 (single), 2 (pair, both formats), 4
  (mark-to-base), 5 (mark-to-ligature), 6 (mark-to-mark), 7 and 8
  (contextual) and 9 (extension).  Type 3 (cursive) is parsed and not
  applied.
- Lookup flags, including mark attachment classes and GDEF mark
  filtering sets; GDEF glyph classes.
- Device tables and variation data are skipped: this is a shaper for
  static fonts.

## What it shapes

The Indic shaper follows HarfBuzz's `hb-ot-shaper-indic.cc`, which in
turn follows Microsoft's Indic OpenType specifications, for
Devanagari, Bengali, Tamil, Telugu and Gujarati: syllables found by the same
grammar, the base consonant found the same way (asking the font which
consonants take below-base or post-base forms), the reph and the
pre-base matra reordered in the same places, the basic features
applied one stage at a time in the order `locl ccmp` / `nukt akhn
rphf rkrf pref blwf abvf half pstf vatu cjct` / `init pres abvs blws
psts haln` with HarfBuzz's per-glyph masks, then `calt clig rclt
rlig` (`liga` is off in the Indic shaper, as in HarfBuzz), then the
GPOS features `abvm blwm mark mkmk curs dist kern`; attachment offsets
propagated down the chain; the joiners and other default-ignorable
characters no lookup consumed given no width and shown as the space
glyph.  Mark advances are the font's, as HarfBuzz's Indic shaper
leaves them.  Clusters are HarfBuzz's level 0.

The per-script differences are HarfBuzz's configurations: the reph
goes before the post-base forms in Devanagari and Gujarati, after the
subjoined ones in Bengali and after the post-base forms in Tamil and
Telugu; Telugu forms a reph only from Ra, Halant, ZWJ and applies
`blwf` to consonants after the base alone; a Tamil pre-base matra
goes right before the base, there being no half forms to pass.
Gujarati is Devanagari's configuration with its own virama and matra
positions.

Text in any other script gets the default shaper: the common features
with no reordering, mark advances zeroed by GDEF class.  Normalization
covers the five blocks: the split vowels (Bengali o and au, Tamil o,
oo and au, Telugu ai) and nukta forms HarfBuzz decomposes, the
consonant-nukta pairs it recomposes when the font has the glyph, and
the canonical order of marks by combining class (the nuktas, the
viramas, the Telugu length marks); Gujarati has nothing to decompose.

Not yet: Unicode beyond the BMP (zpb-ttf reads the format 4 cmap),
cursive attachment, right-to-left runs, dotted circles for broken
syllables and HarfBuzz's vowel-sequence constraints, the old-spec
behaviors of a font that has only an old script tag (`deva`, `taml`
and the like; such a font is shaped by the new-spec rules), the other
Indic scripts.  Their tables are the same shape; the categories and
configurations are what is missing.

## API

- `(akshara:load-font path)`, `(akshara:close-font font)`,
  `(akshara:with-font (var path) ...)`.  A font keeps its file open.
- `(akshara:shape font string &key script language)` — `script` is
  `:devanagari`, `:bengali`, `:tamil`, `:telugu`, `:gujarati`, `nil`
  for the default shaper, or `:guess` (the default) from the first character
  in one of those blocks; `language` an OpenType language tag such as
  `"SAN "`.  Returns a vector of `shaped-glyph`
  (`sg-gid`, `sg-cluster`, `sg-x-advance`, `sg-y-advance`,
  `sg-x-offset`, `sg-y-offset`).
- `(akshara:shaped-width glyphs)` — the line's advance in font units.
- `(akshara:glyph-id font code)`, `(akshara:advance-width font gid)`,
  `(akshara:glyph-class font gid)`, `(akshara:font-units-per-em
  font)`, `(akshara:font-scripts font)`, `(akshara:font-features font
  &optional :gpos)`.
- `(akshara:hb-string glyphs)` and `(akshara:parse-hb-string string)`
  — hb-shape's text format (`[gid=cluster@dx,dy+advance|...]`), the
  language of the tests.

### Onto a page

The system `akshara/pdf` (needs
[cl-pdf](https://github.com/mbattyani/cl-pdf)) puts shaped text on a
page two ways.

As real text, in a glyph-indexed font.  `cl-pdf-glyphs.lisp` adds to
cl-pdf what it lacks: `pdf:load-glyph-font` embeds a TrueType font as
a Type0 font whose two-byte codes are glyph ids (CIDToGIDMap
Identity, widths by glyph, a ToUnicode map for the glyphs the cmap
names), and `pdf:show-glyphs` shows glyph ids in it.  On top of that,
`show-shaped-glyphs` shows a shaped line: TJ arrays whose numbers
correct the viewer's advances to the shaper's, each mark placed by
its offset, and, given the source string, an ActualText span per
cluster so the logical text copies and searches out of the PDF
whatever order the glyphs are in.

```lisp
(pdf:load-glyph-font "fonts/NotoSerifDevanagari-SemiBold.ttf")
(let ((pdf-font (pdf:get-font "NotoSerifDevanagari-SemiBold-GID")))
  (pdf:in-text-mode
    (akshara:show-shaped-text font line x y 12 pdf-font)))
```

As outlines: `draw-shaped-glyphs` and `draw-shaped-text` fill each
glyph's TrueType contours, quadratics as cubic Béziers, through
cl-pdf's path calls.  That prints exactly, embeds no font and is not
text; it is how the shaping was first proved on a page.

```lisp
(akshara:draw-shaped-glyphs font (akshara:shape font line) x y 12)
```

The cl-pdf additions are new definitions in its package, nothing
redefined, written against cl-pdf 20231021 and meant to go upstream.

## Tests

```lisp
(asdf:test-system "akshara")
```

`tests/golden/` is the corpus: every distinct verse line of the ISKCON
Farmington Hills songbook in Devanagari (309 lines) and Bengali (142),
derived from the book's IAST by the
[iast](https://gitlab.genworks.com/genworks/iast) library; 160 lines
of Telugu and 165 of Tamil devotional prose and verse (the
maha-mantra and the names, Bhagavad-gita and Siksastaka verses in
each script, Annamacharya's kirtanas and Andal's Tiruppavai, prose
about the movement, and lines that run through every vowel sign, the
conjuncts with Ra first and second, the joiners, the Grantha letters,
digits and punctuation); 172 lines of Gujarati of the same kind (the
names, Gita and Siksastaka verses in Gujarati script, Narasinha
Mehta's Vaishnava jana, prose about the movement and about Gujarat's
Krishna tirthas, and lines that run through every vowel sign, the
conjuncts with Ra first and second, the reph on each matra, the
nukta, the joiners, digits and punctuation); and what HarfBuzz 10.2
made of each line with the font files in `fonts/` (`make-golden.sh`
regenerates them with `hb-shape`).  A line passes when akshara's
glyph ids, clusters, advances and offsets are all the same.  All
948 pass.

The fonts are Noto Serif Devanagari SemiBold 2.006, Noto Serif
Bengali SemiBold 3.000, Noto Serif Telugu 2.001, Noto Serif Tamil
2.001 and Noto Serif Gujarati 2.002 (Regular and Bold; the goldens
use the Regular cuts, and the Bold cuts shape the same corpora
identically to hb-shape), under the SIL Open Font License
(`fonts/OFL-*`).

## Layout of the source

| file | what |
|---|---|
| `binary.lisp` | big-endian readers, coverage tables, class definitions |
| `layout.lisp` | GSUB and GPOS parsed into structures |
| `font.lisp` | the font: zpb-ttf loader, GDEF, cmap, advances |
| `buffer.lisp` | the glyph record and buffer operations |
| `apply.lisp` | applying lookups: the skipping matcher, each subtable, nested lookups, would-apply |
| `unicode.lisp` | Indic categories and positions, marks, decompositions |
| `indic.lisp` | syllables, initial and final reordering, masks |
| `shape.lisp` | the plan and the pipeline |
| `cl-pdf-glyphs.lisp` | `akshara/pdf`: the glyph-indexed font and `show-glyphs`, additions to cl-pdf |
| `outlines.lisp` | `akshara/pdf`: shaped text as real text or as outlines through cl-pdf |
