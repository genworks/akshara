# akshara

OpenType shaping in portable Common Lisp: a reader for the GSUB and
GPOS tables, an Indic shaper for Devanagari and Bengali, and glyph ids
with positions out, so a Lisp program can set those scripts correctly
with nothing but the font file.  Named for the akṣara, the written
syllable, which is the unit the shaper works in.

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
turn follows Microsoft's Devanagari and Bengali OpenType
specifications: syllables found by the same grammar, the base
consonant found the same way (asking the font which consonants take
below-base or post-base forms), the reph and the pre-base matra
reordered in the same places, the basic features applied one stage at
a time in the order `locl ccmp` / `nukt akhn rphf rkrf pref blwf abvf
half pstf vatu cjct` / `init pres abvs blws psts haln` with
HarfBuzz's per-glyph masks, then `calt clig liga rclt rlig`, then the
GPOS features `abvm blwm mark mkmk curs dist kern`; mark advances
zeroed by GDEF class after positioning and attachment offsets
propagated down the chain.  Clusters are HarfBuzz's level 0.

Text in any other script gets the default shaper: the common features
with no reordering.  Normalization covers the two blocks: the split
vowels and nukta forms HarfBuzz decomposes, and the consonant-nukta
pairs it recomposes when the font has the glyph.

Not yet: Unicode beyond the BMP (zpb-ttf reads the format 4 cmap),
hiding of ZWJ/ZWNJ, cursive attachment, right-to-left runs, the other
Indic scripts.  Their tables are the same shape; the categories and
configurations are what is missing.

## API

- `(akshara:load-font path)`, `(akshara:close-font font)`,
  `(akshara:with-font (var path) ...)`.  A font keeps its file open.
- `(akshara:shape font string &key script language)` — `script` is
  `:devanagari`, `:bengali`, `nil` for the default shaper, or `:guess`
  (the default) from the text; `language` an OpenType language tag
  such as `"SAN "`.  Returns a vector of `shaped-glyph`
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
[cl-pdf](https://github.com/mbattyani/cl-pdf)) draws shaped glyphs as
filled outlines, TrueType quadratics as cubic Béziers in the content
stream:

```lisp
(akshara:draw-shaped-glyphs font (akshara:shape font line) x y 12)
(akshara:draw-shaped-text font line x y 12)
```

The result prints exactly and embeds no font; it is not selectable
text.  Real text needs the PDF to carry the TrueType font and show
glyph indices, which is a change to cl-pdf's font embedding rather
than to this library.

## Tests

```lisp
(asdf:test-system "akshara")
```

`tests/golden/` is the corpus: every distinct verse line of the ISKCON
Farmington Hills songbook in Devanagari (309 lines) and Bengali (142),
derived from the book's IAST by the
[iast](https://gitlab.genworks.com/genworks/iast) library, and what
HarfBuzz 10.2 made of each line with the font files in `fonts/`
(`make-golden.sh` regenerates them with `hb-shape`).  A line passes
when akshara's glyph ids, clusters, advances and offsets are all the
same.  All 451 pass.

The fonts are Noto Serif Devanagari SemiBold 2.006 and Noto Serif
Bengali SemiBold 3.000, under the SIL Open Font License (`fonts/OFL-*`).

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
| `outlines.lisp` | `akshara/pdf`: outlines through cl-pdf |
