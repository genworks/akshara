#!/bin/sh
# Regenerate the golden corpus with HarfBuzz.
#
# deva-lines.txt and beng-lines.txt hold one verse line per line, the
# Devanagari and Bengali of the ISKCON Farmington Hills songbook as the
# krishna-iast engine derives them from its IAST (309 and 142 distinct
# lines on 2026-10-14).  hb-shape shapes each line with the same font
# files the library reads and writes one result per line, in its text
# format with glyph ids (--no-glyph-names), to deva-hb.txt and
# beng-hb.txt.  The tests pin akshara's output to those files.
#
# Needs hb-shape (Debian: libharfbuzz-bin) and a UTF-8 locale.  Run
# from anywhere; paths are relative to this script.

set -e
cd "$(dirname "$0")"
export LANG=C.UTF-8 LC_ALL=C.UTF-8

hb-shape --version

hb-shape --font-file ../../fonts/NotoSerifDevanagari-SemiBold.ttf \
         --text-file deva-lines.txt --no-glyph-names \
         --output-file deva-hb.txt
hb-shape --font-file ../../fonts/NotoSerifBengali-SemiBold.ttf \
         --text-file beng-lines.txt --no-glyph-names \
         --output-file beng-hb.txt

wc -l deva-lines.txt deva-hb.txt beng-lines.txt beng-hb.txt
