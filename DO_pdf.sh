#!/bin/sh -e

# initial latex
pdflatex jules-verne.tex

# make bibliography
bibtex jules-verne

# make glossary
bibtex jules-verne.gls

# make list of symbols
bibtex jules-verne.losa

# make index
makeindex -s gatech-thesis-index.ist jules-verne.idx

# twice more
pdflatex jules-verne.tex
pdflatex jules-verne.tex

# last of all, create the thumbnails
# Acrobat Reader 5 generates these on-the-fly when
# the document is loaded, but that's slow.  This
# process makes .png previews of each page, and
# puts them into the endproduct.  That way, the 
# file has thumbnails even in Acrobat Reader 4, 
# and it opens much faster in Acrobat Reader 5.
#
# First make the thumbnails
thumbpdf.pl jules-verne.pdf
# Then add them to the doc
pdflatex jules-verne.tex

