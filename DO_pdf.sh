#!/bin/sh -e

# initial latex
pdflatex jules-verne.tex

# make bibliography
bibtex jules-verne

# twice more
pdflatex jules-verne.tex
pdflatex jules-verne.tex

