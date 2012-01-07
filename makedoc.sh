#!/bin/sh
pdflatex examplea.tex
pdflatex examplea-frn.tex
pdflatex exampleb.tex
pdflatex exampleb-frn.tex
pdflatex examplec.tex
pdflatex examplec-frn.tex
pdflatex frontespizio.dtx
pdflatex frontespizio-frn.tex
makeindex -s gind frontespizio
makeindex -s gglo -o frontespizio.gls frontespizio.glo
pdflatex frontespizio.dtx
pdflatex frontespizio.dtx
exit
