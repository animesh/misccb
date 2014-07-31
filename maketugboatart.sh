#!/bin/sh
cd latex/
sgml2latex ../doc/macroMan.sgml 
sed 's/article/ltugboat/' macroMan.tex > tugboatMan.tex
latex tugboatMan.tex
dvips -o ../doc/tugboatMan.ps tugboatMan.dvi
rm -v tugboatMan.???
cd ../