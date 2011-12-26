#! /bin/bash
cd latex
latex logofeatpost.tex
dvips -E -o logofeatpost.ps logofeatpost.dvi 
epstopdf -d -nogs logofeatpost.ps > featpost.eps
gs -q -sDEVICE=png256 -r300x300 -dNOPAUSE -sOutputFile=featpost.png featpost.eps </dev/null
cp featpost.png featpost2.png
mogrify -geometry 25% featpost2.png 
cd ..
