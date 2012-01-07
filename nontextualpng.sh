#! /bin/bash
# L. Nobre G.
# IYP (2005)

mkdir nontextualpng
for a in allps/* 
do
    convert -bordercolor white -border 20x20 -transparent white \
	-format png $a nontextualpng/`basename $a`.png
done
