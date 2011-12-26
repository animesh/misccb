#!/bin/sh
tex testfont < testfont.input
rm testfont.log
rm bcghsm.tfm
rm bcghsm.600pk
rm /home/corff/mls/mls-font/fonts/source/mls/tfm/bicig/*
rm /usr/local/tex.local/fonts/tfm/mls/bicig/*
rm /usr/local/tex.local/fonts/pk/ljfive/mls/bicig/*
xdvi testfont
rm testfont.dvi
