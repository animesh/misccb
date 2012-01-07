#!/bin/sh
# $Id: mklegend2.sh,v 1.2 2000/11/29 20:18:00 schwartz Exp $

TITLE="${1:-Annotations}";
STYLE="${PIPHOME:?}/lib/tag.style100";

cat <<!
%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 612 410
%%DocumentNeededResources:
%%+ font Times-Bold Times-Roman Times-Italic Courier Courier-Bold Symbol
%%+ procset PSU_dotplot 1.0 0.0
%%EndComments
%%BeginProlog
/pdfmark where{pop}{userdict /pdfmark /cleartomark load put}ifelse
!

# for color definitions, and maybe some pdf stuff
pmps -procset

cat <<!
DOTPLOT begin
1280 dict begin
%%EndProlog
[/PageMode /UseOutlines /DOCVIEW pdfmark
[/Title (annotation legend) /Dest [{ThisPage} /FitB] /OUT pdfmark

Red
/Times-Roman findfont 12 scalefont setfont
60 90 moveto ($TITLE) show

Blue
/Helvetica findfont 10 scalefont setfont
60 60 moveto (`date`) show
60 45 moveto ($PIPURL) show
60 30 moveto (Genome Research, Vol. 10, Issue 4, pp577-586, April 2000.) show

end %dict
end %DOTPLOT
!
