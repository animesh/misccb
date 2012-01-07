#!/bin/sh
# $Id: pippage-header.sh,v 1.6 1999/12/10 19:38:30 schwartz Exp $
# ps-header

cat <<!
%!PS-Adobe-3.0
%%BoundingBox: 0 0 612 792
%%Title: alignment
%%Pages: (atend)
%%For: $USER
%%Creator: pippage
%%CreationDate: `date`
%%Copyright: software and libraries, Scott Schwartz, 1999
%%DocumentNeededResources:
%%+ font Times Times-Bold Times-Roman Times-Italic
%%+      Helvetica Helvetica-Bold Helvetica-Roman Helvetica-Italic
%%+      Courier Courier-Bold Symbol
%%DocumentSuppliedResources:
%%+ procset PSU_dotplot 1.0 0.0
%%EndComments

%%BeginProlog
/BeginEPSF {
  /EPSF_saved_state save def
  /EPSF_dict_count countdictstack def
  /EPSF_op_count count 1 sub def
  userdict begin
  /showpage {} def
  0 setgray 0 setlinecap 1 setlinewidth 0 setlinejoin
  10 setmiterlimit [] 0 setdash newpath
  /languagelevel where {
    pop languagelevel
    1 ne { false setoverprint false setstrokeadjust } if
  } if
} bind def

/EndEPSF {
  count EPSF_op_count sub { pop } repeat
  countdictstack EPSF_dict_count sub { end } repeat
  EPSF_saved_state restore
} bind def

!

# Standard default
pmps -procset

# User supplied
for f
do
  if [ -r "$f" ]
  then cat "$f"
  elif [ -r "/u/align/lib/$f" ]
  then cat "/u/align/lib/$f"
  else echo "Cannot read $f" 1>&2
  fi
done

cat <<!
%%EndProlog
%%BeginSetup
%%IncludeResource: font Times
%%IncludeResource: font Times-Bold
%%IncludeResource: font Times-Roman
%%IncludeResource: font Times-Italic
%%IncludeResource: font Helvetica
%%IncludeResource: font Helvetica-Bold
%%IncludeResource: font Helvetica-Roman
%%IncludeResource: font Helvetica-Italic
%%IncludeResource: font Courier
%%IncludeResource: font Courier-Bold
%%IncludeResource: font Symbol
% %%BeginFeature: *PageSize letter
% <</PageSize [612 792]>> setpagedevice
% %%EndFeature
%%EndSetup

!
