#!/bin/sh
# $Id: pippage-doeps.sh,v 1.3 1999/06/01 21:03:22 schwartz Exp $
# ps-doeps x y eps
cat <<!
BeginEPSF
$1 $2 translate
%%BeginDocument: $3
!
cat "$3"
#sed '/^%/d' "$3"
cat <<!
%%EndDocument: $3
EndEPSF
!
