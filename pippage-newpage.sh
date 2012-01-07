#!/bin/sh
# $Id: pippage-newpage.sh,v 1.2 1999/05/17 20:39:41 schwartz Exp $
# ps-newpage num
cat <<!
%%Page: $1 $1
%%BeginPageSetup
/pgsave save def
%%EndPageSetup
!
