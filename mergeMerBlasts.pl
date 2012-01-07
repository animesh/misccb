#
# Author: Jarrod Chapmon, Isaac Ho
#
# Copyright 2011 The Regents of the University of California.
# All rights reserved.

# The United States Government has rights in this work pursuant
# to contracts DE-AC03-76SF00098, W-7405-ENG-36 and/or
# W-7405-ENG-48 between the United States Department of Energy
# and the University of California.

# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire
# copyright notice and comment, and (2) distributions including
# binaries display the following acknowledgement:  "This product
# includes software developed by the University of California,
# JGI-PSF and its contributors" in the documentation or other
# materials provided with the distribution and in all advertising
# materials mentioning features or use of this software.  Neither the
# name of the University nor the names of its contributors may be
# used to endorse or promote products derived from this software
# without specific prior written permission.

# THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE.

use strict;

my @F=();
my $lastContigId = -1;
my $lastContigPos = -1;
my $lastReadId = -1;
while( <STDIN> )
{
	 my @F = split;
	 if ( $F[1] eq $lastReadId && $F[5] eq $lastContigId && abs( $F[6] - $lastContigPos ) < 200 )
	 {
		  # skip/merge
   }
	 else
	 {
   		 print $_;
	 }

	 $lastReadId = $F[1];
	 $lastContigId = $F[5];
	 $lastContigPos = $F[6];
}
if ( $F[5] == $lastContigId && abs( $F[6] - $lastContigPos ) < 200 )
{
	  print $_;
}
