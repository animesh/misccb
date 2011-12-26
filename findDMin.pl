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

# input: histogram output of mercounts

my $minPeakM = 5;  # don't look for peak inside of this point
open( F, $ARGV[0] ) || die "couldn't open $ARGV[0]\n";

my $prevCnt = -1;
my $peakCnt = -1;
my $peakM = -1;
my $minPeakM = 10;
my $bMinimaFound = 0;
my $prevM = -1;

# find the peak
while( <F> )
{
	if ( $_ =~ /\|/ )
	{
		my @F = split;
		my ( $m, $cnt ) = ( $F[1], $F[6] );
	 
	  # once the minima has been found, then start tracking the peakCnt
	  if ( $bMinimaFound && ( $cnt > $peakCnt && $m > $minPeakM  ) )
		{ 
		  $peakCnt = $cnt;
		  $peakM = $m;
		}	
		if ( !$bMinimaFound && $cnt > $prevCnt && $prevCnt != -1 ) { $bMinimaFound = 1; $minPeakM = $prevM; }
		$prevCnt = $cnt; $prevM = $m;
	}
}
if ( !$bMinimaFound || $peakM > 300 ) { die "no minima found / peakM suspiciously high at $peakM\n" }
print STDERR "PeakM: $peakM\n";
print STDERR "minM: $minPeakM\n";
sysseek( F, 0, 0 );


# look only from [ 0, peak ]
while( <F> )
{
	if ( $_ =~ /\|/ )
	{
		my @F = split;
		my ( $m, $cnt ) = ( $F[1], $F[6] );

		# d-min should be less than the trough
		if ( $m > $minPeakM ) 
                {
                   die "Couldn't find suitable d-min"; 
                }
		if ( $cnt < $peakCnt )
		{
		    print STDERR "D-min picked as: $m\n";
		    print $m;  # d-min found here
		    exit;
		}
	}
}
die "d-min could not be chosen";


