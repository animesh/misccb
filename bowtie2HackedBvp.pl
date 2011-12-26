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

my $bowtieOutput = $ARGV[0];
my $readLen = $ARGV[1];
my $contigLengths = $ARGV[2];

my %contigLen = ();
open( C, $contigLengths ) || die;
while( <C> )
{
  chomp;
	my @A = split;
  $contigLen{ $A[0] } = $A[1];
}

print "BLAST_TYPE\tQUERY\tQ_START\tQ_STOP\tQ_LENGTH\tSUBJECT\tS_START\tS_STOP\tS_LENGTH\tSTRAND\tSCORE\tE_VALUE\tIDENTITIES\tALIGN_LENGTH\tQ_SEQUENCE\tS_SEQUENCE\n";

open( B, $bowtieOutput ) || die;
while( <B> )
{
	chomp;
	my ( $query, $strand, $subject, $sStart) = split;

	if ( $strand eq "+" ) { $strand = "Plus"; }
	elsif ( $strand eq "-" ) { $strand = "Minus"; }
	else
	{
		 die "illegal strand: $strand\n";
	}
	$sStart++;  # correction factor in coordinate systems
	my $blastType = "BLASTN";
	my $qStart = 1;
	my $qStop = $readLen;
	my $qLength = $readLen;
	my $sStop = $sStart + $qLength;
	my $sLength = $contigLen{ $subject };
	my $score = 99999;
	my $eValue = 0;
	my $identities = $qLength;
	my $alignLength = $qLength;
	my $qSeq = "";
	my $sSeq = "";

	print "$blastType\t$query\t$qStart\t$qStop\t$qLength\t$subject\t$sStart\t$sStop\t$sLength\t";
	print "$strand\t$score\t$eValue\t$identities\t$alignLength\t$qSeq\t$sSeq\n";
}
