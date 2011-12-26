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

#!/usr/local/bin/perl
#script by Jarrod Chapman <jarrod@mays.berkeley.edu> 



$n_args = @ARGV;
if ($n_args != 2) {
    print "Subdivide a fasta file into subfiles of <entries> entries\n";
    print "Usage: ./divide_it.pl <fasta file> <entries>\n";
    die;
}

open(F,$ARGV[0]) || die "Couldn't open file $ARGV[0]\n";
my $entries = $ARGV[1];

my $done = 0;
my $filecount = 0;
my $entrycount = 0;
my $outfile = $ARGV[0] . "." . $filecount; 
open (OUT, ">$outfile");
$filecount += 1;

while (my $i = <F>) {
    if ($i =~ />/) {
	$entrycount += 1;
	if ($entrycount > $entries) {
	    close OUT;
	    $outfile = $ARGV[0] . "." . $filecount; 
	    open (OUT, ">$outfile");
	    $filecount += 1;
	    $entrycount = 1;
	}
    }
    print OUT $i;
}
close OUT;
close F;
