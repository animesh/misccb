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

#!/jgi/tools/bin/perl -w
#
# insertSizeCorrection2.pl by Jarrod Chapman <jchapman@lbl.gov> Tue Jun 16 19:56:11 PDT 2009
# Copyright 2009 Jarrod Chapman. All rights reserved.
#

use Getopt::Std;

my %opts = ();
my $validLine = getopts('m:i:', \%opts);
my @required = ("m","i",);
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= ($nRequired == @required);
if ($validLine != 1) {
    print "Usage: ./insertSizeCorrection2.pl <-m merSize> <-i insertSizeDistributionFile>\n";
    exit;
}

my $merSize = $opts{"m"};
my $insertSizeFile = $opts{"i"};

my $minSize = undef;
my $maxSize = undef;
my %sizeDist = ();

my $totalCounts = 0;

my @observedSizes = ();

open (I,$insertSizeFile) || die "Couldn't open $insertSizeFile\n";
while (my $line = <I>) {
    chomp $line;
    if ($line =~ /^#/) {
	next;
    }
    my ($size,$freq) = $line =~ /(\d+)\s+(\d+)/;
    unless (defined($minSize)) {
	$minSize = $size;
    } else {
	if ($size < $minSize) {
	    $minSize = $size;
	}
    }
    unless (defined($maxSize)) {
	$maxSize = $size;
    } else {
	if ($size > $maxSize) {
	    $maxSize = $size;
	}
    }

    $sizeDist{$size} = $freq;
    push(@observedSizes,$size);
    $totalCounts += $freq;

}
close I;

my $meanSize = 0;
my $meanSquareSize = 0;

foreach my $size (@observedSizes) {
    $sizeDist{$size} /= $totalCounts;
    $meanSize += $size*$sizeDist{$size};
    $meanSquareSize += $size*$size*$sizeDist{$size};
}

my $stdDev = sqrt($meanSquareSize-$meanSize*$meanSize);

#print "#Mean end separation: $meanSize +/- $stdDev\n";

my $twoM = 2*$merSize;

for (my $g = -$merSize+1; $g < $maxSize-$twoM; $g++) {

    my $num = 0;
    my $den = 0;

    foreach my $s (@observedSizes) {
	my $p = ($g <= $s-$twoM) ? ($s-$twoM-$g+1)*$sizeDist{$s} : 0;
	$num += $s*$p;
	$den += $p;
    }

    my $meanSpan = $num/$den;
    my $meanAnchor = $meanSpan - $g;
    print "$g\t$meanAnchor\n";

}

	
