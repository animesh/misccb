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
# merCounterS.pl by Jarrod Chapman <jchapman@lbl.gov> Mon Jun  1 19:41:59 PDT 2009
# Copyright 2009 Jarrod Chapman. All rights reserved.
#

use Getopt::Std;
my %opts = ();
my $validLine = getopts('s:m:P:M:R', \%opts);
my @required = ("s","m");
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= ($nRequired == @required);
if ($validLine != 1) {
    print "Usage: ./merCounterS.pl <-m merSize> <-s seqFileGlob> <<-P prefix>> <<-R(evComp?)>> <<-M minReported>>\n";
    exit;
}

my $merSize = $opts{"m"};
my $seqFileGlob = $opts{"s"};

my $revComp = 0;
if (exists($opts{"R"})) {
    $revComp = 1;
}

my $minReportedCounts = 0;
if (exists($opts{"M"})) {
    $minReportedCounts = $opts{"M"};
}

my $matchString = "[ACGT]{$merSize}";
if (exists($opts{"P"})) {
    my $prefix = $opts{"P"};
    my $matchLen = $merSize-length($prefix);
    if ($matchLen > 0) {
        $matchString = $prefix . "[ACGT]{$matchLen}";
    } else {
        die "Bad options: prefix ($prefix) must be less than mer-size ($merSize)\n";
    }
}

my %mers = ();

my @seqFiles = glob($seqFileGlob);
foreach my $seqFile (@seqFiles) {

    open (S,$seqFile) || die "Couldn't open $seqFile\n";
    while (my $line = <S>) {
	chomp $line;
	my @cols = split(/\:/,$line);
	my $nts = $cols[5];

	my $seqLen = length($nts);

	while ($nts =~ /($matchString)/g) {
	    $mers{$1}++;
	    pos($nts) -= ($merSize-1);
	}
	
	if ($revComp == 1) {
	    my $rc = reverse($nts);
	    $rc =~ tr/ACGT/TGCA/;
	    while ($rc =~ /($matchString)/g) {
		$mers{$1}++;
		pos($rc) -= ($merSize-1);
	    }
	}
    }
    close S;
}

while (my ($mer,$count) = each(%mers)) {
    if ($count >= $minReportedCounts) {
	print "$mer\t$count\n";
    }
}
