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
# blastMapAnalyzer.pl by Jarrod Chapman <jchapman@lbl.gov> Fri Jun 12 12:58:11 PDT 2009
# Copyright 2009 Jarrod Chapman. All rights reserved.
#

use Getopt::Std;
my %opts = ();
my $validLine = getopts('b:i:m:IL:', \%opts);
my @required = ("b","m");
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= (($nRequired == @required) && (exists($opts{"i"}) || exists($opts{"I"})));
if ($validLine != 1) {
    print "Usage: ./blastMapAnalyzer.pl <-b blastMap> <-m minMatch> ( <-i insertSize> || <-I(nsertSizeTest?)> ) <<-L longPairSize>>\n";
    exit;
}

my $blastMapFile = $opts{"b"};

my $longPairs = 0;
if (exists($opts{"L"})) {
    $longPairs = $opts{"L"};
}
my $minMatch = $opts{"m"};

my $insertSize = undef;
my $endDistance = undef;
my $testMode = 0;
my $nFullPairs = 0;
my $nNonStandard = 0;
my %testLocations = ();
my $shortPair = 600;
my $sampleSize = 200000;

if (exists($opts{"I"})) {
    $testMode = 1;
} else {
    $insertSize = $opts{"i"};
    $endDistance = 1.2*$insertSize;
}

open (B,$blastMapFile) || die "Couldn't open $blastMapFile\n";

my $currentPair = undef;
my $pairInfo = "";

my %pairSizes = ();

my $wiggleRoom = 5;

while (my $line = <B>) {
    chomp $line;

    my ($blastType,$query,$qStart,$qStop,$qLength,$subject,$sStart,$sStop,$sLength,
	$strand,$score,$eValue,$identities,$alignLength) = split(/\t/,$line);

    unless (($blastType eq "BLASTN") && ($identities > $minMatch))  {
	next;
    }

    if ($longPairs) {
	unless ($sLength > $longPairs) {
	    next;
	}

	$strand = ($strand eq "Plus") ? "Minus" : "Plus";
	$line = join("\t",$blastType,$query,$qStart,$qStop,$qLength,
		     $subject,$sStart,$sStop,$sLength,$strand,$score,$eValue,$identities,$alignLength);

    }


    my $unalignedStart = $qStart-1;
    my $projectedStart = undef;
    if ($strand eq "Plus") {
	$projectedStart = $sStart - $unalignedStart;
    } else {
	$projectedStart = $sStop + $unalignedStart;
    }

    if ($longPairs) {
	my $minBackDistance = $shortPair;
	my $backDistance = $projectedStart;
	if ($strand eq "Minus") {
	    $backDistance = $sLength - $projectedStart;
	}
	unless ($backDistance > $minBackDistance) {
	    next;
	}
    }

    my $startStatus = undef;
    if ($unalignedStart == 0) {
	$startStatus = "FULL";
    } elsif ( ($projectedStart < 1) || ($projectedStart > $sLength)) {
	$startStatus = "GAP";
    } elsif ($unalignedStart < $wiggleRoom) {
	$startStatus = "INC";
    } else {
	$startStatus = "TRUNC";
    }

    my $unalignedEnd = $qLength-$qStop;
    my $projectedEnd = undef;
    if ($strand eq "Plus") {
	$projectedEnd = $sStop + $unalignedEnd;
    } else {
	$projectedEnd = $sStart - $unalignedEnd;
    }
    my $endStatus = undef;
    if ($unalignedEnd == 0) {
	$endStatus = "FULL";
    } elsif ( ($projectedEnd < 1) || ($projectedEnd > $sLength)) {
	$endStatus = "GAP";
    } elsif ($unalignedEnd < $wiggleRoom) {
	$endStatus = "INC";
    } else {
	$endStatus = "TRUNC";
    }

    my $location = "UNK";

    unless ($testMode) {

	if ($strand eq "Plus") {
	    if ($projectedStart > ($sLength-$endDistance)) {
		$location = "OUT";
	    } elsif ($projectedEnd < $endDistance) {
		$location = "IN";
	    } else {
		$location = "MID";
	    }
	} else {
	    if ($projectedStart < $endDistance) {
		$location = "OUT";
	    } elsif ($projectedEnd > ($sLength-$endDistance)) {
		$location = "IN";
	    } else {
		$location = "MID";
	    }
	}
    }

    my $readStatus = "$startStatus.$endStatus.$location";

    if ($testMode) {
	unless ($readStatus =~ /FULL\.FULL/) {
	    next;
	}
    } else {
	unless (($readStatus =~ /OUT/) || ($readStatus =~ /GAP/)) {
	    next;
	}
    }

    my ($pairName,$pairEnd) = $query =~ /(.+)\/(\d+)/;

    if (defined($currentPair)) {
	if ($pairName eq $currentPair) {
	    $pairInfo .= "$readStatus\t$line\n";
	} else {
	    unless ($testMode) { 
		processPair($pairInfo);
	    } else {
		testProcessPair($pairInfo);
	    }

	    $currentPair = $pairName;
	    $pairInfo = "$readStatus\t$line\n";
	}
    } else {
	$currentPair = $pairName;
	$pairInfo = "$readStatus\t$line\n";
    }
}
close B;

if (defined($currentPair)) {
    if ($testMode) {
	testResults();
    } else {
	processPair($pairInfo);
    }
}

# -------------
# |SUBROUTINES|
# -------------


sub testResults {
    my $meanRedundancy = 0;
    my $nLoci = 0;
    while (my ($locus,$redundancy) = each(%testLocations)) {
	$nLoci++;
	$meanRedundancy += $redundancy;
    }
    $meanRedundancy = sprintf("%.1f",$meanRedundancy/$nLoci);

    print "# $nFullPairs sampled canonical pairs ($nNonStandard non-standard pairs discarded); mean redundancy = $meanRedundancy\n";

    print "# Pair separation distribution:\n";


    my @sortedPairSizes = sort {$a <=> $b} keys(%pairSizes);

    foreach my $size (@sortedPairSizes) {
	my $freq = $pairSizes{$size};
	if ($freq > 9) {
	    print "$size\t$freq\n";
	}
    }

    exit;
}

sub testProcessPair {

    my ($info) = @_;

    my @alignments = split(/\n/,$info);
    my $nAligns = scalar(@alignments);

    unless ($nAligns == 2) {
	return;
    }

    my @sPositions = ();

    my ($status1,$blastType1,$query1,$qStart1,$qStop1,$qLength1,$subject1,$sStart1,$sStop1,$sLength1,
	$strand1,$score1,$eValue1,$identities1,$alignLength1) = split(/\t/,$alignments[0]);
    push (@sPositions,$sStart1);
    push (@sPositions,$sStop1);
    my ($status2,$blastType2,$query2,$qStart2,$qStop2,$qLength2,$subject2,$sStart2,$sStop2,$sLength2,
	$strand2,$score2,$eValue2,$identities2,$alignLength2) = split(/\t/,$alignments[1]);
    push (@sPositions,$sStart2);
    push (@sPositions,$sStop2);

    unless ( ($query1 ne $query2) && ($subject1 eq $subject2) ) {
	return;
    }

    if ($strand1 eq $strand2) {
	$nNonStandard++;
	return;
    }
    
    my $orientation = "undefined";
    if ( (($strand1 eq "Plus") && ($sStart1 < $sStart2) && ($sStart1 < $sStop2)) || 
	 (($strand2 eq "Plus") && ($sStart2 < $sStart1) && ($sStart2 < $sStop1)) ) {
	$orientation = "convergent";
    } elsif ( (($strand1 eq "Plus") && ($sStart1 > $sStop2)) || 
	      (($strand2 eq "Plus") && ($sStart2 > $sStop1)) ) {
	$orientation = "divergent";
    }

    unless ($orientation eq "convergent") {
	$nNonStandard++;
	return;
    }

    my @sPosSorted = sort {$a <=> $b} @sPositions;

    my $testLocation = "$subject1.$sPosSorted[0].$sPosSorted[3]";
    if (exists($testLocations{$testLocation})) {
	$testLocations{$testLocation}++;
	return;
    } else {
	$testLocations{$testLocation} = 1;
    }

    my $extent = $sPosSorted[3]-$sPosSorted[0]+1;

    if ($longPairs) {
	if ($extent < $shortPair) {
	    $nNonStandard++;
	    return;
	}
    }

    $pairSizes{$extent}++;
    $nFullPairs++;

    if ($nFullPairs == $sampleSize) {
	testResults();
    }
}

sub processPair {
    my ($info) = @_;
    my @alignments = split(/\n/,$info);
    my $nAligns = scalar(@alignments);

    my @aStatus = ();
    my @aContig = ();
    my @aStrand = ();
    my %reads = ();

    my @dataSlice = (0,2,3,4,5,6,7,8,9,10);

    for (my $a = 0; $a < $nAligns; $a++) {
	my $alignment = $alignments[$a];
	my ($readStatus,$blastType,$query,$qStart,$qStop,$qLength,$subject,$sStart,$sStop,$sLength,
	    $strand,$score,$eValue,$identities,$alignLength) = split(/\t/,$alignment);
	push(@aStatus,$readStatus);
	push(@aContig,$subject);
	push(@aStrand,$strand);

	if (exists($reads{$query})) {
	    push(@{$reads{$query}},$a);
	} else {
	    $reads{$query} = [$a];
	}
    }

    my @reads = keys(%reads);
    my @rStatus = ();

    foreach my $read (@reads) {
	my $nAnchors = 0;
	my $nOutOfGap = 0;
	my $nIntoGap = 0;
	my $anchor = "";
	my $outOfGapSplint = "";
	my $outOfGapNoSplint = "";
	my $intoGap = "";
	my @aIndex = @{$reads{$read}};
	foreach my $a (@aIndex) {
	    my $status = $aStatus[$a];
	    my ($startStat,$stopStat,$dirStat) = $status =~ /^(.+)\.(.+)\.(.+)$/;
	    my $contig = $aContig[$a];
	    my $strand = $aStrand[$a];

	    unless ( ($startStat =~ /GAP/) || ($stopStat =~ /GAP/) ) {
		$nAnchors++;
		if ($strand eq "Plus") {
		    $anchor = "$contig.3.$a";
		} else {
		    $anchor = "$contig.5.$a";
		}

	    } else {

		if ($startStat =~ /GAP/) {
		    $nOutOfGap++;
		    if ($strand eq "Plus") {
			$outOfGapSplint = "$contig.5.$a";
			$outOfGapNoSplint = "$contig.5.$a";
			if ($dirStat eq "OUT") {
			    $outOfGapNoSplint = "$contig.3.$a";
			}
		    } else {
			$outOfGapSplint = "$contig.3.$a";
			$outOfGapNoSplint = "$contig.3.$a";
			if ($dirStat eq "OUT") {
			    $outOfGapNoSplint = "$contig.5.$a";
			}
		    }
		}
		if ($stopStat =~ /GAP/) {
		    $nIntoGap++;
		    if ($strand eq "Plus") {
			$intoGap = "$contig.3.$a";
		    } else {
			$intoGap = "$contig.5.$a";
		    }
		}
	    }
	}

	if (($nAnchors == 1) && ($nOutOfGap == 0) && ($nIntoGap == 0)) {
	    push(@rStatus,"ANCHOR.$anchor");
	} elsif (($nAnchors == 0) && ($nOutOfGap == 1) && ($nIntoGap == 0)) {
	    push(@rStatus,"OUTGAP.$outOfGapNoSplint");
	} elsif (($nAnchors == 0) && ($nOutOfGap == 0) && ($nIntoGap == 1)) {
	    push(@rStatus,"INTGAP.$intoGap");
	} elsif (($nAnchors == 0) && ($nOutOfGap == 1) && ($nIntoGap == 1)) {
	    push(@rStatus,"SPLINT.$outOfGapSplint.$intoGap");
	} else {
	    push(@rStatus,"CONFLICT");
	}
    }

    my $nReads = scalar(@reads);
    if ($nReads == 2) {
	my $read1 = $reads[0];
	my $readStatus1 = $rStatus[0];
	my $read2 = $reads[1];
	my $readStatus2 = $rStatus[1];

	unless (($readStatus1 eq "CONFLICT") || ($readStatus2 eq "CONFLICT")) {
	    my ($type1,$details1) = $readStatus1 =~ /^([A-Z]{6})\.(.+)$/;
	    my ($type2,$details2) = $readStatus2 =~ /^([A-Z]{6})\.(.+)$/;

	    my $printInfo1 = "";
	    if ($type1 eq "SPLINT") {
		my ($c1,$a1,$c2,$a2) = $details1 =~ /^(.+\.[35])\.(\d+)\.(.+\.[35])\.(\d+)$/;
		my @info1 = (split(/\t/,$alignments[$a1]))[@dataSlice];
		my @info2 = (split(/\t/,$alignments[$a2]))[@dataSlice];
		$printInfo1 = "$c1\t[@info1]\t$c2\t[@info2]";
	    } else {
		my ($c,$a) = $details1 =~ /^(.+\.[35])\.(\d+)$/;
		my @info = (split(/\t/,$alignments[$a]))[@dataSlice];
		$printInfo1 = "$c\t[@info]";
	    }

	    my $printInfo2 = "";
	    if ($type2 eq "SPLINT") {
		my ($c1,$a1,$c2,$a2) = $details2 =~ /^(.+\.[35])\.(\d+)\.(.+\.[35])\.(\d+)$/;
		my @info1 = (split(/\t/,$alignments[$a1]))[@dataSlice];
		my @info2 = (split(/\t/,$alignments[$a2]))[@dataSlice];
		$printInfo2 = "$c1\t[@info1]\t$c2\t[@info2]";
	    } else {
		my ($c,$a) = $details2 =~ /^(.+\.[35])\.(\d+)$/;
		my @info = (split(/\t/,$alignments[$a]))[@dataSlice];
		$printInfo2 = "$c\t[@info]";
	    }

	    print "PAIR\t$type1.$type2\t$printInfo1\t$printInfo2\n";

	}

    } elsif ($nReads == 1) {
	my $read = $reads[0];
	my $readStatus = $rStatus[0];
	unless ($readStatus eq "CONFLICT") {
	    my ($type,$details) = $readStatus =~ /^([A-Z]{6})\.(.+)$/;
	    if ($type eq "SPLINT") {
		my ($c1,$a1,$c2,$a2) = $details =~ /^(.+\.[35])\.(\d+)\.(.+\.[35])\.(\d+)$/;
		my @info1 = (split(/\t/,$alignments[$a1]))[@dataSlice];
		my @info2 = (split(/\t/,$alignments[$a2]))[@dataSlice];
		print "SINGLE\t$type\t$c1\t[@info1]\t$c2\t[@info2]\n";
	    } else {
		my ($c,$a) = $details =~ /^(.+\.[35])\.(\d+)$/;
		my @info = (split(/\t/,$alignments[$a]))[@dataSlice];
		print "SINGLE\t$type\t$c\t[@info]\n";
	    }
	}
    }
}
