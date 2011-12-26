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
# oNo.pl by Jarrod Chapman <jchapman@lbl.gov> Tue May 26 09:02:36 PDT 2009
# Copyright 2009 Jarrod Chapman. All rights reserved.
#

use Getopt::Std;
my %opts = ();
my $validLine = getopts('b:f:m:i:p:g:c:', \%opts);
my @required = ("b","f","i","m","g");
my $nRequired = 0;
map {$nRequired += exists($opts{$_})} @required;
$validLine &= ($nRequired == @required);
if ($validLine != 1) {
    print "Usage: ./oNo.pl <-m merSize> <-b blastMapFile> <-f fastaFile> <-i insertSize> <-g gapModelFile> <<-p pairThrehold>> <<-c minContigSize>>\n";
    exit;
}

my $pairThreshold = 1;
if (exists($opts{"p"})) {
    $pairThreshold = $opts{"p"};
}

my $minContigSize = 1;
if (exists($opts{"c"})) {
    $minContigSize = $opts{"c"};
}

my $merSize = $opts{"m"};
my $blastMapFile = $opts{"b"};
my $fastaFile = $opts{"f"};
my $gapModelFile = $opts{"g"};
my $insertSize = $opts{"i"};

my $endDistance = 1.20*$insertSize;

# Input is reads blastMapped to contig ends and contig fastaFile
# Output is scaffolds

print STDERR "Reading $fastaFile...\n";

my %contigSequence = ();
my %contigLengths = ();
my $currentEntry = undef;
my $seq = "";
my $seqLength = length($seq);
open (F,$fastaFile) || die "Couldn't open $fastaFile\n";
while (my $line = <F>) {
    chomp $line;
    if ($line =~ /^>/) {
	$seqLength = length($seq);
	if (defined($currentEntry) && ($seqLength >= $minContigSize)) {
	    $contigSequence{$currentEntry} = $seq;
	    $contigLengths{$currentEntry} = $seqLength;
	}
	$seq = "";
	($currentEntry) = $line =~ />(\S+)/;
    } else {
#	$seq .= uc($line);
	$seq .= $line;
    }
}
close F;

$seqLength = length($seq);
if (defined($currentEntry) && ($seqLength >= $minContigSize)) {
    $contigSequence{$currentEntry} = $seq;
    $contigLengths{$currentEntry} = $seqLength;
}

print STDERR "Done.\n";

print STDERR "Reading gap model file $gapModelFile.\n";
open (G,$gapModelFile) || die "Couldn't open $gapModelFile\n";
my %gapTable = ();
my $minNetOffset = undef;
my $maxNetOffset = undef;
my %offsetTable = ();
while (my $line = <G>) {
    my ($gap,$netOffset) = split(/\s+/,$line);
    $gapTable{$gap} = $netOffset;
    unless (defined($minNetOffset)) {
	$minNetOffset = $netOffset;
	$maxNetOffset = $netOffset;
    } else {
	if ($netOffset > $maxNetOffset) {
	    $maxNetOffset = $netOffset;
	}
	if ($netOffset < $minNetOffset) {
	    $minNetOffset = $netOffset;
	}
    }
    my $closeOffset = sprintf("%d",$netOffset);
    if (exists($offsetTable{$closeOffset})) {
	my ($minGap,$maxGap) = $offsetTable{$closeOffset} =~ /^(\-?\d+)\:(\-?\d+)$/;
	if ($gap < $minGap) {
	    $minGap = $gap;
	}
	if ($gap > $maxGap) {
	    $maxGap = $gap;
	}
	$offsetTable{$closeOffset} = "$minGap:$maxGap";
    } else {
	$offsetTable{$closeOffset} = "$gap:$gap";
    }
}
close G;	

print STDERR "Done.\nReading blastMapFile $blastMapFile.\n";

my %splints = ();
my %strongLinks = ();
my %weakLinks = ();
my %links = ();

open (B,$blastMapFile) || die "Couldn't open $blastMapFile\n";
while (my $line = <B>) {
    chomp $line;
    my @cols = split(/\t/,$line);

    my ($pairStatus,$linkType) = ($cols[0],$cols[1]);
    if ($pairStatus eq "SINGLE") {
	if ($linkType eq "SPLINT") {
	    my $link = processSplint(@cols[2,3,4,5]);
	}
    } elsif ($pairStatus eq "PAIR") {
	unless ($linkType =~ /SPLINT/) {
	    processPair(@cols[2,3,4,5]);
	} else {
	    my ($type1,$type2) = $linkType =~ /^(.+)\.(.+)$/;
	    if (($type1 eq "SPLINT") && ($type2 ne "SPLINT")) {
		my $link = processSplint(@cols[2,3,4,5]);
		if ($link) {
		    my ($e1,$e2) = $link =~ /^(.+)\=\>(.+)$/;
		    if ($e2 eq $cols[6]) {
			if ($e1 eq $cols[2]) {
			    processPair(@cols[2,3,6,7]);
			} else {
			    processPair(@cols[4,5,6,7]);
			}			    
		    }
		}
	    } elsif (($type1 ne "SPLINT") && ($type2 eq "SPLINT")) {
		my $link = processSplint(@cols[4,5,6,7]);
		if ($link) {
		    my ($e1,$e2) = $link =~ /^(.+)\=\>(.+)$/;
		    if ($e2 eq $cols[2]) {
			if ($e1 eq $cols[4]) {
			    processPair(@cols[2,3,4,5]);
			} else {
			    processPair(@cols[2,3,6,7]);
			}			    
		    }
		}
	    } else {
		my $link1 = processSplint(@cols[2,3,4,5]);
		my $link2 = processSplint(@cols[6,7,8,9]);
	    }
	}
    }
}
close B;
print STDERR "Done.\n";

my %endTies = ();

foreach my $link (keys(%links)) {
    my @data = @{$links{$link}};
    my $nLinks = scalar(@data);
    unless ($nLinks > $pairThreshold) {
	next;
    }
    
    my ($end1,$end2) = $link =~ /(.+)\<\=\>(.+)/;
    my $netSeparation = 0;
    map {$netSeparation += $_} @data;

    my $meanOffset = $insertSize - ($netSeparation/$nLinks);
    my $gapEstimate = findGapSize($meanOffset);
    
    if (exists($endTies{$end1})) {
#	push (@{$endTies{$end1}},"$nLinks.$netSeparation.$end2");
	push (@{$endTies{$end1}},"$nLinks.$gapEstimate.$end2");
    } else {
#	$endTies{$end1} = ["$nLinks.$netSeparation.$end2"];
	$endTies{$end1} = ["$nLinks.$gapEstimate.$end2"];
    }
    
    if (exists($endTies{$end2})) {
#	push (@{$endTies{$end2}},"$nLinks.$netSeparation.$end1");
	push (@{$endTies{$end2}},"$nLinks.$gapEstimate.$end1");
    } else {
#	$endTies{$end2} = ["$nLinks.$netSeparation.$end1"];
	$endTies{$end2} = ["$nLinks.$gapEstimate.$end1"];
    }
}

#my %test = ();
#$test{"Contig1756.5"} = 1;

my %endLocks = ();
my $gapUncertainty = 0.2*$insertSize;
foreach my $end (keys(%endTies)) {

#    my $debug = 0;
#    if (exists($test{$end})) {
#	$debug = 1;
#    }
    
    print STDERR "$end\t@{$endTies{$end}}\n";
    
    if (exists($endLocks{$end})) {
	next;
    } 

    my @possibilities = @{$endTies{$end}};
    my $nPossible = scalar(@possibilities);    
    my $bestPath = undef;

    if ($nPossible == 1) {
	$bestPath = $possibilities[0];
    } elsif ($nPossible == 2) {
	my %choices = ();
	foreach my $possibility (@possibilities) {
	    my ($nLinks,$gapEstimate,$nextEnd) = $possibility =~ /(\d+)\.(\-?\d+)\.(.+)/;
	    my ($nextContig) = $nextEnd =~ /^(.+)\.[35]$/;
	    my $nextContigLength = $contigLengths{$nextContig};
	    $choices{$possibility} = [$nLinks,$gapEstimate,$nextContigLength];
	}
	my @sortedChoices = sort {$choices{$b}->[0] <=> $choices{$a}->[0]} keys(%choices);
	my ($n1,$g1,$l1) = @{$choices{$sortedChoices[0]}};
	my ($n2,$g2,$l2) = @{$choices{$sortedChoices[1]}};
	if ($g2 > ($l1+$g1-$gapUncertainty)) {
	    $bestPath = $sortedChoices[0];
	} else {
	    $endLocks{$end} = "DIVERGENCE";
	    next;
	}
    } else {
	$endLocks{$end} = "DIVERGENCE";
	next;
    }

    my ($nLinks,$gapEstimate,$nextEnd) = $bestPath =~ /(\d+)\.(\-?\d+)\.(.+)/;
    my @backPossibilities = @{$endTies{$nextEnd}};
    my $nBack = scalar(@backPossibilities);
    my $bestPathBack = undef;

    if ($nBack == 1) {
	$bestPathBack = $backPossibilities[0];
    } elsif ($nBack == 2) {
	my %choices = ();
	foreach my $possibility (@backPossibilities) {
	    my ($nBackLinks,$backGapEstimate,$backEnd) = $possibility =~ /(\d+)\.(\-?\d+)\.(.+)/;
	    my ($backContig) = $backEnd =~ /^(.+)\.[35]$/;
	    my $backContigLength = $contigLengths{$backContig};
	    $choices{$possibility} = [$nBackLinks,$backGapEstimate,$backContigLength];
	}
	my @sortedChoices = sort {$choices{$b}->[0] <=> $choices{$a}->[0]} keys(%choices);
	my ($n1,$g1,$l1) = @{$choices{$sortedChoices[0]}};
	my ($n2,$g2,$l2) = @{$choices{$sortedChoices[1]}};
	if ($g2 > ($l1+$g1-$gapUncertainty)) {
	    $bestPathBack = $sortedChoices[0];
	} else {
	    $endLocks{$end} = "CONVERGENCE";
	    $endLocks{$nextEnd} = "DIVERGENCE";
	    next;
	}
    } else {
	$endLocks{$end} = "CONVERGENCE";
	$endLocks{$nextEnd} = "DIVERGENCE";
	next;
    }
    
#    my ($nBackLinks,$netBackSeparation,$backEnd) = $bestPathBack =~ /(\d+)\.(\-?\d+)\.(.+)/;
    my ($nBackLinks,$backGapEstimate,$backEnd) = $bestPathBack =~ /(\d+)\.(\-?\d+)\.(.+)/;
    if ($backEnd eq $end) {
#	my $gap = sprintf("%d",$netSeparation/$nLinks);
	my $gap = sprintf("%d",$gapEstimate);
	$endLocks{$end} = "$nextEnd><$gap";
	$endLocks{$nextEnd} = "$end><$gap";
    } else {
	print STDERR "Inconsistent relation $end -> $nextEnd ; $nextEnd -> $backEnd\n";
    }
}

foreach my $end (keys(%endLocks)) {
    print STDERR "LOCK: $end $endLocks{$end}\n";
}

my $scaffoldId = 1;
my %visitedContigs = ();
foreach my $contig (keys(%contigSequence)) {
    my $startContig = $contig;
    my $startEnd = 5;
    
    if (exists($visitedContigs{$contig})) {
	next;
    }

    my $preState = "TERMINATION";
    my %loopCheck = ();
    while () {
	if (exists($loopCheck{$startContig})) {
	    last;
	}
	$loopCheck{$startContig} = 1;
	
	my $end = "$startContig.$startEnd";
	if (exists($endLocks{$end})) {
	    my $next = $endLocks{$end};
	    if (($next eq "CONVERGENCE") || ($next eq "DIVERGENCE")) {
		$preState = $next;
		last;
	    } else {
		($startContig,$startEnd) = $next =~ /(.+)\.([35])\>\</;
		$startEnd = ($startEnd == 3) ? 5 : 3;
		$preState = "EXTENSION";
	    }
	} else {
	    $preState = "TERMINATION";
	    last;
	}
    }
    
    %loopCheck = ();
    
    print STDERR "Scaffold$scaffoldId: $preState $startContig ($startEnd) ";
    
    my $scaffoldSeq = "";
    my $nextContig = $startContig;
    my $nextEnd = ($startEnd == 3) ? 5 : 3;
    my $nextGap = 0;
    my $prevContig = $nextContig;
    my $prevEnd = $nextEnd;
    
    while () {
	if (exists($loopCheck{$nextContig})) {
	    print STDERR "$nextContig LOOP\n";
	    last;
	}
	$loopCheck{$nextContig} = 1;	    
	$visitedContigs{$nextContig} = 1;
	
	my $contigSeq = $contigSequence{$nextContig};
	if ($nextEnd == 5) {
	    $contigSeq = reverse($contigSeq);
#	    $contigSeq =~ tr/ACGT/TGCA/;
	    $contigSeq =~ tr/ACGTacgt/TGCAtgca/;
	}
	
	if ($scaffoldSeq) {
	    my $end1 = substr($scaffoldSeq,-$merSize);
	    my $end2 = substr($contigSeq,0,$merSize);
	    my $oldNextEnd = ($nextEnd == 3) ? 5 : 3;
	    print STDERR "\nGAP:\tScaffold$scaffoldId\t$prevContig.$prevEnd\t$end1\t$nextGap\t$nextContig.$oldNextEnd\t$end2\n";
	}
	
	$scaffoldSeq = spliceSeq($scaffoldSeq,$contigSeq,$nextGap);
	
	my $end = "$nextContig.$nextEnd";
	if (exists($endLocks{$end})) {
	    my $next = $endLocks{$end};
	    if (($next eq "CONVERGENCE") || ($next eq "DIVERGENCE")) {
		print STDERR "$nextContig ($nextEnd) $next\n";
		last;
	    } else {
		print STDERR "$nextContig ($nextEnd) ";
		($prevContig,$prevEnd) = ($nextContig,$nextEnd);
		($nextContig,$nextEnd,$nextGap) = $next =~ /(.+)\.([35])\>\<(.+)/;
		print STDERR "[$nextGap] $nextContig ($nextEnd) ";
		$nextEnd = ($nextEnd == 3) ? 5 : 3;
	    }
	} else {
	    print STDERR "$nextContig ($nextEnd) TERMINATION\n";
	    last;
	}
    }
    
    printFasta("Scaffold$scaffoldId",$scaffoldSeq);
    $scaffoldId++;
    
}

# ---------------
# | SUBROUTINES |
# ---------------

sub processPair {
    my ($end1,$a1,$end2,$a2) = @_;

    my ($contig1) = $end1 =~ /^(.+)\.[35]$/;
    my ($contig2) = $end2 =~ /^(.+)\.[35]$/;

    unless (exists($contigLengths{$contig1}) && exists($contigLengths{$contig2}) && ($contig1 ne $contig2)) {
	return;
    }

    my $strongLink = 1;

    my ($align) = $a1 =~ /^\[(.+)\]$/;
    my ($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
    if ($rStat =~ /TRUNC/) {
	$strongLink = 0;
    }
    my $d1 = undef;
    if ($strand eq "Plus") {
	$d1 = ($cL-$c0+1)+($r0-1);
    } else {
	$d1 = $c1+($r0-1);
    }
    ($align) = $a2 =~ /^\[(.+)\]$/;
    ($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
    if ($rStat =~ /TRUNC/) {
	$strongLink = 0;
    }
    my $d2 = undef;
    if ($strand eq "Plus") {
	$d2 = ($cL-$c0+1)+($r0-1);
    } else {
	$d2 = $c1+($r0-1);
    }
    
    unless (defined($d1) && defined($d2)) {
	print STDERR "Error comprehending: [$a1] [$a2]\n";
    } else {
	
	unless ($d1 < $endDistance && $d2 < $endDistance) {
	    return;
	}

	my $endSeparation = $insertSize - ($d1+$d2);
	my $link = ($end1 lt $end2) ? "$end1<=>$end2" : "$end2<=>$end1";

	if (exists($links{$link})) {
	    push(@{$links{$link}},$endSeparation);
	} else {
	    $links{$link} = [$endSeparation];
	}	

	if ($strongLink) {
	    $strongLinks{$link}++;
	} else {
	    $weakLinks{$link}++;
	}
    }
}


sub processSplint {
    my ($end1,$a1,$end2,$a2) = @_;

    my $returnVal = "";

    my ($contig1) = $end1 =~ /^(.+)\.[35]$/;
    my ($contig2) = $end2 =~ /^(.+)\.[35]$/;

    unless (exists($contigLengths{$contig1}) && exists($contigLengths{$contig2}) && ($contig1 ne $contig2)) {
	return $returnVal;
    }

    my ($align) = $a1 =~ /^\[(.+)\]$/;
    my ($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
    my $rightCoord = undef;
    my $leftCoord = undef;
    if ($rStat =~ /^GAP/) {

	$returnVal = "$end2=>$end1";

	if ($strand eq "Plus") {
	    $rightCoord = $r0-($c0-1);
	} else {
	    $rightCoord = $r0-($cL-$c1); 
	}
    } else {

	$returnVal = "$end1=>$end2";

	if ($strand eq "Plus") {
	    $leftCoord = $r1+($cL-$c1);
	} else {
	    $leftCoord = $r1+($c0-1); 
	}
    }
    
    ($align) = $a2 =~ /^\[(.+)\]$/;
    ($rStat,$r,$r0,$r1,$rL,$c,$c0,$c1,$cL,$strand) = split(/\s+/,$align);
    if ($rStat =~ /^GAP/) {
	if ($strand eq "Plus") {
	    $rightCoord = $r0-($c0-1);
	} else {
	    $rightCoord = $r0-($cL-$c1); 
	}
    } else {
	if ($strand eq "Plus") {
	    $leftCoord = $r1+($cL-$c1);
	} else {
	    $leftCoord = $r1+($c0-1); 
	}
    }
    
    unless (defined($rightCoord) && defined($leftCoord)) {
	print STDERR "Error comprehending: [$a1] [$a2]\n";
    } else {
	my $endSeparation = $rightCoord - $leftCoord - 1;
	my $link = ($end1 lt $end2) ? "$end1<=>$end2" : "$end2<=>$end1";
	
	$splints{$link}++;
	if (exists($links{$link})) {
	    push(@{$links{$link}},$endSeparation);
	} else {
	    $links{$link} = [$endSeparation];
	}
    }

    return $returnVal;

}

sub spliceSeq {
    my ($baseSeq,$addSeq,$gap) = @_;

    unless ($baseSeq) {
	return $addSeq;
    }

    my $minGap = 10;
    my $gapSeq = "";
    if ($gap < $minGap) {
	$gapSeq = "N"x$minGap;
    } else {
	$gapSeq = "N"x$gap;
    }

    my $returnSeq = $baseSeq.$gapSeq.$addSeq;

    return $returnSeq;
}

sub printFasta {
    my ($name,$seq) = @_;
    my $seqLen = length($seq);

    my $bpl = 50;
    my $nLines = sprintf("%d",$seqLen/$bpl);
    if ($seqLen%$bpl != 0) {
        $nLines++;
    }

    print ">$name\n";
    for (my $j = 0;$j<$nLines;$j++) {
        my $seqLine = substr($seq,$j*$bpl,$bpl);
        my $text = $seqLine;
        $text.= "\n";
        print $text;
    }
}


sub findGapSize {
    my ($netOffset) = @_;

    my $baseLine = sprintf("%d",$insertSize-$netOffset);

    my $checkOffset = $netOffset;
    if ($netOffset < $minNetOffset) {
	$checkOffset = $minNetOffset;
    } elsif ($netOffset > $maxNetOffset) {
	$checkOffset = $maxNetOffset;
    }

    my $intOffset = sprintf("%d",$checkOffset);
    unless (exists($offsetTable{$intOffset})) {
	warn "findGapSize fails for net offset $netOffset ($intOffset) using baseline gap estimate $baseLine\n";
	return $baseLine;
    }

    my ($minGap,$maxGap) = $offsetTable{$intOffset} =~ /^(\-?\d+)\:(\-?\d+)$/;
    my $bestGap = $minGap;
    my $bestDelta = abs($netOffset-$gapTable{$bestGap});
    for (my $g = $minGap; $g <= $maxGap; $g++) {
	my $d = abs($netOffset-$gapTable{$g});
	if ($d < $bestDelta) {
	    $bestDelta = $d;
	    $bestGap = $g;
	}
    }

    my $checkBaseLine = sprintf("%d",$insertSize-$checkOffset);
    my $correction = $bestGap-$checkBaseLine;
    my $returnGap = $baseLine+$correction;
#    my $returnGap = $baseLine;

#    print STDERR "findGapSize $netOffset : $baseLine -> $returnGap\n";

    return $returnGap;
}
